use crate::text_structs::{Context, ContextCounter, ContextMap, Phrase, PhraseCounter, PhraseMap};
use crate::tokenizer;
use core::cmp::{max, min};
use counter::Counter;
use indexmap::IndexMap;
use log::info;
use regex::Regex;
use serde::{ser::SerializeStruct, Serialize, Serializer};
use std::collections::{HashMap, HashSet};
use std::io::prelude::*;
use std::{fs, io::Write, path};
use zip::{write::FileOptions, ZipArchive, ZipWriter};

/// Struct containing all parameters of a SELMR data structure
#[derive(Deserialize, Serialize, Debug)]
pub struct Params {
    /// minimum number of words in a phrase
    pub min_phrase_len: usize,
    /// maximum number of words in a phrase
    pub max_phrase_len: usize,
    /// minimum number of words in the left part of a context
    pub min_left_context_len: usize,
    /// maximum number of words in the left part of a context
    pub max_left_context_len: usize,
    /// minimum number of words in the right part of a context
    pub min_right_context_len: usize,
    /// maximum number of words in the right part of a context
    pub max_right_context_len: usize,
    /// minimum number of contexts in which a phrase must fit
    pub min_phrase_keys: usize,
    /// minimum number of phrases that must fit into a context
    pub min_context_keys: usize,
    /// the language of the phrases and contexts
    pub language: String,
}

/// The SELMR data structure
#[derive(Deserialize)]
pub struct SELMR {
    /// the phrases to contexts map
    pub phrases: PhraseMap,
    /// the contexts to phrases map
    #[serde(skip)]
    pub contexts: ContextMap,
    /// the parameters of the SELMR struct
    pub params: Params,
}

impl Default for SELMR {
    fn default() -> Self {
        Self::new()
    }
}

#[inline]
fn count_index<T: std::cmp::Eq + std::hash::Hash>(a: &HashSet<&T>, b: &HashSet<&T>) -> f32 {
    a.intersection(b).collect::<Vec<_>>().len() as f32
}

#[inline]
fn jaccard_index<T: std::cmp::Eq + std::hash::Hash>(a: &HashSet<&T>, b: &HashSet<&T>) -> f32 {
    a.intersection(b).collect::<Vec<_>>().len() as f32 / max(a.len(), b.len()) as f32
}

#[inline]
fn weighted_jaccard_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &HashMap<&T, &usize>,
    b: &HashMap<&T, &usize>,
) -> f32 {
    let mut num: f32 = 0.0;
    let mut denom: f32 = 0.0;
    for context in a
        .keys()
        .collect::<HashSet<_>>()
        .union(&b.keys().collect())
    {
        num += *min(*(a.get(*context).unwrap_or(&&0)), *(b.get(*context).unwrap_or(&&0))) as f32;
        denom += *max(*(a.get(*context).unwrap_or(&&0)), *(b.get(*context).unwrap_or(&&0))) as f32;
    }
    if denom != 0.0 {
        num / denom
    } else {
        0.0
    }
}

// Core SELMR methods
impl SELMR {
    /// Initializes an empty SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new();
    /// ```
    pub fn new() -> Self {
        SELMR {
            phrases: PhraseMap::new(),
            contexts: ContextMap::new(),
            params: Params {
                min_phrase_len: 1,
                max_phrase_len: 3,
                min_left_context_len: 1,
                max_left_context_len: 3,
                min_right_context_len: 1,
                max_right_context_len: 3,
                min_phrase_keys: 5,
                min_context_keys: 5,
                language: "en".to_string(),
            },
        }
    }

    /// Write SELMR data structure to a file
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new();
    /// s.write("test.json", "json").expect("REASON");
    /// ```
    pub fn write(&self, file: &str, format: &str) -> Result<(), std::io::Error> {
        let mut f = fs::File::create(file)?;
        if format == "json" {
            let s = serde_json::to_string_pretty(&self).unwrap();
            write!(f, "{}", s).unwrap();
        } else if format == "zip" {
            let mut zip = ZipWriter::new(f);
            let s = serde_json::to_string_pretty(&self).unwrap();
            match zip.start_file("data.json", FileOptions::default()) {
                Ok(()) => match zip.write_all(s.as_bytes()) {
                    Ok(()) => match zip.finish() {
                        Ok(_) => return Ok(()),
                        Err(e) => return Err(e.into()),
                    },
                    Err(e) => return Err(e),
                },
                Err(e) => return Err(e.into()),
            }
        }
        Ok(())
    }

    /// Read SELMR data structure to a file
    ///
    /// # Example
    ///
    /// ```
    /// let mut s = selmr::selmr::SELMR::new();
    /// s.read("test.json", "json").expect("REASON");
    /// ```
    pub fn read(&mut self, file: &str, format: &str) -> Result<(), std::io::Error> {
        let mut s: Option<SELMR> = None;
        if format == "json" {
            let data = fs::read_to_string(file);
            match data {
                Ok(data) => {
                    let from_str = serde_json::from_str(&data);
                    match from_str {
                        Ok(from_str) => {
                            s = from_str;
                        }
                        Err(e) => return Err(e.into()),
                    }
                }
                Err(e) => return Err(e),
            }
        } else if format == "zip" {
            let path = path::Path::new(file);
            let zip_file = fs::File::open(path)?;
            let mut archive = ZipArchive::new(zip_file).unwrap();
            match archive.by_name("data.json") {
                Ok(mut file_in_zip) => {
                    let mut data = String::new();
                    file_in_zip.read_to_string(&mut data).unwrap();
                    let from_str = serde_json::from_str(&data);
                    match from_str {
                        Ok(from_str) => {
                            s = from_str;
                        }
                        Err(e) => {
                            return Err(e.into());
                        }
                    }
                }
                Err(e) => {
                    return Err(e.into());
                }
            };
        }
        if let Some(selmr) = s {
            self.merge(&selmr);
        }
        Ok(())
    }

    /// merge the context of another SELMR struct into the current one
    pub fn merge(&mut self, other: &SELMR) {
        // merge the content of s with that of the current struct
        for (new_p, new_contexts) in other.phrases.map.iter() {
            for (new_c, new_count) in new_contexts.map.iter() {
                self.phrases
                    .map
                    .entry(new_p.clone())
                    .or_default()
                    .map
                    .entry(new_c.clone())
                    .and_modify(|count| *count += new_count)
                    .or_insert(*new_count);
            }
        }
        self.create_and_order_contexts();
    }

    fn create_and_order_contexts(&mut self) {
        // the self.contexts is derived from self.phrases, and must be ordered
        let mut unordered_contexts = HashMap::<Context, Counter<Phrase>>::new();
        for (p, contexts) in self.phrases.map.iter() {
            for (c, &n) in contexts.map.iter() {
                unordered_contexts
                    .entry(c.clone())
                    .or_default()
                    .insert(p.clone(), n);
            }
        }
        self.contexts = ContextMap::new();
        for (context, context_phrases) in unordered_contexts.iter() {
            let ordered_context_phrases: IndexMap<Phrase, usize> =
                context_phrases.most_common_ordered().into_iter().collect();
            self.contexts.map.insert(
                context.clone(),
                PhraseCounter {
                    map: ordered_context_phrases,
                },
            );
        }
    }

    /// add text data to SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new();
    /// ```
    pub fn add<'a>(
        &mut self,
        text: &str,
        min_phrase_len: usize,
        max_phrase_len: usize,
        min_left_context_len: usize,
        max_left_context_len: usize,
        min_right_context_len: usize,
        max_right_context_len: usize,
        min_phrase_keys: usize,
        min_context_keys: usize,
        language: &str,
    ) {
        let binding = text.to_string();
        let text = &tokenizer::tokenize(&binding);
        self.params = Params {
            min_phrase_len,
            max_phrase_len,
            min_left_context_len,
            max_left_context_len,
            min_right_context_len,
            max_right_context_len,
            min_phrase_keys,
            min_context_keys,
            language: language.to_string(),
        };
        info!(target: "selmr", "adding {} tokens", text.len());
        let mut phrases = HashMap::<&'a [&'a str], Counter<(&'a [&'a str], &'a [&'a str])>>::new();
        for l_len in self.params.min_left_context_len..self.params.max_left_context_len + 1 {
            for r_len in self.params.min_right_context_len..self.params.max_right_context_len + 1 {
                let mut new_phrases =
                    HashMap::<&'a [&'a str], Counter<(&'a [&'a str], &'a [&'a str])>>::new();
                let mut new_contexts =
                    HashMap::<(&'a [&'a str], &'a [&'a str]), Counter<&'a [&'a str]>>::new();
                for p_len in self.params.min_phrase_len..self.params.max_phrase_len + 1 {
                    for (l, p, r) in (ContextPhraseWindowGenerator {
                        text,
                        l_len,
                        p_len,
                        r_len,
                    }) {
                        new_phrases
                            .entry(p)
                            .or_default()
                            .entry((l, r))
                            .and_modify(|count| *count += 1)
                            .or_insert(1);
                        new_contexts
                            .entry((l, r))
                            .or_default()
                            .entry(p)
                            .and_modify(|count| *count += 1)
                            .or_insert(1);
                    }
                }
                new_phrases.retain(|_, cs| cs.len() >= self.params.min_phrase_keys);
                new_contexts.retain(|_, ps| ps.len() >= self.params.min_context_keys);
                let new_contexts: HashSet<_> = new_contexts.keys().cloned().collect();
                for (p, ps) in new_phrases.iter() {
                    for ((l, r), count) in ps.iter() {
                        if new_contexts.contains(&(*l, *r)) {
                            if l.len() > self.params.min_left_context_len {
                                if let Some(cs) = phrases.get(p) {
                                    if let Some(context_count) = cs.get(&(&l[1..], r)) {
                                        if *context_count != *count {
                                            phrases.entry(p).or_default()[&(*l, *r)] = *count;
                                        }
                                    }
                                }
                            }
                            if r.len() > self.params.min_right_context_len {
                                if let Some(cs) = phrases.get(p) {
                                    if let Some(context_count) = cs.get(&(l, &r[..r.len() - 1])) {
                                        if *context_count != *count {
                                            phrases.entry(p).or_default()[&(*l, *r)] = *count;
                                        }
                                    }
                                }
                            }
                            if l.len() > self.params.min_left_context_len
                                && r.len() > self.params.min_right_context_len
                            {
                                if let Some(cs) = phrases.get(p) {
                                    if let Some(context_count) =
                                        cs.get(&(&l[1..], &r[..r.len() - 1]))
                                    {
                                        if *context_count != *count {
                                            phrases.entry(p).or_default()[&(*l, *r)] = *count;
                                        }
                                    }
                                }
                            }
                            if l.len() == self.params.min_left_context_len
                                && r.len() == self.params.min_right_context_len
                            {
                                phrases.entry(p).or_default()[&(*l, *r)] = *count;
                            }
                        }
                    }
                }
            }
        }
        // construct the (unordered) phrases and contexts
        let mut unordered_phrases = HashMap::<Phrase, Counter<Context>>::new();
        let mut unordered_contexts = HashMap::<Context, Counter<Phrase>>::new();
        for (phrase, phrase_contexts) in phrases.iter() {
            let p = Phrase {
                value: phrase.join(" "),
            };
            for ((left, right), n) in phrase_contexts.iter() {
                let c = Context {
                    left_value: left.join(" "),
                    right_value: right.join(" "),
                };
                unordered_phrases.entry(p.clone()).or_default()[&c] = *n;
                unordered_contexts.entry(c).or_default()[&p] = *n;
            }
        }
        // create the phrases with ordered ContextCounters
        self.phrases = PhraseMap::new();
        for (phrase, phrase_contexts) in unordered_phrases.iter() {
            let ordered_phrase_contexts: IndexMap<Context, usize> =
                phrase_contexts.most_common_ordered().into_iter().collect();
            self.phrases.map.insert(
                phrase.clone(),
                ContextCounter {
                    map: ordered_phrase_contexts,
                },
            );
        }
        // create the contexts with ordered PhraseCounters
        self.contexts = ContextMap::new();
        for (context, context_phrases) in unordered_contexts.iter() {
            let ordered_context_phrases: IndexMap<Phrase, usize> =
                context_phrases.most_common_ordered().into_iter().collect();
            self.contexts.map.insert(
                context.clone(),
                PhraseCounter {
                    map: ordered_context_phrases,
                },
            );
        }
    }

    /// Returns the most similar phrases based on common contexts
    ///
    /// # Example 1
    ///
    /// ```
    /// use selmr::selmr::SELMR;
    /// use selmr::text_structs::Phrase;
    ///
    /// let mut s = SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", 1, 3, 1, 3, 1, 3, 1, 1, "");
    /// let actual = s.most_similar("2".to_string(), None, 25, 25, 15, "count").unwrap();
    /// let expect = [("2".to_string(), 5.0), ("1".to_string(), 1.0)].iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    /// # Example 2
    ///
    /// ```
    /// use selmr::selmr::SELMR;
    /// use selmr::text_structs::Phrase;
    ///
    /// let mut s = selmr::selmr::SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", 1, 3, 1, 3, 1, 3, 1, 1, "");
    /// let actual = s.most_similar("2".to_string(), None, 25, 25, 15, "jaccard").unwrap();
    /// let expect = [("2".to_string(), 1.0), ("1".to_string(), 0.2)].iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    /// # Example 3
    ///
    /// ```
    /// use selmr::selmr::SELMR;
    /// use selmr::text_structs::Phrase;
    ///
    /// let mut s = selmr::selmr::SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", 1, 3, 1, 3, 1, 3, 1, 1, "");
    /// let actual = s.most_similar("2".to_string(), None, 25, 25, 15, "weighted_jaccard").unwrap();
    /// let expect = [("2".to_string(), 1.0), ("1".to_string(), 0.16666667)].iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    pub fn most_similar(
        &self,
        phrase: String,
        context: Option<String>,
        topcontexts: usize,
        topphrases: usize,
        topn: usize,
        measure: &str,
    ) -> Result<Vec<(String, f32)>, String> {
        let p = Phrase::new(phrase.as_str());
        let c = context.map(|c|Context::new(c.as_str()));
        if let Some(multiset) = self.get_phrase_contexts(&p, topcontexts) {
            self.most_similar_from_multiset(
                &multiset,
                c,
                topcontexts,
                topphrases,
                topn,
                measure,
            )
        } else {
            Err(format!("Phrase not found: {}", phrase))
        }
    }

    /// Find the most similar phrases from a given multiset
    pub fn most_similar_from_multiset(
        &self,
        multiset: &HashMap<&Context, &usize>,
        context: Option<Context>,
        topcontexts: usize,
        topphrases: usize,
        topn: usize,
        measure: &str,
    ) -> Result<Vec<(String, f32)>, String> {
        match self.get_phrases_to_evaluate(&multiset, context, topphrases) {
            Ok(phrases_to_evaluate) => self.most_similar_from_phrases(
                multiset,
                &phrases_to_evaluate,
                topcontexts,
                topn,
                measure,
            ),
            Err(e) => Err(e),
        }
    }

    /// Find all phrases that fit a multiset of contexts
    pub fn get_phrases_to_evaluate(
        &self,
        input_phrase_multiset: &HashMap<&Context, &usize>,
        context: Option<Context>,
        topphrases: usize,
    ) -> Result<HashSet<&Phrase>, String> {
        // the most_similar function returns the phrases with the most contexts in common with the input phrase
        // first: contexts of the input phrase
        let mut phrases_to_evaluate: HashSet<&Phrase> = HashSet::new();
        match context {
            Some(context) => {
                // if there is an input context then
                // retrieve phrases that fit that context
                if let Some(items) = self.get_context_phrases(&context, topphrases) {
                    phrases_to_evaluate.extend(items.keys());
                }
            }
            None => {
                // if there is no input context then
                // for each context in the most common contexts cs update
                for c in input_phrase_multiset.keys() {
                    if let Some(items) = self.get_context_phrases(c, topphrases) {
                        phrases_to_evaluate.extend(items.keys());
                    }
                }
            }
        }
        Ok(phrases_to_evaluate)
    }

    /// Find most similar phrases from a list of phrases to evaluate
    pub fn most_similar_from_phrases(
        &self,
        input_phrase_multiset: &HashMap<&Context, &usize>,
        phrases_to_evaluate: &HashSet<&Phrase>,
        topcontexts: usize,
        topn: usize,
        measure: &str,
    ) -> Result<Vec<(String, f32)>, String> {
        let mut result = Vec::<(String, f32)>::new();
        match measure {
            "count" | "jaccard" => {
                let set_b: HashSet<&Context> = input_phrase_multiset.keys().cloned().collect();
                for phrase in phrases_to_evaluate {
                    let set_a = &self
                        .get_phrase_contexts(phrase, topcontexts)
                        .unwrap()
                        .keys()
                        .cloned()
                        .collect();
                    let value: f32 = match measure {
                        "count" => count_index(set_a, &set_b),
                        "jaccard" => jaccard_index(set_a, &set_b),
                        _ => 0.0,
                    };
                    result.push((phrase.to_string(), value));
                }
            }
            "weighted_jaccard" => {
                // iterate over first n phrases that fit context c
                for phrase in phrases_to_evaluate {
                    if let Some(multiset_a) = &self.phrases.get(phrase, topcontexts) {
                        let value: f32 = weighted_jaccard_index(multiset_a, input_phrase_multiset);
                        result.push((phrase.to_string(), value));
                    } else {
                        return Err(format!("Phrase not found: {}", phrase))
                    }
                }
            }
            m => return Err(format!("Measure not implemented: {}", m)),
        }
        // sort result based on measure
        result.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        // select topn results
        result = result[..min(result.len(), topn)].to_vec();
        Ok(result)
    }

    /// Returns the context multiset of a phrase
    ///
    /// # Example
    ///
    /// ```
    /// use std::collections::HashMap;
    /// use selmr::selmr::SELMR;
    /// use selmr::text_structs::{Phrase, Context};
    ///
    /// let mut s = selmr::selmr::SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", 1, 3, 1, 3, 1, 3, 1, 1, "");
    /// let actual = s.get_phrase_contexts(&Phrase::new("1"), 15).unwrap();
    /// let context = Context::new("a ... b");
    /// let expect = [(&context, &1)].iter().cloned().collect::<HashMap<_, _>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    pub fn get_phrase_contexts(
        &self,
        phrase: &Phrase,
        n: usize,
    ) -> Option<HashMap<&Context, &usize>> {
        self.phrases.get(phrase, n)
    }

    /// Returns the context multiset of a list of phrases
    pub fn get_phrases_contexts(
        &self,
        phrases: &Vec<Phrase>,
        n: usize,
    ) -> Result<HashMap<Context, usize>, String> {
        let mut result = HashMap::<Context, usize>::new();
        for phrase in phrases {
            if let Some(ps) = self.phrases.get(phrase, n) {
                for (&p, &count) in &ps {
                    *result.entry(p.clone()).or_insert(0) += count;
                }
            } else {
                return Err(format!("Phrase not found: {}", phrase));
            }
        }
        Ok(result)
    }

    /// Returns the phrase multiset of a context
    pub fn get_context_phrases(
        &self,
        context: &Context,
        n: usize,
    ) -> Option<HashMap<&Phrase, &usize>> {
        self.contexts.get(context, n)
    }

    /// Returns the phrase multiset of a list of contexts
    pub fn get_contexts_phrases(
        &self,
        contexts: &Vec<Context>,
        n: usize,
    ) -> Result<HashMap<Phrase, usize>, String> {
        let mut result = HashMap::<Phrase, usize>::new();
        for context in contexts {
            if let Some(cs) = self.contexts.get(context, n) {
                for (&c, &count) in &cs {
                    *result.entry(c.clone()).or_insert(0) += count;
                }
            } else {
                return Err(format!("Context not found: {}", context));
            }
        }
        Ok(result)
    }

    /// Prunes a SELMR data structure
    pub fn prune(&mut self, topn_phrases: usize, topn_contexts: usize) {
        let mut new_phrases = PhraseMap::new();
        let mut new_contexts = ContextMap::new();
        for p in self.phrases.map.keys() {
            let mut new_counter = ContextCounter {
                map: IndexMap::<Context, usize>::new(),
            };
            let len_p = min(self.phrases.map[&p.clone()].map.len(), topn_phrases);
            for (key, count) in &self.phrases.map[&p.clone()].map[..len_p] {
                new_counter.map.insert(key.clone(), *count);
            }
            new_phrases.map.insert(p.clone(), new_counter);
        }
        self.phrases = new_phrases;
        for c in self.contexts.map.keys() {
            let mut new_counter = PhraseCounter {
                map: IndexMap::<Phrase, usize>::new(),
            };
            let len_c = min(self.contexts.map[&c].map.len(), topn_contexts);
            for (key, count) in &self.contexts.map[&c].map[..len_c] {
                new_counter.map.insert(key.clone(), *count);
            }
            new_contexts.map.insert(c.clone(), new_counter);
        }
        self.contexts = new_contexts;
    }

    /// Filters a SELMR data structure on given regexes
    pub fn matches(
        &self,
        tuple: (&str, &str, &str),
    ) -> Option<HashMap<(String, String, String), usize>> {
        let (left, phrase, right) = tuple;
        let re_l = Regex::new(left).unwrap();
        let re_p = Regex::new(phrase).unwrap();
        let re_r = Regex::new(right).unwrap();

        let mut results = HashMap::<(String, String, String), usize>::new();
        for p in self.phrases.map.keys() {
            for _result_p in re_p.find_iter(&p.value) {
                for (c, count) in &self.phrases.map[&p.clone()].map {
                    for _result_l in re_l.find_iter(&c.left_value) {
                        for _result_r in re_r.find_iter(&c.right_value) {
                            results.insert(
                                (
                                    _result_l.as_str().to_string(),
                                    _result_p.as_str().to_string(),
                                    _result_r.as_str().to_string(),
                                ),
                                *count,
                            );
                        }
                    }
                }
            }
        }
        Some(results)
    }
}

impl Serialize for SELMR {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("selmr", 2)?;
        state.serialize_field("phrases", &self.phrases)?;
        state.serialize_field("params", &self.params)?;
        state.end()
    }
}

type ContextPhraseTuple<'a> = (&'a [&'a str], &'a [&'a str], &'a [&'a str]);

/// Generator struct for ContextPhraseWindows
pub struct ContextPhraseWindowGenerator<'a> {
    text: &'a [&'a str],
    l_len: usize,
    p_len: usize,
    r_len: usize,
}

impl<'a> Iterator for ContextPhraseWindowGenerator<'a> {
    type Item = ContextPhraseTuple<'a>;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.l_len + self.r_len + self.p_len > self.text.len() {
            None
        } else {
            let ret = (
                &self.text[..self.l_len],
                &self.text[self.l_len..self.p_len + self.l_len],
                &self.text[self.l_len + self.p_len..self.l_len + self.p_len + self.r_len],
            );
            self.text = &self.text[1..];
            Some(ret)
        }
    }
}
