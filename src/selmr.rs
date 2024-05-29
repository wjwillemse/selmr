use crate::text_structs::{Text, TextCounter, TextMap};
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
use crate::hac::HAC;

/// Struct containing all parameters of a SELMR data structure
#[derive(Deserialize, Serialize, Debug, Clone)]
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

impl Default for Params {
    fn default() -> Self {
        Self::minimal()
    }
}

impl Params {
    /// the minimal default params set
    pub fn minimal() -> Self {
        Params {
            min_phrase_len: 1,
            max_phrase_len: 3,
            min_left_context_len: 1,
            max_left_context_len: 3,
            min_right_context_len: 1,
            max_right_context_len: 3,
            min_phrase_keys: 2,
            min_context_keys: 2,
            language: "en".to_string(),
        }
    }
    /// the params set for test purposes
    pub fn test() -> Self {
        Params{
            min_phrase_len: 1,
            max_phrase_len: 3,
            min_left_context_len: 1,
            max_left_context_len: 3,
            min_right_context_len: 1,
            max_right_context_len: 3,
            min_phrase_keys: 1,
            min_context_keys: 1,
            language: "en".to_string(),
        }
    }
}

/// The SELMR data structure
#[derive(Deserialize, Clone)]
pub struct SELMR {
    /// the phrases to contexts map
    pub phrases: TextMap,
    /// the contexts to phrases map
    #[serde(skip)]
    pub contexts: TextMap,
    /// the phrases tree clusters
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phrases_tree: Option<HashMap::<usize, (Option<(usize, usize)>, Option<usize>)>>,
    /// the contexts tree clusters
    #[serde(skip_serializing_if = "Option::is_none")]
    pub contexts_tree: Option<HashMap::<usize, (Option<(usize, usize)>, Option<usize>)>>,
    /// the parameters of the SELMR struct
    pub params: Params,
}

impl Default for SELMR {
    fn default() -> Self {
        Self::new()
    }
}

#[inline]
fn count_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &HashSet<&T>, 
    b: &HashSet<&T>
) -> f32 {
    a.intersection(b).collect::<Vec<_>>().len() as f32
}

#[inline]
fn jaccard_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &HashSet<&T>, 
    b: &HashSet<&T>
) -> f32 {
    a.intersection(b).collect::<Vec<_>>().len() as f32 / max(a.len(), b.len()) as f32
}

#[inline]
fn weighted_jaccard_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &IndexMap<&T, usize>,
    b: &IndexMap<&T, usize>,
) -> f32 {
    let mut num: f32 = 0.0;
    let mut denom: f32 = 0.0;
    for context in a
        .keys()
        .collect::<HashSet<_>>()
        .union(&b.keys().collect())
    {
        num += *min(a.get(*context).unwrap_or(&0), b.get(*context).unwrap_or(&0)) as f32;
        denom += *max(a.get(*context).unwrap_or(&0), b.get(*context).unwrap_or(&0)) as f32;
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
            // phrases: PhraseMap::new(),
            phrases: TextMap::new(),
            // contexts: ContextMap::new(),
            contexts: TextMap::new(),
            phrases_tree: None,
            contexts_tree: None,
            params: Params::minimal(),
        }
    }
    /// Write SELMR data structure to a file
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new();
    /// ```
    pub fn write(
        &self, 
        file: &str, 
        format: &str
    ) -> Result<(), std::io::Error> {
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
    /// ```
    pub fn read(
        &mut self, 
        file: &str, 
        format: &str
    ) -> Result<(), std::io::Error> {
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
    pub fn merge(
        &mut self, 
        other: &SELMR
    ) {
        // merge the content of s with that of the current struct
        self.phrases.merge(&other.phrases);
        self.contexts = self.phrases.swap();
        // to do: perform actual merge instead of replace
        self.phrases_tree = other.phrases_tree.clone();
        self.contexts_tree = other.contexts_tree.clone();
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
        params: Option<&Params>,
    ) {
        if params.is_some() {
            self.params = params.unwrap().clone();
        }
        let binding = text.to_string();
        let text = &tokenizer::tokenize(&binding);
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
        let mut unordered_phrases = HashMap::<Text, Counter<Text>>::new();
        // let mut unordered_contexts = HashMap::<Text, Counter<Text>>::new();
        for (phrase, phrase_contexts) in phrases.iter() {
            let p = Text::word(&phrase.join(" "));
            for ((left, right), n) in phrase_contexts.iter() {
                let c = Text::context(&(format!("{} ... {}", left.join(" "), right.join(" "))));
                unordered_phrases.entry(p.clone()).or_default()[&c] = *n;
                // unordered_contexts.entry(c).or_default()[&p] = *n;
            }
        }
        // create the phrases with ordered ContextCounters
        let mut other = SELMR::new();
        for (phrase, phrase_contexts) in unordered_phrases.iter() {
            let ordered_phrase_contexts: IndexMap<Text, usize> =
                phrase_contexts.most_common_ordered().into_iter().collect();
            other.phrases.map.insert(
                phrase.clone(),
                TextCounter {
                    map: ordered_phrase_contexts,
                },
            );
        }
        self.merge(&other);
    }
    /// Returns the most similar phrases based on common contexts
    ///
    /// # Example 1
    ///
    /// ```
    /// use selmr::selmr::{SELMR, Params};
    /// use selmr::text_structs::Text;
    ///
    /// let params = Params::test();
    /// let mut s = SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", Some(&params));
    /// let actual = s.most_similar(Text::word("2"), None, Some(25), Some(15), "count").unwrap();
    /// let expect = [(Text::word("2"), 5.0), (Text::word("1"), 1.0)].iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    /// # Example 2
    ///
    /// ```
    /// use selmr::selmr::{SELMR, Params};
    /// use selmr::text_structs::Text;
    ///
    /// let params = Params::test();
    /// let mut s = SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", Some(&params));
    /// let actual = s.most_similar(Text::word("2"), None, Some(25), Some(15), "jaccard").unwrap();
    /// let expect = [(Text::word("2"), 1.0), (Text::word("1"), 0.2)].iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    /// # Example 3
    ///
    /// ```
    /// use selmr::selmr::{SELMR, Params};
    /// use selmr::text_structs::Text;
    ///
    /// let params = Params::test();
    /// let mut s = SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", Some(&params));
    /// let actual = s.most_similar(Text::word("2"), None, Some(25), Some(15), "weighted_jaccard").unwrap();
    /// let expect = [(Text::word("2"), 1.0), (Text::word("1"), 0.16666667)].iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    pub fn most_similar(
        &self,
        text: Text,
        constraint: Option<Text>,
        multiset_topn: Option<usize>,
        topn: Option<usize>,
        measure: &str,
    ) -> Result<Vec<(Text, f32)>, String> {
        if let Some(multiset) = self.get_multiset(&text, multiset_topn) {
            self.most_similar_from_multiset(
                &multiset,
                constraint,
                multiset_topn,
                topn,
                measure,
            )
        } else {
            Err(format!("Text not found: {}", text))
        }
    }
    /// Find the most similar phrases from a given multiset
    pub fn most_similar_from_multiset(
        &self,
        multiset: &IndexMap<&Text, usize>,
        constraint: Option<Text>,
        multiset_topn: Option<usize>,
        topn: Option<usize>,
        measure: &str,
    ) -> Result<Vec<(Text, f32)>, String> {
        match self.get_to_evaluate(multiset, constraint, multiset_topn) {
            Ok(to_evaluate) => self.most_similar_from_text(
                multiset,
                &to_evaluate,
                multiset_topn,
                topn,
                measure,
            ),
            Err(e) => Err(e),
        }
    }
    /// Find all phrases that fit a multiset of contexts
    pub fn get_to_evaluate(
        &self,
        multiset: &IndexMap<&Text, usize>,
        constraint: Option<Text>,
        multiset_topn: Option<usize>,
    ) -> Result<HashSet<&Text>, String> {
        let mut to_evaluate: HashSet<&Text> = HashSet::new();
        match constraint {
            Some(constraint) => {
                if let Some(items) = self.get_multiset(&constraint, multiset_topn) {
                    to_evaluate.extend(items.keys());
                }
            }
            None => {
                for item in multiset.keys() {
                    if let Some(items) = self.get_multiset(item, multiset_topn) {
                        to_evaluate.extend(items.keys());
                    }
                }
            }
        }
        Ok(to_evaluate)
    }
    /// Find most similar phrases from a list of phrases to evaluate
    pub fn most_similar_from_text(
        &self,
        multiset: &IndexMap<&Text, usize>,
        to_evaluate: &HashSet<&Text>,
        multiset_topn: Option<usize>,
        topn: Option<usize>,
        measure: &str,
    ) -> Result<Vec<(Text, f32)>, String> {
        let mut result = Vec::<(Text, f32)>::new();
        match measure {
            "count" | "jaccard" => {
                let set_b: HashSet<&Text> = multiset.keys().cloned().collect();
                for text in to_evaluate {
                    let set_a = &self
                        .get_multiset(text, multiset_topn)
                        .unwrap()
                        .keys()
                        .cloned()
                        .collect();
                    let value: f32 = match measure {
                        "count" => count_index(set_a, &set_b),
                        "jaccard" => jaccard_index(set_a, &set_b),
                        _ => 0.0,
                    };
                    result.push(((*text).clone(), value));
                }
            }
            "weighted_jaccard" => {
                // iterate over first n phrases that fit context c
                for text in to_evaluate {
                    if let Some(multiset_a) = &self.get_multiset(text, multiset_topn) {
                        let value: f32 = weighted_jaccard_index(multiset_a, multiset);
                        result.push(((*text).clone(), value));
                    } else {
                        return Err(format!("Text not found: {}", text))
                    }
                }
            }
            m => return Err(format!("Measure not implemented: {}", m)),
        }
        // sort result based on measure
        result.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        // select topn results
        match topn {
            Some(topn) => {
                Ok(result[..min(result.len(), topn)].to_vec())
            },
            None => { 
                Ok(result)
            }
        }
    }
    /// Returns the multiset of a text element
    ///
    /// # Example
    ///
    /// ```
    /// use indexmap::IndexMap;
    /// use selmr::selmr::{SELMR, Params};
    /// use selmr::text_structs::Text;
    ///
    /// let params = Params::test();
    /// let mut s = SELMR::new();
    /// s.add("a 1 b c. a 2 b c. a 2 b d.", Some(&params));
    /// let actual = s.get_multiset(&Text::word("1"), Some(15)).unwrap();
    /// let context = Text::context("a ... b");
    /// let expect = [(&context, &1)].iter().cloned().map(|(p, n)|(p, *n)).collect::<IndexMap<_, _>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    #[inline]
    /// Retrieve the multiset of a given text element
    pub fn get_multiset(
        &self,
        text: &Text,
        n: Option<usize>,
    ) -> Option<IndexMap<&Text, usize>> {
        match text {
            Text::Word(_) => self.phrases.get(text, n),
            Text::Context(_) => self.contexts.get(text, n)
        }
    }
    /// Returns the multiset of a list of text elements
    pub fn get_list_multiset(
        &self,
        texts: &Vec<Text>,
        n: Option<usize>,
    ) -> Result<HashMap<Text, usize>, String> {
        let mut result = HashMap::<Text, usize>::new();
        for text in texts {
            if let Some(es) = match text {
                Text::Word(_) => self.phrases.get(text, n),
                Text::Context(_) => self.contexts.get(text, n) } {
                for (&e, &n) in &es {
                    *result.entry(e.clone()).or_insert(0) += n;
                }
            } else {
                return Err(format!("Text not found: {}", text));
            }
        }
        Ok(result)
    }
    /// Prunes a SELMR data structure
    pub fn prune(
        &mut self, 
        topn_phrases: usize, 
        topn_contexts: usize
    ) {
        for (phrase, contexts) in &mut self.phrases.map {
            let topn = min(contexts.map.len(), topn_phrases);
            for (context, count) in contexts.map.drain(topn..) {
                *self.contexts.map.entry(context).or_default()
                    .map.entry(phrase.clone()).or_insert(0) -= count;
            }
        }
        for (context, phrases) in &mut self.contexts.map {
            let topn = min(phrases.map.len(), topn_contexts);
            for (phrase, count) in phrases.map.drain(topn..) {
                *self.phrases.map.entry(phrase).or_default()
                    .map.entry(context.clone()).or_insert(0) -= count;
            }
        }
        for (_, contexts) in &mut self.phrases.map {
            contexts.map.retain(|_, n| *n > 0);
        }
        // for (_, phrases) in &mut self.contexts.map {
        //     phrases.map.retain(|_, n| *n > 0);
        // }
        self.phrases.map.retain(|_, v|!v.map.is_empty());
        // self.contexts.map.retain(|_, v|v.map.len() > 0);
        // reorder contexts so that when an object is read it has the same order
        self.contexts = self.phrases.swap()
    }

    /// Filters the phrase-context combinations on given regexes
    pub fn phrase_context_matches(
        &self,
        phrase_regex: &str,
        context_regex: &str,
    ) -> Option<HashMap<(String, String), usize>> {
        let re_c = Regex::new(context_regex).unwrap();
        let re_p = Regex::new(phrase_regex).unwrap();
        let mut results = HashMap::<(String, String), usize>::new();
        for p in self.phrases.map.keys() {
            for _result_p in re_p.find_iter(&p.to_string()) {
                if let Some(result_phrases) = self.phrases.map.get(&Text::context(_result_p.as_str())) {
                    for (c, count) in &result_phrases.map {
                        for _result_c in re_c.find_iter(&c.to_string()) {
                            results.insert(
                                (
                                    _result_p.as_str().to_string(),
                                    _result_c.as_str().to_string(),
                                ),
                                *count,
                            );
                        }
                    }
                } else {
                    println!("{:?}", _result_p);
                }
            }
        }
        Some(results)
    }

    /// Filters the phrases on given regex
    pub fn phrase_matches(
        &self,
        phrase: &str,
    ) -> Option<HashMap<String, usize>> {
        let re_p = Regex::new(phrase).unwrap();
        let mut results = HashMap::<String, usize>::new();
        for (p, cs) in &self.phrases.map {
            for _result_p in re_p.find_iter(&p.to_string()) {
                results.insert(
                    _result_p.as_str().to_string(),
                    cs.map.values().sum(),
                );
            }
        }
        Some(results)
    }
    /// Filters the phrases on given regex
    pub fn context_matches(
        &self,
        context: &str,
    ) -> Option<HashMap<String, usize>> {
        let re_c = Regex::new(context).unwrap();
        let mut results = HashMap::<String, usize>::new();
        for (c, ps) in &self.contexts.map {
            for _result_c in re_c.find_iter(&c.to_string()) {
                results.insert(
                    _result_c.as_str().to_string(),
                    ps.map.values().sum(),
                );
            }
        }
        Some(results)
    }
    /// Lemmatize phrase based on SELMR data
    pub fn lemmatize(
        &self,
        phrase: &str
    ) -> Option<Vec<(String, String, String)>> {

        let mut results = Vec::<(String, String, String)>::new();

        let simple_rules = Vec::from([
            // personal pronouns
            ("me", "I", "personal pronoun", "me -> I"),
            ("him", "he", "personal pronoun", "him -> he"),
            ("her", "she", "personal pronoun", "her -> she"),
            ("us", "we", "personal pronoun", "us -> we"),
            ("them", "they", "personal pronoun", "them -> they"),
            // possessive pronouns
            ("my", "I", "possessive pronouns", "my -> I"),
            ("your", "you", "possessive pronouns", "your -> you"),
            ("his", "he", "possessive pronouns", "his -> he"),
            ("her", "she", "possessive pronouns", "her -> she"),
            ("its", "it", "possessive pronouns", "its -> it"),
            ("their", "they", "possessive pronouns", "their -> they"),
            ("our", "we", "possessive pronouns", "our -> we"),
            // irregular verbs
            ("is", "be", "verb", "irregular verb"),
            ("are", "be", "verb", "irregular verb"),
            ("was", "be", "verb", "irregular verb"),
            ("were", "be", "verb", "irregular verb"),
            ("been", "be", "verb", "irregular verb"),
            ("has", "have", "verb", "irregular verb"),
            ("have", "have", "verb", "irregular verb"),
            ("had", "have", "verb", "irregular verb"),
            ("began", "begin", "verb", "irregular verb"),
            ("begun", "begin", "verb", "irregular verb"),
            ("broke", "break", "verb", "irregular verb"),
            ("broken", "break", "verb", "irregular verb"),
            ("brought", "bring", "verb", "irregular verb"),
            ("bought", "buy", "verb", "irregular verb"),
            ("built", "build", "verb", "irregular verb"),
            ("chose", "choose", "verb", "irregular verb"),
            ("chosen", "choose", "verb", "irregular verb"),
            ("came", "come", "verb", "irregular verb"),
            ("did", "do", "verb", "irregular verb"),
            ("done", "do", "verb", "irregular verb"),
            ("drew", "draw", "verb", "irregular verb"),
            ("drawn", "draw", "verb", "irregular verb"),
            ("drove", "drive", "verb", "irregular verb"),
            ("driven", "drive", "verb", "irregular verb"),
            ("ate", "eat", "verb", "irregular verb"),
            ("eaten", "eat", "verb", "irregular verb"),
            ("felt", "feel", "verb", "irregular verb"),
            ("found", "find", "verb", "irregular verb"),
            ("fought", "fight", "verb", "irregular verb"),
            ("get", "got", "verb", "irregular verb"),
            ("gave", "give", "verb", "irregular verb"),
            ("given", "give", "verb", "irregular verb"),
            ("gone", "go", "verb", "irregular verb"),
            ("went", "go", "verb", "irregular verb"),
            ("heard", "hear", "verb", "irregular verb"),
            ("held", "hold", "verb", "irregular verb"),
            ("kept", "keep", "verb", "irregular verb"),
            ("knew", "know", "verb", "irregular verb"),
            ("known", "know", "verb", "irregular verb"),
            ("left", "leave", "verb", "irregular verb"),
            ("led", "lead", "verb", "irregular verb"),
            ("lay", "lie", "verb", "irregular verb"),
            ("lain", "lie", "verb", "irregular verb"),
            ("lost", "lose", "verb", "irregular verb"),
            ("made", "make", "verb", "irregular verb"),
            ("meant", "mean", "verb", "irregular verb"),
            ("met", "meet", "verb", "irregular verb"),
            ("paid", "pay", "verb", "irregular verb"),
            ("ran", "run", "verb", "irregular verb"),
            ("said", "say", "verb", "irregular verb"),
            ("saw", "see", "verb", "irregular verb"),
            ("seen", "see", "verb", "irregular verb"),
            ("sold", "sell", "verb", "irregular verb"),
            ("sent", "send", "verb", "irregular verb"),
            ("sat", "sit", "verb", "irregular verb"),
            ("spoke", "speak", "verb", "irregular verb"),
            ("spoken", "speak", "verb", "irregular verb"),
            ("spent", "spend", "verb", "irregular verb"),
            ("stood", "stand", "verb", "irregular verb"),
            ("took", "take", "verb", "irregular verb"),
            ("taken", "take", "verb", "irregular verb"),
            ("taught", "teach", "verb", "irregular verb"),
            ("told", "tell", "verb", "irregular verb"),
            ("thought", "think", "verb", "irregular verb"),
            ("understood", "understand", "verb", "irregular verb"),
            ("wore", "wear", "verb", "irregular verb"),
            ("worn", "wear", "verb", "irregular verb"),
            ("won", "win", "verb", "irregular verb"),
            ("wrote", "write", "verb", "irregular verb"),
            ("written", "write", "verb", "irregular verb"),
        ]);
        for (p, singular, rule, verbose) in &simple_rules {
            if phrase == *p {
                results.push((singular.to_string(), rule.to_string(), verbose.to_string()));
            }
        }
        if results.is_empty() {
            let hm = Vec::from([
                // proper and common noun rules
                ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])ies", "y", "noun", "-consonant and -y -> change -y to -ies"),
                ("([a-zA-Z][a-z]*[aeuoi])ys", "y", "noun", "-vowel and -y -> add an -s"),
                ("([a-zA-Z][a-z]*)ves", "f", "noun", "-f -> change to -ve and add an -s"),
                ("([a-zA-Z][a-z]*)ves", "fe", "noun", "-fe -> change to -ve and add an -s"),
                ("([a-zA-Z][a-z]*)i", "us", "noun", "-us -> replace with an -i"),
                ("([a-zA-Z][a-z]*)oes", "o", "noun", "-o -> add an -es"),
                ("([a-zA-Z][a-z]*)es", "is", "noun", "-is -> replace with an -es"),
                ("([a-zA-Z][a-z]*)a", "on", "noun", "-on -> replace with an -a"),
                ("([a-zA-Z][a-z]*)ses", "s", "noun", "‑s -> add an -es"),
                ("([a-zA-Z][a-z]*)xes", "s", "noun", "-x -> add an -es"),
                ("([a-zA-Z][a-z]*)zes", "s", "noun", "-z -> add an -es"),
                ("([a-zA-Z][a-z]*)sses", "ss", "noun", "‑ss -> add an -es"),
                ("([a-zA-Z][a-z]*)shes", "sh", "noun", "‑sh -> add an -es"),
                ("([a-zA-Z][a-z]*)ches", "ch", "noun", "‑ch -> add an -es"),
                ("([a-zA-Z][a-z]*)s", "", "noun", "_ -> add an -s"),
                // regular verb rules
                ("([a-z]+[aeuoi])d", "", "verb", "-vowel -> add an -d"),
                ("([a-z]+[b-df-hj-np-tv-z])ed", "", "verb", "-consonant -> add an -ed"),
                ("([a-z]+[b-df-hj-np-tv-z])ied", "y", "verb", "-consonent and -y -> add an -ied"),
                ("([a-z]+[b-df-hj-np-tv-z])[b-df-hj-np-tv-z]ed", "", "verb", "-consonant -> add an -ed"),
                ("([a-z]+)s", "", "verb", "_ -> add an -s"),
                ("([a-z]+)ches", "ch", "verb", "ch -> add an -es"),
                ("([a-z]+)shes", "sh", "verb", "sh -> add an -es"),
                ("([a-z]+)ses", "s", "verb", "s -> add an -es"),
                ("([a-z]+)xes", "x", "verb", "x -> add an -es"),
                ("([a-z]+)zes", "z", "verb", "z -> add an -es"),
                ("([a-z]+[b-df-hj-np-tv-z])ies", "y", "verb", "-consonant and y -> replace y with ies"),
                ("([a-z]+)ing", "a", "verb", "-a -> add an -ing"),
                ("([a-z]+)ing", "e", "verb", "-e -> add an -ing"),
                ("([a-z]+)ing", "u", "verb", "-u -> add an -ing"),
                ("([a-z]+)ing", "o", "verb", "-o -> add an -ing"),
                ("([a-z]+)ing", "i", "verb", "-i -> add an -ing"),
                ("([a-z]+[b-df-hj-np-tv-z])ing", "", "verb", "-consonant -> add an -ing"),
                ("([a-z]+[b-df-hj-np-tv-z])[b-df-hj-np-tv-z]ing", "", "verb", "-consonant -> add double consonant and -ing"),

            ]);
            for (regex, singular, rule, verbose) in &hm {
                let re_1 = Regex::new(format!("^{}$", regex).as_str()).unwrap();
                if let Some(caps) = re_1.captures(phrase) {
                    if let Some(cap) = caps.get(1) {
                        let singular_text = format!("{}{}", cap.as_str(), singular);
                        if self.phrases.map.contains_key(&Text::word(&singular_text)) {
                            results.push((singular_text.to_string(), rule.to_string(), verbose.to_string()));
                        }
                    } else {
                        let singular_text = singular.to_string();
                        if self.phrases.map.contains_key(&Text::word(&singular_text)) {
                            results.push((singular_text.to_string(), rule.to_string(), verbose.to_string()));
                        }
                    }
                }
            }
        }
        if results.is_empty() {
            results.push((phrase.to_string(), "none".to_string(), "none".to_string()));
        }
        Some(results)
    }
    /// Perform clustering of the phrases to create tree
    pub fn generate_trees(
        &mut self, 
        phrases_n: usize, 
        contexts_n: usize, 
        multiset_topn: usize
    ) {
        if phrases_n > 0 {
            let mut hac = HAC::new(self, &self.phrases, multiset_topn);
            let _ = hac.iterate(phrases_n);
            self.phrases_tree = Some(hac.get_tree().unwrap());
        }
        if contexts_n > 0 {
            let mut hac = HAC::new(self, &self.contexts, multiset_topn);
            let _ = hac.iterate(contexts_n);
            self.contexts_tree = Some(hac.get_tree().unwrap());
        }
    }
    /// Get the tree path of a text
    pub fn get_cluster_from_text(
        &self, 
        text: Text, 
        depth: usize
    ) -> Result<Vec<String>, String> {
        let textmap = match text {
            Text::Word(_) => &self.phrases,
            Text::Context(_) => &self.contexts,
        };
        let t = match text {
            Text::Word(_) => &self.phrases_tree,
            Text::Context(_) => &self.contexts_tree,
        };
        let tree;
        if let Some(tt) = t {
            tree = tt;
        } else {
            return Err("Error".to_string())
        }   
        let binding = textmap.map.get_index_of(&text.clone()).unwrap();
        let mut idx = Some(&binding);
        for _ in 0..depth {
            if let Some((_, Some(a))) = tree.get(idx.unwrap()) {
                // println!("{:?} -> {:?}", idx, leaves);
                idx = Some(a);
            } else {
                idx = None;
                break;
            }
        }
        let mut r = Vec::<usize>::new();
        if idx.is_some() {
            r = self.collect_leaves(tree, r, *idx.unwrap());
            let result = r.iter()
                .map(|idx|textmap.map.get_index(*idx).unwrap().0.to_string())
                .collect();
            Ok(result)
        } else {
            Err("depth not available".to_string())
        }
    }
    fn collect_leaves(
        &self, 
        tree: &HashMap::<usize, (Option<(usize, usize)>, Option<usize>)>, 
        mut leaves: Vec<usize>,
        idx: usize) -> Vec<usize> {
        if tree.contains_key(&idx) {
            if let Some((from, _)) = tree.get(&idx) {
                if let Some((a, b)) = from {
                    leaves = self.collect_leaves(tree, leaves, *a);
                    leaves = self.collect_leaves(tree, leaves, *b);
                } else {
                    leaves.push(idx);
                }
            }
        }
        leaves
    }
}

impl Serialize for SELMR {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut state = serializer.serialize_struct("selmr", 2)?;
        state.serialize_field("phrases", &self.phrases)?;
        state.serialize_field("phrases_tree", &self.phrases_tree)?;
        state.serialize_field("contexts_tree", &self.contexts_tree)?;
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
