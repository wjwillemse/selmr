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

pub enum Measure {
    CountIndex,
    JaccardIndex,
    WeightedJaccardIndex,
}

#[inline]
fn count_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &HashSet<T>, 
    b: &HashSet<T>
) -> f32 {
    a.intersection(b).collect::<Vec<_>>().len() as f32
}

#[inline]
fn jaccard_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &HashSet<T>, 
    b: &HashSet<T>
) -> f32 {
    a.intersection(b).collect::<Vec<_>>().len() as f32 / max(a.len(), b.len()) as f32
}

#[inline]
fn weighted_jaccard_index<T: std::cmp::Eq + std::hash::Hash>(
    a: &IndexMap<T, usize>,
    b: &IndexMap<T, usize>,
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

/// The SELMR data structure
#[derive(Deserialize, Clone)]
pub struct SELMR {
    /// the phrases to contexts map
    pub phrases: TextMap,
    #[serde(skip)]
    pub phrase_roots: HashMap<Text, Text>,
    /// the root phrases to contexts map
    #[serde(skip)]
    pub root_phrases: HashMap<Vec<String>, Text>,
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
            phrases: TextMap::new(),
            phrase_roots: HashMap::new(),
            root_phrases: HashMap::new(),
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
        &mut self, 
        file: &str, 
        format: &str
    ) -> Result<(), std::io::Error> {
        self.phrases = self.phrases.combine_phrases();
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
                        Ok(_) => {
                            self.phrase_roots = HashMap::new();
                            for phrase in self.phrases.map.keys() {
                                match phrase {
                                    Text::Word(w) => {
                                        if let Some(tokens) = &w.tokens {
                                            self.phrase_roots.insert(Text::word(&w.value, None), phrase.clone());
                                            self.root_phrases.insert(tokens.clone(), Text::word(&w.value, None));
                                        }
                                    },
                                    Text::Context(_) => todo!()
                                }
                            }
                            self.phrases = self.phrases.derive_root_phrases();
                            return Ok(())
                        },
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
        // rebuild the phrase_roots
        self.phrase_roots = HashMap::new();
        for phrase in self.phrases.map.keys() {
            match phrase {
                Text::Word(w) => {
                    if let Some(tokens) = &w.tokens {
                        self.phrase_roots.insert(Text::word(&w.value, None), phrase.clone());
                        self.root_phrases.insert(tokens.clone(), Text::word(&w.value, None));
                    }
                },
                Text::Context(_) => todo!()
            }
        }
        self.phrases = self.phrases.derive_root_phrases();
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
        let sentences = self.text_tokens(&binding);
        info!(target: "selmr", "adding {} tokens", text.len());
        let mut phrases = HashMap::<&'a [&'a str], Counter<(&'a [&'a str], &'a [&'a str])>>::new();
        for l_len in self.params.min_left_context_len..self.params.max_left_context_len + 1 {
            for r_len in self.params.min_right_context_len..self.params.max_right_context_len + 1 {
                let mut new_phrases =
                    HashMap::<&'a [&'a str], Counter<(&'a [&'a str], &'a [&'a str])>>::new();
                let mut new_contexts =
                    HashMap::<(&'a [&'a str], &'a [&'a str]), Counter<&'a [&'a str]>>::new();
                for p_len in self.params.min_phrase_len..self.params.max_phrase_len + 1 {
                    for text in sentences.iter() {
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
        // construct the phrases textmap
        let mut untokenized = SELMR::new();
        for (phrase, multiset) in phrases.iter() {
            let phrase = Text::word(&phrase.join(" "), None);
            let mut new_multiset = IndexMap::<Text, usize>::new();
            for ((left, right), n) in multiset.iter() {
                let c = Text::context(&left.join(" "), &right.join(" "));
                *new_multiset.entry(c.clone()).or_default() = *n;
            }
            untokenized.phrases.map.insert(
                phrase.clone(), 
                TextCounter {
                    map: new_multiset,
                }
            );
        }
        let mut tokenized = SELMR::new();
        for (phrase, multiset) in untokenized.phrases.map.iter() {
            let mut new_multiset = IndexMap::<Text, usize>::new();
            for (c, n) in multiset.map.iter() {
                *new_multiset.entry(c.clone()).or_default() = *n;
            }
            let new_phrase = &untokenized.phrases.tokenize_phrase(phrase)[0];
            tokenized.phrases.map.insert(
                new_phrase.clone(), 
                TextCounter {
                    map: new_multiset,
                }
            );
        }
        self.merge(&tokenized);
    }
    ///
    pub fn get_multiset(
        &self,
        text: &Text,
        n: Option<usize>,
        literal: bool,
    ) -> Option<IndexMap<Text, usize>> {
        match text {
            Text::Word(_) => {
                if !literal {
                    if let Some(r) = &self.phrase_roots.get(text) {
                        match r {
                            Text::Word(w) => {
                                if let Some(tokens) = &w.tokens {
                                    let root = &Text::word(&tokens[0], None);
                                    let suffix = &tokens[1];
                                    self.phrases.get_filtered_multiset(root, suffix, n)
                                } else {
                                    self.phrases.get_multiset(text, n)
                                }
                            },
                            Text::Context(_) => todo!()
                        }
                    } else {
                        // tokenized word is not found
                        self.phrases.get_multiset(text, n)
                    }
                } else {
                    self.phrases.get_multiset(text, n)
                }
            },
            Text::Context(_) => {
                if text.to_string().contains('-') {
                    let suffix = text.clone().remove_to_right();
                    let multiset = self.contexts.get_multiset(text, n);
                    let mut new_multiset = IndexMap::<Text, usize>::new();
                    for (root, n) in multiset.clone().unwrap() {
                        let w = root.to_string();
                        let tokens = Vec::from(
                            [w.clone(), suffix.clone()]
                        );
                        if let Some(r) = &self.root_phrases.get(&tokens) {
                            new_multiset.insert(r.clone().clone(), n);
                        }
                    }
                    Some(new_multiset)
                } else {
                    self.contexts.get_multiset(text, n)
                }
            },
        }
    }
    /// Returns the multiset of a list of text elements
    pub fn get_list_multiset(
        &self,
        texts: &Vec<Text>,
        n: Option<usize>,
        literal: bool
    ) -> Result<HashMap<Text, usize>, String> {
        let mut result = HashMap::<Text, usize>::new();
        for text in texts {
            if let Some(es) = self.get_multiset(text, n, literal) {
                for (e, n) in es {
                    *result.entry(e.clone()).or_insert(0) += n;
                }
            } else {
                return Err(format!("Text not found: {}", text));
            }
        }
        Ok(result)
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
        measure: Measure,
        literal: bool
    ) -> Result<Vec<(Text, f32)>, String> {
        if let Some(multiset) = self.get_multiset(&text, multiset_topn, literal) {
            self.most_similar_from_multiset(
                &multiset,
                constraint,
                multiset_topn,
                topn,
                measure,
                literal,
            )
        } else {
            Err(format!("Text not found: {}", text))
        }
    }
    /// Find the most similar phrases from a given multiset
    pub fn most_similar_from_multiset(
        &self,
        multiset: &IndexMap<Text, usize>,
        constraint: Option<Text>,
        multiset_topn: Option<usize>,
        topn: Option<usize>,
        measure: Measure,
        literal: bool,
    ) -> Result<Vec<(Text, f32)>, String> {
        match self.get_to_evaluate(multiset, constraint, multiset_topn, literal) {
            Ok(to_evaluate) => {
                self.most_similar_from_text(
                multiset,
                &to_evaluate,
                multiset_topn,
                topn,
                measure,
                literal,
            )},
            Err(e) => Err(e),
        }
    }
    /// Find all phrases that fit a multiset of contexts
    pub fn get_to_evaluate(
        &self,
        multiset: &IndexMap<Text, usize>,
        constraint: Option<Text>,
        multiset_topn: Option<usize>,
        literal: bool,
    ) -> Result<HashSet<Text>, String> {
        let mut to_evaluate: HashSet<Text> = HashSet::new();
        match constraint {
            Some(constraint) => {
                if let Some(items) = self.get_multiset(&constraint, multiset_topn, literal) {
                    to_evaluate.extend(items.into_keys());
                }
            }
            None => {
                for item in multiset.keys() {
                    if let Some(items) = self.get_multiset(item, multiset_topn, literal) {
                        to_evaluate.extend(items.into_keys());
                    }
                }
            }
        }
        Ok(to_evaluate)
    }
    /// Find most similar phrases from a list of phrases to evaluate
    pub fn most_similar_from_text(
        &self,
        multiset: &IndexMap<Text, usize>,
        to_evaluate: &HashSet<Text>,
        multiset_topn: Option<usize>,
        topn: Option<usize>,
        measure: Measure,
        literal: bool,
    ) -> Result<Vec<(Text, f32)>, String> {
        let mut result = Vec::<(Text, f32)>::new();
        match measure {
            Measure::CountIndex | Measure::JaccardIndex => {
                let set_b: HashSet<Text> = multiset.clone().into_keys().collect();
                for text in to_evaluate {
                    let set_a = &self
                        .get_multiset(text, multiset_topn, literal).unwrap()
                        .into_keys()
                        .collect();
                    let value: f32 = match measure {
                        Measure::CountIndex => count_index(set_a, &set_b),
                        Measure::JaccardIndex => jaccard_index(set_a, &set_b),
                        _ => 0.0,
                    };
                    result.push(((*text).clone(), value));
                }
            }
            Measure::WeightedJaccardIndex => {
                // iterate over first n phrases that fit context c
                for text in to_evaluate {
                    if let Some(multiset_a) = &self.get_multiset(text, multiset_topn, literal) {
                        let value: f32 = weighted_jaccard_index(multiset_a, multiset);
                        result.push(((*text).clone(), value));
                    } else {
                        return Err(format!("Text not found: {}", text))
                    }
                }
            }
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
        self.phrases.sort();
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
                if let Some(result_phrases) = self.phrases.map.get(&Text::extract(_result_p.as_str())) {
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
    /// tokenize text
    pub fn text_tokens<'a>(
        &'a self,
        text: &'a str) -> Vec<Vec<&str>>{
        let sentences = tokenizer::tokenize(text);
        sentences
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
