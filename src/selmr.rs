use crate::text_structs::{Context, ContextCounter, ContextMap, Phrase, PhraseCounter, PhraseMap};
use crate::tokenizer;
use core::cmp::min;
use counter::Counter;
use indexmap::IndexMap;
use log::info;
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use regex::Regex;
use serde::{ser::SerializeStruct, Serialize, Serializer};
use std::collections::{HashMap, HashSet};
use std::io::prelude::*;
use std::{fs, io::Write, path};
use zip::{write::FileOptions, ZipArchive, ZipWriter};

#[pymodule]
fn pyselmr(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    pyo3_log::init();
    m.add_class::<SELMR>()?;
    Ok(())
}

/// Struct containing all parameters of a SELMR data structure
#[derive(Deserialize, Serialize, Debug)]
pub struct Params {
    min_phrase_len: usize,
    max_phrase_len: usize,
    min_context_len: usize,
    max_context_len: usize,
    min_phrase_keys: usize,
    min_context_keys: usize,
    language: String,
}

/// The SELMR data structure
#[derive(Deserialize)]
#[pyclass]
pub struct SELMR {
    phrases: PhraseMap,
    #[serde(skip)]
    contexts: ContextMap,
    params: Params,
}

// Core SELMR methods
#[pymethods]
impl SELMR {
    /// Initializes an empty SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// ```
    #[new]
    #[pyo3(signature = (
        min_phrase_len=1,
        max_phrase_len=3,
        min_context_len=1,
        max_context_len=3,
        min_phrase_keys=2,
        min_context_keys=2,
        language="en")
    )]
    pub fn new(
        min_phrase_len: usize,
        max_phrase_len: usize,
        min_context_len: usize,
        max_context_len: usize,
        min_phrase_keys: usize,
        min_context_keys: usize,
        language: &str,
    ) -> Self {
        SELMR {
            phrases: PhraseMap::new(),
            contexts: ContextMap::new(),
            params: Params {
                min_phrase_len,
                max_phrase_len,
                min_context_len,
                max_context_len,
                min_phrase_keys,
                min_context_keys,
                language: language.to_string(),
            },
        }
    }

    /// Write SELMR data structure to a file
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// s.write("test.json", "json").expect("REASON");
    /// ```
    pub fn write(&self, file: &str, format: &str) -> Result<(), PyErr> {
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
                        Err(_) => {
                            return Err(PyErr::new::<PyTypeError, _>(
                                "error closing finishing writing file in zip file",
                            ))
                        }
                    },
                    Err(_) => {
                        return Err(PyErr::new::<PyTypeError, _>(
                            "error writing to file in zip file",
                        ))
                    }
                },
                Err(_) => {
                    return Err(PyErr::new::<PyTypeError, _>(
                        "data.json not found in zip file",
                    ))
                }
            }
        }
        Ok(())
    }

    /// Read SELMR data structure to a file
    ///
    /// # Example
    ///
    /// ```
    /// let mut s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// s.read("test.json", "json").expect("REASON");
    /// ```
    pub fn read(&mut self, file: &str, format: &str) -> Result<(), PyErr> {
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
                        Err(_) => return Err(PyErr::new::<PyTypeError, _>("Unable to parse")),
                    }
                }
                Err(e) => return Err(PyErr::new::<PyTypeError, _>(e)),
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
                            return Err(PyErr::new::<PyTypeError, _>(e.to_string()));
                        }
                    }
                }
                Err(..) => {
                    return Err(PyErr::new::<PyTypeError, _>(
                        "data.json not found in zipfile.",
                    ));
                }
            };
        }
        // the self.contexts is derived from self.phrases, and must be ordered
        if let Some(selmr) = s {
            *self = selmr;
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
        Ok(())
    }

    /// add text data to SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// ```
    pub fn add<'a>(&mut self, text: &str) {
        let binding = text.to_string();
        let text = &tokenizer::tokenize(&binding);

        info!(target: "selmr", "adding {} tokens", text.len());

        let mut phrases = HashMap::<&'a [&'a str], Counter<(&'a [&'a str], &'a [&'a str])>>::new();
        for l_len in self.params.min_context_len..self.params.max_context_len + 1 {
            for r_len in self.params.min_context_len..self.params.max_context_len + 1 {
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
                            if l.len() > 1 {
                                if let Some(cs) = phrases.get(p) {
                                    if let Some(context_count) = cs.get(&(&l[1..], r)) {
                                        if *context_count != *count {
                                            phrases.entry(p).or_default()[&(*l, *r)] = *count;
                                        }
                                    }
                                }
                            }
                            if r.len() > 1 {
                                if let Some(cs) = phrases.get(p) {
                                    if let Some(context_count) = cs.get(&(l, &r[..r.len() - 1])) {
                                        if *context_count != *count {
                                            phrases.entry(p).or_default()[&(*l, *r)] = *count;
                                        }
                                    }
                                }
                            }
                            if l.len() > 1 && r.len() > 1 {
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
                            if l.len() == 1 && r.len() == 1 {
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
    /// # Example
    ///
    /// ```
    /// let mut s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// s.add("My sister lives in the city. My brother lives in the mountains.");
    /// let actual = s.most_similar("sister", None, 25, 25, 15).unwrap();
    /// let expect = [("brother".to_string(), 1), ("sister".to_string(), 1)]
    ///     .iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    #[pyo3(signature = (phrase, context=None, topcontexts=25, topphrases=25, topn=15))]
    pub fn most_similar(
        &self,
        phrase: &str,
        context: Option<(&str, &str)>,
        topcontexts: usize,
        topphrases: usize,
        topn: usize,
    ) -> Result<Vec<(String, usize)>, PyErr> {
        // the most_similar function returns the phrases with the most contexts in common with the input phrase
        // first: contexts of the input phrase
        let cs = self.phrases.map.get(&Phrase {
            value: String::from(phrase),
        });
        // the counter that will contain the results
        let mut most_similar = Counter::<Phrase>::new();
        match cs {
            Some(cs) => {
                // take the most common contexts of the input phrase
                let most_common_cs: HashSet<_> = cs.map[..min(cs.map.len(), topcontexts)]
                    .into_iter()
                    .map(|x| x.0)
                    .collect();
                match context {
                    // if there is an input context then retrieve phrases that fit that context
                    // the intersection between the context of the input phrase
                    // and the contexts of each phrase that fits the input context
                    Some(context) => {
                        let c = Context {
                            left_value: String::from(context.0),
                            right_value: String::from(context.1),
                        };
                        let len_p = min(self.contexts.map[&c].map.len(), topphrases);
                        let ps: HashSet<_> = self.contexts.map[&c].map[..len_p]
                            .into_iter()
                            .map(|x| x.0)
                            .collect();
                        for p in &ps {
                            let len_c = min(self.phrases.map[&p].map.len(), topcontexts);
                            let p_cs: HashSet<_> = self.phrases.map[&p].map[..len_c]
                                .into_iter()
                                .map(|x| x.0)
                                .collect();
                            most_similar[&p] =
                                p_cs.intersection(&most_common_cs).collect::<Vec<_>>().len();
                        }
                    }
                    None => {
                        // if there is not input context then
                        // for each context in the most common contexts cs update
                        // the counter with the phrases that fit in that context
                        let mut ps: HashSet<_> = HashSet::new();
                        for c in &most_common_cs {
                            let len_c = min(self.contexts.map[&c].map.len(), topphrases);
                            for p in self.contexts.map[&c].map[..len_c]
                                .into_iter()
                                .map(|x| x.0)
                                .collect::<Vec<_>>()
                            {
                                ps.insert(p);
                            }
                        }
                        for p in ps {
                            let len_p = min(self.phrases.map[&p].map.len(), topcontexts);
                            let p_cs: HashSet<_> = self.phrases.map[&p].map[..len_p]
                                .into_iter()
                                .map(|x| x.0)
                                .collect();
                            most_similar[&p] =
                                most_common_cs.intersection(&p_cs).collect::<Vec<_>>().len();
                        }
                    }
                }
                // the count of occurrences is not used because then
                // phrases with high occurrences are preferred in the output
            }
            None => {
                return Err(PyErr::new::<PyTypeError, _>(
                    "Phrase was not found when calling most_similar",
                ))
            }
        }
        Ok(most_similar
            .k_most_common_ordered(topn)
            .iter()
            .map(|(phrase, count)| (phrase.to_string(), *count))
            .collect())
    }

    /// Returns the most common contexts of a phrase
    ///
    /// # Example
    ///
    /// ```
    /// let mut s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// s.add("My sister lives in the city. My brother lives in the mountains.");
    /// let actual = s.get_contexts(Some("sister"), None, 15).unwrap();
    /// let expect = [("My ... lives".to_string(), 1)]
    ///     .iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    #[pyo3(signature = (phrase=None, phrases=None, topn=15))]
    pub fn get_contexts(
        &self,
        phrase: Option<&str>,
        phrases: Option<Vec<&str>>,
        topn: usize,
    ) -> Result<Vec<(String, usize)>, PyErr> {
        match phrase {
            Some(phrase) => {
                let contexts = self.phrases.map.get(&Phrase {
                    value: String::from(phrase),
                });
                match contexts {
                    Some(contexts) => Ok(contexts.map[..min(topn, contexts.map.len())]
                        .iter()
                        .map(|(context, count)| (context.to_string(), *count))
                        .collect()),
                    None => Err(PyErr::new::<PyTypeError, _>(
                        "Contexts of phrase not found: ".to_owned() + phrase,
                    )),
                }
            }
            None => match phrases {
                Some(_phrases) => {
                    let mut new_counter = Counter::<Context>::new();
                    for phrase in _phrases {
                        let contexts = self.phrases.map.get(&Phrase {
                            value: String::from(phrase),
                        });
                        match contexts {
                            Some(contexts) => {
                                let len_c = min(contexts.map.len(), topn);
                                let contexts = &contexts.map[..len_c];
                                for (context, count) in contexts.iter() {
                                    new_counter
                                        .entry(context.clone())
                                        .and_modify(|c| *c += count)
                                        .or_insert(*count);
                                }
                            }
                            None => {
                                return Err(PyErr::new::<PyTypeError, _>(
                                    "Contexts of phrase not found: ".to_owned() + phrase,
                                ))
                            }
                        }
                    }
                    let ordered_new_counter: IndexMap<Context, usize> =
                        new_counter.most_common_ordered().into_iter().collect();
                    Ok(ordered_new_counter[..min(new_counter.len(), topn)]
                        .iter()
                        .map(|(context, count)| (context.to_string(), *count))
                        .collect())
                }
                None => Err(PyErr::new::<PyTypeError, _>(
                    "No phrase(s) given when calling get_contexts",
                )),
            },
        }
    }

    /// Returns the most common phrases of a context
    ///
    /// # Example
    ///
    /// ```
    /// let mut s = selmr::selmr::SELMR::new(1, 3, 1, 3, 1, 1, "en");
    /// s.add("My sister lives in the city. My brother lives in the mountains.");
    /// let actual = s.get_phrases(Some(("My", "lives")), 15).unwrap();
    /// let expect = [("brother".to_string(), 1), ("sister".to_string(), 1)]
    ///     .iter().cloned().collect::<Vec<_>>();
    /// assert_eq!(actual, expect);
    /// ```
    #[pyo3(signature = (context=None, topn=15))]
    pub fn get_phrases(
        &self,
        context: Option<(&str, &str)>,
        topn: usize,
    ) -> Result<Vec<(String, usize)>, PyErr> {
        match context {
            Some(context) => {
                let phrases = self.contexts.map.get(&Context {
                    left_value: String::from(context.0),
                    right_value: String::from(context.1),
                });
                match phrases {
                    Some(phrases) => Ok(phrases.map[..min(phrases.map.len(), topn)]
                        .iter()
                        .map(|(phrase, count)| (phrase.to_string(), *count))
                        .collect()),
                    None => Err(PyErr::new::<PyTypeError, _>(
                        "Context not found when calling get_phrases",
                    )),
                }
            },
            None => Err(PyErr::new::<PyTypeError, _>(
                "No context given when calling get_phrases",
            )),
        }
    }

    /// Prunes a SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// ```
    #[pyo3(signature = (topn_phrases=50, topn_contexts=50))]
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
    ///
    /// # Example
    ///
    /// ```
    /// ```
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
