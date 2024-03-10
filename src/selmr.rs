use crate::text_structs::{Context, ContextCounter, ContextMap, Phrase, PhraseCounter, PhraseMap};
use crate::tokenizer;
use counter::Counter;
use postcard::{from_bytes, to_stdvec};
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use regex::Regex;
use serde::ser::SerializeStruct;
use serde::Serialize;
use serde::Serializer;
use std::collections::{HashMap, HashSet};
use std::{fs, io::Write};

#[pymodule]
fn selmr(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<SELMR>()?;
    m.add_class::<Phrase>()?;
    m.add_class::<Context>()?;
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
    /// use selmr::SELMR;
    ///
    /// let s = SELMR();
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
    /// let s = SELMR();
    /// s.write("test.json", format="json")
    /// ```
    pub fn write(&self, path: &str, format: &str) -> Result<(), PyErr> {
        let mut f = fs::File::create(path)?;
        if format == "json" {
            let s = serde_json::to_string_pretty(&self).unwrap();
            write!(f, "{}", s).unwrap();
        } else if format == "postcard" {
            let s = to_stdvec(&self).unwrap();
            let _ = f.write_all(&s);
        }
        Ok(())
    }

    /// Read SELMR data structure to a file
    ///
    /// # Example
    ///
    /// ```
    /// let s = SELMR();
    /// s.read("test.json", format="json")
    /// ```
    pub fn read(&mut self, path: &str, format: &str) -> Result<(), PyErr> {
        let mut s = None;
        if format == "json" {
            let data = fs::read_to_string(path);
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
        } else if format == "postcard" {
            let data = fs::read(path);
            match data {
                Ok(data) => {
                    let from_bytes = from_bytes(&data);
                    match from_bytes {
                        Ok(from_bytes) => {
                            s = from_bytes;
                        }
                        Err(_) => return Err(PyErr::new::<PyTypeError, _>("Unable to parse")),
                    }
                }
                Err(e) => return Err(PyErr::new::<PyTypeError, _>(e)),
            }
        }
        if let Some(selmr) = s {
            *self = selmr;
            for (p, contexts) in self.phrases.map.iter() {
                for (c, &n) in contexts.map.iter() {
                    self.contexts
                        .map
                        .entry(c.clone())
                        .or_insert(PhraseCounter {
                            map: Counter::<Phrase>::new(),
                        })
                        .map[&p] += n;
                }
            }
        }
        Ok(())
    }

    /// add text data to SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = SELMR();
    /// ```
    pub fn add(&mut self, text: &str) {
        let binding = text.to_string();
        let tokenized_text = tokenizer::tokenize(&binding);
        let doc_phrases = ContextPhraseCounter::new(&tokenized_text, &self.params);
        for ((left, phrase, right), &n) in doc_phrases.value.iter() {
            let p = Phrase {
                value: phrase.join(" "),
            };
            let c = Context {
                left_value: left.join(" "),
                right_value: right.join(" "),
            };
            self.phrases
                .map
                .entry(p.clone())
                .or_insert(ContextCounter {
                    map: Counter::<Context>::new(),
                })
                .map[&c] += n;
            self.contexts
                .map
                .entry(c.clone())
                .or_insert(PhraseCounter {
                    map: Counter::<Phrase>::new(),
                })
                .map[&p] += n;
        }
    }

    /// Returns the most similar phrases based on common contexts
    ///
    /// # Example
    ///
    /// ```
    /// let s = SELMR();
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
                let most_common_cs: HashSet<_> = cs
                    .map
                    .k_most_common_ordered(topcontexts)
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
                        let ps: HashSet<_> = self.contexts.map[&c]
                            .map
                            .k_most_common_ordered(topphrases)
                            .into_iter()
                            .map(|x| x.0)
                            .collect();
                        for p in &ps {
                            let p_cs: HashSet<_> = self.phrases.map[&p]
                                .map
                                .k_most_common_ordered(topcontexts)
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
                        for c in &most_common_cs {
                            for p in self.contexts.map[&c].map.keys() {
                                most_similar[p] += 1;
                            }
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
    /// let s = SELMR();
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
                    Some(contexts) => Ok(contexts
                        .map
                        .k_most_common_ordered(topn)
                        .iter()
                        .map(|(context, count)| (context.to_string(), *count))
                        .collect()),
                    None => {
                        return Err(PyErr::new::<PyTypeError, _>(
                            "Contexts of phrase not found: ".to_owned() + phrase,
                        ))
                    }
                }
            }
            None => match phrases {
                Some(_phrases) => {
                    let mut new_counter = ContextCounter {
                        map: Counter::<Context>::new(),
                    };
                    for phrase in _phrases {
                        let contexts = self.phrases.map.get(&Phrase {
                            value: String::from(phrase),
                        });
                        match contexts {
                            Some(contexts) => {
                                let contexts = contexts.map.k_most_common_ordered(topn);
                                for (context, count) in contexts.iter() {
                                    new_counter.map[&context] += count;
                                }
                            }
                            None => {
                                return Err(PyErr::new::<PyTypeError, _>(
                                    "Contexts of phrase not found: ".to_owned() + phrase,
                                ))
                            }
                        }
                    }
                    Ok(new_counter
                        .map
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
    /// let s = SELMR();
    /// ```
    pub fn get_phrases(
        &self,
        context: (&str, &str),
        topn: usize,
    ) -> Result<Vec<(String, usize)>, PyErr> {
        let phrases = self.contexts.map.get(&Context {
            left_value: String::from(context.0),
            right_value: String::from(context.1),
        });
        match phrases {
            Some(phrases) => Ok(phrases
                .map
                .k_most_common_ordered(topn)
                .iter()
                .map(|(phrase, count)| (phrase.to_string(), *count))
                .collect()),
            None => Err(PyErr::new::<PyTypeError, _>(
                "Context not found when calling get_phrases",
            )),
        }
    }

    /// Prunes a SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = SELMR();
    /// ```
    #[pyo3(signature = (topn_phrases=50, topn_contexts=50))]
    pub fn prune(&mut self, topn_phrases: usize, topn_contexts: usize) {
        let mut new_phrases = PhraseMap::new();
        let mut new_contexts = ContextMap::new();

        for p in self.phrases.map.keys() {
            let mut new_counter = ContextCounter {
                map: Counter::<Context>::new(),
            };
            for (key, count) in self.phrases.map[&p].map.k_most_common_ordered(topn_phrases) {
                new_counter.map[&key] = count;
            }
            new_phrases.map.insert(p.clone(), new_counter);
        }
        self.phrases = new_phrases;
        for c in self.contexts.map.keys() {
            let mut new_counter = PhraseCounter {
                map: Counter::<Phrase>::new(),
            };
            for (key, count) in self.contexts.map[&c]
                .map
                .k_most_common_ordered(topn_contexts)
            {
                new_counter.map[&key] = count;
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
    /// let s = SELMR();
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
                for (c, count) in &self.phrases.map[&p].map {
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
        let mut state = serializer.serialize_struct("selmr", 1)?;
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

/// Struct that contains Context Phrase combinations with their number of occurrences
pub struct ContextPhraseCounter<'a> {
    value: Counter<ContextPhraseTuple<'a>>,
}

impl<'a> ContextPhraseCounter<'a> {
    /// Returns the ContextPhrase combinations for a given text
    pub fn new(text: &'a Vec<&'a str>, params: &'a Params) -> Self {
        let mut pset: Counter<ContextPhraseTuple> = Counter::new();
        // create hashmap based on left and right context of length 1
        for p_len in params.min_phrase_len..params.max_phrase_len + 1 {
            pset.update(ContextPhraseWindowGenerator {
                text,
                l_len: 1,
                p_len,
                r_len: 1,
            });
        }
        let mut h = HashMap::<(&'a [&'a str], &'a [&'a str]), HashSet<&'a [&'a str]>>::new();
        for ((l, p, r), _count) in &pset {
            h.entry((l, r)).or_default().insert(p);
        }
        pset = pset
            .into_iter()
            .filter(|((l, _, r), _)| h[&(*l, *r)].len() >= params.min_context_keys)
            .collect();
        let mut h = HashMap::<&'a [&'a str], HashSet<(&'a [&'a str], &'a [&'a str])>>::new();
        for ((l, p, r), _count) in &pset {
            h.entry(p).or_default().insert((l, r));
        }
        pset = pset
            .into_iter()
            .filter(|((_, p, _), _)| h[p].len() >= params.min_phrase_keys)
            .collect();
        for l_len in params.min_context_len..params.max_context_len + 1 {
            for r_len in params.min_context_len..params.max_context_len + 1 {
                let mut new_pset: Counter<ContextPhraseTuple> = Counter::new();
                for p_len in params.min_phrase_len..params.max_phrase_len + 1 {
                    new_pset.update(ContextPhraseWindowGenerator {
                        text,
                        l_len,
                        p_len,
                        r_len,
                    });
                }
                let mut hashmap_c =
                    HashMap::<(&'a [&'a str], &'a [&'a str]), HashSet<&'a [&'a str]>>::new();
                for ((l, p, r), _count) in &new_pset {
                    hashmap_c.entry((l, r)).or_default().insert(p);
                }
                new_pset = new_pset
                    .into_iter()
                    .filter(|((l, _, r), _)| hashmap_c[&(*l, *r)].len() >= params.min_context_keys)
                    .collect();
                let mut h =
                    HashMap::<&'a [&'a str], HashSet<(&'a [&'a str], &'a [&'a str])>>::new();
                for ((l, p, r), _count) in &new_pset {
                    h.entry(p).or_default().insert((l, r));
                }
                new_pset = new_pset
                    .into_iter()
                    .filter(|((_, p, _), _)| h[p].len() >= params.min_phrase_keys)
                    .collect();
                for ((l, p, r), count) in new_pset.iter() {
                    if let Some(item_count) = pset.get(&(&l[1..], p, r)) {
                        if *item_count != *count {
                            pset.insert((l, p, r), *count);
                        }
                    }
                    if let Some(item_count) = pset.get(&(l, p, &r[..r.len() - 1])) {
                        if *item_count != *count {
                            pset.insert((l, p, r), *count);
                        }
                    }
                    if let Some(item_count1) = pset.get(&(&l[1..], p, &r[..r.len() - 1])) {
                        if *item_count1 != *count {
                            if let Some(item_count2) = pset.get(&(l, p, &r[..r.len() - 1])) {
                                if *item_count2 != *count {
                                    if let Some(item_count3) = pset.get(&(&l[1..], p, r)) {
                                        if *item_count3 != *count {
                                            pset.insert((l, p, r), *count);
                                        }
                                    }
                                }
                            }
                        }
                    }
                    // let mut key_found = false;
                    // let mut count_equal = false;
                    // for l_idx in (1..l_len).rev() {
                    //     for r_idx in (1..r_len).rev() {
                    //         if let Some(item_count) = pset.get(&(&l[l_idx..], p, &r[..r.len()-r_idx])) {
                    //             key_found = true;
                    //             if *item_count == *count {
                    //                 count_equal = true;
                    //             }
                    //         }
                    //     }
                    // }
                    // if key_found && !count_equal {
                    //     pset.insert((l, p, r), *count);
                    // }
                }
            }
        }
        ContextPhraseCounter { value: pset }
    }
}
