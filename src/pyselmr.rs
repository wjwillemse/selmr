use crate::selmr::{Params, SELMR};
use crate::text_structs::{Context, ContextMap, Phrase, PhraseMap};
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use std::collections::HashMap;

#[pymodule]
fn selmr(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<PYSELMR>()?;
    Ok(())
}

/// The struct to access the SELMR struct from Python
#[pyclass]
pub struct PYSELMR {
    selmr: SELMR,
}

// impl Default for PYSELMR {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// Core SELMR methods
#[pymethods]
impl PYSELMR {
    /// Initializes an empty SELMR data structure
    ///
    /// # Example
    ///
    /// ```
    /// let s = selmr::selmr::SELMR::new();
    /// ```
    #[new]
    #[pyo3(signature = ())]
    pub fn new() -> Self {
        PYSELMR {
            selmr: SELMR {
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
            },
        }
    }

    /// Write SELMR data structure to a file
    pub fn write(&self, file: &str, format: &str) -> Result<(), PyErr> {
        match self.selmr.write(file, format) {
            Ok(r) => Ok(r),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }

    /// Read SELMR data structure to a file
    pub fn read(&mut self, file: &str, format: &str) -> Result<(), PyErr> {
        match self.selmr.read(file, format) {
            Ok(r) => Ok(r),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }

    /// Merge another PYSELMR struct into current one
    pub fn merge(&mut self, other: &PYSELMR) {
        self.selmr.merge(&other.selmr)
    }

    /// add text data to SELMR data structure
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
        self.selmr.add(
            text,
            min_phrase_len,
            max_phrase_len,
            min_left_context_len,
            max_left_context_len,
            min_right_context_len,
            max_right_context_len,
            min_phrase_keys,
            min_context_keys,
            language,
        )
    }

    /// Returns the most similar phrases based on common contexts
    #[pyo3(signature = (phrase, context=None, topcontexts=25, topphrases=25, topn=15, measure="count"))]
    pub fn most_similar(
        &self,
        phrase: &str,
        context: Option<&str>,
        topcontexts: usize,
        topphrases: usize,
        topn: usize,
        measure: &str,
    ) -> Result<Vec<(String, f32)>, PyErr> {
        let p = Phrase::new(phrase);
        let c = context.map(Context::new);
        match self
            .selmr
            .most_similar(p, c, topcontexts, topphrases, topn, measure)
        {
            Ok(r) => Ok(r.iter().map(|(p, v)| (p.to_string(), *v)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }

    /// Find the most similar phrases from a given multiset
    #[pyo3(signature = (multiset, context=None, topcontexts=25, topphrases=25, topn=15, measure="count"))]
    pub fn most_similar_from_multiset(
        &self,
        multiset: HashMap<&str, usize>,
        context: Option<&str>,
        topcontexts: usize,
        topphrases: usize,
        topn: usize,
        measure: &str,
    ) -> Result<Vec<(String, f32)>, PyErr> {
        let keys = multiset
            .keys()
            .map(|c| Context::new(c))
            .collect::<Vec<_>>();
        let values = multiset.values().collect::<Vec<_>>();
        let mut multiset = HashMap::<&Context, &usize>::new();
        for idx in 0..keys.len() {
            multiset.insert(&keys[idx], values[idx]);
        }
        let context = context.map(|context| Context::new(context));
        match self.selmr.most_similar_from_multiset(
            &multiset,
            context,
            topcontexts,
            topphrases,
            topn,
            measure,
        ) {
            Ok(r) => Ok(r.iter().map(|(p, v)| (p.to_string(), *v)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }

    /// Get the topn context of the phrase
    pub fn get_phrase_contexts(
        &self,
        phrase: &str,
        topn: usize,
    ) -> Result<HashMap<String, usize>, PyErr> {
        let phrase = Phrase::new(phrase);
        match self.selmr.get_phrase_contexts(&phrase, topn) {
            Some(r) => Ok(r.iter().map(|(c, v)| (c.to_string(), **v)).collect()),
            None => Err(PyErr::new::<PyTypeError, _>("Phrase not found")),
        }
    }

    /// Get the topn context of the phrases
    pub fn get_phrases_contexts(
        &self,
        phrases: Vec<&str>,
        topn: usize,
    ) -> Result<HashMap<String, usize>, PyErr> {
        let phrases = phrases.iter().map(|p| Phrase::new(p)).collect();
        match self.selmr.get_phrases_contexts(&phrases, topn) {
            Ok(r) => Ok(r.iter().map(|(c, v)| (c.to_string(), *v)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }

    /// Get the topn phrasesof the context
    pub fn get_context_phrases(
        &self,
        context: &str,
        topn: usize,
    ) -> Result<HashMap<String, usize>, PyErr> {
        let context = Context::new(context);
        match self.selmr.get_context_phrases(&context, topn) {
            Some(r) => Ok(r.iter().map(|(p, v)| (p.to_string(), **v)).collect()),
            None => Err(PyErr::new::<PyTypeError, _>("Context not found")),
        }
    }

    /// Get the topn phrasesof the contexts
    pub fn get_contexts_phrases(
        &self,
        contexts: Vec<&str>,
        topn: usize,
    ) -> Result<HashMap<String, usize>, PyErr> {
        let contexts = contexts.iter().map(|c| Context::new(c)).collect();
        match self.selmr.get_contexts_phrases(&contexts, topn) {
            Ok(r) => Ok(r.iter().map(|(p, v)| (p.to_string(), *v)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }

    /// Prunes a SELMR data structure
    #[pyo3(signature = (topn_phrases=50, topn_contexts=50))]
    pub fn prune(&mut self, topn_phrases: usize, topn_contexts: usize) {
        self.selmr.prune(topn_phrases, topn_contexts)
    }

    /// Filters a SELMR data structure on given regexes
    pub fn matches(
        &self,
        tuple: (&str, &str, &str),
    ) -> Option<HashMap<(String, String, String), usize>> {
        self.selmr.matches(tuple)
    }
}
