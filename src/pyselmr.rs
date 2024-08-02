use crate::selmr::{Params, SELMR};
use crate::hac::PyHAC;
use crate::text_structs::{Text, TextMap};
use crate::tokenizer::tokenize;
use crate::selmr::Measure;
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use std::collections::HashMap;

#[pymodule]
fn selmr(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_class::<PySELMR>()?;
    m.add_class::<PyHAC>()?;
    Ok(())
}

/// The struct to access the SELMR struct from Python
#[pyclass]
#[derive(Clone)]
pub struct PySELMR {
    /// A PySELMR struct contains a SELMR struct
    pub selmr: SELMR,
}
impl Default for PySELMR {
    fn default() -> Self {
        Self::new()
    }
}
// PySELMR methods
#[pymethods]
impl PySELMR {
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
        PySELMR {
            selmr: SELMR {
                phrases: TextMap::new(),
                phrase_roots: HashMap::new(),
                root_phrases: HashMap::new(),
                contexts: TextMap::new(),
                phrases_tree: None,
                contexts_tree: None,
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
    pub fn write(
        &mut self,
        file: &str, 
        format: &str
    ) -> Result<(), PyErr> {
        match self.selmr.write(file, format) {
            Ok(r) => Ok(r),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }
    /// Read SELMR data structure to a file
    pub fn read(
        &mut self, 
        file: &str, 
        format: &str
    ) -> Result<(), PyErr> {
        match self.selmr.read(file, format) {
            Ok(r) => Ok(r),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }
    /// Merge another PySELMR struct into current one
    pub fn merge(
        &mut self, 
        other: &PySELMR
    ) {
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
        let params = Params {
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
        self.selmr.add(text, Some(&params))
    }
    /// Get the phrases index of text
    pub fn get_phrases_index_of(
        &self, 
        text: String
    ) -> usize {
        let t = Text::extract(&text);
        self.selmr.phrases.map.get_index_of(&t).unwrap()
    }
    /// Get the phrases key of idx
    pub fn get_phrases_index(
        &self, 
        idx: usize
    ) -> String {
        let (key, _) = self.selmr.phrases.map.get_index(idx).unwrap();
        key.to_string()
    }
    /// Get the contexts index of text
    pub fn get_contexts_index_of(
        &self, 
        text: String
    ) -> usize {
        let t = Text::extract(&text);
        self.selmr.contexts.map.get_index_of(&t).unwrap()
    }
    /// Get the contexts key of text
    pub fn get_contexts_index(
        &self, 
        idx: usize
    ) -> String {
        let (key, _) = self.selmr.contexts.map.get_index(idx).unwrap();
        key.to_string()
    }
    /// Returns the most similar texts based on common contexts
    #[pyo3(signature = (text, constraint=None, multiset_topn=25, topn=15, measure="count", literal=true))]
    pub fn most_similar(
        &self,
        text: String,
        constraint: Option<String>,
        multiset_topn: usize,
        topn: usize,
        measure: &str,
        literal: bool,
    ) -> Result<Vec<(String, f32)>, PyErr> {
        let text = Text::extract(text.as_str());
        let constraint = constraint.map(|c|Text::extract(c.as_str()));
        let measure = if measure == "jaccard" {
            Measure::JaccardIndex
        } else if measure == "weighted_jaccard" {
            Measure::WeightedJaccardIndex
        } else {
            Measure::CountIndex
        };
        match self.selmr.most_similar(
            text, 
            constraint, 
            Some(multiset_topn), 
            Some(topn), 
            measure,
            literal)
        {
            Ok(r) => Ok(r.iter().map(|(p, n)|(p.to_string(), *n)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }
    // /// Find the most similar phrases from a given multiset
    // #[pyo3(signature = (multiset, constraint=None, multiset_topn=25, topn=15, measure="count"))]
    // pub fn most_similar_from_multiset(
    //     &self,
    //     multiset: HashMap<&str, usize>,
    //     constraint: Option<&str>,
    //     multiset_topn: usize,
    //     topn: usize,
    //     measure: &str,
    // ) -> Result<Vec<(String, f32)>, PyErr> {
    //     let keys = multiset
    //         .keys()
    //         .map(|c| Text::extract(c))
    //         .collect::<Vec<_>>();
    //     let values = multiset.values().collect::<Vec<_>>();
    //     let mut multiset = IndexMap::<&Text, usize>::new();
    //     for idx in 0..keys.len() {
    //         multiset.insert(&keys[idx], *values[idx]);
    //     }
    //     let constraint = constraint.map(|value| Text::extract(value));
    //     match self.selmr.most_similar_from_multiset(
    //         &multiset,
    //         constraint,
    //         Some(multiset_topn),
    //         Some(topn),
    //         measure,
    //     ) {
    //         Ok(r) => Ok(r.iter().map(|(p, n)|(p.to_string(), *n)).collect()),
    //         Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
    //     }
    // }
    /// Get the topn associations of a text
    #[pyo3(signature = (text="", topn=15, literal=true))]
    pub fn get_multiset(
        &self,
        text: &str,
        topn: usize,
        literal: bool,
    ) -> Result<HashMap<String, usize>, PyErr> {
        let item = Text::extract(text);
        match self.selmr.get_multiset(&item, Some(topn), literal) {
            Some(r) => Ok(r.iter().map(|(c, v)| (c.to_string(), *v)).collect()),
            None => Err(PyErr::new::<PyTypeError, _>("Text not found")),
        }
    }
    /// Get the topn context of the phrases
    #[pyo3(signature = (texts, topn=15, literal=true))]
    pub fn get_list_multiset(
        &self,
        texts: Vec<&str>,
        topn: usize,
        literal: bool,
    ) -> Result<HashMap<String, usize>, PyErr> {
        let texts = texts.iter().map(|e| Text::extract(e)).collect();
        match self.selmr.get_list_multiset(&texts, Some(topn), literal) {
            Ok(r) => Ok(r.iter().map(|(m, n)| (m.to_string(), *n)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }
    /// Prunes a SELMR data structure
    #[pyo3(signature = (topn_phrases=50, topn_contexts=50))]
    pub fn prune(&mut self, topn_phrases: usize, topn_contexts: usize
    ) {
        self.selmr.prune(topn_phrases, topn_contexts)
    }
    /// Filters the phrase-context combinations on given regexes
    pub fn phrase_context_matches(
        &self,
        phrase: &str,
        context: &str,
    ) -> Option<HashMap<(String, String), usize>> {
        self.selmr.phrase_context_matches(phrase, context)
    }

    /// Filters the phrases on given regex
    pub fn phrase_matches(
        &self,
        phrase: &str,
    ) -> Option<HashMap<String, usize>> {
        self.selmr.phrase_matches(phrase)
    }
    /// Filters the contexts on given regex
    pub fn context_matches(
        &self,
        context: &str,
    ) -> Option<HashMap<String, usize>> {
        self.selmr.context_matches(context)
    }
    /// Lemmatize phrase based on SELMR data
    pub fn tokenize_phrase(
        &self,
        phrase: &str,
    ) -> Option<Vec<String>> {
        Some(
            self.selmr.phrases
                .tokenize_phrase(&Text::extract(phrase))
                .iter()
                .map(|s|s.to_string())
                .collect()
        )
    }
    /// Generate the cluster trees of the selmr data
    pub fn generate_trees(
        &mut self, 
        phrases_n: usize, 
        contexts_n: usize, 
        multiset_topn: usize,
    ) {
        self.selmr.generate_trees(
            phrases_n, 
            contexts_n, 
            multiset_topn,
        )
    }
    /// 
    pub fn get_cluster_from_text(
        &self, 
        text: String,
        depth: usize,
    ) -> Result<Vec<String>, PyErr> {
        let text = Text::extract(text.as_str());
        match self.selmr.get_cluster_from_text(text, depth) {
            Ok(r) => Ok(r),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
    }
    /// Tokenizer
    pub fn tokenize<'a>(
        &'a self,
        contents: &'a str) -> Result<Vec<Vec<&str>>, PyErr> {
        Ok(tokenize(contents))
    }
}
