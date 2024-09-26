use counter::Counter;
use core::fmt::Formatter;
use core::hash::Hash;
use core::cmp::min;
use indexmap::IndexMap;
use pyo3::prelude::*;
use serde::de::MapAccess;
use serde::de::Visitor;
use serde::ser::SerializeMap;
use serde::Deserialize;
use serde::Deserializer;
use serde::Serialize;
use serde::Serializer;
use std::fmt;
use std::marker::PhantomData;
use regex::Regex;
use std::collections::{HashMap, HashSet};

/// The representation of a Context with a left_value and a right_value
#[derive(Hash, Clone, Eq, Ord, PartialEq, PartialOrd, Debug, Serialize)]
#[pyclass]
pub struct ContextValue {
    /// The string of the preceding (left) words of the Context
    pub left_value: String,
    /// The string of the following (right) words of the Context
    pub right_value: String,
}

impl ContextValue {
    /// Returns a ContextValue
    pub fn new(left_value: &str, right_value: &str) -> Self {
        ContextValue {
            left_value: left_value.to_string(),
            right_value: right_value.to_string(),
        }
    }
    /// add prefix to the right side of the context
    pub fn add_to_right(&self, prefix: &str) -> Self {
        ContextValue {
            left_value: self.left_value.to_string(),
            right_value: prefix.to_owned()+" "+&self.right_value
        }
    }
    /// remove prefix of the right side of the context
    pub fn remove_to_right(&mut self) -> String {
        let s = self.right_value.to_string();
        let l: Vec<_> = s.split(' ').collect();
        self.right_value = l[1..].join(" ");
        l[0].to_string()
    }
}

/// The representation of a Phrase with a value containing the string
/// of the phrase
#[derive(Hash, Clone, Eq, Ord, PartialEq, PartialOrd, Serialize, Debug)]
#[pyclass]
pub struct WordValue {
    /// the string of the words
    pub value: String,
    /// the tokens of the words
    pub tokens: Option<Vec<String>>,
}

impl WordValue {
    /// Returns an PhraseValue
    pub fn new(value: &str, tokens: Option<Vec<String>>) -> Self {
        WordValue {
            value: value.to_string(),
            tokens,
        }
    }
    /// Add a suffix to the current word
    pub fn set_tokens(&self, tokens: Vec<String>) -> Self {
        WordValue {
            value: self.value.to_string(),
            tokens: Some(tokens),
        }
    }
}

/// Text struct definition. Text is either a Word or a Context
#[derive(Hash, Clone, Eq, Ord, PartialEq, PartialOrd, Serialize, Debug)]
pub enum Text {
    /// Word variant of a Text has a WordValue
    Word(WordValue),
    /// Context variant of a Text has a ContextValue
    Context(ContextValue),
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Text::Word(w) => {
                if let Some(tokens) = &w.tokens {
                    write!(f, "{}; {}", w.value, tokens.join(", "))
                } else {
                    write!(f, "{}", w.value)
                }
            },
            Text::Context(c) => write!(f, "{} ... {}", c.left_value, c.right_value)
        }
    }
}

impl Text {
    /// Returns a Text Context element with value
    pub fn context(left_value: &str, right_value: &str) -> Self {
        Text::Context(ContextValue::new(left_value, right_value))
    }
    /// Returns a Text Word element with value
    pub fn word(value: &str, tokens: Option<Vec<String>>) -> Self {
        Text::Word(WordValue::new(value, tokens))
    }
    /// Determine the type of value from the string
    pub fn extract(value: &str) -> Self {
        let c = value.split(" ... ").collect::<Vec<&str>>();
        if c.len() == 1 {
            let w = value.split("; ").collect::<Vec<&str>>();
            if w.len() > 1 {
                Text::word(
                    w[0],
                    Some(w[1].split(", ").map(|s|s.to_string()).collect())
                )
            } else {
                Text::word(w[0], None)
            }
        } else {
            Text::context(c[0], c[1])
        }
    }
    /// add a prefix to the right side of the context
    pub fn add_right_prefix(&self, prefix: &str) -> Self {
        match self {
            Text::Context(c) => {
                Text::Context(c.add_to_right(prefix))
            },
            Text::Word(_) => todo!()
        }
    }
    /// remove a prefix to the right side of the context
    pub fn remove_to_right(&mut self) -> String {
        match self {
            Text::Context(c) => {
                c.remove_to_right()
            },
            Text::Word(_) => todo!()
        }
    }
    /// add a prefix to the right side of the context
    pub fn set_tokens(&self, tokens: Vec<String>) -> Self {
        match self {
            Text::Word(w) => {
                Text::Word(w.set_tokens(tokens))
            },
            Text::Context(_) => todo!()
        }
    }
}

impl<'de> Deserialize<'de> for Text {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: Deserializer<'de>,
    {
        let value = String::deserialize(deserializer)?;
        Ok(Text::extract(&value))
    }
}

/// The counter with the number of occurrences of each Context
#[derive(Clone, Debug)]
pub struct TextCounter {
    /// the map that contains the actual Counter
    // #[serde(deserialize_with = "indexmap::map::serde_seq::deserialize")]
    pub map: IndexMap<Text, usize>,
}

impl TextCounter {
    /// Returns an empty TextCounter
    pub fn new() -> Self {
        TextCounter {
            map: IndexMap::<Text, usize>::new(),
        }
    }
}

impl Default for TextCounter {
    fn default() -> Self {
        Self::new()
    }
}

impl Serialize for TextCounter {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // the IndexMap is serialized as a map
        // this makes the json file more readable
        let mut map = serializer.serialize_map(Some(self.map.len()))?;
        for (key, value) in &self.map {
            map.serialize_entry(&key.to_string(), value)?;
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for TextCounter {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(TextCounter {
            map: deserializer.deserialize_map(IndexMapVisitor(PhantomData))?,
        })
    }
}

struct IndexMapVisitor<K, V>(PhantomData<(K, V)>);

impl<'de, K, V> Visitor<'de> for IndexMapVisitor<K, V>
where
    K: Deserialize<'de> + std::hash::Hash + std::cmp::Eq,
    V: Deserialize<'de>,
{
    type Value = IndexMap<K, V>;

    fn expecting(&self, formatter: &mut Formatter<'_>) -> fmt::Result {
        write!(formatter, "TextCounter")
    }

    fn visit_map<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        // Deserializing to IndexMap using the order in the json file
        let mut map = IndexMap::new();
        while let Some((key, value)) = seq.next_entry()? {
            map.insert(key, value);
        }
        Ok(map)
    }
}


/// Text map definition
#[derive(Clone)]
pub struct TextMap {
    /// the map contains the actual IndexMap
    pub map: IndexMap<Text, TextCounter>,
    /// the map with root en words have this root
    pub phrase2tokens: HashMap<Text, HashSet<Text>>,
}

impl TextMap {
    /// Return an empty TextMap
    pub fn new() -> Self {
        TextMap {
            map: IndexMap::<Text, TextCounter>::new(),
            phrase2tokens: HashMap::<Text, HashSet<Text>>::new(),
        }
    }
    /// this function creates a new textmap based on the content of the multisets as keys
    /// and the keys as content of the multiset
    pub fn swap(&self) -> TextMap {
        let mut reversed = TextMap::new();
        let mut unordered = IndexMap::<Text, Counter<Text>>::new();
        for (text, multiset) in self.map.iter() {
            for (item, &n) in multiset.map.iter() {
                unordered
                    .entry(item.clone())
                    .or_default()
                    .insert(text.clone(), n);
            }
        }
        for (text, multiset) in unordered.iter() {
            let ordered: IndexMap<Text, usize> =
                multiset.most_common_ordered().into_iter().collect();
            reversed.map.insert(
                text.clone(),
                TextCounter { map: ordered },
            );
        }
        reversed
    }
    /// Merge the other TextMap into the current one
    pub fn merge(&mut self, other: &TextMap) {
        for (text, multiset) in other.map.iter() {
            for (item, new_n) in multiset.map.iter() {
                self.map
                    .entry(text.clone())
                    .or_default()
                    .map
                    .entry(item.clone())
                    .and_modify(|n| *n += new_n)
                    .or_insert(*new_n);
            }
        }
        self.sort();
        // to do: 
        self.phrase2tokens = other.phrase2tokens.clone();
    }
    /// Sort the multisets of the TextMap
    pub fn sort(&mut self) {
        for (_, multiset) in self.map.iter_mut() {
            multiset.map.sort_by(|_, v1, _, v2| v2.cmp(v1));
        }
    }
    /// tokenize phrases and put suffix in right side of context
    pub fn derive_root_phrases(&mut self) -> TextMap {
        let mut new = TextMap::new();
        for (phrase, multiset) in self.map.iter() {
            match phrase {
                Text::Word(p) => {
                    if let Some(tokens) = &p.tokens {
                        let phrase_base = Text::word(&tokens[0], None);
                        let phrase_suffix = tokens[1].as_str();
                        new.phrase2tokens.entry(phrase_base.clone()).or_default().insert(
                            phrase.clone()
                        );
                        // if the tokenized word is not yet added, add it once with contents of word
                        // so: if 'discover-' is not yet added, initialize with multiset of 'discover'
                        if !new.map.contains_key(&phrase_base) {
                            let phrase_root = Text::word(&tokens[0][0..tokens[0].len()-1], None);
                            if let Some(multiset) = self.get_multiset(&phrase_root, None) {
                                let new_multiset = TextCounter {
                                    map: multiset
                                        .iter()
                                        .map(|(t, n)|(t.clone().clone(), *n))
                                        .collect()
                                };
                                for (item, new_n) in new_multiset.map.iter() {
                                    new.map.entry(phrase_base.clone())
                                        .or_default()
                                        .map
                                        .entry(item.clone())
                                        .and_modify(|n| *n += new_n)
                                        .or_insert(*new_n);
                                }
                            }
                        }
                        // add multiset of tokenized word
                        // so: 'discovers' is tokenized to 'discover-' and '. ... -s .' context is added to 'discover-'
                        if let Some(multiset) = self.get_multiset(phrase, None) {
                            let new_multiset = TextCounter {
                                map: multiset
                                    .iter()
                                    .map(|(c, n)|(c.add_right_prefix(phrase_suffix), *n))
                                    .collect()
                            };
                            for (item, new_n) in new_multiset.map.iter() {
                                new.map.entry(phrase_base.clone())
                                    .or_default()
                                    .map
                                    .entry(item.clone())
                                    .and_modify(|n| *n += new_n)
                                    .or_insert(*new_n);
                            }
                        }
                        // add also the existing multiset
                        let phrase = Text::word(&p.value, None);
                        new.map.insert(phrase.clone(), multiset.clone());
                    } else {
                        let phrase = Text::word(&p.value, None);
                        new.map.insert(phrase.clone(), multiset.clone());
                    }
                },
                Text::Context(_) => todo!()
            }
        }
        new.sort();
        new
    }
    /// reverse tokenizing the phrases (suffix from right side of context returns to phrase)
    pub fn combine_phrases(&mut self) -> Self {
        let mut new = TextMap::new();
        for (phrase, multiset) in self.map.iter() {
            match phrase {
                Text::Word(_) => {
                    if self.phrase2tokens.contains_key(phrase) {
                        // println!("Found {:?}", phrase);
                        for item in self.phrase2tokens[phrase].iter() {
                            match item {
                                Text::Word(i) => {
                                    if let Some(tokens) = &i.tokens {
                                        let m = tokens[1].to_owned() + " ";
                                        let new_multiset = TextCounter {
                                            map: multiset.map
                                                .iter()
                                                .filter(|(c, _)|c.to_string().contains(&m))
                                                .map(|(c, n)|(Text::extract(&c.to_string().replace(&m, "")), *n))
                                                .collect()
                                        };
                                        new.map.insert(item.clone(), new_multiset);
                                    }
                                },
                                Text::Context(_) => todo!()
                            }
                        }
                    } else {
                        new.map.insert(phrase.clone(), multiset.clone());
                    }
                },
                Text::Context(_) => todo!()
            }
        }
        new.sort();
        new
    }
    /// tokenize phrase based TextMap content
    pub fn tokenize_phrase(
        &self,
        text: &Text
    ) -> Vec<Text> {
        let mut results = Vec::<Text>::new();
        match text {
            Text::Word(text) => {
                let text = &text.value;
                let words = text.split(' ').collect::<Vec<&str>>();
                let last_word = words[words.len()-1];
                if last_word == "as" ||
                   last_word == "king" {
                    let res = Text::word(text, None);
                    results.push(res);
                    return results
                }
                let initial_words = words[0..words.len()-1].join(" ");
                let simple_rules = Vec::from([
                    ("is", "be", "verb", "irregular verb", "-s"),
                    ("are", "be", "verb", "irregular verb", "-"),
                    ("was", "be", "verb", "irregular verb", "-ed"),
                    ("were", "be", "verb", "irregular verb", "-ed"),
                    ("been", "be", "verb", "irregular verb", "-"),
                    ("has", "have", "verb", "irregular verb", "-s"),
                    ("have", "have", "verb", "irregular verb", "-"),
                    ("had", "have", "verb", "irregular verb", "-ed"),
                    ("began", "begin", "verb", "irregular verb", "-ed"),
                    ("begun", "begin", "verb", "irregular verb", "-ed"),
                    ("broke", "break", "verb", "irregular verb", "-ed"),
                    ("broken", "break", "verb", "irregular verb", "-ed"),
                    ("brought", "bring", "verb", "irregular verb", "-ed"),
                    ("bought", "buy", "verb", "irregular verb", "-ed"),
                    ("built", "build", "verb", "irregular verb", "-ed"),
                    ("chose", "choose", "verb", "irregular verb", "-ed"),
                    ("chosen", "choose", "verb", "irregular verb", "-ed"),
                    ("came", "come", "verb", "irregular verb", "-ed"),
                    ("did", "do", "verb", "irregular verb", "-ed"),
                    ("done", "do", "verb", "irregular verb", "-ed"),
                    ("drew", "draw", "verb", "irregular verb", "-ed"),
                    ("drawn", "draw", "verb", "irregular verb", "-ed"),
                    ("drove", "drive", "verb", "irregular verb", "-ed"),
                    ("driven", "drive", "verb", "irregular verb", "-ed"),
                    ("ate", "eat", "verb", "irregular verb", "-ed"),
                    ("eaten", "eat", "verb", "irregular verb", "-ed"),
                    ("felt", "feel", "verb", "irregular verb", "-ed"),
                    ("fled", "flee", "verb", "irregular verb", "-ed"),
                    ("formed", "form", "verb", "irrelar verb", "-ed"),
                    ("found", "find", "verb", "irregular verb", "-ed"),
                    ("fought", "fight", "verb", "irregular verb", "-ed"),
                    ("get", "got", "verb", "irregular verb", "-ed"),
                    ("gave", "give", "verb", "irregular verb", "-ed"),
                    ("given", "give", "verb", "irregular verb", "-ed"),
                    ("gone", "go", "verb", "irregular verb", "-ed"),
                    ("went", "go", "verb", "irregular verb", "-ed"),
                    ("heard", "hear", "verb", "irregular verb", "-ed"),
                    ("held", "hold", "verb", "irregular verb", "-ed"),
                    ("kept", "keep", "verb", "irregular verb", "-ed"),
                    ("knew", "know", "verb", "irregular verb", "-ed"),
                    ("known", "know", "verb", "irregular verb", "-ed"),
                    ("left", "leave", "verb", "irregular verb", "-ed"),
                    ("led", "lead", "verb", "irregular verb", "-ed"),
                    ("lay", "lie", "verb", "irregular verb", "-ed"),
                    ("lain", "lie", "verb", "irregular verb", "-ed"),
                    ("lost", "lose", "verb", "irregular verb", "-ed"),
                    ("made", "make", "verb", "irregular verb", "-ed"),
                    ("meant", "mean", "verb", "irregular verb", "-ed"),
                    ("met", "meet", "verb", "irregular verb", "-ed"),
                    ("paid", "pay", "verb", "irregular verb", "-ed"),
                    ("ran", "run", "verb", "irregular verb", "-ed"),
                    ("said", "say", "verb", "irregular verb", "-ed"),
                    ("saw", "see", "verb", "irregular verb", "-ed"),
                    ("seen", "see", "verb", "irregular verb", "-ed"),
                    ("sold", "sell", "verb", "irregular verb", "-ed"),
                    ("sent", "send", "verb", "irregular verb", "-ed"),
                    ("sat", "sit", "verb", "irregular verb", "-ed"),
                    ("spoke", "speak", "verb", "irregular verb", "-ed"),
                    ("spoken", "speak", "verb", "irregular verb", "-ed"),
                    ("spent", "spend", "verb", "irregular verb", "-ed"),
                    ("stood", "stand", "verb", "irregular verb", "-ed"),
                    ("took", "take", "verb", "irregular verb", "-ed"),
                    ("taken", "take", "verb", "irregular verb", "-ed"),
                    ("taught", "teach", "verb", "irregular verb", "-ed"),
                    ("told", "tell", "verb", "irregular verb", "-ed"),
                    ("thought", "think", "verb", "irregular verb", "-ed"),
                    ("understood", "understand", "verb", "irregular verb", "-ed"),
                    ("wore", "wear", "verb", "irregular verb", "-ed"),
                    ("worn", "wear", "verb", "irregular verb", "-ed"),
                    ("won", "win", "verb", "irregular verb", "-ed"),
                    ("wrote", "write", "verb", "irregular verb", "-ed"),
                    ("written", "write", "verb", "irregular verb", "-ed"),
                ]);
                for (p, lemma, _rule, _verbose, suffix) in &simple_rules {
                    if last_word ==*p {
                        let res = if words.len() > 1 {
                            let tokens = Vec::<String>::from([
                                initial_words.to_owned()+" "+lemma+"-",
                                suffix.to_string(),
                            ]);
                            Text::word(text, Some(tokens))
                        } else {
                            let tokens = Vec::<String>::from([
                                lemma.to_string()+"-",
                                suffix.to_string(),
                            ]);
                            Text::word(text, Some(tokens))
                        };
                        results.push(res);
                    }
                }
                if results.is_empty() {
                    let hm = Vec::from([
                        // adjectives/adverbs
                        ("([a-zA-Z][a-z]{3,})lly", "l", "adjective/adverb", "-lly change to -l", "-ly"),
                        ("([a-zA-Z][a-z]{3,})ily", "y", "adjective/adverb", "-ily change to -y", "-ly"),
                        ("([a-zA-Z][a-z]{3,})ely", "e", "adjective/adverb", "-ely change to -e", "-ly"),
                        ("([a-zA-Z][a-z]{3,})ly", "", "adjective/adverb", "-ly change to -", "-ly"),
                        // proper and common noun rules
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])ies", "y", "noun", "-consonant and -y -> change -y to -ies", "-s"),
                        ("([a-zA-Z][a-z]*[aeuoi])ys", "y", "noun", "-vowel and -y -> add an -s", "-s"),
                        ("([a-zA-Z][a-z]*)ves", "f", "noun", "-f -> change to -ve and add an -s", "-s"),
                        ("([a-zA-Z][a-z]*)ves", "fe", "noun", "-fe -> change to -ve and add an -s", "-s"),
                        ("([a-zA-Z][a-z]*)i", "us", "noun", "-us -> replace with an -i", "-s"),
                        ("([a-zA-Z][a-z]*)oes", "o", "noun", "-o -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)es", "is", "noun", "-is -> replace with an -es", "-s"),
                        ("([a-zA-Z][a-z]*)a", "on", "noun", "-on -> replace with an -a", "-s"),
                        ("([a-zA-Z][a-z]*)ses", "s", "noun", "‑s -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)xes", "s", "noun", "-x -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)zes", "s", "noun", "-z -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)sses", "ss", "noun", "‑ss -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)shes", "sh", "noun", "‑sh -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)ches", "ch", "noun", "‑ch -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)s", "", "noun", "_ -> add an -s", "-s"),
                        // regular verb rules
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])ied", "y", "verb", "-consonent and -y -> add an -ied", "-ed"),
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])ed", "", "verb", "-consonant -> add an -ed", "-ed"),
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])[b-df-hj-np-tv-z]ed", "", "verb", "-consonant -> add an -ed", "-ed"),
                        ("([a-zA-Z][a-z]*[aeuoi])d", "", "verb", "-vowel -> add an -d", "-ed"),
                        ("([a-zA-Z][a-z]*)s", "", "verb", "_ -> add an -s", "-s"),
                        ("([a-zA-Z][a-z]*)ches", "ch", "verb", "ch -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)shes", "sh", "verb", "sh -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)ses", "s", "verb", "s -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)xes", "x", "verb", "x -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*)zes", "z", "verb", "z -> add an -es", "-s"),
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])ies", "y", "verb", "-consonant and y -> replace y with ies", "-s"),
                        ("([a-zA-Z][a-z]*)ing", "a", "verb", "-a -> add an -ing", "-ing"),
                        ("([a-zA-Z][a-z]*)ing", "e", "verb", "-e -> add an -ing", "-ing"),
                        ("([a-zA-Z][a-z]*)ing", "u", "verb", "-u -> add an -ing", "-ing"),
                        ("([a-zA-Z][a-z]*)ing", "o", "verb", "-o -> add an -ing", "-ing"),
                        ("([a-zA-Z][a-z]*)ing", "i", "verb", "-i -> add an -ing", "-ing"),
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])ing", "", "verb", "-consonant -> add an -ing", "-ing"),
                        ("([a-zA-Z][a-z]*[b-df-hj-np-tv-z])[b-df-hj-np-tv-z]ing", "", "verb", "-consonant -> add double consonant and -ing", "-ing"),

                    ]);
                    for (regex, singular, _rule, _verbose, suffix) in &hm {
                        let re_1 = Regex::new(format!("^{}$", regex).as_str()).unwrap();
                        if let Some(caps) = re_1.captures(last_word) {
                            if let Some(cap) = caps.get(1) {
                                let lemma = format!("{}{}", cap.as_str(), singular);
                                if self.map.contains_key(&Text::word(&lemma, None)) ||
                                   self.map.contains_key(&Text::word(&lemma.to_lowercase(), None))
                                {
                                    let res = if words.len() > 1 {
                                        let tokens = Vec::<String>::from([
                                            initial_words.to_owned()+" "+&lemma+"-",
                                            suffix.to_string()
                                        ]);
                                        Text::word(text, Some(tokens))
                                    } else {
                                        let tokens = Vec::<String>::from([
                                            lemma+"-",
                                            suffix.to_string(),
                                        ]);
                                        Text::word(text, Some(tokens))
                                    };
                                    results.push(res);
                                }
                            } else {
                                let lemma = singular.to_string();
                                if self.map.contains_key(&Text::word(&lemma, None)) ||
                                   self.map.contains_key(&Text::word(&lemma.to_lowercase(), None))
                                {
                                    let res = if words.len() > 1 {
                                        let tokens = Vec::<String>::from([
                                            initial_words.to_owned()+" "+&lemma+"-",
                                            suffix.to_string(),
                                        ]);
                                        Text::word(text, Some(tokens))
                                    } else {
                                        let tokens = Vec::<String>::from([
                                            lemma+"-",
                                            suffix.to_string(),
                                        ]);
                                        Text::word(text, Some(tokens))
                                    };
                                    results.push(res);
                                }
                            }
                        }
                    }
                }
            },
            Text::Context(_) => todo!(),
        }
        if results.is_empty() {
            results.push(text.clone());
        }
        results
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
    /// let mut text = Vec::<String>::new();
    /// text.push("a 1 b c. a 2 b c. a 2 b d.".to_string());
    /// s.add(text, Some(&params));
    /// let actual = s.get_multiset(&Text::word("1", None), Some(15), true).unwrap();
    /// let context = Text::context("a", "b");
    /// let expect = [(context, &1)].iter().cloned().map(|(p, n)|(p, *n)).collect::<IndexMap<_, _>>();
    /// assert_eq!(actual, expect);
    /// ```
    ///
    #[inline]
    /// Retrieve the multiset of a given text element
    pub fn get_multiset(
        &self,
        text: &Text,
        topn: Option<usize>,
    ) -> Option<IndexMap<Text, usize>> {
        let es = self.map.get(text);
        match topn {
            Some(topn) => {
                es.map(|es| es.map[..min(es.map.len(), topn)]
                    .iter()
                    .map(|(e, n)|(e.clone(), *n))
                    .collect())
            },
            None => {
                es.map(|es| es.map
                    .iter()
                    .map(|(e, n)|(e.clone(), *n))
                    .collect()) 
            }
        }
    }
    /// Retrieve the multiset of a given text element
    pub fn get_filtered_multiset(
        &self,
        text: &Text,
        filter: &str,
        topn: Option<usize>,
    ) -> Option<IndexMap<Text, usize>> {
        let es = self.map.get(text);
        match topn {
            Some(topn) => {
                let mut multiset = IndexMap::<Text, usize>::new();
                let mut i = 0;
                while multiset.len() < topn && i < es.unwrap().map.len() {
                    if let Some((c, n)) = es.unwrap().map.get_index(i) {
                        if c.to_string().contains(filter) {
                            multiset.insert(c.clone(), *n);
                        }
                    }
                    i += 1;
                }
                Some(multiset)
            },
            None => {
                es.map(|es| es.map
                    .iter()
                    .filter(|(e, _)|(e.to_string().contains(filter)))
                    .map(|(e, n)|(e.clone(), *n))
                    .collect()) 
            }
        }
    }
}

impl Default for TextMap {
    fn default() -> Self {
        Self::new()
    }
}

impl Serialize for TextMap {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut map = serializer.serialize_map(Some(self.map.len()))?;
        for (k, v) in &self.map {
            map.serialize_entry(&k.to_string(), v)?;
        }
        map.end()
    }
}

struct TextMapVisitor {
    marker: PhantomData<fn() -> TextMap>,
}

impl TextMapVisitor {
    fn new() -> Self {
        TextMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for TextMapVisitor {
    type Value = TextMap;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("TextMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut textmap = TextMap::new(); //with_capacity(access.size_hint().unwrap_or(0));
        while let Some((key, value)) = access.next_entry()? {
            textmap.map.insert(key, value);
        }
        Ok(textmap)
    }
}

impl<'de> Deserialize<'de> for TextMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = TextMapVisitor::new();
        deserializer.deserialize_map(m)
    }
}