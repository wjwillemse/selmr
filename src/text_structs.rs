use counter::Counter;
use core::cmp::min;
use core::fmt::Formatter;
use core::hash::Hash;
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
    pub fn new(value: &str) -> Self {
        let c = value.split(" ... ").collect::<Vec<&str>>();
        ContextValue {
            left_value: c[0].to_string(),
            right_value: c[1].to_string(),
        }
    }
}

impl fmt::Display for ContextValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ... {}", self.left_value, self.right_value)
    }
}

/// The representation of a Phrase with a value containing the string
/// of the phrase
#[derive(Hash, Clone, Eq, Ord, PartialEq, PartialOrd, Serialize, Debug)]
#[pyclass]
pub struct WordValue {
    /// the string of the words in the Phrase
    pub value: String,
}

impl WordValue {
    /// Returns an PhraseValue
    pub fn new(value: &str) -> Self {
        WordValue {
            value: value.to_string(),
        }
    }
}

impl fmt::Display for WordValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
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
            Text::Word(w) => write!(f, "{}", w.value),
            Text::Context(c) => write!(f, "{} ... {}", c.left_value, c.right_value)
        }
    }
}

impl Text {
    /// Returns a Text Context element with value
    pub fn context(value: &str) -> Self {
        Text::Context(ContextValue::new(value))
    }
    /// Returns a Text Word element with value
    pub fn word(value: &str) -> Self {
        Text::Word(WordValue::new(value))
    }
    /// Determine the type of value from the string
    pub fn extract(value: &str) -> Self {
        let c = value.split(" ... ").collect::<Vec<&str>>();
        if c.len() == 1 {
            Text::word(value)
        } else {
            Text::context(value)
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
    /// Returns an empty ContextCounter
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
}

impl TextMap {
    /// Return an empty TextMap
    pub fn new() -> Self {
        TextMap {
            map: IndexMap::<Text, TextCounter>::new(),
        }
    }
    /// Returns the topn items from the context multiset of the phrase
    pub fn get(&self, text: &Text, topn: Option<usize>) -> Option<IndexMap<&Text, usize>> {
        let es = self.map.get(text);
        match topn {
            Some(topn) => {
                es.map(|es| es.map[..min(es.map.len(), topn)]
                    .iter()
                    .map(|(e, n)|(e, *n))
                    .collect())
            },
            None => { 
                es.map(|es| es.map
                    .iter()
                    .map(|(e, n)|(e, *n))
                    .collect()) 
            }
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