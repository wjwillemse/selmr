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
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;

/// The representation of a Context with a left_value and a right_value
#[derive(Hash, Clone, Eq, Ord, PartialEq, PartialOrd, Debug, Serialize)]
#[pyclass]
pub struct Context {
    /// The string of the preceding (left) words of the Context
    pub left_value: String,
    /// The string of the following (right) words of the Context
    pub right_value: String,
}

impl Context {
    /// Returns a Context
    pub fn new(value: &str) -> Self {
        let c = value.split(" ... ").collect::<Vec<&str>>();
        Context {
            left_value: c[0].to_string(),
            right_value: c[1].to_string(),
        }
    }
}

impl fmt::Display for Context {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ... {}", self.left_value, self.right_value)
    }
}

impl<'de> Deserialize<'de> for Context {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let binding = String::deserialize(deserializer)?;
        let s = binding.split(" ... ").collect::<Vec<&str>>();
        let left_value = s[0].to_string();
        let right_value = s[1].to_string();
        Ok(Context {
            left_value,
            right_value,
        })
    }
}

/// The representation of a Phrase with a value containing the string
/// of the phrase
#[derive(Hash, Clone, Eq, Ord, PartialEq, PartialOrd, Serialize, Debug)]
#[pyclass]
pub struct Phrase {
    /// the string of the words in the Phrase
    pub value: String,
}

impl Phrase {
    /// Returns an Phrase
    pub fn new(value: &str) -> Self {
        Phrase {
            value: value.to_string(),
        }
    }
}

impl fmt::Display for Phrase {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl<'de> Deserialize<'de> for Phrase {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(Phrase {
            value: s.to_string(),
        })
    }
}

/// The counter with the number of occurrences of each Context
#[derive(Debug, Clone)]
pub struct ContextCounter {
    /// the map that contains the actual Counter
    pub map: IndexMap<Context, usize>,
}

impl ContextCounter {
    /// Returns an empty ContextCounter
    pub fn new() -> Self {
        ContextCounter {
            map: IndexMap::<Context, usize>::new(),
        }
    }
}

impl Default for ContextCounter {
    fn default() -> Self {
        Self::new()
    }
}

impl Serialize for ContextCounter {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // the IndexMap is serialized as a HashMap
        // this makes the json file more readable
        let mut map = serializer.serialize_map(Some(self.map.len()))?;
        for (key, value) in &self.map {
            map.serialize_entry(&key.to_string(), value)?;
        }
        map.end()
    }
}

impl<'de> Deserialize<'de> for ContextCounter {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        Ok(ContextCounter {
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
        write!(formatter, "ContextCounter")
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

// impl<'de> Visitor<'de> for ContextCounterVisitor {
//     type Value = ContextCounter;

//     fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
//         formatter.write_str("ContextCounter")
//     }

//     fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
//     where
//         M: MapAccess<'de>,
//     {
//         let mut map = ContextCounter::new(); //with_capacity(access.size_hint().unwrap_or(0));
//         while let Some((key, value)) = access.next_entry()? {
//             map.map.insert(key, value);
//         }
//         Ok(map)
//     }
// }

// struct ContextCounterVisitor {
//     marker: PhantomData<fn() -> ContextCounter>,
// }

// impl ContextCounterVisitor {
//     fn new() -> Self {
//         ContextCounterVisitor {
//             marker: PhantomData,
//         }
//     }
// }

/// The counter with the number of occurrences of each Phrase
#[derive(Deserialize, Serialize, Clone, Debug)]
pub struct PhraseCounter {
    /// the map that contains the actual Counter
    #[serde(deserialize_with = "indexmap::map::serde_seq::deserialize")]
    pub map: IndexMap<Phrase, usize>,
}

/// The counter with the number of occurrences of each Phrase
impl PhraseCounter {
    /// Returns an empty PhraseCounter
    pub fn new() -> Self {
        PhraseCounter {
            map: IndexMap::<Phrase, usize>::new(),
        }
    }
}

impl Default for PhraseCounter {
    fn default() -> Self {
        Self::new()
    }
}

// impl Serialize for PhraseCounter {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         let mut map = serializer.serialize_map(Some(self.map.len()))?;
//         for (k, v) in &self.map {
//             map.serialize_entry(&k.to_string(), v)?;
//         }
//         map.end()
//     }
// }

// impl<'de> Deserialize<'de> for PhraseCounter {
//     fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
//     where
//         D: Deserializer<'de>,
//     {
//         deserializer.deserialize_map(PhraseCounterVisitor::new())
//     }
// }

// impl<'de> Visitor<'de> for PhraseCounterVisitor {
//     type Value = PhraseCounter;

//     fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
//         formatter.write_str("PhraseCounter")
//     }

//     fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
//     where
//         M: MapAccess<'de>,
//     {
//         let mut map = PhraseCounter::new(); //with_capacity(access.size_hint().unwrap_or(0));
//         while let Some((key, value)) = access.next_entry()? {
//             map.map.insert(key, value);
//         }
//         Ok(map)
//     }
// }

// struct PhraseCounterVisitor {
//     marker: PhantomData<fn() -> PhraseCounter>,
// }

// impl PhraseCounterVisitor {
//     pub fn new() -> Self {
//         PhraseCounterVisitor {
//             marker: PhantomData,
//         }
//     }
// }

/// The IndexMap of Phrases and their ContextCounter
#[derive(Clone)]
pub struct PhraseMap {
    /// the map contains the actual IndexMap
    pub map: IndexMap<Phrase, ContextCounter>,
}

impl PhraseMap {
    /// Return an empty PhraseMap
    pub fn new() -> Self {
        PhraseMap {
            map: IndexMap::<Phrase, ContextCounter>::new(),
        }
    }
    /// Returns the topn items from the context multiset of the phrase
    pub fn get(&self, phrase: &Phrase, topn: Option<usize>) -> Option<IndexMap<&Context, usize>> {
        let contexts = self.map.get(phrase);
        match topn {
            Some(topn) => {
                contexts.map(|contexts| {
                    contexts.map[..min(contexts.map.len(), topn)]
                    .iter()
                    .map(|(c, n)|(c, *n))
                    .collect()
                })
            },
            None => { 
                contexts.map(|contexts| {
                    contexts.map
                    .iter()
                    .map(|(c, n)|(c, *n))
                    .collect()
                })
            }
        }
    }
}

impl Default for PhraseMap {
    fn default() -> Self {
        Self::new()
    }
}

impl Serialize for PhraseMap {
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

struct PhraseMapVisitor {
    marker: PhantomData<fn() -> PhraseMap>,
}

impl PhraseMapVisitor {
    fn new() -> Self {
        PhraseMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for PhraseMapVisitor {
    type Value = PhraseMap;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("PhraseMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut phrasemap = PhraseMap::new(); //with_capacity(access.size_hint().unwrap_or(0));
        while let Some((key, value)) = access.next_entry()? {
            phrasemap.map.insert(key, value);
        }
        Ok(phrasemap)
    }
}

impl<'de> Deserialize<'de> for PhraseMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let m = PhraseMapVisitor::new();
        deserializer.deserialize_map(m)
    }
}

/// The IndexMap of Contexts and their PhraseCounter
#[derive(Default, Clone)]
pub struct ContextMap {
    /// the map contains the actual IndexMap
    pub map: IndexMap<Context, PhraseCounter>,
}

impl ContextMap {
    /// Returns an empty ContextMap
    pub fn new() -> Self {
        ContextMap {
            map: IndexMap::<Context, PhraseCounter>::new(),
        }
    }
    /// Returns the topn items from the phrase multiset of the context
    pub fn get(&self, context: &Context, topn: Option<usize>) -> Option<IndexMap<&Phrase, usize>> {
        let phrases = self.map.get(context);
        match topn {
            Some(topn) => {
                phrases.map(|phrases| phrases.map[..min(phrases.map.len(), topn)]
                    .iter()
                    .map(|(p, n)|(p, *n))
                    .collect())
            },
            None => { 
                phrases.map(|phrases| phrases.map
                    .iter()
                    .map(|(p, n)|(p, *n))
                    .collect()) 
            }
        }
    }
}

impl Serialize for ContextMap {
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

struct ContextMapVisitor {
    marker: PhantomData<fn() -> PhraseMap>,
}

impl ContextMapVisitor {
    fn new() -> Self {
        ContextMapVisitor {
            marker: PhantomData,
        }
    }
}

impl<'de> Visitor<'de> for ContextMapVisitor {
    type Value = ContextMap;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("ContextMap")
    }

    fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
    where
        M: MapAccess<'de>,
    {
        let mut map = ContextMap::new(); //with_capacity(access.size_hint().unwrap_or(0));
        while let Some((key, value)) = access.next_entry()? {
            map.map.insert(key, value);
        }
        Ok(map)
    }
}

impl<'de> Deserialize<'de> for ContextMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(ContextMapVisitor::new())
    }
}
