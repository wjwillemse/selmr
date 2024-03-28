/*!
This crate provides a library for generating and using simple text data structures
that work like language models. The data structures do not use real-valued vector
embeddings; instead they use the mathematical concept of multisets and are derived
directly from plain text data.

The data structures are named Simple Explainable Language Multiset Representations
(SELMRs) and consist of multisets created from all multi-word expressions and all
multi-word-context combinations contained in a collection of documents given some
contraints. The multisets can be used for downstream NLP tasks like text classifications
and searching, in a similar manner as real-valued vector embeddings.

SELMRs produce explainable results without any randomness and enable explicit links
with lexical, linguistical and terminological annotations. No model is trained and no
dimensionality reduction is applied.

# Installation in Rust

This crate is [on crates.io](https://crates.io/crates/selmr) and can be
used by adding `selmr` to your dependencies in your project"s `Cargo.toml`.

```toml
[dependencies]
selmr = "1"
```

# Installation in Python

The Python package is also on [pypi.org](https://pypi.org/project/pyselmr) and can be
installed with

```python
pip install pyselmr
```

# Example: (multi-)word similarities

Take for example the multi-word "has been suggested". The data structure based on the
plain text data of 10.000 DBpedia pages (217 Mb) returns the following top ten most
similar multi-words

```ignore
use std::collections::HashMap;
let actual = s.most_similar("has been suggested", 10).unwrap();
let expected = HashMap::from([
    ("has been suggested", 25),
    ("has been argued", 12),
    ("has been claimed", 11),
    ("has been speculated", 11),
    ("is probable", 11),
    ("is speculated", 11),
    ("is argued", 10),
    ("is clear", 10),
    ("is more likely", 10),
    ("can be argued", 9)
]);
assert!(actual == expected)
```

This shows the multi-words with the number of contexts that they have in common with "has been
suggested" (see below). So "is probable" has 11 contexts in common with "has been suggested".
The more contexts that a multi-word has in common the more "similar" it is. Note that this
similarity measure is only based on patterns in text; it is not a syntactical or semantic
similarity measure.

An example with the name "Aldous Huxley" which returns the following.

```ignore
use std::collections::HashMap;
let actual = s.most_similar("Aldous Huxley", 10).unwrap();
let expected = HashMap::from([
    ("Aldous Huxley", 25),
    ("Ben Bova", 9),
    ("Frank Herbert", 9),
    ("A E Housman", 8),
    ("Anatole France", 8),
    ("Apuleius", 8),
    ("Blaise Pascal", 8),
    ("Clark Ashton Smith", 8),
    ("Fridtjof Nansen", 8),
    ("Henri Bergson", 8)
]);
assert!(actual == expected)
```

The results are based on the common contexts with "Aldous Huxley". For example
the coinciding contexts of "Aldous Huxley" and "Ben Bova" are:

```ignore
    ("Works by ... at LibriVox", 1),
    ("Works by ... at Open", 1),
    ("Works by ... at Project", 1),
    ("about ... at", 1),
    ("article ... bibliography", 1),
    ("by ... at", 3),
    ("by ... at LibriVox", 1),
    ("by ... at Open", 1),
    ("by ... at Project", 1),
```

So in this case similarities are found because DBpedia contains a bibliography of
their work and (some of) their works are available on LibriVox, Open (Library) and
Project (Gutenberg).

# How does it work: multiset representations

A multiset is a modification of a set that allows for multiple instances for
each of its elements. It contains elements with their number of instances,
called multiplicities. Instead of real-valued vectors, multisets contain a
variable number of elements with their multiplicities. The corresponding data
structure of a multiset is not an fixed length array of numbers but a dictionary
with elements as keys and multiplicities as values.

This concept can be used to represent multi-words in a corpus by creating a
multiset for each multi-word that contains all contexts in which the multi-word
occurs with their number of occurrences. A context of a multi-word is a combination
(a tuple) of the preceding list of words (the left side) and the following list of
words (the right side) of that multiword, with a certain maximum length and a certain
minimum number of unique occurrences.

```ignore
use std::collections::HashMap;
let actual = s.get_contexts("has", 10).unwrap();
let expected = HashMap::from([
    ("It ... been", 1001),
    ("it ... been", 991),
    ("and ... been", 615),
    ("which ... been", 493),
    ("that ... been", 416),
    ("also ... a", 401),
    ("and ... a", 401),
    ("there ... been", 401),
    ("it ... a", 324),
    ("which ... a", 318)
]);
assert!(actual == expected)
```

Below the 10 most common multi-words that fit in "it ... been" are listed.

```ignore
use std::collections::HashMap;
let actual = s.get_phrases(("it", "been"), 10).unwrap();
let expected = HashMap::from([
    ("has", 991),
    ("had", 385),
    ("may have", 83),
    ("would have", 75),
    ("has also", 59),
    ("has not", 53),
    ("might have", 25),
    ("could have", 24),
    ("not", 22),
    ("had not", 21)
]);
assert!(actual == expected)
```

The most similar multi-words are found by looking at the number of common contexts
of an input multi-word and another mult-word in the corpus. The multi-words that have
the highest number of contexts in common are the most "similar" of the input multi-word.

# Taking into account contexts

Some words have multiple meanings. The different usages of a word can, to some extent,
be derived from the contexts in which they are used. Take for example the word "deal"
which is used as a noun and as a verb. By adding the context to the function call we
can restrict the results to similar words that occur in the input context. We can
restrict the output by providing the context "a ... with":

```ignore
use std::collections::HashMap;
let actual = s.most_similar("deal", ("a", "with"), 10).unwrap();
let expected = HashMap::from([
    ("deal", 25),
    ("contract", 7),
    ("treaty", 6),
    ("dispute", 5),
    ("close relationship", 4),
    ("partnership", 4),
    ("peace treaty", 4),
    ("collaboration", 3),
    ("meeting", 4),
    ("problem", 4),
]);
assert!(actual == expected)
```

Compare these results to the results when the context is "to ... with":

```ignore
use std::collections::HashMap;
let actual = s.most_similar("deal", ("to", "with"), 10).unwrap();
let expected = HashMap::from([
    ("deal", 25),
    ("cope", 5),
    ("interact", 5),
    ("coincide", 5),
    ("come up", 5),
    ("compete", 4),
    ("comply", 4),
    ("cooperate", 4),
    ("communicate", 4),
    ("experiment", 4)
]);
assert!(actual == expected)
```

# Most similar multi-words

To find similar words, the Jaccard distance of the contexts of the input multi-words and the
contexts of each multi-word in the corpus is calculated. The most similar multi-words of input
multi-word $p$ are calculated with

$$
\displaystyle\max_{k \in \mathrm{\mathbf{P}}} 1 - J(\operatorname{Contexts}(k), \operatorname{Contexts}(p))
$$

where
- $\mathrm{\mathbf{P}}$ is the set of phrases that fits one of the contexts of multi-word $p$,
- $\operatorname{Contexts}(k)$ is the set of contexts of multi-word $k$,
- $J(A, B)$ is the Jaccard index of set $A$ and set $B$ (and $1 - J(A, B)$ is also called the Jaccard distance).

If an input context is also given then we restrict the phrases by replacing $\mathrm{\mathbf{P}}$ with
$\operatorname{Phrases}(c)$, i.e. the phrases that fit into context $c$:

$$
\displaystyle\max_{k \in \operatorname{Phrases}(c)} 1 - J(\operatorname{Contexts}(k), \operatorname{Contexts}(p))
$$

where
- $p$ is the input phrase and $c$ is the input context,
- $\operatorname{Phrases}(c)$ is the set of phrases that fit into context $c$.

Note that the most_similar function has an argument $topcontexts$ to specify the number
of most common contexts that has to be used when calculating the most similar multi-words.
Taking into account only a limited number of contexts yields better results. Similarly,
if a context is specified, then the argument $topphrases$ specifies the number of most
common multi-words that are used. Also note that we do not use the actual number of
occurrences of multi-words and contexts.

# Context-multi-word matches

You can search in the data structure with regex. For example common nouns ending with "-ion"
and start with one of the determiners "the", "a" or "an":

```ignore
let binding = s.matches(("the|a|an", "^[a-z]*ion$", ".*"))
    .expect("REASON");
let words_ion = binding
    .keys()
    .map(|(_, p, _)| p.as_str())
    .collect::<Vec<_>>();
```

# Constructing a SELMR

Create an empty SELMR data structure with

```ignore
let selmr = selmr::selmr::SELMR::new(
    1, // min_phrase_len=
    3, // max_phrase_len
    1, // min_context_len
    3, // max_context_len
    1, // min_phrase_keys
    2, // min_context_keys
    "en", // language
);
```

- min_phrase_len: the minimum number of single words in the multi-words
- max_phrase_len: the maximum number of single words in the multi-words
- min_context_len: the minimum number of single words in the left and right part of the contexts
- min_context_len: the maximum number of single words in the left and right part of the contexts
- min_phrase_keys: the minimum number of unique contexts each phrase must contain
- min_context_keys: the minimum number of unique phrases each context must contain
- language: the language of the data structure content

Then add text with:

```ignore
s.add("We went to the park to walk. And then we went to the city to shop.")
```

Then you can run:

```ignore
let r = s.most_similar("city");
assert!(r == [("city", 1), ("park", 1)])
```

Contexts with more words on left or right will only be added if the number of occurrences is different, so
if "the ... to" has two occurrences and "to the ... to" has also two occurrences then this latter context
will not be added because it does not add any information.

Write the data structure to a zip-file:

```ignore
s.write(file_name, "zip");
```

The zip-file contains one file named "data.json" that contains all data of the structure in json format.

Read the data structure from a zip file:

```ignore
s.read(file_name, "zip");
```

*/
#![warn(missing_docs)]

#[macro_use]
extern crate serde;

/// Module with core SELMR functions
pub mod selmr;
/// Module with text structs used by SELMR
pub mod text_structs;
/// Module with tokenizer
pub mod tokenizer;
