# SELMR

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

## Usage

This crate is [on crates.io](https://crates.io/crates/selmr) and can be
used by adding `selmr` to your dependencies in your project"s `Cargo.toml`.

```toml
[dependencies]
selmr = "1"
```

## Example: (multi-)word similarities

Take for example the multi-word "has been suggested". The data structure based on the
plain text data of 10.000 DBpedia pages (217 Mb) returns the following.

```rust
let actual = selmr.most_similar("has been suggested", topn=10).unwrap();
let expected = HashMap::from([
    ("has been suggested", 15),
    ("is likely", 14),
    ("is possible", 14),
    ("is thought", 14),
    ("appears", 13),
    ("has been argued", 13),
    ("is believed", 13),
    ("is known", 12),
    ("is speculated", 12),
    ("can be shown", 11)
]);
assert!(actual == expected)
```

This shows the multi-words with the number of contexts that they have in common with "has been
suggested" (see below). So "is likely" has 14 contexts in common with "has been suggested".
The more contexts that a multi-word has in common the more "similar" it is. Note that this
similarity measure is only based on patterns in text; it is not a syntactical or semantic similarity
measure.

An example with the name "Aldous Huxley" which returns the following.

```rust
let actual = selmr.most_similar("Aldous Huxley", topn=10).unwrap();
let expected = HashMap::from([
    ("Aldous Huxley", 15),
    ("Herman Melville", 5),
    ("August Derleth", 5),
    ("Ben Bova", 5),
    ("Kurt Vonnegut", 5),
    ("Anton Chekhov", 5),
    ("Edgar Allan Poe", 5),
    ("Frank Herbert", 5),
    ("A E Housman", 4),
    ("Adam Smith", 4),
]);
assert!(actual == expected)
```

The results are based on the common contexts with "Aldous Huxley", with most of them
American writers. For example "Herman Melville", these are: {("about ... at", 1),
("article ... bibliography", 1), ("by ... at LibriVox", 1), ("by ... at Project", 1),
and ("like ... and", 1)}. So in this case similarities are found because DBpedia contains
a bibliography of their work and (some of) their works are available on LibriVox and
Open Library.

## How does it work: multiset representations

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

```rust
let actual = selmr.get_contexts("has", topn=10).unwrap();
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

```rust
let actual = selmr.get_phrases(("it", "been"), topn=10).unwrap();
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

To find similar multi-words, the algorithm counts for each multi-word in the corpus
the number of contexts it has in common with (the most common) contexts of the input
multi-word.

# Taking into account contexts

Some words have multiple meanings. The different usages of a word can, to some extent,
be derived from the contexts in which they are used. Take for example the word "deal"
which is used as a noun and as a verb. By adding the context to the function call we
can restrict the results to similar words that occur in the input context. We can
restrict the output by providing the context "a ... with":

```rust
let actual = selmr.most_similar(phrase="deal", context=("a", "with"), topn=10).unwrap();
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

```rust
let actual = selmr.most_similar(phrase="deal", context=("to", "with"), topn=10).unwrap();
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

# Constructing a SELMR

Create an empty SELMR data structure with

```rust
selmr = SELMR(
    min_phrase_len=1,
    max_phrase_len=3,
    min_context_len=1,
    max_context_len=3,
    min_phrase_keys=1,
    min_context_keys=2,
)
```

- min_phrase_len: the minimum number of single words in the multi-words
- max_phrase_len: the maximum number of single words in the multi-words
- min_context_len: the minimum number of single words in the left and right part of the contexts
- min_context_len: the maximum number of single words in the left and right part of the contexts
- min_phrase_keys: the minimum number of unique contexts each phrase must contain
- min_context_keys: the minimum number of unique phrases each context must contain

Then add text with:

```rust
selmr.add("We went to the park to walk. And then we went to the city to shop.")
```

Then you can run:

```rust
r = s.most_similar("city")
assert!(r == [("city", 1), ("park", 1)])
```

Contexts with more words on left or right will only be added if the number of occurrences is different, so
if "the ... to" has two occurrences and "to the ... to" has also two occurrences then this latter context
will not be added because it does not add any information.
