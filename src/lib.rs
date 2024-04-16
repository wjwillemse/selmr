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

For information on how to use this package, please look [here](https://www.mangosaurus.nl/posts/introduction-to-selmr/).

# Installation in Rust

This crate is [on crates.io](https://crates.io/crates/selmr) and can be
used by adding `selmr` to your dependencies in your project"s `Cargo.toml`.

```toml
[dependencies]
selmr = "1"
```

*/
#![warn(missing_docs)]

#[macro_use]
extern crate serde;

/// Module with Python SELMR functions
pub mod pyselmr;
/// Module with core SELMR functions
pub mod selmr;
/// Module with text structs used by SELMR
pub mod text_structs;
/// Module with tokenizer
pub mod tokenizer;
