/*!
Selmr is a library for generating and using simple multiset embeddings that work like language models. The multiset embeddings do not use real-valued vectors; instead they use multisets and are derived directly from plain text data.
 
Multiset embeddings, called Simple Explainable Language Multiset Representations (SELMRs), consist of multisets created from all word and context combinations contained in a collection of documents given some contraints. They can be used for downstream NLP tasks like text classifications and searching, in a similar manner as real-valued vector embeddings.

SELMRs produce explainable results without any randomness and enable explicit links with lexical, linguistical and terminological annotations. No model is trained and no dimensionality reduction is applied.

For information on how to use this package, please look [here](https://www.mangosaurus.nl/posts/introduction-to-selmr/).

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
/// Module with Hierarchical Agglomerative Clustering (HAC)
pub mod hac;
