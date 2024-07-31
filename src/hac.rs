use crate::pyselmr::PySELMR;
use crate::selmr::SELMR;
use crate::text_structs::{Text, TextCounter, TextMap};
use crate::selmr::Measure;
use pyo3::exceptions::PyTypeError;
use pyo3::prelude::*;
use std::collections::{HashMap, HashSet};
use std::cmp::{min, max};
use indexmap::IndexMap;
use priority_queue::PriorityQueue;
use ordered_float::OrderedFloat;

/// A TextCluster
#[derive(Clone, Debug)]
#[pyclass]
pub struct TextCluster {
	/// set of text elements contained in the cluster
	pub texts: HashSet<Text>,
	/// the multiset derived from text elements (union of multisets of text elements)
	pub multiset: TextCounter,
	/// the ordered most similar clusters with their distance
	pub most_similar_clusters: PriorityQueue<usize, OrderedFloat<f32>>,
	/// cluster from which this cluster is merged
	pub from_cluster: Option<(usize, usize)>,
	/// cluster to which this cluster is merged
	pub to_cluster: Option<usize>,
}

impl TextCluster {
	/// Create new TextCluster
    pub fn new(
    	texts: HashSet<Text>, 
    	multiset: TextCounter,
    	most_similar_clusters: PriorityQueue<usize, OrderedFloat<f32>>
    ) -> Self {
    	TextCluster {
			texts,
			multiset,
			most_similar_clusters,
			from_cluster: None,
			to_cluster: None,
		}
	}
}

/// The HAC data structure
#[derive(Clone)]
pub struct HAC {
	/// The clusters of the struct
	/// Initial cluster have the same index as the indexmap in the SELMR structure
	/// New clusters are appended with the same index of the indexmap itself
	pub clusters: IndexMap<usize, TextCluster>,
	/// Obsolete clusters (clusters that are already merged)
	pub obsolete: HashSet<usize>,
	/// latest_key (used for id of next cluster)
	pub latest_key: usize,
	/// to_process
	pub queue: PriorityQueue<usize, OrderedFloat<f32>>,
	/// measure by which hac is determined
	pub measure: String,
}

// #[inline]
// fn count_index<T: std::cmp::Eq + std::hash::Hash>(a: HashSet<T>, b: HashSet<T>) -> usize {
//     a.intersection(&b).collect::<Vec<_>>().len()
// }

// #[inline]
// fn jaccard_index<T: std::cmp::Eq + std::hash::Hash>(a: HashSet<T>, b: HashSet<T>) -> f32 {
//     a.intersection(&b).collect::<Vec<_>>().len() as f32 / max(a.len(), b.len()) as f32
// }

#[inline]
fn weighted_jaccard_index<T: std::cmp::Eq + std::hash::Hash>(
    a: IndexMap<T, usize>,
    b: IndexMap<T, usize>,
) -> f32 {
    let mut num: f32 = 0.0;
    let mut denom: f32 = 0.0;
    for context in a
        .keys()
        .collect::<HashSet<_>>()
        .union(&b.keys().collect())
    {
        num += *min(a.get(*context).unwrap_or(&0), b.get(*context).unwrap_or(&0)) as f32;
        denom += *max(a.get(*context).unwrap_or(&0), b.get(*context).unwrap_or(&0)) as f32;
    }
    if denom != 0.0 {
        num / denom
    } else {
        0.0
    }
}

/// The result of a merge (original clusters a and b, similarity score, 
/// length of new cluster, and key of new cluster)
pub struct MergeResult{
	a: usize,
	b: usize,
	similarity: OrderedFloat<f32>,
	length: usize,
	key: usize,
}

#[pyclass]
#[derive(Clone)]
/// The PyHAC data structure
pub struct PyHAC {
	/// The PyHAC encapsulates a HAC struct
	pub hac: HAC,
}

#[pymethods]
impl PyHAC {
    #[new]
    /// Create a empty HAC
	pub fn new(selmr: PySELMR, multiset_topn: usize) -> Self {
    	PyHAC { hac: HAC::new(
    		&selmr.selmr,
    		&selmr.selmr.phrases,
    		multiset_topn,
    	) }
    }
    /// iterate
  	pub fn iterate(&mut self, n: usize) -> Result<Vec<(usize, usize, f32, usize, usize)>, PyErr> {
        match self.hac.iterate(n) {
            Ok(r) => Ok(r.iter().map(|m|(m.a, m.b, m.similarity.into_inner(), m.length, m.key)).collect()),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
  	}
    /// Retrieve the two most similar clusters
  	pub fn most_similar_clusters(&mut self) -> Result<(usize, usize, f32), PyErr> {
        match self.hac.most_similar_clusters() {
            Ok(r) => Ok((r.0, r.1, r.2.into_inner())),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
  	}
  	/// Merge two clusters a and b
  	pub fn merge_clusters(&mut self, a: usize, b: usize) -> Result<(usize, usize, f32, usize, usize), PyErr> {
        match self.hac.merge_clusters(a, b) {
            Ok(r) => Ok((r.a, r.b, r.similarity.into_inner(), r.length, r.key)),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
  	}
  	/// Retrieve the text elements of the cluster with the given key
  	pub fn get_cluster_text_elements(&self, key: usize) -> Result<HashSet<String>, PyErr>{
  		Ok(self.hac.clusters[&key].texts
  			.iter()
  			.map(|p|p.to_string())
  			.collect::<HashSet<String>>())
  	}
  	/// Retrieve the contexts multiset of the cluster with the given key
  	pub fn get_cluster_multiset(&self, key: usize) -> Result<Vec<(String, usize)>, PyErr>{
  		Ok(self.hac.clusters[&key].multiset.map
  			.iter()
  			.map(|(i, n)|(i.to_string(), *n))
  			.collect::<Vec<(String, usize)>>())
  	}
  	/// Retrieve the most similar clusters of the cluster with the given key
  	pub fn get_cluster_most_similar(&self, key: usize) -> Result<Vec<(usize, f32)>, PyErr>{
  		Ok(self.hac.clusters[&key].most_similar_clusters
  			.iter()
  			.map(|(p, v)|(*p, v.into_inner()))
  			.collect())
  	}
  	/// Retrieve the data of the cluster with the given key
  	pub fn get_cluster_by_index(&self, key: usize) -> Result<(HashSet<String>, Vec<(String, usize)>, Vec<(usize, f32)>, Option<(usize, usize)>, Option<usize>), PyErr> {
  		Ok((
			self.hac.clusters[&key].texts
				.iter()
				.map(|p|p.to_string())
				.collect::<HashSet<String>>(),
			self.hac.clusters[&key].multiset.map
				.iter()
				.map(|(i, n)|(i.to_string(), *n))
				.collect::<Vec<(String, usize)>>(),
			self.hac.clusters[&key].most_similar_clusters
				.clone()
				.into_sorted_iter()
				.map(|(p, v)|(p, v.into_inner()))
				.collect(),
			self.hac.clusters[&key].from_cluster,
			self.hac.clusters[&key].to_cluster,
  		))
  	}
  	/// Retrieve the number of clusters
  	pub fn get_number_of_clusters(&self) -> Result<usize, PyErr> {
  		Ok(self.hac.clusters.len() - self.hac.obsolete.len())
  	}
  	/// Retrieve the number of cluster yet to process
  	pub fn get_number_of_clusters_to_process(&self) -> Result<usize, PyErr> {
  		Ok(self.hac.queue.len())
  	}
  	/// Retrieve cluster tree
  	pub fn get_tree(&self) -> Result<HashMap<usize, (Option<(usize, usize)>, Option<usize>)>, PyErr> {
        match self.hac.get_tree() {
            Ok(r) => Ok(r),
            Err(e) => Err(PyErr::new::<PyTypeError, _>(e)),
        }
  	}
}

impl HAC {
    /// Returns a HAC data structure
    pub fn new(
    	selmr: &SELMR, 
    	textmap: &TextMap, 
    	multiset_topn: usize
    ) -> Self {
    	let clusters = IndexMap::<usize, TextCluster>::new();
    	let obsolete = HashSet::<usize>::with_capacity(textmap.map.keys().len());
    	let latest_key = 0;
    	let queue = PriorityQueue::new();
    	let measure = "weighted_jaccard".to_string();
    	let mut hac = HAC {
    		clusters,
    		obsolete,
    		latest_key,
			queue,
			measure,
    	};
    	hac.initialize(selmr, textmap, multiset_topn);
    	hac
	}
	/// Initialize the HAC
	pub fn initialize(&mut self,
		selmr: &SELMR,
		textmap: &TextMap, 
		multiset_topn: usize
	) {
		// creating new index for clusters
		for text in textmap.map.keys() {
			let key = textmap.map.get_index_of(text).unwrap();
			let texts = HashSet::from([text.clone()]);
			let multiset_map = selmr.get_multiset(text, Some(multiset_topn), false).unwrap();
			let multiset = TextCounter {
				map: multiset_map
					.iter()
					.map(|(c, v)| ((*c).clone(), *v))
					.collect()
			};
			let mut most_similar_list = match selmr.most_similar_from_multiset (
				&multiset_map,
				None, // no specific context
				None, // all topcontexts
				None, // all topn
				Measure::WeightedJaccardIndex,
				false,
			) {
				Ok(r) => r.iter().map(|(p, v)|(p.clone(), OrderedFloat(*v))).collect(),
				Err(e) => {
					println!("Error: {:?}", e);
					Vec::<(Text, OrderedFloat<f32>)>::new()
				},
			};
			// if the text is similar to itself, so this one is deleted
			most_similar_list.retain(|(t, _)|*t!=*text);
			// replace most similar text with indices of texts in indexmap
			let mut most_similar = PriorityQueue::<usize, OrderedFloat<f32>>::new();
			for (ms_text, n) in most_similar_list.iter() {
				most_similar.push(textmap.map.get_index_of(ms_text).unwrap(), *n);
			}
			// if there is at least one most similar context then the cluster is added
			if !most_similar.is_empty() {
				self.queue.push(
					key, 
					*most_similar.peek().unwrap().1);
				let cluster_struct = TextCluster::new(
					texts,
					multiset,
					most_similar,
				);
				self.clusters.insert(key, cluster_struct);
			}
		}
		for item in self.clusters.keys() {
			if *item > self.latest_key {
				self.latest_key = *item;
			}
		}
		println!("Number of clusters: {:?}", self.clusters.len());
	}
	/// Get the two most similar clusters and their similarity 
	/// and remove them from the priority queue
	pub fn most_similar_clusters(&mut self) -> Result<(usize, usize, OrderedFloat<f32>), String> {
		match self.queue.pop() {
			Some((a, similarity)) => {
				let b = *self.clusters[&a].most_similar_clusters.peek().unwrap().0;
				self.queue.remove(&b);
				Ok((a, b, similarity))
			},
			None => Err("No clusters in processing queue".to_string())
		}
	}
	/// merge two clusters a and b, add new cluster and keep a and b
	pub fn merge_clusters(
		&mut self, 
		a: usize, 
		b: usize
	) -> Result<MergeResult, String> {
		if !self.obsolete.contains(&a) && !self.obsolete.contains(&b) {
			let key = self.latest_key + 1;
     		// merge cluster
			let a_texts = self.clusters[&a].texts.clone();
			let b_texts = self.clusters[&b].texts.clone();
			let texts: HashSet<Text> = a_texts
				.union(&b_texts)
				.cloned()
				.collect();
			let a_cluster = &self.clusters[&a].clone();
			let b_cluster = &self.clusters[&b].clone();
			// merge cluster context multiset
			let mut multiset = a_cluster.multiset.clone();
			for (p, n) in b_cluster.multiset.map.iter() {
				multiset.map
	                .entry(p.clone())
	                .and_modify(|count| *count += n)
	                .or_insert(*n);
			}
			// construct the most similar of the merged cluster
			let mut most_similar = PriorityQueue::<usize, OrderedFloat<f32>>::new();
			let a_most_similar = a_cluster.most_similar_clusters
					.iter()
					.map(|(i, _)|*i)
					.collect::<HashSet::<usize>>();
			let b_most_similar = b_cluster.most_similar_clusters
					.iter()
					.map(|(i, _)|*i)
					.collect::<HashSet::<usize>>();
			for &item in a_most_similar.union(&b_most_similar) {
				if item != a && item != b {
					// add to most similar of merged cluster
		   			let new_value = OrderedFloat(
		   				weighted_jaccard_index(
		   					self.clusters[&item].multiset.map.clone(), 
		   					multiset.map.clone())
		   			);
					most_similar.push(item, new_value);
					// correct similarities in other cluster
	   				let mut item_cluster = self.clusters.get(&item).unwrap().clone();
					item_cluster.most_similar_clusters.remove(&a);
					item_cluster.most_similar_clusters.remove(&b);
					item_cluster.most_similar_clusters.push(key, new_value);
					if !item_cluster.most_similar_clusters.is_empty() {
				        self.queue.push(
				        	item, *item_cluster.most_similar_clusters.peek().unwrap().1);
					}
			        self.clusters.insert(item, item_cluster);
				}
			}
			// new clusters that are added have the same index as the indexmap
			let length = texts.len();
			if !most_similar.is_empty() {
				self.queue.push(key, *most_similar.peek().unwrap().1);
			}
			// println!("a {:?} b {:?} -> {:?} - {:?}", a, b, texts, *a_cluster.most_similar_clusters.peek().unwrap().1);
			self.clusters.insert(
				key,
				TextCluster::new(
					texts, 
					multiset,
					most_similar,
				)
			);
			self.latest_key += 1;
			self.obsolete.insert(a);
			self.obsolete.insert(b);
			let similarity = *a_cluster.most_similar_clusters.peek().unwrap().1;
			Ok(MergeResult{a, b, similarity, length, key})
		} else {
			Err(format!("Cluster is already obsolete: {} or {}", a, b))
		}
	}
	/// perform iteration to merge n times two most similar clusters
	pub fn iterate(&mut self, n: usize) -> Result<Vec<MergeResult>, String> {
		let mut results = Vec::<MergeResult>::new();
		for _i in 0..n {
			if let Ok((a, b, _)) = self.most_similar_clusters() {
				let c = self.merge_clusters(a, b).unwrap();
				self.clusters[&c.key].from_cluster = Some((a, b));
				self.clusters[&a].to_cluster = Some(c.key);
				self.clusters[&b].to_cluster = Some(c.key);
				results.push(c);
			} else {
				return Ok(results)
			}
		}
		Ok(results)
	}
	/// Get the tree of related clusters
  	pub fn get_tree(&self) -> Result<HashMap<usize, (Option<(usize, usize)>, Option<usize>)>, String> {
  		let mut r = HashMap::<usize, (Option<(usize, usize)>, Option<usize>)>::new();
  		for key in self.clusters.keys() {
  			if self.clusters.get(key).unwrap().from_cluster.is_some() || self.clusters.get(key).unwrap().to_cluster.is_some() {
  				r.insert(
					*key,
					(
						self.clusters.get(key).unwrap().from_cluster,
						self.clusters.get(key).unwrap().to_cluster, 
  					)
  				);
  				// println!("{:?} -> {:?} , {:?}", key, self.clusters.get(key).unwrap().from_cluster.clone(), self.clusters.get(key).unwrap().to_cluster.clone())
  			}
  		};
  		Ok(r)
  	}
}
