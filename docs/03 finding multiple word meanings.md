---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.15.2
  kernelspec:
    display_name: Python 3 (ipykernel)
    language: python
    name: python3
---

# Finding different word meanings using KMeans


## Visualizing phrase-context dendrograms

```python
from selmr import SELMR

# use a SELMR object based on 10000 dbpedia pages
selmr = SELMR(
    path="..//data//dbpedia_10000",
    params={"uncased": True, "lemmatized": False}
)
```

```python
from selmr import jaccard_distance_matrix
from scipy.spatial.distance import squareform
import scipy.cluster.hierarchy as shc
import matplotlib.pyplot as plt
from collections import OrderedDict

def plot_dendrogram(c: OrderedDict = None):
    """
    Simple function to plot the dendrogram of
    """
    distances = jaccard_distance_matrix(c)
    sf_distances = squareform(distances)
    clusters = shc.linkage(
        sf_distances,
        method='ward',
        metric="euclidean"
    )
    shc.dendrogram(
        Z=clusters,
        labels=list(c.keys()),
        orientation="left",
        leaf_font_size=7,
        show_leaf_counts=True
    )
    plt.savefig('dendrogram.png', bbox_inches='tight')
    plt.show()

```

```python
phrase = "deal"

c = selmr.phrase_base_contexts(phrase=phrase)

plot_dendrogram(c)
```

For illustrative purposes, this shows only the base contexts (contexts with one word on the left side and one word on the right side of the context). The branches of the first four lines show the contexts in which the word 'deal' is used as a verb ('to deal with' and 'that deal with'). In the lower five branches the word is used as a common noun ('a deal with' and 'a deal was'). In the middle general branch the word is used in the contexts of 'great deal to' or 'good deal of' (also a noun but with a different usage).

```python
phrase = "set"

c = selmr.phrase_base_contexts(phrase=phrase, topn=25)

plot_dendrogram(c)
```

The word 'set' is a bit more complicated. The four leaves at the stop identify variations of the use of the verb 'to set up'. The four leaves at the bottom identify 'to set in' and 'to set to'. The middle leaves use the word 'set' as a noun (with one exception): set theory, a set of, one set of, etc.


## Clustering word meanings with KMeans

```python
phrase = "deal"

word_contexts = selmr.phrase_base_contexts(phrase=phrase, topn=25)

distances = jaccard_distance_matrix(word_contexts)

weights = [selmr._contexts[context][phrase] for context, values in word_contexts.items()]
```

```python
from sklearn.cluster import KMeans
import pandas as pd
from collections import defaultdict

n_clusters = 3

a = KMeans(
    n_clusters=n_clusters,
    max_iter=10000,
    n_init=250,
    tol=1e-10,
    random_state=0
).fit(distances, weights)

for cluster in range(0, n_clusters):
    base_contexts = pd.Series(word_contexts.keys())[a.labels_==cluster].values
    print("Cluster "+str(cluster)+": base_clusters = "+str(base_contexts))
```

```python
# add underlying contexts to base clusters

clustered_dict = defaultdict(dict)
for cluster in range(0, n_clusters):
    base_contexts = pd.Series(word_contexts.keys())[a.labels_==cluster].values
    cluster_contexts = set(base_contexts)
    for context in base_contexts:
        sub_contexts = [
            c for c in selmr.contexts(phrase, topn=None)
            if (c[0].count(" ")>=1 or c[1].count(" ")>=1)
            and (context[0] == c[0].split(" ")[-1] and context[1] == c[1].split(" ")[0])
            and "SENTEND" not in c[1] and "SENTSTART" not in c[0]
        ]
        cluster_contexts.update(sub_contexts)
    clustered_dict[phrase][cluster] = cluster_contexts
```

## Splitting the phrase-context multisets

```python
from selmr import Multiset

for cluster in range(0, n_clusters):
    new_word = phrase+"-"+str(cluster+1)
    selmr._phrases[new_word] = Multiset()
    for context in clustered_dict[phrase][cluster]:
        count = selmr._phrases[phrase][context]
        del selmr._phrases[phrase][context]
        selmr._phrases[new_word][context] = count
        del selmr._contexts[context][phrase]
        selmr._contexts[context][new_word] = count
```

```python
# now we can look up the most similar words of the new phrases

for cluster in range(0, n_clusters):
    new_phrase = phrase+"-"+str(cluster+1)
    print(new_phrase+": \n  "+str(selmr.most_similar(new_phrase, topn=5, topcontexts=15)))
    print("  "+str(selmr.contexts(new_phrase, topn=5)))
```

```python

```

```python

```
