---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.14.6
  kernelspec:
    display_name: Python 3 (ipykernel)
    language: python
    name: python3
---

```python
import pickle
with open('..//data//dbpedia_phrases_10000.pickle', 'rb') as handle:
    phrases = pickle.load(handle)
with open('..//data//dbpedia_contexts_10000.pickle', 'rb') as handle:
    contexts = pickle.load(handle)
```

```python
from selmr import SELMR, LanguageMultisets

selmr = SELMR(
    multisets=LanguageMultisets(phrases, contexts)
)
```

```python
word = "discovered"
contexts = [c for c in selmr.contexts(word, topn=50) if not 'SENT' in str(c)]
c = dict()
for context in contexts:
    c[context] = selmr.most_similar(
        phrase=word,
        context=context,
        topcontexts=15,
        topphrases=15,
        topn=25
    ).keys()
```

```python
from nifigator import jaccard_index
import numpy as np
d = np.zeros([len(c.keys()), len(c.keys())])
for i, key1 in enumerate(c.keys()):
    for j, key2 in enumerate(c.keys()):
        d[i, j] = jaccard_index(c[key1], c[key2])
```

```python
import pandas as pd
from sklearn.cluster import AgglomerativeClustering
from scipy.cluster import hierarchy
import matplotlib.pyplot as plt
import seaborn as sns
clusters = hierarchy.linkage(d, method="ward", optimal_ordering=True)
plt.figure(figsize=(8, 6))
dendrogram = hierarchy.dendrogram(clusters, labels=list(c.keys()), leaf_font_size=7)
plt.axhline(150, color='red', linestyle='--')
plt.axhline(100, color='crimson')
```

```python

```

```python

```
