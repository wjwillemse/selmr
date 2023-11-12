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

# Search engine




```python
import os, sys, logging
logging.basicConfig(stream=sys.stdout,
                    format='%(asctime)s %(message)s',
                    level=logging.INFO)
```


## load DBpedia data

```python
from rdflib import URIRef

database_url = 'http://localhost:3030/dbpedia_en'
identifier = URIRef("https://mangosaurus.eu/dbpedia")
```

```python
from rdflib.plugins.stores.sparqlstore import SPARQLUpdateStore
from nifigator import NifGraph

# Connect to triplestore
store = SPARQLUpdateStore(
    query_endpoint = database_url+'/sparql',
    update_endpoint = database_url+'/update'
)
# Create NifVectorGraph with this store
g = NifGraph(
    store=store,
    identifier=identifier
)
```


Import the vector representations of the phrases, lemmas and contexts


```python
lang = "en"

stop_words_en = [
    'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of',
    'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during',
    'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off',
    'over', 'under', 'further',
    'per',
]
# set the parameters to create the NifVector graph
params = {
    "words_filter": {
        "data": stop_words_en if lang=="en" else stop_words_nl,
        "name": "nifvec.stopwords"
    },
}
```

```python
from selmr import SELMR, LanguageMultisets

# construct a SELMR data structure with the DBpedia phrases and contexts
selmr = SELMR(
    path="..//data//dbpedia_10000",
    params={**params, **{"uncased": True, "lemmatized": False}}
)
```

```python
selmr.most_similar("revealed", topcontexts=15)
```

### Load and set up two DBpedia pages


We take two pages from DBpedia about two stars, Aldebaran and Antares.

```python
# Read two documents in DBpedia about Aldebaran and Antares stars
doc1 = g.get(
    URIRef("http://dbpedia.org/resource/Aldebaran?dbpv=2020-07&nif=context")
)
doc2 = g.get(
    URIRef("http://dbpedia.org/resource/Antares?dbpv=2020-07&nif=context")
)
```

The first document reads:

```python
print(doc1)
```

```console
(nif:Context) uri = <http://dbpedia.org/resource/Aldebaran?dbpv=2020-07&nif=context>
  sourceUrl : <http://en.wikipedia.org/wiki/Aldebaran?oldid=964792900&ns=0>
  predLang : <http://lexvo.org/id/iso639-3/eng>
  isString : 'Aldebaran , designated α Tauri (Latinized to Alpha Tauri, abbreviated Alpha Tau, α Tau), is an orange giant star measured to be about 65 light-years from the Sun in the zodiac constellation Taurus. It is the brightest star in Taurus and generally the fourteenth-brightest star in the night sky, though it varies slowly in brightness between magnitude 0.75 and 0.95. Aldebaran is believed to host a planet several times the mass of Jupiter, named Aldebaran b. Aldebaran is a red giant, cooler than the sun with a surface temperature of 3,900 K, but its radius is about 44 times the sun\'s, so it is over 400 times as luminous. It spins slowly and takes 520 days to complete a rotation. The planetary exploration probe Pioneer 10 is heading in the general direction of the star and should make its closest approach in about two million years.\n\nNomenclature\nThe traditional name Aldebaran derives from the Arabic al Dabarān, meaning "the follower", because it seems to follow the Pleiades. In 2016, the I... '
  firstSentence : 'Aldebaran , designated α Tauri (Latinized to Alpha Tauri, abbreviated Alpha Tau, α Tau), is an orange giant star measured to be about 65 light-years from the Sun in the zodiac constellation Taurus.'
  lastSentence : '* Daytime occultation of Aldebaran by the Moon (Moscow, Russia) YouTube video'
```

THe second document reads:

```python
print(doc2)
```

```console
(nif:Context) uri = <http://dbpedia.org/resource/Antares?dbpv=2020-07&nif=context>
  sourceUrl : <http://en.wikipedia.org/wiki/Antares?oldid=964919229&ns=0>
  predLang : <http://lexvo.org/id/iso639-3/eng>
  isString : 'Antares , designated α Scorpii (Latinised to Alpha Scorpii, abbreviated Alpha Sco, α Sco), is on average the fifteenth-brightest star in the night sky, and the brightest object in the constellation of Scorpius. Distinctly reddish when viewed with the naked eye, Antares is a slow irregular variable star that ranges in brightness from apparent magnitude +0.6 to +1.6. Often referred to as "the heart of the scorpion", Antares is flanked by σ Scorpii and τ Scorpii near the center of the constellation. Classified as spectral type M1.5Iab-Ib, Antares is a red supergiant, a large evolved massive star and one of the largest stars visible to the naked eye. Its exact size remains uncertain, but if placed at the center of the Solar System, it would reach to somewhere between the orbits of Mars and Jupiter. Its mass is calculated to be around 12 times that of the Sun. Antares is the brightest, most massive, and most evolved stellar member of the nearest OB association, the Scorpius–Centaurus Associ... '
  firstSentence : 'Antares , designated α Scorpii (Latinised to Alpha Scorpii, abbreviated Alpha Sco, α Sco), is on average the fifteenth-brightest star in the night sky, and the brightest object in the constellation of Scorpius.'
  lastSentence : '* Best Ever Image of a Star’s Surface and Atmosphere - First map of motion of material on a star other than the Sun'
```


## Find similar sentences


For sentences similarities we sum the contexts of the all the phrases in the sentences, thereby obtaining a multiset representation of the sentence. Then we calculate the Jaccard distance between the sentences and sort with increasing distance.

The Jaccard index is

```{math}
J(A, B) = \frac { | A \bigcap B |} { |A \bigcup B| }
```


Create a vector of every sentences of both documents.


```python
# setup dictionary with sentences and their vector representation
doc1_vector = {
    sent.anchorOf: selmr.derive_multisets(
        document=sent.anchorOf,
        merge_dict=True
    )
    for sent in doc1.sentences
}
doc2_vector = {
    sent.anchorOf: selmr.derive_multisets(
        document=sent.anchorOf,
        merge_dict=True
    )
    for sent in doc2.sentences
}
```

Calculate the distances (based on Jaccard index) of all sentence combinations of first and second document.


```python
from selmr.multisets import jaccard_index, merge_multiset, weighted_jaccard_index

# Calculate the Jaccard distance for all sentence combinations
d = {
    (s1, s2): weighted_jaccard_index(c1, c2)
    for s1, c1 in doc1_vector.items()
    for s2, c2 in doc2_vector.items()
}
# Sort the results with lowest distance
similarities = sorted(d.items(), key=lambda item: item[1], reverse=True)
```

Print the results


```python
# print the results
for item in similarities[0:6]:
    print(repr(item[0][0]) + " \n<- distance = {0:.4f}".format(item[1])+" ->\n"+repr(item[0][1])+"\n")
```


## Explainable text search


For text search we need another distance function. Now we are interested in the extent to which the vector of a sentence contains the vector representation of a query. For this we use the containment or support, defined by the cardinality of the intersection between A and B divided by the cardinality of A.

```{math}
containment(A, B) = \frac { | A \bigcap B |} { |A| }
```

The sentence with the highest containment has the most contexts in common and thus is the closest to the text.

```python
from selmr import SELMR, LanguageMultisets

# construct a SELMR data structure with the DBpedia phrases and contexts
selmr = SELMR(
    path="..//data//dbpedia_10000",
    params={**params, **{"uncased": True, "lemmatized": True}}
)
```

```python
# setup dictionary with sentences and their contexts
v_doc_lemmas = {
    sent.anchorOf: selmr.derive_multisets(
        document=sent.anchorOf
    )
    for sent in doc1.sentences+doc2.sentences
}
```


### Using the MinHashSearch

```python
# from selmr import MinHashSearch

# mhs = MinHashSearch(
#     selmr=selmr,
#     documents=v_doc_lemmas
# )
```

```python
# import pickle
# with open('..\\data\\minhash.pickle', 'wb') as fh:
#     pickle.dump(mhs.minhash_dict, fh)
```

```python
import pickle
with open('..\\data\\minhash.pickle', 'rb') as fh:
    minhash_dict = pickle.load(fh)
```

```python
from selmr import MinHashSearch

mhs = MinHashSearch(
    selmr=selmr,
    minhash_dict=minhash_dict,
    documents=v_doc_lemmas,
)
```

```python
from nifigator import jaccard_index

s1 = 'large'
s2 = 'small'
print("estimated Jaccard index: "+str(mhs.minhash_dict[s2].jaccard(mhs.minhash_dict[s1])))
print("actual Jaccard index: "+str(float(jaccard_index(
    set(p[0] for p in selmr.contexts(s1).most_common(15)),
    set(p[0] for p in selmr.contexts(s2).most_common(15)),
))))
```

```python
query = "the brightest star in the constellation of Taurus"
scores = mhs.get_scores(query, selmr=selmr)
for item, distance in list(scores.items())[0:3]:
    print('Score {0:.4f}: '.format(float(distance)) + repr(item))
```

```python
query = "the sun in the Taurus cluster"
scores = mhs.get_scores(query, selmr=selmr)
for item, distance in list(scores.items())[0:3]:
    print('Score {0:.4f}: '.format(float(distance)) + repr(item))
```

```python
query = "scientist Herschel find to Aldebaran"
scores = mhs.get_scores(query, selmr=selmr)
for item, distance in list(scores.items())[0:5]:
    print('Score {0:.4f}: '.format(float(distance)) + repr(item))
```

```python
i = mhs.matches(
   "scientist Herschel find to Aldebaran",
   'English astronomer William Herschel discovered a faint companion to Aldebaran in 1782; an 11th magnitude star at an angular separation of 117″.'
)
print("Full matches:")
for key, values in i.full_matches.items():
    for value in values:
        print("  "+repr(key))
print("Close matches:")
for key, values in i.close_matches.items():
    for value in values:
        print("  "+repr(key) + " -> " +repr(value[0]) + " ({0:.4f})".format(float(value[1])))

```

```python

```
