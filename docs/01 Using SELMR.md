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

# Using SELMR

## Introduction

A *Simple Explainable Language Multiset Representation* (SELMR) is a text data structure that works like a language model. The SELMR data structure consists of multisets created from all phrases (i.e. multiword expressions) and all phrase-context combinations contained in a collection of documents given some contraints. The multisets can be used for downstream NLP tasks like text classifications and searching, similar to real-valued vector embeddings.

SELMRs produce explainable results without any randomness and enable explicit links with lexical, linguistical and terminological annotations. No model is trained to get real-valued vector embeddings and no dimensionality reduction is applied.

The building blocks for SELMR are phrases and contexts.

* A phrase can be any list of consecutive words that occurs in a collection of documents, with a certain maximum length and a certain minimum number of occurrences.

* A context of a phrase is the combination of the preceding list of words (the left side) and the following list of words (the right side) of that phrase, with a certain maximum length and a certain minimum number of occurrences.

Of each phrase the SELMR data structure contains the contexts in which the phrase occurs with the number of that phrase-context combinations in the documents (forming a multiset or a collections.Counter in Python). Of each context the SELMR data structure contains the multiset with the phrases that occur in the context with their respective number of occurrences in the documents.

```python
documents = [
    "We walked in the beautiful park.",
    "Then we did some shopping in the city."
]
```

```python
from selmr import SELMR

# Create a SELMR data structure given the two sentences above
selmr = SELMR(
    documents=documents
)
```

```python
selmr.contexts("city")
```

```console
Multiset({('the', '.'): 1, ('in the', '.'): 1, ('shopping in the', '.'): 1})
```

```python
selmr.contexts("beautiful park")
```

```console
Multiset({('the', '.'): 1, ('in the', '.'): 1, ('walked in the', '.'): 1})
```

```python
selmr.most_similar("city")
```

```console
Multiset({'city': 3, 'beautiful park': 2})
```


## SELMR based on DBpedia

These are results of a SELMR created with 10.000 DBpedia pages. We defined a context of a word in it simplest form: the tuple of the previous multiwords and the next multiwords (no preprocessing, no changes to the text, i.e. no deletion of stopwords and punctuation). The maximum phrase length is five words, the maximum left and right context length is also five words.

```python
import logging, sys
logging.basicConfig(stream=sys.stdout,
                    format='%(asctime)s %(message)s',
                    level=logging.DEBUG)
```

```python
from selmr import SELMR, LanguageMultisets

# construct a SELMR data structure with the DBpedia phrases and contexts
selmr = SELMR(
    path="..//data//dbpedia_10000",
    params={"uncased": False, "lemmatized": False}
)
```

### Most frequent contexts of a phrase

The ten most frequent contexts in which the word 'has' occurs with their number of occurrences are the following:

```python
# most frequent contexts of the word "has"
selmr.contexts("has", topn=10)
```

This results in

```console
Multiset({('It', 'been'): 2014,
          ('it', 'been'): 1970,
          ('SENTSTART It', 'been'): 1858,
          ('and', 'been'): 1201,
          ('which', 'been'): 987,
          ('that', 'been'): 813,
          ('and', 'a'): 806,
          ('also', 'a'): 774,
          ('there', 'been'): 764,
          ('which', 'a'): 624})
```

This means that the corpus contains ... occurrences of 'It has been', i.e. occurrences where the word 'has' occurred in the context ('It', 'been').

SENTSTART and SENTEND are tokens to indicate the start and end of a sentence. This makes it possible to derive the contexts of a phrase that starts or ends a sentence.


### Phrase and context frequencies


The contexts in which a word occurs represent to some extent the properties and the meaning of a word. If you derive the phrases that share the most frequent contexts of the word 'has' then you get the following table (the columns contains the contexts, the rows the phrases that have the most contexts in common):

```python
import pandas as pd
pd.DataFrame().from_dict(
    selmr.dict_phrases_contexts("has", topcontexts=10), orient='tight'
)
```

This results in:

```console
                 It     it      SENTSTART It    and 	which 	that 	and 	also 	there 	which
                 been   been    been            been 	been 	been 	a 	a 	been 	a
has              2014   1970    1858            1201 	987 	813 	806 	774 	764 	624
had              139    815     130             327 	1696 	1388 	623 	524 	350 	306
would have       26     156     25              19 	110 	97 	2 	2 	31 	4
may have         48     151     48              113 	146 	85 	6 	0 	60 	6
have             0      19      0               412 	477 	942 	370 	299 	773 	185
has not          14     104     14              34 	27 	47 	0 	0 	10 	0
could have       2      42      2               4 	17 	40 	0 	0 	7 	0
```


The contexts that a word has in common with contexts of another word can be used as a measure of similarity. The word 'had' (second row) has eight contexts in common with the word 'has' so this word is very similar. The phrase 'would have' (seventh row) has seven contexts in common, so 'would have' is also similar but less similar than the word 'had'. We used a limited number of contexts to show the idea; normally a higher number of contexts can be used to compare the similarity of words.

The word similarities found can in this case explained as follows. Similar words are forms of the verb 'have'. This is because the verb is often used in the construction of perfect tenses where the verb 'have' is combined with the past participle of another verb, in this case the often occuring 'been'. Note that the list contains 'has not'.


### Phrase similarities


Based on the approach above we can derive top phrase similarities.

```python
# top phrase similarities of the word "has"
selmr.most_similar("has been suggested", topn=10, topcontexts=25)
```

This results in

```console
Multiset({'has been suggested': 25,
          'is possible': 15,
          'is believed': 13,
          'is likely': 12,
          'is thought': 11,
          'is known': 10,
          'has been argued': 10,
          'is estimated': 10,
          'has been speculated': 10,
          'appears': 10})
```

Now take a look at similar words of 'larger'.

```python
# top phrase similarities of the word "larger"
selmr.most_similar("larger", topn=10, topcontexts=15)
```

Resulting in:

```console
Multiset({'larger': 15,
          'smaller': 14,
          'higher': 13,
          'greater': 13,
          'longer': 11,
          'faster': 11,
          'less': 11,
          'more': 10,
          'better': 10,
          'shorter': 10})
```

Like the word 'larger' these are all comparative adjectives. These words are similar because they share the most frequent contexts, in this case contexts like (is, than) and (much, than).

```python
# top phrase similarities of the word "might"
selmr.most_similar("might", topn=10, topcontexts=25)
```

```console
Multiset({'might': 25,
          'may': 25,
          'should': 25,
          'would': 25,
          'could': 25,
          'must': 24,
          'would not': 22,
          'will': 22,
          'can': 21,
          'may not': 21})
```

Most frequent coinciding contexts are in this case ('it', 'be'), ('he', 'have') and ('that', 'be').

Contexts can also be used to find 'semantic' similarities.

```python
# top phrase similarities of the word "King"
selmr.most_similar("king", topn=10, topcontexts=25)
```

This results in

```console
Multiset({'king': 25,
          'King': 25,
          'ruler': 23,
          'Emperor': 22,
          'emperor': 22,
          'president': 21,
          'Queen': 21,
          'President': 21,
          'head': 20,
          'Prime Minister': 20})


```



Instead of single words we can also find the similarities of multiwords

```python
# top phrase similarities of Barack Obama
selmr.most_similar("Barack Obama", topn=10, topcontexts=25)
```

```console
Multiset({'Barack Obama': 17,
          'Ronald Reagan': 6,
          '': ,
          'of the United States': 5,
          'Franklin D Roosevelt': 4,
          'Bill Clinton': 4,
          'George W Bush': 4,
          'Bush': 4,
          'Lyndon B Johnson': 4,
          'Lukashenko': 3})
```


### Most frequent phrases of a context


Here are some examples of the most frequent phrases of a context.

```python
context = ("King", "of England")
for r in selmr.phrases(context, topn=10).items():
    print(r)
```

```console
('Charles II', 59)
('Charles I', 42)
('James I', 38)
('Henry VIII', 34)
('Edward I', 27)
('James II', 25)
('Henry VII', 24)
('John', 20)
('Henry III', 19)
('Edward III', 18)
```

```python
context = ("the", "city")
for r in selmr.phrases(context, topn=10).items():
    print(r)
```

```console
('capital', 355)
('largest', 266)
('inner', 120)
('old', 114)
('ancient', 92)
('first', 91)
('second largest', 88)
('capital and largest', 84)
('host', 66)
('centre of the', 60)
```

```python
context = ("he", "that")
for r in selmr.phrases(context, topn=10).items():
    print(r)
```

```console
('believed', 244)
('said', 207)
('stated', 194)
('argued', 148)
('felt', 135)
('wrote', 115)
('argues', 108)
('found', 97)
('noted', 89)
('claimed', 88)
```


### Phrase similarities given a specific context

Some phrases have multiple meanings. Take a look at the contexts of the word 'deal':

```python
selmr.contexts("deal", topn=10)
```

<!-- #region -->
This results in:

```console
Multiset({('to', 'with'): 946,
          ('great', 'of'): 649,
          ('a great', 'of'): 610,
          ('to', 'with the'): 332,
          ('a', 'with'): 225,
          ('good', 'of'): 86,
          ('a good', 'of'): 83,
          ('had to', 'with'): 71,
          ('the', 'SENTEND'): 57,
          ('a', 'to'): 53})
```


In some of these contexts 'deal' is a verb meaning 'to do business' and in other contexts 'deal' is a noun meaning a 'contract' or an 'agreement'. The specific meaning can be derived from the context in which the phrase is used.

It is possible to take into account a specific context when using the most_similar function in the following way:
<!-- #endregion -->

```python
selmr.most_similar(phrase="deal", context=("to", "with"), topcontexts=50, topphrases=100, topn=10)
```

The result is:

```console
Multiset({'deal': 50,
          'work': 21,
          'compete': 10,
          'comply': 8,
          'cope': 8,
          'communicate': 7,
          'do': 6,
          'live': 6,
          'meet': 5,
          'be confused': 4})
```

So these are all verbs, similar to the verb 'deal'.

```python
selmr.most_similar(phrase="deal", context=("a", "with"), topcontexts=50, topphrases=100, topn=10)
```

In this case the result is:

```console
Multiset({'deal': 50,
          'contract': 15,
          'treaty': 13,
          'relationship': 12,
          'meeting': 12,
          'man': 10,
          'person': 9,
          'partnership': 7,
          'collaboration': 4,
          'joint venture': 3})
```

So, now the results are nouns, and similar to the noun 'deal'.


### Phrase similarities given a set of contexts

If you want to find the phrases that fit a set of contexts then this is also possible.

```python
c1 = [
        c[0] for c in (
            selmr.contexts("considered", topn=None) &
            selmr.contexts("believed", topn=None)
         ).most_common(15)
]
```

This results in:

```console
[('is', 'to'),
 ('is', 'to be'),
 ('are', 'to'),
 ('was', 'to'),
 ('are', 'to be'),
 ('is', 'to have'),
 ('is', 'to be the'),
 ('was', 'to be'),
 ('were', 'to'),
 ('generally', 'to'),
 ('are', 'to have'),
 ('is', 'to be a'),
 ('is', 'by'),
 ('widely', 'to'),
 ('he', 'to')]
```

```python
# Not implemented yet
# selmr.most_similar(contexts=c1, topn=10)
```
