"""
"""

import logging
import os
import pickle
from collections import OrderedDict, defaultdict

from .const import (
    MAX_CONTEXT_LENGTH,
    MAX_PHRASE_LENGTH,
    MIN_CONTEXT_COUNT,
    MIN_PHRASE_COUNT,
    MIN_PHRASECONTEXT_COUNT,
    WORDS_FILTER,
    LanguageMultisets,
)
from .extractor import extract_phrases, process_documents
from .multisets import Multiset, merge_multiset
from .tokenizer import preprocess

# from selmr import __version__


__author__ = "Willem Jan Willemse"
__copyright__ = "Willem Jan Willemse"
__license__ = "MIT"

_logger = logging.getLogger(__name__)


default_min_phrase_count = 1
default_min_phrasecontext_count = 1
default_min_context_count = 1
default_max_context_length = 5
default_max_phrase_length = 5


class SELMR(object):
    """
    SELMR (Simple Explainable Language Multiset Representations) is a class for
    creating and using multiset representations from a collection documents

    Parameters:
        documents (list): A list of documents to be used to create the
                          multiset representation.
        multisets (LanguageMultisets): An instance of LanguageMultisets.
        lang (str): The language used for processing the documents.
        params (dict): A dictionary of parameters to customize the behavior of SELMR.

    Attributes:
        multisets (LanguageMultisets): An instance of LanguageMultisets.
        lang (str): The language used for processing the documents.

    Methods:
        - set_params(params): Set custom parameters for the SELMR instance.
        - set_language(lang): Set the language for processing documents.
        - set_multisets(multisets): Set LanguageMultisets.
        - add(documents): Add documents to the multisets
        - phrases(context): Returns the phrases that fit in a context
        - contexts(phrase): Returns the contexts that in which a phrase fit
        - most_similar(phrase, context): Returns the most similar phrases
          given a phrase and optional context
        - dict_phrases_contexts(phrase): Returns a dictionary with similar
          phrases and contexts of a phrase

    Usage:
        You can create an instance of SELMR.

    """

    def __init__(
        self,
        path: str = None,
        documents: list = None,
        multisets: LanguageMultisets = None,
        lang: str = None,
        params: dict = None,
    ) -> None:
        self.set_params(params=params)
        self.set_language(lang=lang)
        self.set_multisets(multisets=multisets)
        self.set_path(path=path)
        self.add(documents=documents)

    def set_params(self, params: dict = None) -> None:
        """
        Set parameters for configuration.

        This method is used to set parameters or configuration options by
        providing a dictionary of key-value pairs. These parameters control
        the behavior and settings of selmr object.

        Parameters:
            params (dict, optional): A dictionary of key-value pairs
            representing the parameters to be set (default is None).

        Returns:
            None

        Example:
            # Set parameters using a dictionary

        Notes:
            - The specific parameters and their effect on the selmr object
              are documented in the class's documentation or implementation.

        """
        if params is None:
            self.params = {}
        else:
            self.params = params

        min_phrase_count = self.params.get(MIN_PHRASE_COUNT, None)
        if min_phrase_count is None:
            self.params[MIN_PHRASE_COUNT] = default_min_phrase_count

        min_context_count = self.params.get(MIN_CONTEXT_COUNT, None)
        if min_context_count is None:
            self.params[MIN_CONTEXT_COUNT] = default_min_context_count

        min_phrasecontext_count = self.params.get(MIN_PHRASECONTEXT_COUNT, None)
        if min_phrasecontext_count is None:
            self.params[MIN_PHRASECONTEXT_COUNT] = default_min_phrasecontext_count

        max_phrase_length = self.params.get(MAX_PHRASE_LENGTH, None)
        if max_phrase_length is None:
            self.params[MAX_PHRASE_LENGTH] = default_max_phrase_length

        max_context_length = self.params.get(MAX_CONTEXT_LENGTH, None)
        if max_context_length is None:
            self.params[MAX_CONTEXT_LENGTH] = default_max_context_length

        words_filter = self.params.get(WORDS_FILTER, None)
        if words_filter is not None:
            # reformulate to dict for efficiency
            self.params[WORDS_FILTER]["data"] = {
                phrase: True for phrase in words_filter["data"]
            }
        else:
            self.params[WORDS_FILTER] = None

    def set_language(self, lang: str = None) -> None:
        """
        Set the language for the SELMR object.

        This function allows you to set the language for the SELMR object.
        The language should be specified as a string, and it will be used
        for language-specific operations or configurations.

        Parameters:
            lang (str, optional): The language to set. If not provided,
            the language will remain unchanged.

        Returns:
            None

        Example:
            To set the language to English, you can call the function like this:
            set_language("en")

        """
        self.lang = lang

    def set_path(self, path: str = None) -> None:
        if path is not None:
            phrases = None
            lemmas = None
            file = os.path.join(path, "phrases.pickle")
            if os.path.exists(file):
                with open(file, "rb") as handle:
                    phrases = pickle.load(handle)
            file = os.path.join(path, "lemmas.pickle")
            if os.path.exists(file):
                with open(file, "rb") as handle:
                    lemmas = pickle.load(handle)
            multisets = LanguageMultisets(phrases, None, lemmas)
            self.set_multisets(multisets=multisets)

    def uncase_phrases_multisets(self, phrases: dict = None):
        """ """
        ps_uncased = defaultdict(Multiset)
        for p, cs in phrases.items():
            p_uncased = p.lower()
            cs_uncased = Multiset()
            for c, v in cs.items():
                c_uncased = (
                    c[0].lower() if c[0] != "SENTSTART" else c[0],
                    c[1].lower() if c[1] != "SENTEND" else c[1],
                )
                if c_uncased in cs_uncased.keys():
                    cs_uncased[c_uncased] += v
                else:
                    cs_uncased[c_uncased] = v
            ps_uncased[p_uncased].update(cs_uncased)
        return ps_uncased

    def lemmatize_phrases_multisets(self, phrases: dict = None):
        """ """
        assert self._phrase2lemma is not None, "No lemmas defined"
        lemmatized_ps = defaultdict(Multiset)
        for p, ls in self._phrase2lemma.items():
            for le in ls:
                lemmatized_ps[le].update(self._phrases[p])
        return lemmatized_ps

    def set_contexts_multisets(self, phrases: dict = None):
        """ """
        new_cs = defaultdict(Multiset)
        for p, cs in phrases.items():
            for c, v in cs.items():
                new_cs[c][p] = v
        return new_cs

    def set_multisets(self, multisets: LanguageMultisets = None) -> None:
        """
        Set the multiset data for the SELMR object.

        This function sets LanguageMultisets (phrases and lemmas) for the
        SELMR object.

        Parameters:
            multisets (LanguageMultisets, optional): The multisets to set.

        Returns:
            None

        Example:
            To set the multilingual data for the object using a
            `LanguageMultisets` object:
            set_multisets(multisets)

        """
        uncased = self.params.get("uncased", False)
        lemmatized = self.params.get("lemmatized", False)
        if multisets is not None:
            if multisets.phrases is not None:
                self._phrases = dict()
                for phrase, contexts in multisets.phrases.items():
                    self._phrases[phrase] = Multiset(contexts)

                self._phrase2lemma = multisets.lemmas
                if lemmatized:
                    self._phrases = self.lemmatize_phrases_multisets(self._phrases)

                if uncased:
                    self._phrases = self.uncase_phrases_multisets(self._phrases)

                self._contexts = self.set_contexts_multisets(self._phrases)

        else:
            self._phrases = defaultdict(Multiset)
            self._contexts = defaultdict(Multiset)
            self._phrase2lemma = dict()

    def add_multisets(self, multisets: LanguageMultisets = None) -> None:
        """
        Add multiset data for the SELMR object.

        This function add a LanguageMultisets (phrases and contexts)
        for the SELMR object.

        Parameters:
            multisets (LanguageMultisets, optional): The multisets to add.

        Returns:
            None

        Example:
            To add the multilingual data for the object using a
            `LanguageMultisets` object:
            add_multisets(multisets)

        """
        if multisets is not None:
            for d_phrase, d_contexts in multisets.phrases.items():
                self._phrases[d_phrase].update(d_contexts)
            for d_context, d_phrase in multisets.contexts.items():
                self._contexts[d_context].update(d_phrase)

    def add(self, documents: list = None) -> None:
        """
        Add a list of documents to extract phrase and context multisets.

        This method is used to process a list of documents, where each
        document represents a document text or a sentence text.

        Parameters:
            documents (list, optional): A list of documents to be processed
            (default is None).

        Returns:
            None

        Example:
            # Add a list of text documents

        Notes:
            - If no documents are provided, the method returns None.

        """
        if documents is not None:
            multisets = process_documents(
                documents=documents,
                params=self.params,
            )
            self.add_multisets(multisets=multisets)

    def most_similar(
        self,
        phrase: str,
        context: tuple = None,
        topphrases: int = 15,
        topcontexts: int = 15,
        topn: int = 15,
    ) -> dict:
        """
        Find the most similar phrases and contexts to a given phrase and
        optional context.

        This method calculates the most similar phrases to given phrase and
        optional context from the phrase and contexts multisets.

        Parameters:
            phrase (str): The input phrase to find similarities to.
            context (tuple, optional): A tuple context to find similarities to.
            topphrases (int, optional): The number of phrases to retrieve from
            the contexts multisets (default is 15).
            topcontexts (int, optional): The number of contexts to retrieve
            from the phrases multiset (default is 15).
            topn (int, optional): The total number of most similar phrases to
            return (default is 15).

        Returns:
            List of Tuple: A list of tuples, where each tuple contains a similar
            phrase or context and its similarity score.

        Example:
            # Find the most similar phrases to a given input phrase
            similar_phrases = selmr.most_similar("king", topn=10)
            print(similar_phrases)

            # Find the most similar phrases to a given input phrase and context
            similar_phrases = selmr.most_similar(
                "king", context=("the", "of"), topn=10
            )
            print(similar_phrases)

        Notes:
            - If none of the input parameters is provided, the function returns
            an empty dict.

        """
        assert self._contexts is not None, "No contexts multisets defined"
        assert self._phrases is not None, "No phrases multisets defined"

        if phrase is None:
            raise ValueError("No phrase defined")

        if self.params.get("lemmatized", False):
            if self._phrase2lemma is None:
                raise ValueError("No lemmas multisets defined")
            else:
                if phrase not in self._phrase2lemma.keys():
                    raise ValueError("Could not find lemma of phrase " + repr(phrase))
                else:
                    phrase = list(self._phrase2lemma[phrase])[0]

        if phrase not in self._phrases.keys():
            raise ValueError(
                "Phrase " + repr(phrase) + " not found in phrases multisets"
            )

        if context is not None and context not in self._contexts.keys():
            raise ValueError(
                "Context " + repr(context) + " not found in contexts multisets"
            )
        if phrase is None:
            raise ValueError("No phrase defined")

        contexts = list(self._phrases.get(phrase, None).topn(topcontexts).keys())
        if context is not None:
            phrases = list(self._contexts.get(context, None).topn(topphrases).keys())
            return Multiset(
                {
                    phrase: len(self._phrases[phrase].keys() & contexts)
                    for phrase in phrases[:topn]
                }
            )
        else:
            list_of_phrases = [
                self._contexts[context].topn(topn).keys() for context in contexts
            ]
            phrases = set(item for sublist in list_of_phrases for item in sublist)
            return Multiset(
                {
                    phrase: len(self._phrases[phrase].keys() & contexts)
                    for phrase in phrases
                }
            ).topn(topn)

    def contexts(
        self,
        phrase: str = None,
        left: str = None,
        right: str = None,
        topn: int = 15,
    ) -> Multiset:
        """
        Find the most similar contexts for a given phrase and optional left and
        right context.

        This method calculates and returns the most similar contexts for a provided
        phrase and optional specified left and right context.

        Parameters:
            phrase (str, optional): The input phrase to find similar contexts for
            (default is None).
            left (str, optional): The left context of the phrase (default is None).
            right (str, optional): The right context of the phrase (default is None).
            topn (int, optional): The number of most similar contexts to retrieve
            (default is 15).
            include_other_forms (bool, optional): include other inflected forms of the
            phrase in the multisets (default is False)

        Returns:
            selmr.Multisets.Multiset: A Multiset object containing the most similar
            contexts and their respective counts.

        Example:
            # Find the most similar contexts for a given phrase
            similar_contexts = selmr.phrase_contexts(phrase="has", topn=10)
            print(similar_contexts)

            # Find the most similar contexts for a left and right context
            similar_contexts = selmr.phrase_contexts(left="it", right="been", topn=5)
            print(similar_contexts)

        Notes:
            - If no input parameters are provided, the method returns an empty Multiset.

        """
        assert self._contexts is not None, "No contexts multisets defined"
        assert self._phrases is not None, "No phrases multisets defined"

        uncased = self.params.get("uncased", False)
        lemmatized = self.params.get("lemmatized", False)

        if phrase is None:
            raise ValueError("No phrase defined when calling contexts()")

        if lemmatized:
            if self._phrase2lemma is None:
                raise ValueError("No lemmas multisets defined in current object")
            else:
                if (
                    phrase not in self._phrase2lemma.keys()
                    or self._phrase2lemma[phrase] == set()
                ):
                    raise ValueError("Could not find lemma of phrase " + repr(phrase))
                else:
                    phrase = list(self._phrase2lemma[phrase])[0]

        if uncased:
            phrase = phrase.lower()

        if phrase not in self._phrases.keys():
            raise ValueError("Phrase " + phrase + " not found in phrases multisets")
        else:
            multisets = self._phrases[phrase]

        return multisets.topn(topn)

    def phrases(
        self, context: tuple = None, left: str = None, right: str = None, topn: int = 15
    ) -> Multiset:
        """
        Find the phrases from a given context.

        This function extracts and counts phrases from a specified context.

        Parameters:
            context (tuple, optional): A tuple representing the context from which to
                extract and count phrases.
            left (str, optional): A string representing the left part of the context.
                This is used to specify context
            right (str, optional): A string representing the right part of the context.
                This is used to specify context
            topn (int, optional): The number of top phrases to return. Defaults to 15.

        Returns:
            Multiset: A Multiset object containing the counted phrases as key-value
            pairs, where keys are the phrases, and values are their counts.

        Example:
            To extract and count phrases from a specific text context and retrieve the
            top 10 phrases, you can call the function like this:
            selmr.phrases(("the", "of"), topn=10)
        """
        return self._contexts.get(context, Multiset()).topn(topn)

    def dict_phrases_contexts(
        self,
        phrase: str,
        topcontexts: int = 10,
        topn: int = 7,
    ) -> dict:
        """
        Create a dictionary of phrases and their most similar contexts for a given
        phrase.

        This method generates a dictionary that associates a provided phrase with its
        most similar contexts. It allows you to explore the context in which the phrase
        commonly appears.

        Parameters:
            phrase (str): The input phrase to find similar contexts for.
            topcontexts (int, optional): The number of most similar contexts to retrieve
            from the context multisets (default is 10).
            topn (int, optional): The maximum number of similar phrases to associate
            with each context (default is 7).

        Returns:
            dict: A dictionary where keys are similar contexts, and values are lists
            of similar phrases within those contexts.

        Example:
            # Create a dictionary of phrases and their contexts
            phrase = "example phrase"
            context_dict = model.dict_phrases_contexts(phrase, topcontexts=5, topn=3)
            print(context_dict)

        Notes:
            - If no similar contexts are found, the method will return an empty
            dictionary.

        """
        contexts = self.contexts(phrase, topn=topcontexts)
        phrases = self.most_similar(phrase, topn=topn, topcontexts=topcontexts)
        d = {
            "index": phrases.keys(),
            "columns": contexts.keys(),
            "data": [],
            "index_names": ["phrase"],
            "column_names": ["left context", "right context"],
        }
        for p in phrases:
            phrase_contexts = self.contexts(p, topn=None)
            d["data"].append([phrase_contexts.get(c, 0) for c in contexts.keys()])
        return d

    def derive_multisets(
        self,
        document: str = None,
        exclude_stopwords: bool = True,
        merge_dict: bool = False,
        topn: int = 15,
        params: dict = {},
    ):
        """
        extract the phrases of a string and create dict of phrases with their contexts
        """
        lemmatized = self.params.get("lemmatized", False)
        if lemmatized:
            if self._phrase2lemma is None:
                raise ValueError("No lemmas multisets defined")

        document = preprocess(document=document, params=params)
        document_phrases = extract_phrases(documents=[document], params=params)

        if exclude_stopwords and self.params[WORDS_FILTER] is not None:
            to_delete = set()
            for phrase in document_phrases.keys():
                for word in phrase.split(" "):
                    if self.params[WORDS_FILTER]["data"].get(word, False):
                        to_delete.add(phrase)
            for phrase in to_delete:
                del document_phrases[phrase]

        res = dict()
        for phrase in document_phrases.keys():
            # if lemmatized:
            #     if self._phrase2lemma is None:
            #         raise ValueError("No lemmas multisets defined in current object")
            #     else:
            #         if phrase not in self._phrase2lemma.keys():
            #             phrase = None
            #         else:
            #             phrase = list(self._phrase2lemma[phrase])[0]

            # if uncased:
            #     phrase = phrase.lower()

            if lemmatized:
                if (
                    phrase in self._phrase2lemma.keys()
                    and self._phrase2lemma[phrase] != set()
                ):
                    res[phrase] = self.contexts(phrase, topn=topn)
            else:
                if phrase in self._phrases.keys():
                    res[phrase] = self.contexts(phrase, topn=topn)

        if merge_dict:
            res = merge_multiset(res)
        return res

    def phrase_base_contexts(
        self,
        phrase: str = None,
        topn: int = 35,
    ):
        contexts = [
            c
            for c in self.contexts(phrase, topn=topn)
            if c[0].count(" ") == 0
            and c[1].count(" ") == 0
            and "SENTEND" not in c[1]
            and "SENTSTART" not in c[0]
        ]
        c = OrderedDict()
        for context in contexts:
            c[context] = self.most_similar(
                phrase=phrase,
                context=context,
                topcontexts=None,
                topphrases=None,
                topn=None,
            )
        return c
