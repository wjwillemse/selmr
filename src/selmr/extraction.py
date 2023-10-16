"""
"""

import logging
from collections import Counter, defaultdict

from .const import (
    MAX_CONTEXT_LENGTH,
    MAX_PHRASE_LENGTH,
    MIN_CONTEXT_COUNT,
    MIN_PHRASE_COUNT,
    MIN_PHRASECONTEXT_COUNT,
    WORDS_FILTER,
    LanguageMultisets,
)
from .tokenizer import preprocess

# from selmr import __version__


__author__ = "Willem Jan Willemse"
__copyright__ = "Willem Jan Willemse"
__license__ = "MIT"

_logger = logging.getLogger(__name__)


def process_documents(documents: list = None, params: dict = {}):
    """
    Process a list of documents to extract and analyze phrases and contexts using
    the specified LanguageMultisets and parameters.

    Args:
        documents (list): A list of documents to be processed.
        params (dict): A dictionary of parameters to customize the processing.

    Returns:
        LanguageMultisets: An instance of LanguageMultisets containing extracted
        phrases and their associated contexts.

    This function performs the following steps:
    1. Preprocess each document using the specified parameters.
    2. Extract initial phrases from the preprocessed documents.
    3. Extract contexts for the initial phrases based on the documents.
    4. Create a dictionary that maps phrases to their respective contexts.
    5. Create and return a new LanguageMultisets instance with the extracted phrases
       and contexts.

    Example Usage:
    ```
    # Create a documents instance
    documents = []

    # Define parameters
    custom_params = {
        'param1': value1,
        'param2': value2,
    }

    # Process documents and obtain LanguageMultisets
    result = process_documents(
        documents=my_documents,
        params=custom_params
    )
    ```

    Note:
    - The 'params' argument allows customization of the processing behavior.
    - The returned LanguageMultisets instance contains phrases and their associated
      contexts.

    """
    documents = [preprocess(document=document, params=params) for document in documents]
    init_phrases = extract_phrases(documents=documents, params=params)
    contexts = extract_contexts(
        init_phrases=init_phrases, documents=documents, params=params
    )
    # create a new dict with all phrases contained in the the context multisets
    phrases = defaultdict(Counter)
    for d_context, d_phrases in contexts.items():
        for d_phrase, d_value in d_phrases.items():
            phrases[d_phrase][d_context] = d_value
    return LanguageMultisets(phrases, contexts)


def extract_phrases(documents: list = None, params: dict = {}):
    """
    Extract phrases from a collection of documents.

    This function analyzes the provided documents and extracts phrases based on the
    specified parameters.

    Args:
        documents (list): A list of documents.
        params (dict): A dictionary containing custom parameters for phrase extraction.

    Returns:
        dict: A dictionary of extracted phrases, where keys are phrases and values are
        sub-dictionaries with document indices and their respective phrase locations.

    This function performs the following steps:
    1. Iterate through the documents and generate phrases within each document.
    2. Create a dictionary that maps phrases to their occurrences in different
       documents.
    3. Remove phrases that occur less frequently than the specified minimum
       phrase count.
    4. Return the dictionary of extracted phrases.

    Args Description:
    - 'documents' should be a list of documents.
    - 'params' allows customization of the phrase extraction process.

    Returns Description:
    - The returned dictionary contains phrases and their associated occurrences in
      documents.

    Example Usage:
    ```
    # Define documents and parameters
    my_documents = [
        'document1': 'This is a sample document with phrases.',
        'document2': 'Another document for phrase extraction.',
    ]
    custom_params = {
        'min_phrase_count': 2,
        'custom_option': 'value',
    }

    # Extract phrases from the documents
    extracted_phrases = extract_phrases(documents=my_documents, params=custom_params)
    ```

    Note:
    - The 'params' dictionary can be used to customize the phrase extraction behavior.
    - Phrases that occur less frequently than the specified minimum count are removed
      from the results.

    """
    _logger.debug(".. extracting phrases")

    min_phrase_count = params.get(MIN_PHRASE_COUNT, 1)

    # create a dict for each phrase that contain the phrase locations
    phrases = defaultdict(lambda: defaultdict(set))
    for document_idx, sentences in enumerate(documents):
        for phrase, loc in generate_sentence_phrases(
            sentences=sentences, params=params
        ):
            phrases[phrase][document_idx].add(loc)

    # delete all phrases that occur less than then the min_phrase_count
    to_delete = set()
    for phrase, docs in phrases.items():
        if sum(len(loc) for loc in docs.values()) < min_phrase_count:
            to_delete.add(phrase)
    for phrase in to_delete:
        del phrases[phrase]

    _logger.debug(".... found phrases: " + str(len(phrases.keys())))

    return phrases


def extract_contexts(
    init_phrases: dict = None, documents: list = None, params: dict = {}
):
    """
    Extract and analyze contexts for a set of initial phrases within the given
    documents.

    This function examines the relationships between phrases and their surrounding
    context to identify meaningful contexts.

    Args:
        init_phrases (dict): A dictionary of initial phrases and their document
        occurrences.
        documents (list): A list of documents.
        params (dict): A dictionary containing custom parameters for context
        extraction.

    Returns:
        dict: A dictionary of extracted contexts, where keys are context tuples, and
        values are phrase counters indicating the presence of phrases in those
        contexts.

    This function performs the following steps:
    1. Initialize and create an initial dictionary for contexts based on phrases and
       document occurrences.
    2. Process and analyze these initial contexts to identify meaningful context
       relationships.
    3. Continuously evaluate and extend the contexts based on phrase co-occurrence.
    4. Return the final dictionary of extracted contexts.

    Args Description:
    - 'init_phrases' is a dictionary of phrases and their occurrences in documents.
    - 'documents' is a list of documents.
    - 'params' allows customization of the context extraction process.

    Returns Description:
    - The returned dictionary contains context tuples and their associated phrase
      counters.

    Example Usage:
    # Define initial phrases, documents, and parameters
    initial_phrases = {
        'phrase1': {0: {(1, 2, 3)}, 1: {(0, 2, 4)}},
        'phrase2': {0: {(2, 3, 4)}},
    }
    my_documents = [
        'This is a sample document with phrases.',
        'Another document for context extraction.',
    ]
    custom_params = {
        'max_context_length': 5,
        'min_context_count': 2,
    }

    # Extract contexts from the initial phrases and documents
    extracted_contexts = extract_contexts(
        init_phrases=initial_phrases,
        documents=my_documents,
        params=custom_params
    )

    Note:
    - The 'params' dictionary can be used to customize the context extraction behavior.
    - The returned dictionary contains meaningful contexts and their associated phrase
      counts.
    """
    _logger.debug(".. extracting contexts started")

    max_context_length = params.get(MAX_CONTEXT_LENGTH, 5)
    min_context_count = params.get(MIN_CONTEXT_COUNT, 1)
    min_phrasecontext_count = params.get(MIN_PHRASECONTEXT_COUNT, 1)
    phrase_sep = " "

    init_contexts = defaultdict(lambda: defaultdict(lambda: defaultdict(set)))
    for phrase, docs in init_phrases.items():
        for doc, locs in docs.items():
            for sent_idx, begin_idx, end_idx in locs:
                sent = documents[doc][sent_idx]
                if begin_idx - 1 >= 0 and end_idx + 1 <= len(sent):
                    left = sent[begin_idx - 1]
                    right = sent[end_idx]
                    init_contexts[(left, right)][phrase][doc].add(
                        (sent_idx, begin_idx, end_idx)
                    )
    del init_phrases

    to_process_contexts = dict()
    for d_context, d_phrases in init_contexts.items():
        if len(d_phrases.keys()) > 1:
            to_process_contexts[d_context] = (d_phrases, 1, 1)

    # aggegrate results into contexts dict
    final_contexts = defaultdict(Counter)
    for d_context, d_phrases in init_contexts.items():
        d_phrase_counter = Counter(
            {
                d_phrase: sum(len(loc) for loc in docs.values())
                for d_phrase, docs in d_phrases.items()
                if sum(len(loc) for loc in docs.values()) >= min_phrasecontext_count
            }
        )
        if (
            len(d_phrase_counter.keys()) > 0
            and sum(v for v in d_phrase_counter.values()) >= min_context_count
        ):
            final_contexts[d_context] = d_phrase_counter
        else:
            if d_context in to_process_contexts.keys():
                del to_process_contexts[d_context]

    del init_contexts

    _logger.debug(".... added contexts: " + str(len(to_process_contexts)))

    while to_process_contexts != dict():
        new_contexts = defaultdict(lambda: defaultdict(lambda: defaultdict(set)))
        for d_context, (
            (d_phrases, left_size, right_size)
        ) in to_process_contexts.items():
            for phrase, docs in d_phrases.items():
                for doc, locs in docs.items():
                    for sent_idx, begin_idx, end_idx in locs:
                        sent = documents[doc][sent_idx]
                        if d_context == (
                            phrase_sep.join(sent[begin_idx - left_size : begin_idx]),
                            phrase_sep.join(sent[end_idx : end_idx + right_size]),
                        ):
                            # right
                            if (
                                begin_idx - left_size >= 0
                                and end_idx + right_size + 1 <= len(sent)
                            ):
                                left = phrase_sep.join(
                                    sent[begin_idx - left_size : begin_idx]
                                )
                                right = phrase_sep.join(
                                    sent[end_idx : end_idx + right_size + 1]
                                )
                                new_contexts[(left, right)][phrase][doc].add(
                                    (sent_idx, begin_idx, end_idx)
                                )
                            # left
                            if (
                                begin_idx - left_size - 1 >= 0
                                and end_idx + right_size <= len(sent)
                            ):
                                left = phrase_sep.join(
                                    sent[begin_idx - left_size - 1 : begin_idx]
                                )
                                right = phrase_sep.join(
                                    sent[end_idx : end_idx + right_size]
                                )
                                new_contexts[(left, right)][phrase][doc].add(
                                    (sent_idx, begin_idx, end_idx)
                                )

        # determine contexts for further processing
        to_process_contexts = dict()
        for ((left_part, right_part)), d_phrases in new_contexts.items():
            if (
                len(d_phrases.keys()) > 1
                and len(left_part.split(phrase_sep)) < max_context_length
                and len(right_part.split(phrase_sep)) < max_context_length
            ):
                to_process_contexts[(left_part, right_part)] = (
                    d_phrases,
                    len(left_part.split(phrase_sep)),
                    len(right_part.split(phrase_sep)),
                )

        # add new contexts to contexts
        for d_context, d_phrases in new_contexts.items():
            d_phrase_counter = Counter(
                {
                    d_phrase: sum(len(loc) for loc in docs.values())
                    for d_phrase, docs in d_phrases.items()
                    if (
                        sum(len(loc) for loc in docs.values())
                        >= min_phrasecontext_count
                    )
                }
            )
            if len(d_phrase_counter.keys()) > 0 and (
                sum(v for v in d_phrase_counter.values()) >= min_context_count
            ):
                final_contexts[d_context] = d_phrase_counter
            else:
                if d_context in to_process_contexts.keys():
                    del to_process_contexts[d_context]

        _logger.debug(".... added contexts: " + str(len(to_process_contexts)))

    _logger.debug(".... total contexts: " + str(len(final_contexts.keys())))

    return final_contexts


def generate_sentence_phrases(
    sentences: list = None,
    params: dict = {},
):
    """
    Generate and yield phrases along with their locations within sentences.

    This function iterates through the provided sentences and generates phrases
    of varying lengths. Phrases are yielded along with their respective locations
    in the sentences.

    Args:
        sentences (list): A list of sentences to generate phrases from.
        params (dict): A dictionary containing custom parameters for phrase generation.

    Yields:
        tuple: A tuple containing a generated phrase and its location within the
        sentence.

    This generator function performs the following steps:
    1. Iterate through the sentences.
    2. Generate phrases of varying lengths starting from each word.
    3. Yield each generated phrase along with its location if it meets specified
       criteria.

    Args Description:
    - 'sentences' is a list of sentences to generate phrases from.
    - 'params' allows customization of the phrase generation process.

    Yields Description:
    - The generator yields tuples, where the first element is the generated phrase
    and the second element is a tuple specifying the location within the sentence.

    Example Usage:
    ```
    # Define a list of sentences and parameters
    my_sentences = [
        ['This', 'is', 'a', 'sample', 'sentence.'],
        ['Another', 'example', 'sentence', 'for', 'phrase', 'generation.']
    ]
    custom_params = {
        'max_phrase_length': 4,
        'words_filter': {
            'data': {
                'is': True,
                'for': True,
            }
        }
    }

    # Generate phrases from the sentences and process them
    for phrase, location in generate_sentence_phrases(sentences=my_sentences,
    params=custom_params):
        print(f"Generated phrase: {phrase} | Location: {location}")
    ```

    Note:
    - The 'params' dictionary can be used to customize the phrase generation behavior.
    - The generator yields phrases and their locations, subject to optional
      filtering criteria.

    """
    phrase_sep = " "
    max_phrase_length = params.get(MAX_PHRASE_LENGTH, 5)
    words_filter = params.get(WORDS_FILTER, None)
    for sent_idx, sentence in enumerate(sentences):
        for word_idx, word in enumerate(sentence):
            for phrase_length in range(1, max_phrase_length + 1):
                if word_idx + phrase_length <= len(sentence):
                    phrase_list = [
                        sentence[word_idx + i] for i in range(0, phrase_length)
                    ]
                    phrase = phrase_sep.join(word for word in phrase_list)
                    if words_filter is None:
                        yield (
                            phrase,
                            (sent_idx, word_idx, word_idx + phrase_length),
                        )
                    else:
                        # phrases may not start or end with one of the stopwords
                        phrase_stop_words = [
                            words_filter["data"].get(word.lower(), False)
                            for word in [phrase_list[0], phrase_list[-1]]
                        ]
                        if not any(phrase_stop_words):
                            yield (
                                phrase,
                                (sent_idx, word_idx, word_idx + phrase_length),
                            )
