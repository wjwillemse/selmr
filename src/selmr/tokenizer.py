"""
"""

import logging

import regex as re
import syntok.segmenter as segmenter

from .const import ADD_SENT_TOKENS, FORCED_SENTENCE_SPLIT_CHARACTERS, REGEX_FILTER

# from selmr import __version__


__author__ = "Willem Jan Willemse"
__copyright__ = "Willem Jan Willemse"
__license__ = "MIT"

_logger = logging.getLogger(__name__)

default_regex_filter = None  # "^[0-9]*[a-zA-Z]*$"


def preprocess(
    document: str = None,
    params: dict = {},
):
    """ """
    split_characters = params.get(FORCED_SENTENCE_SPLIT_CHARACTERS, [])
    regex_filter = params.get(REGEX_FILTER, default_regex_filter)
    add_sent_tokens = params.get(ADD_SENT_TOKENS, False)
    # tokenize documents into sentences
    sentences = [
        [word["text"] for word in sentence]
        for sentence in tokenize_text(document, split_characters)
    ]
    if regex_filter is not None:
        # select tokens given a regex filter and add start and end of sentence
        # tokens SENTSTART and SENTEND
        if add_sent_tokens:
            preprocessed = [
                ["SENTSTART"]
                + [word for word in sentence if re.match(regex_filter, word)]
                + ["SENTEND"]
                for sentence in sentences
            ]
        else:
            preprocessed = [
                [word for word in sentence if re.match(regex_filter, word)]
                for sentence in sentences
            ]
    else:
        if add_sent_tokens:
            preprocessed = [
                ["SENTSTART"] + sentence + ["SENTEND"] for sentence in sentences
            ]
        else:
            preprocessed = sentences
    return preprocessed


def tokenize_text(text: list = None, forced_sentence_split_characters: list = []):
    """ """
    tokenized_text = tokenizer(text)
    tokenized_new = []
    for sentence in tokenized_text:
        tok_sent = []
        for token in sentence:
            if token["text"] in forced_sentence_split_characters:
                if tok_sent != []:
                    tokenized_new.append(tok_sent)
                tok_sent = []
            tok_sent.append(token)
        tokenized_new.append(tok_sent)
    tokenized_text = tokenized_new
    # delete empty tokens
    if tokenized_text != []:
        tokenized_text = [
            sentence if sentence[-1]["text"] != "" else sentence[:-1]
            for sentence in tokenized_text
        ]
    return tokenized_text


def tokenizer(text: str = None):
    """
    Function to create list of sentences with list of words
    with text and start_char and end_char of each word

    :param text: the text to be tokenized

    """
    sentences = list()
    for paragraph in segmenter.analyze(text):
        for sentence in paragraph:
            words = list()
            for token in sentence:
                value = text[token.offset : token.offset + len(token.value)]
                if value != token.value:
                    _logger.error("Error: incorrect offsets in syntok.segmenter.")
                else:
                    words.append(
                        {
                            "text": token.value,
                            "start_char": token.offset,
                            "end_char": token.offset + len(token.value),
                        }
                    )
            sentences.append(words)
    return sentences
