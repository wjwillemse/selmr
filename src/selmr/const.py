"""
"""

import logging
from collections import namedtuple

# from selmr import __version__

__author__ = "Willem Jan Willemse"
__copyright__ = "Willem Jan Willemse"
__license__ = "MIT"

_logger = logging.getLogger(__name__)


# Constants for parameter keys
WORDS_FILTER = "words_filter"
FORCED_SENTENCE_SPLIT_CHARACTERS = "forced_sentence_split_characters"
ADD_SENT_TOKENS = "add_sent_tokens"
REGEX_FILTER = "regex_filter"
MIN_PHRASE_COUNT = "min_phrase_count"
MIN_CONTEXT_COUNT = "min_context_count"
MIN_PHRASECONTEXT_COUNT = "min_phrasecontext_count"
MAX_PHRASE_LENGTH = "max_phrase_length"
MAX_CONTEXT_LENGTH = "max_context_length"

# Definition of the LanguageMultisets namedtuple
LanguageMultisets = namedtuple("Multisets", ["phrases", "contexts", "lemmas"])

STOPWORDS = {
    "en": [
        "a",
        "an",
        "the",
        "and",
        "but",
        "if",
        "or",
        "because",
        "as",
        "until",
        "while",
        "of",
        "at",
        "by",
        "for",
        "with",
        "about",
        "against",
        "between",
        "into",
        "through",
        "during",
        "before",
        "after",
        "above",
        "below",
        "to",
        "from",
        "up",
        "down",
        "in",
        "out",
        "on",
        "off",
        "over",
        "under",
        "further",
        "per",
    ],
    "nl": [
        "een",
        "de",
        "het",
        "en",
        "maar",
        "als",
        "of",
        "omdat",
        "van",
        "te",
        "in",
        "op",
        "aan",
        "met",
        "voor",
        "er",
        "om",
        "dan",
        "of",
        "door",
        "over",
        "bij",
        "ook",
        "tot",
        "uit",
        "naar",
        "want",
        "nog",
        "toch",
        "al",
        "dus",
        "onder",
        "tegen",
        "na",
        "reeds",
    ],
}
