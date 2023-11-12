import sys

if sys.version_info[:2] >= (3, 8):
    # TODO: Import directly (no need for conditional) when `python_requires = >= 3.8`
    from importlib.metadata import PackageNotFoundError, version  # pragma: no cover
else:
    from importlib_metadata import PackageNotFoundError, version  # pragma: no cover

try:
    # Change here if project is renamed and does not equal the package name
    dist_name = __name__
    __version__ = version(dist_name)
except PackageNotFoundError:  # pragma: no cover
    __version__ = "unknown"
finally:
    del version, PackageNotFoundError

from .const import (
    FORCED_SENTENCE_SPLIT_CHARACTERS,
    MAX_CONTEXT_LENGTH,
    MAX_PHRASE_LENGTH,
    MIN_CONTEXT_COUNT,
    MIN_PHRASE_COUNT,
    MIN_PHRASECONTEXT_COUNT,
    REGEX_FILTER,
    WORDS_FILTER,
    LanguageMultisets,
)
from .extractor import (
    extract_contexts,
    extract_phrases,
    generate_sentence_phrases,
    process_documents,
)
from .multisets import (
    Multiset,
    containment_index,
    jaccard_distance_matrix,
    jaccard_index,
    weighted_jaccard_index,
)
from .search import MinHashSearch
from .selmr import SELMR
from .tokenizer import preprocess, tokenize_text, tokenizer
