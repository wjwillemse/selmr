# -*- coding: utf-8 -*-

"""
"""

from collections import Counter
from fractions import Fraction

import numpy as np


class Multiset(Counter):
    def __init__(self, *args, **kwargs):
        super(Multiset, self).__init__(*args, **kwargs)

    def __and__(self, other):
        return Multiset(super(Multiset, self).__and__(other))

    def __or__(self, other):
        return Multiset(super(Multiset, self).__or__(other))

    def __sub__(self, other):
        return Multiset(super(Multiset, self).__sub__(other))

    def topn(self, topn=15):
        return Multiset({m[0]: m[1] for m in super().most_common(topn)})


def jaccard_index(c1: set = None, c2: set = None):
    """
    Function to calculate the Jaccard index of two sets
    """
    denom = len(c1 | c2)
    if denom != 0:
        return Fraction(len(c1 & c2), denom)
    else:
        return 0


def containment_index(c1: set = None, c2: set = None):
    """
    Function to calculate the containment of set B in set A
    """
    denom = len(c1)
    if denom != 0:
        return Fraction(len(c1 & c2), denom)
    else:
        return 0


def merge_multiset(d: dict = None):
    """
    Function to calculate the multiset from a dict of phrases
    """
    x = Multiset()
    for item in d.values():
        x += item
    return x


def weighted_jaccard_index(c1: Counter = None, c2: Counter = None):
    r = 0
    for item in set(c1.keys() & c2.keys()):
        r += min(c1[item], c2[item]) / max(c1[item], c2[item])
    return r


def jaccard_distance_matrix(c: dict = None):
    distances = np.zeros([len(c.keys()), len(c.keys())])
    for i, key1 in enumerate(c.keys()):
        for j, key2 in enumerate(c.keys()):
            distances[i, j] = 1 - jaccard_index(c[key1], c[key2])
    return distances
