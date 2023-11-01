.. These are examples of badges you might want to add to your README:
   please update the URLs accordingly

.. image:: https://readthedocs.org/projects/selmr/badge/?version=latest
    :alt: ReadTheDocs
    :target: https://selmr.readthedocs.io/en/stable/
.. image:: https://img.shields.io/coveralls/github/wjwillemse/selmr/main.svg
    :alt: Coveralls
    :target: https://coveralls.io/r/wjwillemse/selmr
.. image:: https://img.shields.io/pypi/v/selmr.svg
    :alt: PyPI-Server
    :target: https://pypi.org/project/selmr/
.. image:: https://img.shields.io/conda/vn/conda-forge/selmr.svg
    :alt: Conda-Forge
    :target: https://anaconda.org/conda-forge/selmr
.. image:: https://img.shields.io/badge/-PyScaffold-005CA0?logo=pyscaffold
    :alt: Project generated with PyScaffold
    :target: https://pyscaffold.org/
.. image:: https://img.shields.io/badge/code%20style-black-000000.svg
        :target: https://github.com/psf/black
        :alt: Code style: black

=====
selmr
=====


    Python package to create and use Simple Explainable Language Multisets Representations (SELMR)

*This package is currently a personal research repo*

Create SELMRs that work like language models that allow you to

  - create from the ground up explainable word and phrase representations without random results, and to

  - combine these representations with lexical, linguistic and terminological annotations


See the `documentation <https://selmr.readthedocs.io>`_ built from the code.


Installation
============

To install selmr, run this command in your terminal:

.. code-block:: console

    $ pip install selmr

To install the package from Github

.. code-block:: console

    $ pip install -e git+https://github.com/wjwillemse/selmr.git


Making Changes & Contributing
=============================

This project uses `pre-commit`_, please make sure to install it before making any
changes::

    pip install pre-commit
    cd selmr
    pre-commit install

It is a good idea to update the hooks to the latest version::

    pre-commit autoupdate

Don't forget to tell your contributors to also install and use pre-commit.

.. _pre-commit: https://pre-commit.com/
