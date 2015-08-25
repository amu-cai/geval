# GEval

GEval is a library (and a stand-alone tool) for evaluating the results
of solutions to machine learning challenges as defined in the Gonito
platform.

Note that GEval is only about machine learning evaluation. No actual
machine learning algorithms are available here.

## Installing

You need [Haskell Stack](https://github.com/commercialhaskell/stack),
then install with:

    git clone https://github.com/filipg/geval
    cd geval
    stack setup
    stack install

By default `geval` library is installed in `$HOME/.local/bin`, so to
run `geval` you need to either add `$HOME/.local/bin` to `$PATH` or to
type:

    PATH="$HOME/.local/bin" geval

## Directory structure of a Gonito challenge

A definition of a Gonito challenge should be put in a separate
directory (preferably as a separate Git repo). Such a directory should
have the following structure:

* `config.txt` — simple configuration file with options the same as
  the ones accepted by `geval` binary (see below), usually just a
  metric is specified here (e.g. `--metric BLEU`), also non-default
  file names could be given here (e.g. `--test-name test-B` for a
  non-standard test subdirectory)
* `README.md` — description of a challenge in Markdown
* `train/` — subdirectory with training data (if training data are
  supplied for a given Gonito challenge at all)
* `train/train.tsv` — the usual name of training data (this name is
  not required and could be more than file), the first column is the
  target (predicted value), the other columns represent features, no
  header is assumed
* `dev-0/` — subdirectory with a development set (a sample test set,
  which won't be used for the final evaluation)
* `dev-0/in.tsv` — input data (the same format as `train/train.tsv`,
  but without the first column)
* `dev-0/expected.tsv` — values to be guessed (note that `paste
  dev-0/expected.tsv dev-0/in.tsv` should give the same format as
  `train/train.tsv`)
* `dev-1/`, `dev-2`, ... — other dev sets (if supplied)
* `test-A/` — subdirectory with the test set
* `test-A/in.tsv` — test input (the same format as `dev-0/in.tsv`)
* `test-A/expected.tsv` — values to be guessed (the same format as
  `dev-0/expected.tsv`), note that this file should be "hidden" by the
  organizers of a Gonito challenge, see notes on the structure of
  commits below
* `test-B`, `test-C`, ... — other alternative test sets (if supplied)
