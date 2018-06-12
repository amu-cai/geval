# GEval

GEval is a Haskell library and a stand-alone tool for evaluating the
results of solutions to machine learning challenges as defined on the
[Gonito](http://gonito.net) platform. Also could be used outside the
context of Gonito.net challenges, assuming the test data is given in
simple TSV (tab-separated values) files.

Note that GEval is only about machine learning evaluation. No actual
machine learning algorithms are available here.

The official repository is `git://gonito.net/geval`, browsable at
<https://gonito.net/gitlist/geval.git/>.

## Installing

You need [Haskell Stack](https://github.com/commercialhaskell/stack).
You could install Stack with your package manager or with:

    curl -sSL https://get.haskellstack.org/ | sh

When you've got Haskell Stack, install GEval with:

    git clone git://gonito.net/geval
    cd geval
    stack setup
    stack test
    stack install

(Note that when you're running Haskell Stack for the first time it
will take some time and a couple of gigabytes on your disk.)

By default, `geval` binary is installed in `$HOME/.local/bin`, so in
order to run `geval` you need to either add `$HOME/.local/bin` to
`$PATH` in your configuration or to type:

    PATH="$HOME/.local/bin" geval ...

### Plan B — just download the GEval binary

(Assuming you have a standard 64-bit Linux.)

    wget https://gonito.net/get/bin/geval
    chmod u+x geval
    ./geval --help

## Examples

Let us download a Gonito.net challenge:

    git clone git://gonito.net/sentiment-by-emoticons

The task is to predict the sentiment of a Polish short text -- whether
it is positive or negative (or to be precise: to guess whether a
positive or negative emoticon was used).

## Preparing a Gonito challenge

### Directory structure of a Gonito challenge

A definition of a [Gonito](http://gonito.net) challenge should be put in a separate
directory. Such a directory should
have the following structure:

* `README.md` — description of a challenge in Markdown, the first header
  will be used as the challenge title, the first paragraph — as its short
  description
* `config.txt` — simple configuration file with options the same as
  the ones accepted by `geval` binary (see below), usually just a
  metric is specified here (e.g. `--metric BLEU`), also non-default
  file names could be given here (e.g. `--test-name test-B` for a
  non-standard test subdirectory)
* `train/` — subdirectory with training data (if training data are
  supplied for a given Gonito challenge at all)
* `train/train.tsv` — the usual name of the training data file (this
  name is not required and could be more than one file), the first
  column is the target (predicted) value, the other columns represent
  features, no header is assumed
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
  `dev-0/expected.tsv`), note that this file should be “hidden” by the
  organisers of a Gonito challenge, see notes on the structure of
  commits below
* `test-B`, `test-C`, ... — other alternative test sets (if supplied)

### Initiating a Gonito challenge with geval

You can use `geval` to initiate a [Gonito](http://gonito.net) challenge:

    geval --init --expected-directory my-challenge

(This will generate a sample toy challenge about guessing planet masses).

A metric (other than the default `RMSE` — root-mean-square error) can
be given to generate another type of toy challenge:

    geval --init --expected-directory my-machine-translation-challenge --metric BLEU

### Preparing a Git repository

[Gonito](http://gonito.net) platform expects a Git repository with a challenge to be
submitted. The suggested way to do this is as follows:

1. Prepare a branch with all the files _without_
   `test-A/expected.tsv`. This branch will be cloned by people taking
   up the challenge.
2. Prepare a separate branch (or even a repo) with
   `test-A/expected.tsv` added. This branch should be accessible by
   Gonito platform, but should be kept “hidden” for regular users (or
   at least they should be kindly asked not to peek there). It is
   recommended (though not obligatory) that this branch contain all
   the source codes and data used to generate the train/dev/test sets.
   (Use [git-annex](https://git-annex.branchable.com/) if you have really big files there.)

Branch (1) should be the parent of the branch (2), for instance, the
repo (for the toy “planets” challenge) could be created as follows:

    geval --init --expected-directory planets
    cd planets
    git init
    git add .gitignore config.txt README.md train/train.tsv dev-0/{in,expected}.tsv test-A/in.tsv
    git commit -m 'init challenge'
    git remote add origin ssh://gitolite@gonito.net/filipg/planets
    git push origin master
    git branch dont-peek
    git checkout dont-peek
    git add test-A/expected.tsv
    git commit -m 'with expected results'
    git push origin dont-peek

## Taking up a Gonito challenge

Clone the repo with a challenge, as given on the [Gonito](http://gonito.net) web-site, e.g.
for the toy “planets” challenge (as generated with `geval --init`):

    git clone git://gonito.net/planets

Now use the train data and whatever machine learning tools you like to
guess the values for the dev set and the test set, put them,
respectively, as:

* `dev-0/out.tsv`
* `test-A/out.tsv`

(These files must have exactly the same number of lines as,
respectively, `dev-0/in.tsv` and `test-0/in.tsv`. They should contain
only the predicted values.)

Check the result for the dev set with `geval`:

    geval --test-name dev-0

(the current directory is assumed for `--out-directory` and `--expected-directory`).

If you'd like and if you have access to the test set results, you can
“cheat” and check the results for the test set:

    cd ..
    git clone git://gonito.net/planets planets-secret --branch dont-peek
    cd planets
    geval --expected-directory ../planets-secret

### Uploading your results to Gonito platform

Uploading is via Git — commit your “out” files and push the commit to
your own repo. On [Gonito](http://gonito.net) you are encouraged to share your code, so
be nice and commit also your source codes.

    git remote add mine git@github.com/johnsmith/planets-johnsmith
    git add {dev-0,test-A}/out.tsv
    git add Makefile magic-bullet.py ... # whatever scripts/source codes you have
    git commit -m 'my solution to the challenge'
    git push mine master

Then let Gonito pull them and evaluate your results.

## `geval` options

```
Usage: geval ([--init] | [-l|--line-by-line] | [-d|--diff OTHER-OUT])
             ([-s|--sort] | [-r|--reverse-sort]) [--out-directory OUT-DIRECTORY]
             [--expected-directory EXPECTED-DIRECTORY] [-t|--test-name NAME]
             [-o|--out-file OUT] [-e|--expected-file EXPECTED]
             [-i|--input-file INPUT] [-a|--alt-metric METRIC]
             [-m|--metric METRIC] [-p|--precision NUMBER-OF-FRACTIONAL-DIGITS]
  Run evaluation for tests in Gonito platform

Available options:
  -h,--help                Show this help text
  --init                   Init a sample Gonito challenge rather than run an
                           evaluation
  -l,--line-by-line        Give scores for each line rather than the whole test
                           set
  -d,--diff OTHER-OUT      compare results
  -s,--sort                When in line-by-line or diff mode, sort the results
                           from the worst to the best
  -r,--reverse-sort        When in line-by-line or diff mode, sort the results
                           from the best to the worst
  --out-directory OUT-DIRECTORY
                           Directory with test results to be
                           evaluated (default: ".")
  --expected-directory EXPECTED-DIRECTORY
                           Directory with expected test results (the same as
                           OUT-DIRECTORY, if not given)
  -t,--test-name NAME      Test name (i.e. subdirectory with results or expected
                           results) (default: "test-A")
  -o,--out-file OUT        The name of the file to be
                           evaluated (default: "out.tsv")
  -e,--expected-file EXPECTED
                           The name of the file with expected
                           results (default: "expected.tsv")
  -i,--input-file INPUT    The name of the file with the input (applicable only
                           for some metrics) (default: "in.tsv")
  -a,--alt-metric METRIC   Alternative metric (overrides --metric option)
  -m,--metric METRIC       Metric to be used - RMSE, MSE, Accuracy, LogLoss,
                           Likelihood, F-measure (specify as F1, F2, F0.25,
                           etc.), MAP, BLEU, NMI, ClippEU, LogLossHashed,
                           LikelihoodHashed, BIO-F1, BIO-F1-Labels or CharMatch
  -p,--precision NUMBER-OF-FRACTIONAL-DIGITS
                           Arithmetic precision, i.e. the number of fractional
                           digits to be shown
```

If you need another metric, let me know, or do it yourself!

## Licence

Apache License 2.0

## Authors

Filip Graliński
