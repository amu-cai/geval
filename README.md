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

## Quick tour

Let's use GEval to evaluate machine translation (MT) systems (but keep in
mind than GEval could be used for many other machine learning task
types).

First, we will run GEval on WMT-2017, a German-to-English machine
translation challenge repackaged for [Gonito.net](https://gonito.net)
platform and available there (though, in a moment you'll see it can be
run on other test sets, not just the ones conforming to specific
Gonito.net standards). Let's download one of the solutions, it's just
available via git, so you don't have to click anywhere, just type:

    git clone git://gonito.net/wmt-2017 -b submission-01229

Let's step into the repo and run GEval (I assume you added `geval`
path to `$PATH`, so that you could just use `geval` instead of
`/full/path/to/geval`):

    cd submission-01229
    geval

Well, something went wrong:

    geval: No file with the expected results: `./test-A/expected.tsv`

The problem is that the official test set is hidden from you (although
you can find it if you are determined...) You should try running GEval
on the dev set instead:

    geval -t dev-0

and you'll see the result — 0.27358 in [BLEU
metric](https://en.wikipedia.org/wiki/BLEU), which is the default
metric for the WMT-2017 challenge. GEval could do the evaluation using
other metrics, in case of machine translation, (Google) GLEU (alternative to
BLEU) or simple accuracy might make sense:

    geval -t dev-0 --metric GLEU --metric Accuracy

If you wait a moment, you'll see the results:

    BLEU	0.27358
    GLEU	0.31404
    Accuracy	0.01660

Ah, we forgot about the tokenization, in order to properly calculate
BLEU (or GLEU) the way it was done within the official WMT-2017
challenge, you need to tokenize the output of your system and the
expected system using the right tokenizer:

    geval -t dev-0 --metric GLEU --metric Accuracy --tokenizer 13a

    BLEU	0.26901
    GLEU	0.30514
    Accuracy	0.01660

The results do not look good anyway and I'm not talking about
Accuracy, which even for a good MT (or even a human) will be low (as
it measures how many translations are exactly the same as the golden
standard), but rather about BLEU which is not impressive for this
particular task. Actually, it's no wonder as the system we're
evaluating now is a very simple neural machine translation baseline.
Out of curiosity, let's have a look at the worst items, i.e. sentences
for which the GLEU metric is the lowest (GLEU is better than BLEU for
item-per-item evaluation); it's easy with GEval:

    geval -t dev-0 --alt-metric GLEU --line-by-line --sort | head -n 10

    0.0	Tanzfreudiger Nachwuchs gesucht	Dance-crazy youths wanted	Dance joyous offspring sought
    0.0	Bulgarische Gefängnisaufseher protestieren landesweit	Bulgaria 's Prison Officers Stage National Protest	Bulgarian prison guards protest nationwide
    0.0	Schiffe der Küstenwache versenkt	Coastguard ships sunk	Coast Guard vessels sinking
    0.0	Gebraucht kaufen	Buying used	Needed buy
    0.0	Mieten	Renting	Rentals
    0.0	E-Books	E-books	E-Books
    0.021739130434782608	Auch Reservierungen in Hotels gehen deutlich zurück.	There is even a marked decline in the number of hotel reservations .	Reservations also go back to hotels significantly .
    0.023809523809523808	Steuerbelastung von Geschäftsleuten im Raum Washington steigt mit der wirtschaftlichen Erholung	Washington-area business owners " tax burden mounts as economy rebounds	Tax burden of businessmen in the Washington area rises with economic recovery
    0.03333333333333333	Verwunderte Ärzte machten Röntgenaufnahmen seiner Brust und setzen Pleurakathether an, um Flüssigkeit aus den Lungen zu entnehmen und im Labor zu testen.	Puzzled doctors gave him chest X-rays , and administered pleural catheters to draw off fluid from the lungs and send it for assessment .	At the end of his life , she studied medicine at the time .
    0.03333333333333333	Die Tradition der Schulabschlussbälle in den USA wird nun auf die Universitäten übertragen, wo Freshmen Auftritte mit dem Privatflugzeug angeboten werden.	US prom culture hits university life with freshers offered private jet entrances	The tradition of school leavers in the U.S. is now transferred to universities , where freshmen are offered appearances with the private plane .

Well, this way we found some funny utterances for which even a single
word was recovered, but could we get more insight?

The good news is that you could use GEval to debug the MT system in a
black-box manner to find its weak points -- --worst-features is the
option to do this:

    geval -t dev-0 --alt-metric GLEU --worst-features | head -n 10

This command will find the top 10 "worst" features (in either input,
expected output or actual output), i.e. the features which correlate
with low GLEU values in the most significant way.

    exp:"	346	0.27823151	0.00000909178949766883
    out:&apos;&apos;	348	0.28014113	0.00002265047322460752
    exp:castle	23	0.20197660	0.00006393156973075869
    exp:be	191	0.27880383	0.00016009575605100586
    exp:road	9	0.16307514	0.00025767878872874620
    exp:out	78	0.26033671	0.00031551452260174863
    exp:(	52	0.25348798	0.00068739029500072100
    exp:)	52	0.25386216	0.00071404713888387060
    exp:club	28	0.22958093	0.00078051481428704770
    out:`	9	0.17131601	0.00079873676961809170

## Another example

Let us download a Gonito.net challenge:

    git clone git://gonito.net/sentiment-by-emoticons

The task is to predict the sentiment of a Polish short text -- whether
it is positive or negative (or to be precise: to guess whether a
positive or negative emoticon was used). The train set is given
in the `train/train.tsv.xz` file, each item is given in a separate file,
have a look at the first 5 items:

    xzcat train/train.tsv.xz | head -n 5

Now let's try to evaluate some solution to this challenge. Let's fetch it:

    git fetch git://gonito.net/sentiment-by-emoticons submission-01865
    git reset --hard FETCH_HEAD

and now run geval:

    geval -t dev-0

(You need to run `dev-0` test as the expected results for the `test-A`
test is hidden from you.) The evaluation result is 0.47481. This might
be hard to interpret, so you could try other metrics.

    geval -t dev-0 --metric Accuracy --metric Likelihood

So now you can see that the accuracy is over 78% and the likelihood
(i.e. geometric mean of probabilities of the correct classes) is 0.62.

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

Then let Gonito pull them and evaluate your results, either manually clicking
"submit" at the Gonito web site or using `--submit` option (see below).

### Submitting a solution to a Gonito platform with GEval

A solution to a machine learning challenge can be submitted with the
special `--submit` option:

    geval --submit --gonito-host HOST --token TOKEN

where:

* _HOST_ is the name of the host with a Gonito platform
* _TOKEN_ is a special per-user authorisation token (can be copied
  from "your account" page)

_HOST_ must be given when `--submit` is used (unless the creator of the challenge
put `--gonito-host` option in the `config.txt` file, note that in such a case using
`--gonito-host` option will result in an error).

If _TOKEN_ was not given, GEval attempts to read it from the `.token`
file, and if the `.token` file does not exist, the user is asked to
type it (and then the token is cached in `.token` file).

GEval with `--submit` does not commit or push changes, this needs to
be done before running `geval --submit`. On the other hand, GEval will
check whether the changes were committed and pushed.

Note that using `--submit` option for the main instance at
<https://gonito.net> is usually **NOT** needed, as the git
repositories are configured there in such a way that an evaluation is
triggered with each push anyway.

## `geval` options

```
geval - stand-alone evaluation tool for tests in Gonito platform

Usage: geval ([--init] | [-v|--version] | [-l|--line-by-line] |
             [-w|--worst-features] | [-d|--diff OTHER-OUT] |
             [-m|--most-worsening-features ARG] | [-j|--just-tokenize] |
             [-S|--submit]) ([-s|--sort] | [-r|--reverse-sort])
             [--out-directory OUT-DIRECTORY]
             [--expected-directory EXPECTED-DIRECTORY] [-t|--test-name NAME]
             [-o|--out-file OUT] [-e|--expected-file EXPECTED]
             [-i|--input-file INPUT] [-a|--alt-metric METRIC]
             [-m|--metric METRIC] [-p|--precision NUMBER-OF-FRACTIONAL-DIGITS]
             [-T|--tokenizer TOKENIZER] [--gonito-host GONITO_HOST]
             [--token TOKEN]
  Run evaluation for tests in Gonito platform

Available options:
  -h,--help                Show this help text
  --init                   Init a sample Gonito challenge rather than run an
                           evaluation
  -v,--version             Print GEval version
  -l,--line-by-line        Give scores for each line rather than the whole test
                           set
  -w,--worst-features      Print a ranking of worst features, i.e. features that
                           worsen the score significantly. Features are sorted
                           using p-value for Mann-Whitney U test comparing the
                           items with a given feature and without it. For each
                           feature the number of occurrences, average score and
                           p-value is given.
  -d,--diff OTHER-OUT      Compare results of evaluations (line by line) for two
                           outputs.
  -m,--most-worsening-features ARG
                           Print a ranking of the "most worsening" features,
                           i.e. features that worsen the score the most when
                           comparing outputs from two systems.
  -j,--just-tokenize       Just tokenise standard input and print out the tokens
                           (separated by spaces) on the standard output. rather
                           than do any evaluation. The --tokenizer option must
                           be given.
  -S,--submit              Submit current solution for evaluation to an external
                           Gonito instance specified with --gonito-host option.
                           Optionally, specify --token.
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
                           etc.), multi-label F-measure (specify as
                           MultiLabel-F1, MultiLabel-F2, MultiLabel-F0.25,
                           etc.), MAP, BLEU, NMI, ClippEU, LogLossHashed,
                           LikelihoodHashed, BIO-F1, BIO-F1-Labels or CharMatch
  -p,--precision NUMBER-OF-FRACTIONAL-DIGITS
                           Arithmetic precision, i.e. the number of fractional
                           digits to be shown
  -T,--tokenizer TOKENIZER Tokenizer on expected and actual output before
                           running evaluation (makes sense mostly for metrics
                           such BLEU), minimalistic, 13a and v14 tokenizers are
                           implemented so far. Will be also used for tokenizing
                           text into features when in --worst-features and
                           --most-worsening-features modes.
  --gonito-host GONITO_HOST
                           Submit ONLY: Gonito instance location.
  --token TOKEN            Submit ONLY: Token for authorization with Gonito
                           instance.
```

If you need another metric, let me know, or do it yourself!

## Licence

Apache License 2.0

## Authors

Filip Graliński

## Contributors

Piotr Halama
