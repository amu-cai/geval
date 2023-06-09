
## 1.42.2.0 (2022-11-18)

Changes:

* Switch to Stack LTS 19.25
* … much faster compilation as a by-product

## 1.42.1.0 (2022-11-18)

Bug fixes:

* Switch to better random number generator for bootstrap sampling

## 1.42.0.0 (2022-09-08)

Enhancements:

* Add WAR, CAR, Mean/WAR, Mean/CAR metrics

Bug fixes:

* Improve handling empty texts in WER and CER

## 1.41.1.0 (2022-08-17)

Enhancements:

* Add CustomMetric1 metric (for one of PolEval competitions)

## 1.41.0.0 (2022-07-16)

Enhancements:

* Add NDCG metric

## 1.40.9.1 (2022-04-07)

Bug fix:

* Fix a helper function broken for MacroAvg/* metrics

## 1.40.9.0 (2022-02-10)

Enhancements:

* Add {RMSE,MSE,MAE}-Against-Interval metrics
* Add MacroAvg/* metrics

## 1.40.8.0 (2022-02-07)

Enhancements:

* Add Probabilistic-Soft2D-F-measure metric
* Add (optional) probabilities to clippings
* Page number is optional in clippings

## 1.40.7.0 (2022-02-03)

Enhancements:

* Add Improvement@Threshold metric

## 1.40.6.0 (2021-10-28)

Enhancements:

* Add p<P> flag for selecting the P% subset with the highest confidence scores

## 1.40.5.1 (2021-10-09)

Bug fixes:

* Improve line-by-line mode for BIO-F1
* Fix clarifications for submitting solutions

## 1.40.5.0 (2021-08-20)

Enhancements:

* Add PerplexityHashed metric

## 1.40.4.1 (2021-08-03)

Fixes:

* Improve diagnostics for broken metric definitions

## 1.40.4.0 (2021-07-23)

Fixes:

* Fix inconsistencies in handling probabilities in MultiLabel-F-score
* Plot calibration graphs for Probabilities-MultiLabel-F-score
* Improve diagnostics for unknown or broken metric definitions

## 1.40.3.0

Fix:

* Properly validate the train set

## 1.40.2.0

Enhancements:

* Matching specifications (e.g. fuzzy matching) can be used for Accuracy

## 1.40.1.0

Improvements:

* Handle DOS/Windows end-of-lines

## 1.40.0.0

New features:

* Add BIO-Weighted-F1 metric
* Handle filter & match combination of flags properly

## 1.39.0.0

New features:

* Add Haversine metric for distance on a sphere

## 1.38.0.0

New features:

* Add CER (Character-Error Rate) metric
* Spaces can be escaped with backslashes in configuration files

## 1.37.0.0

New features:

* Add --select-metric to select metric(s) by name (useful when you
  have a complicated configuration with a large number of metrics,
  and you want to see the result only for a specific metric, especially
  in --line-by-line or --worst-features more)
* Add --show-preprocessed so that in --line-by-line or similar modes you
  will be shown the results after the proprocessing
* --validate checks whether at least one metric is of priority 1

Bug fixes:

* Fix handling MultiLabel-F1 in --line-by-line mode when used with flags

## 1.36.1.0

* Add "c" and "t" flags

## 1.36.0.0

* Add fuzzy matching for MultiLabel-F1

## 1.35.0.0

* Add simple "mix" ensembling

## 1.34.0.0

* Add filtering (f<...> op for metrics)

## 1.33.0.0

* Handle headers in TSV files

## 1.32.2.0

* Fix bug in cross-tabs

## 1.32.0.0

* Add option to mark worst features

## 1.31.0.0

* Fix validation of challenges with Bootstrap resampling

## 1.30.0.0

* Automatically set precision when in Bootstrap mode

## 1.29.0.0

* Bootstrap resampling for most metrics

## 1.28.0.0

* Add `s` flag for substitution

## 1.27.0.0

* Results are formatted in cross-tables (if possible)

## 1.26.0.0

* Change the meaning of WER (WER is calculated for the whole set now
  - similar to the way BLEU is calculated)
* Use `Mean/WER` if you want the old meaning (average of per-item results)

## 1.25.0.0

* Add --oracle-item-based

## 1.24.0.0

* Introduce metric priorities
* Use "Cartesian" strings in metrics

## 1.23.0.0

* New style of train data is preferred
  - `in.tsv` and `expected.tsv` instead of `train.tsv`
  - though this is not required as sometimes training data look different than test data
  - `--validate` option was changed accordingly

## 1.22.1.0

* Add "Mean/" meta-metric (for the time being working only with MultiLabel-F-measure)
* Add :S flag

## 1.22.0.0

* Add SegmentAccuracy

## 1.21.0.0

* Add Probabilistic-MultiLabel-F-measure

## 1.20.1.0

* Fix Soft2D-F1 metric
* Check for invalid rectangles in Soft2D-F1 metric

## 1.20.0.0

* Add --list-metrics options
* Add Soft2D-F1 metric.

## 1.19.0.0

* Fully static build
* Add preprocessing options for metrics

## 1.18.2.0

* During validation, check the number of columns
* During validation, check the number of lines
* Validate train files

## 1.18.1.0

* During validation, check whether the maximum values is obtained with the expected data

## 1.18.0.0

* Add --validate option

## 1.17.0.0

* Add Probabilistic-Soft-F-score

## 1.16.0.0

* Handle JSONL files (only for MultiLabel-F-score)
* Fix SMAPE metric

## 1.0.0.1

* Added `--version`, `-v` options handling
