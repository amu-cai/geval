
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
