# Changelog

## ppseq 0.2.5

CRAN release: 2024-09-04

- fixed issue with spelling testing

## ppseq 0.2.4

CRAN release: 2024-04-04

- fixed issue with
  [`calc_decision_rules()`](https://www.emilyzabor.com/ppseq/reference/calc_decision_rules.md)
  when `direction="less"` (Closed Issue
  [\#26](https://github.com/zabore/ppseq/issues/26))

## ppseq 0.2.3

CRAN release: 2024-01-23

- added R Journal citation
- removed plot tests because snapshots were failing on continuous
  integration even though they pass locally

## ppseq 0.2.2

CRAN release: 2023-04-17

- updated two-sample vignette results (Closed Issue
  [\#23](https://github.com/zabore/ppseq/issues/23))
- corrected a code bug in
  [`calc_decision_rules()`](https://www.emilyzabor.com/ppseq/reference/calc_decision_rules.md)
  (Closed Issue [\#21](https://github.com/zabore/ppseq/issues/21))
- corrected a code bug in
  [`calibrate_thresholds()`](https://www.emilyzabor.com/ppseq/reference/calibrate_thresholds.md)
  (Closed Issue [\#19](https://github.com/zabore/ppseq/issues/19))
- replaced
  [`purrr::cross_df()`](https://purrr.tidyverse.org/reference/cross.html)
  (deprecated) with
  [`tidyr::expand_grid()`](https://tidyr.tidyverse.org/reference/expand_grid.html)

## ppseq 0.2.1

- updated both datasets to use different predictive thresholds
- correspondingly updated both vignettes

## ppseq 0.2.0

CRAN release: 2022-08-08

- added weighting options to
  [`optimize_design()`](https://www.emilyzabor.com/ppseq/reference/optimize_design.md)
  (Closed Issue [\#7](https://github.com/zabore/ppseq/issues/7))
- added informative messaging when no results returned from
  [`optimize_design()`](https://www.emilyzabor.com/ppseq/reference/optimize_design.md)
  (Closed Issue [\#14](https://github.com/zabore/ppseq/issues/14))
- upgraded package website to bootstrap 5 and changed some styling
- added two-sample randomized trial vignette
- added
  [`calc_next()`](https://www.emilyzabor.com/ppseq/reference/calc_next.md)
  function to calculate probability of response in next patient (Closed
  Issue [\#2](https://github.com/zabore/ppseq/issues/2))
- fixed a bug in the default arguments to
  [`optimize_design()`](https://www.emilyzabor.com/ppseq/reference/optimize_design.md)
  (Closed Issue [\#12](https://github.com/zabore/ppseq/issues/12))
- added efficacy monitoring option, default is still futility monitoring
  (Closed Issue [\#3](https://github.com/zabore/ppseq/issues/3))

## ppseq 0.1.2

- Updating all examples to run without `\donttest{}`
- Some minor documentation changes

## ppseq 0.1.1

CRAN release: 2021-09-09

- Updating the included example data files
- Changes to data files are reflected in the vignette

## ppseq 0.1.0

- Initial package release
