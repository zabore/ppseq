# ppseq 0.2.3

# ppseq 0.2.2

* updated two-sample vignette results (Closed Issue #23)
* corrected a code bug in `calc_decision_rules()` (Closed Issue #21)
* corrected a code bug in `calibrate_thresholds()` (Closed Issue #19)
* replaced `purrr::cross_df()` (deprecated) with `tidyr::expand_grid()`

# ppseq 0.2.1

* updated both datasets to use different predictive thresholds
* correspondingly updated both vignettes

# ppseq 0.2.0

* added weighting options to `optimize_design()` (Closed Issue #7)
* added informative messaging when no results returned from `optimize_design()` (Closed Issue #14)
* upgraded package website to bootstrap 5 and changed some styling
* added two-sample randomized trial vignette
* added `calc_next()` function to calculate probability of response in next patient (Closed Issue #2)
* fixed a bug in the default arguments to `optimize_design()` (Closed Issue #12)
* added efficacy monitoring option, default is still futility monitoring (Closed Issue #3)

# ppseq 0.1.2

* Updating all examples to run without `\donttest{}` 
* Some minor documentation changes

# ppseq 0.1.1

* Updating the included example data files
* Changes to data files are reflected in the vignette


# ppseq 0.1.0

* Initial package release
