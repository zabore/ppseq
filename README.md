
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/zabore/ppseq/workflows/R-CMD-check/badge.svg)](https://github.com/zabore/ppseq/actions)
[![Codecov test
coverage](https://codecov.io/gh/zabore/ppseq/branch/main/graph/badge.svg)](https://codecov.io/gh/zabore/ppseq?branch=main)
<!-- badges: end -->

<br>

## ppseq

The {ppseq} package provides functions to design clinical trials using
Bayesian sequential predictive probability monitoring. Functionality is
available to design
[one-arm](https://www.emilyzabor.com/ppseq/articles/one_sample_expansion.html)
or
[two-arm](https://www.emilyzabor.com/ppseq/articles/two_sample_randomized.html)
trials by searching over a grid of combinations of posterior and
predictive thresholds and identifying the optimal design according to
two criteria: accuracy and efficiency. Interactive plotting allows easy
comparison of the various design options and easy trial implementation
through decision rule plots.

## Installation

You can install the production version of ppseq from CRAN with:

``` r
install.packages("ppseq")
```

Or you can install the development version of ppseq from GitHub with:

``` r
remotes::install_github("zabore/ppseq")
```

## Basic usage
