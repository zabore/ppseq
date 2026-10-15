# Output from a two-sample call to `calc_decision_rules`

This .rda file contains output from a two-sample call to
[`calc_decision_rules()`](https://www.emilyzabor.com/ppseq/reference/calc_decision_rules.md).
See the vignette titled "Two-sample randomized trail" for a description
of the input parameters used.

## Usage

``` r
data(two_sample_decision_tbl)
```

## Format

A tibble containing n, the number of patients enrolled at each futility
monitoring point; r, the number of responses at which we would stop the
trial at a given look if the number of observed responses is \<=r, or at
the end of the trial the treatment is considered promising if the number
of observed responses is \>r; and ppp, the predictive probability at
each given look
