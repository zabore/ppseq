# Calibrate according to posterior probability threshold and predictive probability threshold with interim futility monitoring

This function is meant to be used in the context of a clinical trial
with a binary endpoint. For every combination of the provided posterior
thresholds and predictive thresholds, the function simulates many trials
and then calculates the average number of times a trial was positive. In
the null case, this is the type I error for the given thresholds. In the
alternative case, this is the power for the given thresholds.

## Usage

``` r
calibrate_thresholds(
  p_null,
  p_alt,
  n,
  N,
  pp_threshold,
  ppp_threshold,
  direction = "greater",
  delta = NULL,
  monitoring = "futility",
  prior = c(0.5, 0.5),
  S = 5000,
  nsim = 1000
)
```

## Arguments

- p_null:

  vector of length two containing the probability of event in the
  standard of care and experimental arm c(p0, p1) for the two-sample
  case for the null scenario; integer of event probability for
  one-sample case

- p_alt:

  vector of length two containing the probability of event in the
  standard of care and experimental arm c(p0, p1) for the two-sample
  case for the alternative scenario; integer of event probability for
  one-sample case

- n:

  matrix containing the total number of patients accrued so far at each
  interim look in the standard of care (column 1) and experimental
  (column 2) arms for two-sample case; vector of sample size accrued so
  far at each interim look for one-sample case. The last value should be
  equal to the total sample size at the end of the trial. If only a
  single look will be done at the end of the trial, this can be a vector
  specifying the total sample size c(N0, N1) for the two-sample case or
  an integer specifying the total sample size N for the one-sample case

- N:

  the total planned sample size at the end of the trial, c(N0, N1) for
  two-sample case; integer of total planned sample size at end of trial
  N for one-sample case

- pp_threshold:

  the posterior probability threshold of interest

- ppp_threshold:

  the posterior predictive probability threshold of interest for
  futility monitoring

- direction:

  "greater" (default) if interest is in p(p1 \> p0) and "less" if
  interest is in p(p1 \< p0) for two-sample case. For one-sample case,
  "greater" if interest is in p(p \> p0) and "less" if interest is in
  p(p \< p0).

- delta:

  clinically meaningful difference between groups. Typically 0 for the
  two-sample case. NULL for the one-sample case (default).

- monitoring:

  the type of interim monitoring to be performed. One of "futility" or
  "efficacy". Default is "futility".

- prior:

  hyperparameters of prior beta distribution. Beta(0.5, 0.5) is default

- S:

  number of samples drawn from the posterior. Default is 5000

- nsim:

  Number of simulated trial datasets.

## Value

A list containing a

1.  a tibble 'res_summary' containing the posterior probability
    threshold (pp_threshold), the predictive probability threshold
    (ppp_threshold), the mean sample size under the null (mean_n0_null
    and mean_n1_null for two-sample case; mean_n1_null for one-sample
    case), the proportion of positive trials under the null
    (prop_pos_null), the proportion of trials stopped early under the
    null (prop_stopped_null), the mean sample size under the alternative
    (mean_n0_alt and mean_n1_alt for two-sample case; mean_n1_alt for
    one-sample case), the proportion of positive trials under the
    alternative (prop_pos_alt), the proportion of trials stopped early
    under the alternative (prop_stopped_alt)

2.  'call_list' containing the original function call

3.  'calibrate_thresholds_inputs' a list containing the inputs to the
    original function call

The proportion of positive trials will be a measure of the type I error
for a null setting, and a measure of the power in the alternative
setting.

## Examples

``` r

# One-sample case
set.seed(123)

calibrate_thresholds(
  p_null = 0.1, 
  p_alt = 0.4,
  n = seq(5, 15, 5), 
  N = 15,
  pp_threshold = c(0.85, 0.9),
  ppp_threshold = c(0.1, 0.2),
  S = 10, 
  nsim = 10
  )
#> Warning: package 'future' was built under R version 4.6.1
#> Warning: package 'purrr' was built under R version 4.6.1
#> # A tibble: 4 × 8
#>   pp_threshold ppp_threshold mean_n1_null prop_pos_null prop_stopped_null
#>          <dbl>         <dbl>        <dbl>         <dbl>             <dbl>
#> 1         0.85           0.1         13.5           0.1               0.2
#> 2         0.85           0.2         10.5           0.1               0.6
#> 3         0.9            0.1         12.5           0                 0.3
#> 4         0.9            0.2         11             0.1               0.5
#> # ℹ 3 more variables: mean_n1_alt <dbl>, prop_pos_alt <dbl>,
#> #   prop_stopped_alt <dbl>

# Two-sample case
set.seed(456)

calibrate_thresholds(
  p_null = c(0.1, 0.1), 
  p_alt = c(0.1, 0.5),
  n = cbind(seq(5, 15, 5), seq(5, 15, 5)), 
  N = c(15, 15),
  pp_threshold = c(0.8, 0.85),
  ppp_threshold = c(0.2, 0.3),
  delta = 0,
  S = 10, 
  nsim = 10
  )
#> # A tibble: 4 × 10
#>   pp_threshold ppp_threshold mean_n0_null mean_n1_null prop_pos_null
#>          <dbl>         <dbl>        <dbl>        <dbl>         <dbl>
#> 1         0.8            0.2          9            9               0
#> 2         0.8            0.3          7.5          7.5             0
#> 3         0.85           0.2          8.5          8.5             0
#> 4         0.85           0.3          8            8               0
#> # ℹ 5 more variables: prop_stopped_null <dbl>, mean_n0_alt <dbl>,
#> #   mean_n1_alt <dbl>, prop_pos_alt <dbl>, prop_stopped_alt <dbl>
```
