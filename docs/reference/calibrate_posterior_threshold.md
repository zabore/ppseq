# Calibrate the posterior probability threshold

This function is meant to be used in the context of a clinical trial
with a binary endpoint. For a vector of possible posterior decision
thresholds, the function simulates many trials and then calculates the
average number of times the posterior probability exceeds a given
threshold. In a null case, this will result in the type I error at a
given threshold. In an alternative case, this will result in the power
at a given threshold.

## Usage

``` r
calibrate_posterior_threshold(
  p,
  N,
  p0,
  direction = "greater",
  delta = NULL,
  prior = c(0.5, 0.5),
  S = 5000,
  theta
)
```

## Arguments

- p:

  vector of length two containing the probability of event in the
  standard of care and experimental arm c(p0, p1) for the two-sample
  case; integer of event probability for one-sample case

- N:

  vector of length two containing the total sample size c(N0, N1) for
  two-sample case; integer of sample size so far N for one-sample case

- p0:

  The target value to compare to in the one-sample case. Set to NULL for
  the two-sample case.

- direction:

  "greater" (default) if interest is in p(p1 \> p0) and "less" if
  interest is in p(p1 \< p0) for two-sample case. For one-sample case,
  "greater" if interest is in p(p \> p0) and "less" if interest is in
  p(p \< p0).

- delta:

  clinically meaningful difference between groups. Typically 0 for the
  two-sample case. NULL for the one-sample case (default).

- prior:

  hyperparameters of prior beta distribution. Beta(0.5, 0.5) is default

- S:

  number of samples drawn from the posterior, and number of simulated
  trials. Default is 5000

- theta:

  The target posterior probability thresholds to consider. Integer or
  vector.

## Value

Returns a tibble with the posterior probability threshold(s) and
associated proportion of positive trials.

## Examples

``` r
set.seed(123)

# Setting S = 100 for speed, in practice you would want a much larger sample

# One-sample case
calibrate_posterior_threshold(
  p = 0.1,
  N = 50,
  p0 = 0.1,
  S = 100,
  theta = c(0.9, 0.95)
  )
#> # A tibble: 2 × 2
#>   pp_threshold prop_pos
#>          <dbl>    <dbl>
#> 1         0.9      0.14
#> 2         0.95     0.02

# Two-sample case
calibrate_posterior_threshold(
  p = c(0.1, 0.1),
  N = c(50, 50),
  p0 = NULL,
  delta = 0,
  S = 100,
  theta = c(0.9, 0.95)
  )
#> # A tibble: 2 × 2
#>   pp_threshold prop_pos
#>          <dbl>    <dbl>
#> 1         0.9      0.1 
#> 2         0.95     0.04
  
```
