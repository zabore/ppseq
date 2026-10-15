# Calculate response probability for the next patient

This function is meant to be used in the context of a clinical trial
with a binary endpoint. For the two-sample case, the total number of
events in the standard-of-care arm is y0 and the total number of events
in the experimental arm is y1. The function samples from the posterior
beta distribution based on the data and the prior beta hyperparameters,
and returns the empiric mean and bootstrap confidence interval for the
next patient. The empiric mean represents the probability of the binary
outcome occurring in the next patient. The one-sample case is also
available.

## Usage

``` r
calc_next(y, n, prior = c(0.5, 0.5), S = 5000, interval = 0.95)
```

## Arguments

- y:

  number of events observed so far. Vector of length two c(y0, y1) for
  the two-sample case; integer y for the one-sample case.

- n:

  sample size observed so far. Vector of length two c(n0, n1) for the
  two-sample case; integer n for the one-sample case.

- prior:

  vector of length two containing hyperparameters of the prior beta
  distribution. c(0.5, 0.5) is default, for the Beta(0.5, 0.5)
  distribution.

- S:

  number of samples, default is 5000

- interval:

  a value between 0 and 1 indicating the width of the desired interval,
  default is 0.95

## Value

Returns a tibble with the group indicator (for the two-sample case
only), the empiric mean, the bootstrap confidence interval, and the
specified width of the confidence interval.

## Examples

``` r

set.seed(123)

# One-sample case
calc_next(
  y = 27, 
  n = 100,
  S = 100
  )
#> # A tibble: 1 × 4
#>   Probability `Lower CI` `Upper CI`    CI
#>         <dbl>      <dbl>      <dbl> <dbl>
#> 1        0.31       0.19       0.36  0.95

# Two-sample case
calc_next(
  y = c(14, 23), 
  n = c(100, 100),
  S = 100
  )
#> # A tibble: 2 × 5
#>   Group Probability `Lower CI` `Upper CI`    CI
#>   <dbl>       <dbl>      <dbl>      <dbl> <dbl>
#> 1     0        0.14       0.08       0.22  0.95
#> 2     1        0.22       0.15       0.31  0.95
```
