# Calculate a single posterior probability

This function is meant to be used in the context of a clinical trial
with a binary endpoint. For the two-sample case, the total number of
events in the standard-of-care arm is y0 and the total number of events
in the experimental arm is y1. The function samples from the posterior
beta distribution based on the data and the prior beta hyperparameters,
and returns the posterior probability that p1 is greater than (or less
than) p0 given the data. The one-sample case is also available, in which
a target p0 must be specified and the function returns the posterior
probability that p is greater than (or less than) p0 given the data.

## Usage

``` r
calc_posterior(
  y,
  n,
  p0,
  direction = "greater",
  delta = NULL,
  prior = c(0.5, 0.5),
  S = 5000
)
```

## Arguments

- y:

  number of events observed so far. Vector of length two c(y0, y1) for
  the two-sample case; integer y for the one-sample case.

- n:

  sample size observed so far. Vector of length two c(n0, n1) for the
  two-sample case; integer n for the one-sample case.

- p0:

  the target value to compare to in the one-sample case. Set to NULL for
  the two-sample case.

- direction:

  "greater" (default) if interest is in P(p1 \> p0) in the two-sample
  case or P(p \> p0) in the one-sample case; "less" if interest is in
  P(p1 \< p0) for the two-sample case or P(p \< p0) for the one-sample
  case.

- delta:

  clinically meaningful difference between groups. Typically 0 for the
  two-sample case. NULL for one-sample case (default).

- prior:

  vector of length two containing hyperparameters of the prior beta
  distribution. c(0.5, 0.5) is default, for the Beta(0.5, 0.5)
  distribution.

- S:

  number of samples, default is 5000

## Value

Returns the numeric posterior probability

## Examples

``` r

set.seed(123)

# One-sample case
calc_posterior(
  y = 27, 
  n = 100, 
  p0 = 0.2
  )
#> [1] 0.9558

# Two-sample case
calc_posterior(
  y = c(14, 23), 
  n = c(100, 100), 
  p0 = NULL, 
  delta = 0
  )
#> [1] 0.9508
```
