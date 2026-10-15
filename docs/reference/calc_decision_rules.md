# Calculate a decision rule table for interim monitoring of a pre-specified design

This function will take the posterior and predictive thresholds for a
pre-specified design with a given null response rate and fixed interim
looks and total sample size, and return the decision rules at each
interim analysis and the end of the trial. Intended for use after
selecting an optimal design using the functions `calibrate_thresholds`
and `optimize_design`.

## Usage

``` r
calc_decision_rules(
  n,
  N,
  theta,
  ppp,
  p0,
  direction = "greater",
  delta = NULL,
  prior = c(0.5, 0.5),
  S = 5000
)
```

## Arguments

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

- theta:

  The target posterior probability. e.g. Efficacy decision if P(p1 \>
  p0) \> theta for the two-sample case with greater direction.

- ppp:

  The target predictive probability. e.g. Stop the trial if the
  predictive probability falls below this target.

- p0:

  The target value to compare to in the one-sample case. Set to NULL for
  the two-sample case.

- direction:

  "greater" (default) if interest is in P(p1 \> p0) and "less" if
  interest is in P(p1 \< p0) for two-sample case. For one-sample case,
  "greater" if interest is in P(p \> p0) and "less" if interest is in
  P(p \< p0).

- delta:

  clinically meaningful difference between groups. Typically 0 for
  two-sample case. NULL for one-sample case (default).

- prior:

  hyperparameters of prior beta distribution. Beta(0.5, 0.5) is default

- S:

  number of samples, default is 5000

## Value

In the one-sample case, returns a tibble with n at each look, r at each
look, and ppp, the associated posterior predictive probability. Stop the
trial at that look if the number of observed responses is \<=r. At the
end of the trial, the treatment is considered promising if the number of
observed responses is \>r. In the two-sample case, returns a tibble with
n0 and n1, the number enrolled subjects in the control and experimental
arms at each look, respectively, r0 and r1, the number of possible
responses in the control and experimental arms at each look,
respectively, and ppp, the associated posterior predictive probability.
For a given value of r0, stop the trial if the number of observed
responses in the experimental arm is \<=r1. At the end of the trial, the
treatment is considered promising if the number of observed responses in
the experimental arm is \>r1 for a given r0. Any NA value in either
table represents an interim look where there is no number of responses
that would lead to stopping the trial.

## Examples

``` r
set.seed(123)

# One-sample case
calc_decision_rules(
  n = seq(5, 25, 5), 
  N = 25, 
  theta = 0.86, 
  ppp = 0.2, 
  p0 = 0.1, 
  S = 50
  )
#> # A tibble: 5 × 3
#>       n     r   ppp
#>   <dbl> <int> <dbl>
#> 1     5     0  0.18
#> 2    10     0  0.04
#> 3    15     1  0.08
#> 4    20     2  0.1 
#> 5    25     3  0   

# Two-sample case
calc_decision_rules(
  n = cbind(seq(5, 25, 5), seq(5, 25, 5)), 
  N = c(25, 25),
  theta = 0.86, 
  ppp = 0.2, 
  p0 = NULL, 
  direction = "greater", 
  delta = 0, 
  S = 50
  )
#> # A tibble: 80 × 5
#>       n0    n1    r0    r1   ppp
#>    <dbl> <dbl> <int> <int> <dbl>
#>  1     5     5     0     0  0.2 
#>  2     5     5     1     0  0.06
#>  3     5     5     2     1  0.04
#>  4     5     5     3     2  0.04
#>  5     5     5     4     3  0.06
#>  6     5     5     5     5  0.2 
#>  7    10    10     0     0  0.16
#>  8    10    10     1     1  0.18
#>  9    10    10     2     2  0.06
#> 10    10    10     3     3  0.16
#> # ℹ 70 more rows
```
