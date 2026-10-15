# Simulate a single dataset based on the response probability(ies), the total sample size(s), and the interim look schedule(s)

Helper function for calibrate_thresholds() function that generates a
single dataset of n and response count at each look based on the
response probability(ies)

## Usage

``` r
sim_dat1(p, n)
```

## Arguments

- p:

  vector of length two containing the probability of event in the
  standard of care and experimental arm c(p0, p1) for the two-sample
  case; integer of event probability for one-sample case

- n:

  matrix containing the total number of patients accrued so far at each
  interim look in the standard of care (column 1) and experimental
  (column 2) arms for two-sample case; vector of sample size accrued so
  far at each interim look for one-sample case. The last value should be
  equal to the total sample size at the end of the trial. If only a
  single look will be done at the end of the trial, this can be a vector
  specifying the total sample size c(N0, N1) for the two-sample case or
  an integer specifying the total sample size N for the one-sample case

## Value

Returns a tibble with n0, n1, y0, y1 for the two-sample case and a
tibble with n1 and y1 for the one-sample case
