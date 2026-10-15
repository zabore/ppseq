# Evaluate a single dataset for a single pp_threshold and ppp_threshold combination

Helper function for calibrate_thresholds() function that evaluates a
single combination of a pp_threshold and a ppp_threshold for a single
dataset

## Usage

``` r
eval_thresh(
  data,
  pp_threshold,
  ppp_threshold,
  p0,
  N,
  direction = "greater",
  delta = NULL,
  monitoring = "futility",
  prior = c(0.5, 0.5),
  S = 5000
)
```

## Arguments

- data:

  the name of the dataset

- pp_threshold:

  the posterior probability threshold of interest

- ppp_threshold:

  the posterior probability threshold of interest for futility
  monitoring

- p0:

  The target value to compare to in the one-sample case. Set to NULL for
  the two-sample case.

- N:

  the total planned sample size at the end of the trial, c(N0, N1) for
  two-sample case; integer of total planned sample size at end of trial
  N for one-sample case

- direction:

  "greater" (default) if interest is in P(p1 \> p0) and "less" if
  interest is in P(p1 \< p0) for two-sample case. For one-sample case,
  "greater" if interest is in P(p \> p0) and "less" if interest is in
  P(p \< p0).

- delta:

  clinically meaningful difference between groups. Typically 0 for the
  two-sample case. NULL for the one-sample case (default).

- monitoring:

  the type of interim monitoring to be performed. One of "futility" or
  "efficacy". Default is "futility".

- prior:

  hyperparameters of prior beta distribution. Beta(0.5, 0.5) is default

- S:

  number of samples, default is 5000

## Value

Returns a tibble with the total sample size at the end of the trial, the
number of responses observed at the end of the trial, the pp_threshold
considered, the ppp_threshold considered, the observed predictive
probability generated from calc_predictive(), and an indicator for
whether the trial was positive or not at the end
