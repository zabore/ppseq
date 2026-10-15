# Package index

## Calibrate thresholds

- [`calibrate_thresholds()`](https://www.emilyzabor.com/ppseq/reference/calibrate_thresholds.md)
  : Calibrate according to posterior probability threshold and
  predictive probability threshold with interim futility monitoring
- [`calibrate_posterior_threshold()`](https://www.emilyzabor.com/ppseq/reference/calibrate_posterior_threshold.md)
  : Calibrate the posterior probability threshold

## Identify the optimal design

- [`optimize_design()`](https://www.emilyzabor.com/ppseq/reference/optimize_design.md)
  :

  Function to setup usage of `optimize_design.calibrate_thresholds`

- [`optimize_design(`*`<calibrate_thresholds>`*`)`](https://www.emilyzabor.com/ppseq/reference/optimize_design.calibrate_thresholds.md)
  :

  Custom optimization method for `calibrate_thresholds` objects

## Calculate decision rules

- [`calc_decision_rules()`](https://www.emilyzabor.com/ppseq/reference/calc_decision_rules.md)
  : Calculate a decision rule table for interim monitoring of a
  pre-specified design

## Plotting and printing

- [`plot(`*`<calibrate_thresholds>`*`)`](https://www.emilyzabor.com/ppseq/reference/plot.calibrate_thresholds.md)
  :

  Plot method for `calibrate_thresholds` objects

- [`print(`*`<calibrate_thresholds>`*`)`](https://www.emilyzabor.com/ppseq/reference/print.calibrate_thresholds.md)
  :

  Print method for `calibrate_thresholds` objects

- [`plot(`*`<calc_decision_rules>`*`)`](https://www.emilyzabor.com/ppseq/reference/plot.calc_decision_rules.md)
  :

  Plot method for `calc_decision_rules` objects

## Data files

- [`one_sample_cal_tbl`](https://www.emilyzabor.com/ppseq/reference/one_sample_cal_tbl.md)
  :

  Output from a one-sample call to `calibrate_thresholds`

- [`one_sample_decision_tbl`](https://www.emilyzabor.com/ppseq/reference/one_sample_decision_tbl.md)
  :

  Output from a one-sample call to `calc_decision_rules`

- [`two_sample_cal_tbl`](https://www.emilyzabor.com/ppseq/reference/two_sample_cal_tbl.md)
  :

  Output from a two-sample call to `calibrate_thresholds`

- [`two_sample_decision_tbl`](https://www.emilyzabor.com/ppseq/reference/two_sample_decision_tbl.md)
  :

  Output from a two-sample call to `calc_decision_rules`

## Support functions

- [`calc_posterior()`](https://www.emilyzabor.com/ppseq/reference/calc_posterior.md)
  : Calculate a single posterior probability
- [`calc_predictive()`](https://www.emilyzabor.com/ppseq/reference/calc_predictive.md)
  : Calculate a single posterior predictive probability
- [`calc_next()`](https://www.emilyzabor.com/ppseq/reference/calc_next.md)
  : Calculate response probability for the next patient
- [`eval_thresh()`](https://www.emilyzabor.com/ppseq/reference/eval_thresh.md)
  : Evaluate a single dataset for a single pp_threshold and
  ppp_threshold combination
- [`sim_dat1()`](https://www.emilyzabor.com/ppseq/reference/sim_dat1.md)
  : Simulate a single dataset based on the response probability(ies),
  the total sample size(s), and the interim look schedule(s)
- [`sim_single_trial()`](https://www.emilyzabor.com/ppseq/reference/sim_single_trial.md)
  : Simulate a single trial with posterior probability monitoring
