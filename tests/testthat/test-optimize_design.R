
test_that(
  "optimize_design works",
  {
    set.seed(123)
    cal_tbl <- calibrate_thresholds(
      p_null = 0.1, p_alt = 0.3,
      n = seq(5, 25, 5), direction = "greater", delta = NULL,
      prior = c(0.5, 0.5), S = 1000, N = 25, nsim = 10,
      pp_threshold = c(0.9, 0.95, 0.96, 0.98),
      ppp_threshold = c(0.05, 0.1)
    )
    expect_snapshot(optimize_design(cal_tbl))
  }
)
