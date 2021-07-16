
test_that(
  "optimize_design works for one-sample case",
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

test_that(
  "optimize_design works for two-sample case",
  {
    set.seed(123)
    cal_tbl2 <- calibrate_thresholds(
      p_null = c(0.1, 0.1), 
      p_alt = c(0.1, 0.5),
      n = cbind(c(10, 25), c(10, 25)), 
      N = c(25, 25), 
      pp_threshold = c(0.9, 0.95, 0.96, 0.98),
      ppp_threshold = seq(0.05, 0.2, 0.05),
      direction = "greater", 
      delta = 0,
      prior = c(0.5, 0.5), 
      S = 200, 
      nsim = 100
    )
    expect_snapshot(optimize_design(cal_tbl2))
  }
)