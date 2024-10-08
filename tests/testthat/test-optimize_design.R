test_that(
  "optimize_design works for one-sample case",
  {
    set.seed(123)
    cal_tbl <- calibrate_thresholds(
      p_null = 0.1, 
      p_alt = 0.3,
      n = c(5, 25), 
      N = 25, 
      pp_threshold = 0.9,
      ppp_threshold = 0.05,
      direction = "greater", 
      delta = NULL,
      prior = c(0.5, 0.5), 
      S = 200, 
      nsim = 300
    )


    expect_error(optimize_design.calibrate_thresholds(3))

    expect_error(optimize_design(cal_tbl, minimum_power = 1:2))

    expect_error(optimize_design(cal_tbl, minimum_power = "a"))

    expect_error(optimize_design(cal_tbl, type1_range = 3))

    expect_error(optimize_design(cal_tbl, type1_range = c("a", "b")))

    expect_snapshot(lapply(optimize_design(cal_tbl), as.data.frame))
  }
)

test_that(
  "optimize_design works for two-sample case",
  {
    set.seed(123)
    cal_tbl <- calibrate_thresholds(
      p_null = c(0.1, 0.1), 
      p_alt = c(0.1, 0.5),
      n = cbind(c(10, 25), c(10, 25)), 
      N = c(25, 25), 
      pp_threshold = 0.9,
      ppp_threshold = 0.2,
      direction = "greater", 
      delta = 0,
      prior = c(0.5, 0.5), 
      S = 200, 
      nsim = 300
    )
    expect_snapshot(lapply(optimize_design(cal_tbl), as.data.frame))
  }
)
