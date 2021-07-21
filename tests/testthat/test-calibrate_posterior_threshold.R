
test_that(
  "calibrate_posterior_threshold works for two-sample case",
  {
    set.seed(123)
    expect_snapshot(
      calibrate_posterior_threshold(
        p = c(0.1, 0.1), N = c(50, 50),
        direction = "greater", p0 = NULL, delta = 0, prior = c(0.5, 0.5),
        S = 50, theta = c(0.9, 0.95)
      )
    )
  }
)

test_that(
  "calibrate_posterior_threshold works for one-sample case",
  {
    set.seed(123)
    expect_snapshot(
      calibrate_posterior_threshold(
        p = 0.1, 
        N = 50, 
        direction = "greater",
        p0 = 0.1, 
        delta = NULL, 
        prior = c(0.5, 0.5), 
        S = 50, 
        theta = c(0.9, 0.95)
      )
    )
  }
)


