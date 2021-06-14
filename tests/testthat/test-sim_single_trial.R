
test_that(
  "one-sample single trial simulation",
  {
    set.seed(123)
    expect_snapshot(
      sim_single_trial(prob = 0.3, n = c(5, 10), direction = "greater",
                       p = 0.1, delta = NULL, prior = c(0.5, 0.5), S = 5000, 
                       N = 25, theta = 0.95)
    )
  }
)

test_that(
  "two-sample single trial simulation",
  {
    set.seed(123)
    expect_snapshot(
      sim_single_trial(prob = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)),
                       direction = "greater", p0 = NULL, delta = 0, 
                       prior = c(0.5, 0.5), S = 5000, N = c(50, 50), 
                       theta = 0.95)
    )
  }
)
