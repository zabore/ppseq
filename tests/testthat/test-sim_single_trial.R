test_that("test error and warning messages", {
  
  expect_error(
    sim_single_trial(p = 0.3, n = c(5, 10), direction = "greater",
                     p0 = 0.1, delta = 0, prior = c(0.5, 0.5), S = 5000, 
                     N = 25, theta = 0.95)
    )
  
  expect_error(
    sim_single_trial(p = 0.3, n = c(5, 10), direction = "equal",
                     p0 = 0.1, delta = NULL, prior = c(0.5, 0.5), S = 5000, 
                     N = 25, theta = 0.95)
    )
  
  expect_error(
    sim_single_trial(p = 0.3, n = c(5, 10), direction = "greater",
                     p0 = NULL, delta = 0, prior = c(0.5, 0.5), S = 5000, 
                     N = 25, theta = 0.95)
    )
  
  expect_error(
    sim_single_trial(p = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)),
                     direction = "greater", p0 = 0.1, delta = NULL, 
                     prior = c(0.5, 0.5), S = 5000, N = c(50, 50), 
                     theta = 0.95)
    )
  
})


test_that(
  "one-sample single trial simulation",
  {
    set.seed(123)
    expect_snapshot(
      sim_single_trial(p = 0.3, n = c(5, 10), direction = "greater",
                       p0 = 0.1, delta = NULL, prior = c(0.5, 0.5), S = 5000, 
                       N = 25, theta = 0.95)
    )
  }
)

test_that(
  "two-sample single trial simulation",
  {
    set.seed(123)
    expect_snapshot(
      sim_single_trial(p = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)),
                       direction = "greater", p0 = NULL, delta = 0, 
                       prior = c(0.5, 0.5), S = 5000, N = c(50, 50), 
                       theta = 0.95)
    )
    
    expect_error(
      sim_single_trial(p = c(0.1, 0.3), n = c(50, 50),
                       direction = "greater", p0 = NULL, delta = 0, 
                       prior = c(0.5, 0.5), S = 100, N = c(50, 50), 
                       theta = 0.95),
      NA
      )
  }
)
