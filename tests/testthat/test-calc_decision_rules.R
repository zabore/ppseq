test_that(
  "one-sample calc_decision_rules works",
  {
    set.seed(123)
    expect_snapshot(
      calc_decision_rules(
        n = c(10, 20), 
        N = 25, 
        theta = 0.86, 
        ppp = 0.2,
        p0 = 0.1, 
        direction = "greater", 
        delta = NULL, 
        prior = c(0.5, 0.5), 
        S = 100
        )
    )
  }
)

test_that(
  "two-sample calc_decision_rules works",
  {
    set.seed(123)
    expect_snapshot(
      calc_decision_rules(
        n = cbind(c(10, 20), c(10, 20)), 
        N = c(25, 25),
        theta = 0.86, 
        ppp = 0.2, 
        p0 = NULL, 
        direction = "greater", 
        delta = 0,
        prior = c(0.5, 0.5), 
        S = 100)
    )
  }
)