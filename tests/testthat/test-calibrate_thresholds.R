
test_that(
  "one-sample simulate data",
  {
    set.seed(123)
    expect_snapshot(sim_dat1(p = 0.1, n = c(5, 10)))
  }
)

test_that(
  "two-sample simulate data",
  {
    set.seed(123)
    expect_snapshot(
      sim_dat1(p = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)))
    )
  }
)

test_that(
  "evaluate threshold one-sample case",
  {
    set.seed(123)
    dat1 <- sim_dat1(p = 0.1, n = c(5, 10))
    expect_snapshot(
      eval_thresh(dat1, 0.95, 0.3, p0 = 0.1, delta = NULL, S = 500, N = 25)
    )
  }
)

test_that(
  "evaluate threshold two-sample case",
  {
    set.seed(123)
    dat2 <- sim_dat1(p = c(0.1, 0.3), 
                     n = cbind(c(5, 10), c(5, 10)))
    expect_snapshot(eval_thresh(dat2, 0.95, 0.3, S = 500, N = c(25, 25)))
  }
)

# TODO: need a small example below that returns something
# TODO: need a two-sample calibrate_thresholds example.
# TODO: the following is a hack to avoid snapshot issues.

set.seed(123)
calibrate_thresholds( 
  p_null = 0.01, 
  p_alt = 0.9, 
  n = c(5, 25), 
  direction = "greater", 
  delta = NULL,
  prior = c(0.5, 0.5), 
  S = 200, 
  N = 100, 
  nsim = 100,
  pp_threshold = c(0.98),
  ppp_threshold = c(0.01)
)

test_that(
  "single-sample calibrate thresholds",
  {
    set.seed(123)
    expect_snapshot(
      calibrate_thresholds( 
        p_null = 0.01, 
        p_alt = 0.9, 
        n = c(5, 25), 
        direction = "greater", 
        delta = NULL,
        prior = c(0.5, 0.5), 
        S = 200, 
        N = 100, 
        nsim = 100,
        pp_threshold = c(0.98),
        ppp_threshold = c(0.01)
      )
    )
  }
)

