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
  "two-sample vector argument to n",
  {
    set.seed(123)
    expect_snapshot(
      sim_dat1(p = c(0.1, 0.3), n = c(5, 5))
    )
  }
)

test_that(
  "evaluate threshold one-sample case",
  {
    set.seed(123)
    dat1 <- sim_dat1(p = 0.1, n = c(5, 10))
    
    expect_error(
      eval_thresh(dat1, 0.95, 0.3, p0 = 0.1, delta = NULL, S = 500, N = 25, 
                  monitoring = "none")
    )
    
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
    
    expect_snapshot(eval_thresh(dat2, 0.95, 0.3, p0 = NULL, delta = 0,
                                S = 500, N = c(25, 25)))
  }
)

# Note that as.data.frame is being used because tibble formatting in the test()
# environment appears to be slightly different than that in the check() 
# environment.
test_that(
  "one-sample calibrate thresholds",
  {
    set.seed(123)
    
    expect_error(
      calibrate_thresholds(
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
        nsim = 100,
        monitoring = "none"
      )
    )
    
    expect_snapshot_output(
      as.data.frame(
        calibrate_thresholds(
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
          nsim = 100
        )$res_summary
      )
    )
    
  }
)


test_that(
  "two-sample calibrate thresholds",
  {
    set.seed(123)
    expect_snapshot_output(
      as.data.frame(
        calibrate_thresholds(
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
          nsim = 100
        )$res_summary
      )
    )
  }
)