library(vdiffr)

test_that(
  "one-sample calibrate threshold plotting",
  {
    expect_error(plot.calibrate_thresholds(3))

    set.seed(123)
    one_sample_cal_tbl <- 
      calibrate_thresholds(
          p_null = 0.1,
          p_alt = 0.3,
          n = c(5, 25),
          N = 25,
          pp_threshold = c(0.2, 0.9),
          ppp_threshold = c(0.05, 0.2),
          direction = "greater",
          delta = NULL,
          prior = c(0.5, 0.5),
          S = 200,
          nsim = 100
        )

    expect_error(plot(one_sample_cal_tbl, type1_range = 3))

    expect_error(
      plot(one_sample_cal_tbl, type1_range = c("a", "b"))
    )

    expect_error(
      plot(one_sample_cal_tbl, minimum_power = c(1, 2))
    )

    expect_error(plot(one_sample_cal_tbl, minimum_power = "a"))

    expect_doppelganger('plot_one_sample_cal_tbl', plot(one_sample_cal_tbl))
  }
)

# Emily, can you provide an example where this works and plots?
#test_that(
#  "two-sample calibrate threshold plotting",
#  {
#    set.seed(123)
#    two_sample_cal_tbl <- 
#      calibrate_thresholds(
#          p_null = c(0.1, 0.1),
#          p_alt = c(0.5, 0.6),
#          n = cbind(c(50, 75), c(50, 75)),
#          N = c(100, 100),
#          pp_threshold = 0.4,
#          ppp_threshold = 0.2,
#          direction = "greater",
#          delta = 0,
#          prior = c(0.5, 0.5),
#          S = 200,
#          nsim = 200
#      )
#
#    expect_doppelganger('plot_two_sample_cal_tbl', plot(two_sample_cal_tbl))
#
#  }
#)

test_that(
  'plot one-sample decision rules works',
  {
    set.seed(123)
    cd1 <- 
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

    expect_error(plot.calc_decision_rules(3))

    expect_doppelganger(
      'plot_one_sample_decision_rules',
      plot(cd1, plotly = FALSE)
    )
  }
)

test_that(
  'plot two-sample decision rules works',
  {
    set.seed(123)
    cd2 <-  
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

    expect_doppelganger(
      'plot_two_sample_decision_rules',
      plot(cd2, plotly = FALSE)
    )
  }
)
