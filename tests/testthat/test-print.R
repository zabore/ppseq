set.seed(123)
cal_tbl <- calibrate_thresholds(
  p_null = 0.1, 
  p_alt = 0.4,
  n = c(5, 25), 
  N = 25, 
  pp_threshold = 0.9,
  ppp_threshold = 0.05,
  direction = "greater", 
  delta = NULL,
  prior = c(0.5, 0.5), 
  S = 200, 
  nsim = 400
)

test_that(
  "print calibrate thresh NULL, NULL works both NULL",
  {
    expect_snapshot(print(cal_tbl, type1_range = NULL, minimum_power = NULL))
  }
)

test_that(
  "print calibrate thresh minimum_power == NULL works",
  {
    expect_snapshot(print.calibrate_thresholds(cal_tbl, minimum_power = NULL))
  }
)

test_that(
  "print calibrate thresh type1_range == NULL works",
  {
    expect_snapshot(print.calibrate_thresholds(cal_tbl, type1_range = NULL))
  }
)

test_that(
  "print calibrate thresh works",
  {
    expect_snapshot(print.calibrate_thresholds(cal_tbl))
  }
)


test_that("errors when expected", {
  expect_error(print(cal_tbl, type1_range = 0.1, minimum_power = NULL), "*")
  expect_error(print(cal_tbl, type1_range = NULL, minimum_power = c(0.7, 0.8)), 
               "*")
  expect_error(print(cal_tbl, type1_range = NULL, minimum_power = "A"), "*")
  expect_error(print(cal_tbl, type1_range = c("A", "B"), minimum_power = NULL), 
               "*")
  expect_error(print(cal_tbl, type1_range = NULL, minimum_power = 2), "*")
  expect_error(print(cal_tbl, type1_range = c(-0.1, 0.1), minimum_power = NULL), 
               "*")
  expect_error(print(cal_tbl, type1_range = c(1.2, 0.1), minimum_power = NULL), 
               "*")
  expect_error(print(cal_tbl, type1_range = c(0.05, -0.1), minimum_power = NULL), 
               "*")
  expect_error(print(cal_tbl, type1_range = c(0.05, 1.2), minimum_power = NULL), 
               "*")
})
