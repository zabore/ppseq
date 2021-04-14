test_that("no errors or warnings with correct use", {
  expect_error(calc_predictive(y = c(7, 12), n = c(50, 50), N = c(100, 100),
                               S = 100), NA)
  expect_warning(calc_predictive(y = c(7, 12), n = c(50, 50), N = c(100, 100),
                                 S = 100), NA)
  
  expect_error(calc_predictive(y = 14, n = 50, p0 = 0.2, delta = NULL, N = 100, 
                               S = 100), NA)
  expect_warning(calc_predictive(y = 14, n = 50, p0 = 0.2, delta = NULL, 
                                 N = 100, S = 100), NA)
})

test_that("errors when expected", {
  expect_error(calc_predictive(y = c(7, 12), n = 50, N = c(100, 100), S = 100), 
               "*")
  expect_error(calc_predictive(y = c(7, 12), n = c(50, 50), N = c(100, 100),
                               p0 = 0.1, delta = 0, S = 100), "*")
  expect_error(calc_predictive(y = c(7, 12), n = c(50, 50), N = c(100, 100),
                               S = 100, direction = "south"), "*")
  expect_error(calc_predictive(y = 14, n = 50, delta = NULL, N = 100, 
                               S = 100), "*")
  expect_error(calc_predictive(y = c(7, 12), n = c(50, 50), N = c(100, 100),
               S = 100, delta = NULL), "*")
  expect_error(calc_predictive(y = c(7, 12), n = c(50, 50), N = 100,
                               S = 100), "*")
})
