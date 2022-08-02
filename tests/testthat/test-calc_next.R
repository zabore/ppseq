test_that("error messages work", {
  expect_error(
    calc_next(
      y = 27,
      n = c(100, 100)
    )
  )
  
  expect_error(
    calc_next(
      y = 27,
      n = 100,
      interval = 1.5
    )
  )
})

test_that("one-sample works as expected", {
  set.seed(123)
  expect_error(
    calc_next(
      y = 27,
      n = 100,
      S = 100
      ),
    NA)
})

test_that("two-sample works as expected", {
  set.seed(123)
  expect_error(
    calc_next(
      y = c(14, 23),
      n = c(100, 100),
      S = 100
      ),
    NA)
})
