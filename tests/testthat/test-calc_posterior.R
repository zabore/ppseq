
test_that("no errors or warnings with correct use", {

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      p0 = NULL, 
      delta = 0
    ), 
    NA
  )

  expect_warning(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      p0 = NULL, 
      delta = 0
    ), 
    NA
  )

  expect_error(
    calc_posterior(
      y = 27, 
      n = 100, 
      p0 = 0.2
    ), 
    NA
  )

  expect_warning(
    calc_posterior(
      y = 27, 
      n = 100, 
      p0 = 0.2
    ), 
    NA
  )

})

test_that("errors when expected", {

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = 100
    )
  )
  
  expect_error(
    calc_posterior(
      y = 14,
      n = 100,
      p0 = NULL,
      delta = 0
    ) 
  )

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      p0 = 0.2,
      delta = NULL
    ) 
  )

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      p0 = 0.2,
      delta = 0
    ) 
  )

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100),
      direction = "south"
    ) 
  )

  expect_error(
    calc_posterior(
      y = 27, 
      n = 100, 
      delta = NULL
    )
  )

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      delta = NULL
    )
  )

  expect_error(
    calc_posterior(
      y = 27, 
      n = 100, 
      p0 = NULL, 
      delta = NULL
    )
  )

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      delta = NULL
    )
  )

  expect_error(
    calc_posterior(
      y = 27, 
      n = 100, 
      p0 = 0.2, 
      direction = "south"
    )
  )

  expect_error( 
    calc_posterior(
      y = 27, 
      n = 100
    )
  )

  expect_error(
    calc_posterior(
      y = c(14, 23), 
      n = c(100, 100), 
      p0 = NULL
    )
  )
})

