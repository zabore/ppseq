## ----eval = FALSE-------------------------------------------------------------
#> install.packages("ppseq")


## -----------------------------------------------------------------------------
library(ppseq)


## ----eval = FALSE-------------------------------------------------------------
#> library(future)
#> 
#> set.seed(123)
#> 
#> plan(multicore(workers = 76))
#> 
#> one_sample_cal_tbl <-
#>   calibrate_thresholds(
#>     p_null = 0.1,
#>     p_alt = 0.2,
#>     n = seq(5, 95, 5),
#>     N = 95,
#>     pp_threshold = c(0, 0.7, 0.74, 0.78, 0.82, 0.86, 0.9, 0.92, 0.93, 0.94,
#>                      0.95, 0.96, 0.97, 0.98, 0.99, 0.999, 0.9999, 0.99999, 1),
#>     ppp_threshold = seq(0.05, 0.2, 0.05),
#>     direction = "greater",
#>     delta = NULL,
#>     prior = c(0.5, 0.5),
#>     S = 5000,
#>     nsim = 1000
#>     )


## -----------------------------------------------------------------------------
print(
  one_sample_cal_tbl,
  type1_range = c(0.01, 0.2),
  minimum_power = 0.7
  )


## -----------------------------------------------------------------------------
optimize_design(
  one_sample_cal_tbl, 
  type1_range = c(0.05, 0.1), 
  minimum_power = 0.7
  )


## ----eval = FALSE-------------------------------------------------------------
#> plot(
#>   one_sample_cal_tbl,
#>   type1_range = c(0.01, 0.2),
#>   minimum_power = 0.7,
#>   plotly = TRUE
#>   )


## ----echo = FALSE, fig.width = 9, fig.height = 6------------------------------
# Note that the above code does not render properly in R Markdown, though it does produce the plots correctly in an interactive session
p <- plot(
  one_sample_cal_tbl, 
  type1_range = c(0.01, 0.2), 
  minimum_power = 0.7,
  plotly = TRUE
  )

p[[1]]
p[[2]]


## ----eval = FALSE-------------------------------------------------------------
#> set.seed(123)
#> 
#> one_sample_decision_tbl <-
#>   calc_decision_rules(
#>     n = seq(5, 95, 5),
#>     N = 95,
#>     theta = 0.92,
#>     ppp = 0.1,
#>     p0 = 0.1,
#>     direction = "greater",
#>     delta = NULL,
#>     prior = c(0.5, 0.5),
#>     S = 5000
#>     )


## -----------------------------------------------------------------------------
one_sample_decision_tbl

