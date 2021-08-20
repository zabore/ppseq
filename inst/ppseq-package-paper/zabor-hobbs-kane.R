## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
library(plotly)
library(ggplot2)


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


## ----echo = FALSE, fig.width = 7, fig.height = 5, include = knitr::is_latex_output(), eval = knitr::is_latex_output(), fig.cap = "Plot of design options made with ggplot. The color represents the Euclidean distance to the top left point and the optimal design is indicated by a diamond."----
p <- plot(
  one_sample_cal_tbl,
  type1_range = c(0.01, 0.2),
  minimum_power = 0.7,
  plotly = FALSE
  )
p


## ----echo = FALSE, fig.width = 6, fig.height = 4, include=knitr::is_html_output(), eval=knitr::is_html_output(), fig.cap = "Plot of design options made with plotly. The color represents the Euclidean distance to the top left point and the optimal design is indicated by a diamond."----
#> p <- plot(
#>   one_sample_cal_tbl,
#>   type1_range = c(0.01, 0.2),
#>   minimum_power = 0.7,
#>   plotly = TRUE
#>   )
#> p[[1]]
#> p[[2]]


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


## ----message = FALSE, fig.height = 8, fig.width = 6, include = knitr::is_latex_output(), eval = knitr::is_latex_output(), fig.cap = "Plot of decision rules made with ggplot. The color indicates whether the trial should stop or proceed for a given number of responses at each interim analysis."----
plot(one_sample_decision_tbl, plotly = FALSE)


## ----message = FALSE, fig.height = 8, fig.width = 6, include=knitr::is_html_output(), eval=knitr::is_html_output(), fig.cap = "Plot of decision rules made with plotly. The color indicates whether the trial should stop or proceed for a given number of responses at each interim analysis."----
#> plot(one_sample_decision_tbl)

```{.r .distill-force-highlighting-css}
```
