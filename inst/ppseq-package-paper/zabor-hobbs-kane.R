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
#>   )


## -----------------------------------------------------------------------------
print(
  one_sample_cal_tbl,
  type1_range = c(0.05, 0.1),
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
#>   type1_range = c(0.05, 0.1),
#>   minimum_power = 0.7,
#>   plotly = TRUE
#> )


## ----echo = FALSE, fig.width = 6, fig.height = 4, include = knitr::is_latex_output(), eval = knitr::is_latex_output(), fig.cap = "Plot of design options made with ggplot. The color represents the Euclidean distance to the top left point and the optimal design is indicated by a diamond."----
p <- plot(
  one_sample_cal_tbl,
  type1_range = c(0.05, 0.1),
  minimum_power = 0.7,
  plotly = FALSE
  )
p


## ----echo = FALSE, eval = FALSE-----------------------------------------------
#> library(dplyr)
#> x <- one_sample_cal_tbl
#> type1_range <- c(0.05, 0.1)
#> minimum_power <- 0.7
#> 
#> plot_x <-
#>   rename(
#>     mutate(
#>       filter(
#>         x$res_summary,
#>         prop_pos_null >= type1_range[1] &
#>           prop_pos_null <= type1_range[2] &
#>           prop_pos_alt >= minimum_power
#>       ),
#>       Design = paste0(
#>         "Posterior threshold = ", pp_threshold,
#>         " and predictive threshold = ", ppp_threshold
#>       ),
#>       ab_dist_metric = ((prop_pos_null - 0)^2 +
#>         (prop_pos_alt - 1)^2)^(1 / 2),
#>       n_dist_metric = ((mean_n1_null - min(mean_n1_null))^2 +
#>         (mean_n1_alt - max(mean_n1_alt))^2)^(1 / 2)
#>     ),
#>     `Type I error` = prop_pos_null,
#>     Power = prop_pos_alt,
#>     `Average N under the null` = mean_n1_null,
#>     `Average N under the alternative` = mean_n1_alt,
#>     `Distance to optimal efficiency` = n_dist_metric,
#>     `Distance to optimal accuracy` = ab_dist_metric
#>   )
#> 
#> 
#> plot_ab <-
#> mutate(
#>   ungroup(
#>     slice(
#>       group_by(
#>         arrange(
#>           plot_x,
#>           `Distance to optimal accuracy`,
#>           -pp_threshold,
#>           -ppp_threshold
#>         ),
#>         `Distance to optimal accuracy`
#>       ),
#>       1
#>     )
#>   ),
#>   optimal_accuracy = ifelse(row_number() == 1, TRUE, FALSE)
#> )
#> 
#> plot_nn <-
#> mutate(
#>   ungroup(
#>     slice(
#>       group_by(
#>         arrange(
#>           plot_x,
#>           `Distance to optimal efficiency`,
#>           -pp_threshold, -ppp_threshold
#>         ),
#>         `Distance to optimal efficiency`
#>       ),
#>       1
#>     )
#>   ),
#>   optimal_efficiency = ifelse(row_number() == 1, TRUE, FALSE)
#> )
#> 
#> p1 <-
#> ggplot(
#>   plot_ab,
#>   aes(
#>     x = `Type I error`,
#>     y = Power,
#>     color = `Distance to optimal accuracy`,
#>     Design = Design,
#>     `Average N under the null` = `Average N under the null`,
#>     `Average N under the alternative` = `Average N under the alternative`
#>   )
#> ) +
#> geom_point(
#>   shape = ifelse(plot_ab$optimal_accuracy == TRUE, 18, 19),
#>   size = ifelse(plot_ab$optimal_accuracy == TRUE, 4, 2)
#> ) +
#> ylim(0, 1) +
#> xlim(0, 1) +
#> labs(
#>   x = "Type I error",
#>   y = "Power"
#> ) +
#> scale_color_viridis_c() +
#> theme_bw() +
#> theme(legend.position = "bottom")
#> 
#> p2 <-
#> ggplot(
#>   plot_nn,
#>   aes(
#>     x = `Average N under the null`,
#>     y = `Average N under the alternative`,
#>     color = `Distance to optimal efficiency`,
#>     Design = Design,
#>     `Type I error` = `Type I error`,
#>     Power = Power
#>   )
#> ) +
#> geom_point(
#>   shape = ifelse(plot_nn$optimal_efficiency == TRUE, 18, 19),
#>   size = ifelse(plot_nn$optimal_efficiency == TRUE, 4, 2)
#> ) +
#> ylim(
#>   min(plot_x$`Average N under the null`),
#>   max(plot_x$`Average N under the alternative`)
#> ) +
#> xlim(
#>   min(plot_x$`Average N under the null`),
#>   max(plot_x$`Average N under the alternative`)
#> ) +
#> labs(
#>   x = "Average N under the null",
#>   y = "Average N under the alternative"
#> ) +
#> scale_color_viridis_c() +
#> theme_bw() +
#> theme(legend.position = "bottom")
#> 
#> 
#>   p1 + p2


## ----echo = FALSE, fig.width = 6, fig.height = 4, include=knitr::is_html_output(), eval=knitr::is_html_output(), fig.cap = "Plot of design options made with plotly. The color represents the Euclidean distance to the top left point and the optimal design is indicated by a diamond."----
#> p <- plot(
#>   one_sample_cal_tbl,
#>   type1_range = c(0.05, 0.1),
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
#>   )


## -----------------------------------------------------------------------------
one_sample_decision_tbl


## ----message = FALSE, fig.height = 9, fig.width = 6, include = knitr::is_latex_output(), eval = knitr::is_latex_output(), fig.cap = "Plot of decision rules made with ggplot. The color indicates whether the trial should stop or proceed for a given number of responses at each interim analysis."----
plot(one_sample_decision_tbl, plotly = FALSE)


## ----message = FALSE, fig.height = 9, fig.width = 6, include=knitr::is_html_output(), eval=knitr::is_html_output(), fig.cap = "Plot of decision rules made with plotly. The color indicates whether the trial should stop or proceed for a given number of responses at each interim analysis."----
#> plot(one_sample_decision_tbl)

```{.r .distill-force-highlighting-css}
```
