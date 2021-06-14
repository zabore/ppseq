#' Plot method for \code{calibrate_thresholds} objects
#'
#' @description Returns two interactive \code{plotly} plots to compare results
#' from various designs generated from a call to \code{calibrate_thresholds}
#' based on various criteria, and to assist in selecting an optimal design.
#'
#' @param x an object of class 'calibrate_thresholds', usually returned by the
#' \code{calibrate_thresholds} function
#' @param type1_range a vector specifying the minimum and maximum acceptable
#' type I error. Specify NULL to return the full range of resulting type I
#' error. Defaults to c(0.05, 0.1)
#' @param minimum_power a numeric between 0 and 1 specifying the minimum
#' acceptable power. Specify NULL to return the full range of resulting power.
#' Defaults to 0.8.
#' @param plotly a logical indicator of whether you want the plots returned as
#' interactive plotly plots or non-interactive ggplots
#' @param ... unused
#'
#' @importFrom dplyr rename mutate filter ungroup slice group_by arrange
#'                   row_number
#' @importFrom ggplot2 ggplot aes geom_point xlim ylim labs 
#'                     scale_color_viridis_c theme_bw theme
#' @importFrom gridExtra grid.arrange
#' @importFrom plotly ggplotly
#' @export

plot.calibrate_thresholds <- function(x,
                                      type1_range = c(0.05, 0.1),
                                      minimum_power = 0.8,
                                      plotly = FALSE,
                                      ...) {
  if (any(class(x) == "calibrate_thresholds") == FALSE)
    stop("x must be class 'calibrate_thresholds', usually an object returned from a call to the function ppseq::calibrate_thresholds()")

  if (length(type1_range) != 2 & !is.null(type1_range))
    stop("type1_range must be a vector of length 2 defining the range of acceptable type I error, or can be set equal to NULL to return all values of type I error")

  if (length(minimum_power) != 1 & !is.null(minimum_power))
    stop("minimum_power must be a numeric value of length 1 defining the minimum acceptable power, or can be set equal to NULL to return all values of power")

  if (!is.numeric(minimum_power) & !is.null(minimum_power))
    stop("minimum_power must be a numeric value of length 1 or NULL")

  if (!is.numeric(type1_range) & !is.null(type1_range))
    stop("type1_range must be a numeric vector of length 2 or NULL")

  if (length(x$inputs$prob_null) == 2) {
    plot_x <-
      rename(
        mutate(
          filter(
            x$res_summary,
            prop_pos_null >= type1_range[1] &
              prop_pos_null <= type1_range[2] &
              prop_pos_alt >= minimum_power
          ),
          Design = paste0(
            "Posterior threshold ", pp_threshold,
            " and predictive threshold ", ppp_threshold
          ),
          ab_dist_metric = ((prop_pos_null - 0)^2 +
            (prop_pos_alt - 1)^2)^(1 / 2),
          mean_n_null = (mean_n0_null + mean_n1_null) / 2,
          mean_n_alt = (mean_n0_alt + mean_n1_alt) / 2,
          n_dist_metric = ((mean_n_null - min(mean_n_null))^2 +
            (mean_n_alt - max(mean_n_alt))^2)^(1 / 2)
        ),
        `Type I error` = prop_pos_null,
        Power = prop_pos_alt,
        `Average N under the null` = mean_n_null,
        `Average N under the alternative` = mean_n_alt,
        `Distance to min(N under null) and max(N under alt)` = n_dist_metric,
        `Distance to (0, 1)` = ab_dist_metric
      )
  } else if (length(x$inputs$prob_null) == 1) {
    plot_x <-
      rename(
        mutate(
          filter(
            x$res_summary,
            prop_pos_null >= type1_range[1] &
              prop_pos_null <= type1_range[2] &
              prop_pos_alt >= minimum_power
          ),
          Design = paste0(
            "Posterior threshold = ", pp_threshold,
            " and predictive threshold = ", ppp_threshold
          ),
          ab_dist_metric = ((prop_pos_null - 0)^2 +
            (prop_pos_alt - 1)^2)^(1 / 2),
          n_dist_metric = ((mean_n1_null - min(mean_n1_null))^2 +
            (mean_n1_alt - max(mean_n1_alt))^2)^(1 / 2)
        ),
        `Type I error` = prop_pos_null,
        Power = prop_pos_alt,
        `Average N under the null` = mean_n1_null,
        `Average N under the alternative` = mean_n1_alt,
        `Distance to optimal efficiency` = n_dist_metric,
        `Distance to optimal accuracy` = ab_dist_metric
      )
  }

  plot_ab <-
    mutate(
      ungroup(
        slice(
          group_by(
            arrange(
              plot_x,
              `Distance to optimal accuracy`,
              -pp_threshold,
              -ppp_threshold
            ),
            `Distance to optimal accuracy`
          ),
          1
        )
      ),
      optimal_accuracy = ifelse(row_number() == 1, TRUE, FALSE)
    )

  plot_nn <-
    mutate(
      ungroup(
        slice(
          group_by(
            arrange(
              plot_x,
              `Distance to optimal efficiency`,
              -pp_threshold, -ppp_threshold
            ),
            `Distance to optimal efficiency`
          ),
          1
        )
      ),
      optimal_efficiency = ifelse(row_number() == 1, TRUE, FALSE)
    )

  p1 <-
    ggplot(
      plot_ab,
      aes(
        x = `Type I error`,
        y = Power,
        color = `Distance to optimal accuracy`,
        Design = Design
      )
    ) +
    geom_point(
      shape = ifelse(plot_ab$optimal_accuracy == TRUE, 18, 19),
      size = ifelse(plot_ab$optimal_accuracy == TRUE, 4, 2)
    ) +
    ylim(0, 1) +
    xlim(0, 1) +
    labs(
      x = "Type I error",
      y = "Power"
    ) +
    scale_color_viridis_c() +
    theme_bw() +
    theme(legend.position = "bottom")

  p2 <-
    ggplot(
      plot_nn,
      aes(
        x = `Average N under the null`,
        y = `Average N under the alternative`,
        color = `Distance to optimal efficiency`,
        Design = Design
      )
    ) +
    geom_point(
      shape = ifelse(plot_nn$optimal_efficiency == TRUE, 18, 19),
      size = ifelse(plot_nn$optimal_efficiency == TRUE, 4, 2)
    ) +
    ylim(
      min(plot_x$`Average N under the null`),
      max(plot_x$`Average N under the alternative`)
    ) +
    xlim(
      min(plot_x$`Average N under the null`),
      max(plot_x$`Average N under the alternative`)
    ) +
    labs(
      x = "Average N under the null",
      y = "Average N under the alternative"
    ) +
    scale_color_viridis_c() +
    theme_bw() +
    theme(legend.position = "bottom")

  ifelse(plotly == TRUE,
    list(ggplotly(p1), ggplotly(p2)),
    grid.arrange(p1, p2, nrow = 1, ncol = 2)
  )
}
