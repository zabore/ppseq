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
#' @param ... unused
#' 
#' @export

plot.calibrate_thresholds <- function(x, 
                                      type1_range = c(0.05, 0.1), 
                                      minimum_power = 0.8, ...) {
  
  if(any(class(x) =="calibrate_thresholds") == FALSE)
    stop("x must be class 'calibrate_thresholds', usually an object returned from a call to the function ppseq::calibrate_thresholds()")
  
  if(length(type1_range) != 2 & !is.null(type1_range))
    stop("type1_range must be a vector of length 2 defining the range of acceptable type I error, or can be set equal to NULL to return all values of type I error")
  
  if(length(minimum_power) != 1 & !is.null(minimum_power))
    stop("minimum_power must be a numeric value of length 1 defining the minimum acceptable power, or can be set equal to NULL to return all values of power")
  
  if(!is.numeric(minimum_power) & !is.null(minimum_power))
    stop("minimum_power must be a numeric value of length 1 or NULL")
  
  if(!is.numeric(type1_range) & !is.null(type1_range))
    stop("type1_range must be a numeric vector of length 2 or NULL")
  
  plot_x <- 
    dplyr::rename(
      dplyr::mutate(
        dplyr::filter(
          x$res_summary, 
          prop_pos_null >= type1_range[1] & 
            prop_pos_null <= type1_range[2] &
            prop_pos_alt >= minimum_power
        ),
        `Youden's index` = prop_pos_alt + prop_stopped_null - 1,
        Design = paste0("Posterior threshold ", pp_threshold, 
                        " and predictive threshold ", ppp_threshold),
        n_dist_metric = ((mean_n1_null - min(mean_n1_null))^2 + 
                           (mean_n1_alt - max(mean_n1_alt))^2)^(1/2)
      ),
      `Type I error` = prop_pos_null,
      Power = prop_pos_alt,
      `Average N under the null` = mean_n1_null,
      `Average N under the alternative` = mean_n1_alt,
      `Distance to min(N under null) and max(N under alt)` = n_dist_metric
    )
  
  p1 <- 
    ggplot2::ggplot(plot_x, 
                    ggplot2::aes(
                      x = `Type I error`, 
                      y = Power, 
                      color = `Youden's index`,
                      Design = Design)) + 
    ggplot2::geom_point() + 
    ggplot2::ylim(0, 1) + 
    ggplot2::xlim(0, 1) +
    ggplot2::labs(
      x = "Type I error",
      y = "Power"
    ) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::theme_bw()
  
  p2 <- 
    ggplot2::ggplot(plot_x, 
                    ggplot2::aes(
                      x = `Average N under the null`, 
                      y = `Average N under the alternative`,
                      color = `Distance to min(N under null) and max(N under alt)`,
                      Design = Design)) + 
    ggplot2::geom_point() + 
    ggplot2::ylim(min(plot_x$mean_n1_null), max(plot_x$mean_n1_alt)) +
    ggplot2::xlim(min(plot_x$mean_n1_null), max(plot_x$mean_n1_alt)) +
    ggplot2::labs(
      x = "Average N under the null",
      y = "Average N under the alternative",
      color = "Distance to best"
    ) +
    ggplot2::scale_color_viridis_c() +
    ggplot2::theme_bw()
  
  # gridExtra::grid.arrange(p1, p2, ncol = 2)
  plotly::ggplotly(p1)
  plotly::ggplotly(p2)

}