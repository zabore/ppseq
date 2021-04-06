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
    dplyr::mutate(
      dplyr::filter(x$res_summary, 
                    prop_pos_null >= type1_range[1] & 
                      prop_pos_null <= type1_range[2] &
                      prop_pos_alt >= minimum_power),
      `Youden's index` = prop_pos_alt + prop_stopped_null - 1
    )
  
  p1 <- 
    ggplot2::ggplot(plot_x, ggplot2::aes(x = prop_stopped_null, 
                                         y = prop_pos_alt, 
                                         color = `Youden's index`)) + 
    ggplot2::geom_point() + 
    ggplot2::scale_x_reverse(limits = c(1, 0)) +
    ggplot2::ylim(0, 1) + 
    ggplot2::labs(
      x = "Proportion stopped under the null (Specificity)",
      y = "Proportion positive under alternative (Sensitiivity)"
    ) 
  
  simon_lower <- clinfun::ph2simon(pu = x$inputs$prob_null, 
                                   pa = x$inputs$prob_alt, 
                                   ep1 = type1_range[1], 
                                   ep2 = 1 - minimum_power)
  
  xopt_lower <- simon_lower$out[
    c(((1:nrow(simon_lower$out))[simon_lower$out[, 5] == min(simon_lower$out[, 5])])[1], 1), ]
  
  plot_simon_lower <- tibble::add_column(
    dplyr::rename(
      tibble::as_tibble(xopt_lower), 
      mean_n1_null = "EN(p0)", 
      prop_stopped_null = "PET(p0)"
    ),
    prop_pos_null = type1_range[1],
    design = c(paste0("Simon's optimal \n Type I error = ", type1_range[1]), 
               paste0("Simon's minimax \n Type I error = ", type1_range[1])))
  
  simon_upper <- clinfun::ph2simon(pu = x$inputs$prob_null, 
                                   pa = x$inputs$prob_alt, 
                                   ep1 = type1_range[2],
                                   ep2 = 1 - minimum_power)
  
  xopt_upper <- simon_upper$out[
    c(((1:nrow(simon_upper$out))[simon_upper$out[, 5] == min(simon_upper$out[, 5])])[1], 1), ]
  
  plot_simon_upper <- tibble::add_column(
    dplyr::rename(
      tibble::as_tibble(xopt_upper), 
      mean_n1_null = "EN(p0)", 
      prop_stopped_null = "PET(p0)"
    ),
    prop_pos_null = type1_range[2],
    design = c(paste0("Simon's optimal \n Type I error = ", type1_range[2]), 
               paste0("Simon's minimax \n Type I error = ", type1_range[2])))
  
  p2 <- ggplot2::ggplot(plot_x, ggplot2::aes(x = mean_n1_null, 
                                             y = prop_stopped_null, 
                                             color = prop_pos_null)) + 
    ggplot2::geom_point() + 
    ggplot2::ylim(0, 1) + 
    ggplot2::labs(
      x = "Expected N under the null",
      y = "Probability of early stopping under the null",
      color = "Type I error"
    ) +
    ggplot2::geom_point(data = plot_simon_lower) +
    ggplot2::geom_text(
      data = plot_simon_lower,
      ggplot2::aes(label = design),
      size = 3
    ) +
    ggplot2::geom_point(data = plot_simon_upper) +
    ggplot2::geom_text(
      data = plot_simon_upper,
      ggplot2::aes(label = design),
      size = 3
    )
  
  # gridExtra::grid.arrange(p1, p2, ncol = 2)
  plotly::ggplotly(p1)
  plotly::ggplotly(p2)

}