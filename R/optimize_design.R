#' Function to setup usage of `optimize_design.calibrate_thresholds`
#' 
#' @param x an object of class 'calibrate_thresholds', usually returned by the 
#' \code{calibrate_thresholds} function
#' @param type1_range a vector specifying the minimum and maximum acceptable
#' type I error. Specify NULL to return the full range of resulting type I 
#' error. Defaults to c(0.05, 0.1)
#' @param minimum_power a numeric between 0 and 1 specifying the minimum 
#' acceptable power. Specify NULL to return the full range of resulting power.
#' Defaults to 0.8.
#' @param ... ignored
#' 
#' @export

optimize_design <- function(x, 
                            type1_range = c(0.05, 0.1), 
                            minimum_power = 0.8, ...) {
  UseMethod("optimize_design")
}


#' Custom optimization method for \code{calibrate_thresholds} objects
#' 
#' @description Determines the optimal designs based on a variety of criteria
#' 
#' @param x an object of class 'calibrate_thresholds', usually returned by the 
#' \code{calibrate_thresholds} function
#' @param type1_range a vector specifying the minimum and maximum acceptable
#' type I error. Specify NULL to return the full range of resulting type I 
#' error. Defaults to c(0.05, 0.1)
#' @param minimum_power a numeric between 0 and 1 specifying the minimum 
#' acceptable power. Specify NULL to return the full range of resulting power.
#' Defaults to 0.8.
#' @param ... ignored
#' 
#' @examples 
#' 
#' \dontrun{
#' set.seed(123)
#' 
#' cal_tbl <- calibrate_thresholds(prob_null = 0.1, prob_alt = 0.3, 
#' n = seq(5, 25, 5), direction = "greater", p0 = 0.1, delta = NULL, 
#' prior = c(0.5, 0.5), S = 5000, N = 25, nsim = 1000, 
#' pp_threshold = c(0.9, 0.95, 0.96, 0.98), 
#' ppp_threshold = seq(0.05, 0.2, 0.05))
#' 
#' optimize_design(cal_tbl)
#' }
#' 
#' @export

optimize_design.calibrate_thresholds <- function(x, 
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
  
  options(tibble.width = Inf)
  
  plot_x <- 
    dplyr::mutate(
      dplyr::filter(x$res_summary, 
                    prop_pos_null >= type1_range[1] & 
                      prop_pos_null <= type1_range[2] &
                      prop_pos_alt >= minimum_power),
      `Youden's index` = prop_pos_alt + prop_stopped_null - 1
    )
  
  list(
    "The design that maximizes specificity (i.e. the proportion stopped under the null) is:" = 
      plot_x[which.max(plot_x$prop_stopped_null), ],
    "The design that maximizes sensitivity (i.e. the proportion positive under the alternative) is:" = 
      plot_x[which.max(plot_x$prop_pos_alt), ],
    "The design that maximizes Youden's index is:" =
      plot_x[which.max(plot_x$`Youden's index`), ]
  )
}