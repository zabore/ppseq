#' Print method for \code{calibrate_thresholds} objects
#'
#' @description By default prints only the res_summary table from an object of
#' class 'calibrate_thresholds'. The table can be limited to a range of type 1
#' error and a minimum value of power using the arguments 'type1_range' and
#' 'minimum_power' respectively.
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
#' @return
#' Returns a tibble
#'
#' @examples
#' \dontrun{
#' set.seed(123)
#'
#' cal_tbl <- calibrate_thresholds(
#'   p_null = 0.1, p_alt = 0.3,
#'   n = seq(5, 25, 5), N = 25, 
#'   pp_threshold = c(0.9, 0.95, 0.96, 0.98),
#'   ppp_threshold = seq(0.05, 0.2, 0.05),
#'   direction = "greater", delta = NULL,
#'   prior = c(0.5, 0.5), S = 5000, nsim = 1000
#' )
#'
#' print(cal_tbl)
#' print(cal_tbl, type1_range = NULL, minimum_power = NULL)
#' }
#'
#' @export

print.calibrate_thresholds <- function(x,
                                       type1_range = c(0.05, 0.1),
                                       minimum_power = 0.8,
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

  if (minimum_power < 0 | minimum_power > 1)
    stop("minimum_power must be a numeric value between 0 and 1")

  if (type1_range[1] < 0 | type1_range[1] > 1 |
    type1_range[2] < 0 | type1_range[2] > 1 |
    type1_range[1] > type1_range[2])
    stop("type1_range must be a vector of numeric values, both between 0 and 1, with the first value less than the second value")

  if (is.null(type1_range) & is.null(minimum_power)) {
    print(x$res_summary)
  } else if (is.null(type1_range) & !is.null(minimum_power)) {
    print(x$res_summary[x$res_summary$prop_pos_alt >= minimum_power, ])
  } else if (!is.null(type1_range) & is.null(minimum_power)) {
    print(x$res_summary[x$res_summary$prop_pos_null >= type1_range[1] &
      x$res_summary$prop_pos_null <= type1_range[2], ])
  } else if (!is.null(type1_range) & !is.null(minimum_power)) {
    print(x$res_summary[x$res_summary$prop_pos_null >= type1_range[1] &
      x$res_summary$prop_pos_null <= type1_range[2] &
      x$res_summary$prop_pos_alt >= minimum_power, ])
  }
}
