#' Function to setup usage of `optimize_design.calibrate_thresholds`
#'
#' @param x an object of class 'calibrate_thresholds', usually returned by the
#' \code{calibrate_thresholds} function
#' @param type1_range a vector specifying the minimum and maximum acceptable
#' type I error. Specify NULL to return the full range of resulting type I
#' error. Defaults to c(0, 1) to return all results.
#' @param minimum_power a numeric between 0 and 1 specifying the minimum
#' acceptable power. Specify NULL to return the full range of resulting power.
#' Defaults to 0 to return all results.
#' @param ... ignored
#' 
#' @return No return value, called for side effects 
#'
#' @export

optimize_design <- function(x,
                            type1_range = c(0, 1),
                            minimum_power = 0, ...) {
  UseMethod("optimize_design")
}


#' Custom optimization method for \code{calibrate_thresholds} objects
#'
#' @description Determines the optimal designs based on a variety of criteria.
#' The optimal efficiency design is the one with the shortest Euclidean 
#' distance to the upper left point on a plot of the average sample size under
#' the null by the average sample size under the alternative. The optimal
#' accuracy design is the one with the shortest Euclidean distance to the 
#' upper left point on a plot of the type I error by the power.
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
#' @return A list of length two containing details of the optimal efficiency
#' and optimal accuracy designs
#'
#' @examples
#' 
#' # Setting S = 50 and nsim = 50 for speed
#' # In practice you would want a much larger sample and more simulations
#' 
#' # One-sample case
#' set.seed(123)
#' 
#' cal_tbl1 <- calibrate_thresholds(
#'   p_null = 0.1, 
#'   p_alt = 0.4,
#'   n = seq(5, 15, 5), 
#'   N = 15,
#'   pp_threshold = c(0.85, 0.9),
#'   ppp_threshold = c(0.1, 0.2),
#'   S = 50, 
#'   nsim = 50
#'   )
#' 
#' optimize_design(cal_tbl1)
#' 
#' 
#' # Two-sample case
#' set.seed(456)
#' 
#' cal_tbl2 <- calibrate_thresholds(
#'   p_null = c(0.1, 0.1), 
#'   p_alt = c(0.1, 0.5),
#'   n = cbind(seq(5, 15, 5), seq(5, 15, 5)), 
#'   N = c(15, 15),
#'   pp_threshold = c(0.8, 0.85),
#'   ppp_threshold = c(0.2, 0.3),
#'   delta = 0,
#'   S = 50, 
#'   nsim = 50
#'   )
#' 
#' optimize_design(cal_tbl2)
#'
#' @importFrom dplyr rename mutate filter slice group_by arrange
#' @export

optimize_design.calibrate_thresholds <- function(x,
                                                 type1_range = c(0.05, 0.1),
                                                 minimum_power = 0.8, ...) {
  if (any(class(x) == "calibrate_thresholds") == FALSE)
    stop("x must be class 'calibrate_thresholds', usually an object ",
         "returned from a call to the function ppseq::calibrate_thresholds()")

  if (length(type1_range) != 2 & !is.null(type1_range))
    stop("type1_range must be a vector of length 2 defining the ", 
         "range of acceptable type I error, or can be set equal to NULL ",
         "to return all values of type I error")

  if (length(minimum_power) != 1 & !is.null(minimum_power))
    stop("minimum_power must be a numeric value of length 1 defining the ",
         "minimum acceptable power, or can be set equal to NULL to return ",
         "all values of power")

  if (!is.numeric(minimum_power) & !is.null(minimum_power))
    stop("minimum_power must be a numeric value of length 1 or NULL")

  if (!is.numeric(type1_range) & !is.null(type1_range))
    stop("type1_range must be a numeric vector of length 2 or NULL")

  options(tibble.width = Inf)

  if (length(x$inputs$p_null) == 2) {
    opt_x <-
      rename(
        mutate(
          filter(
            x$res_summary,
            prop_pos_null >= type1_range[1] &
              prop_pos_null <= type1_range[2] &
              prop_pos_alt >= minimum_power
          ),
          mean_n_null = (mean_n0_null + mean_n1_null) / 2,
          mean_n_alt = (mean_n0_alt + mean_n1_alt) / 2,
          ab_dist_metric = ((prop_pos_null - 0)^2 +
            (prop_pos_alt - 1)^2)^(1 / 2),
          n_dist_metric = ((mean_n_null - min(mean_n_null))^2 +
            (mean_n_alt - max(mean_n_alt))^2)^(1 / 2)
        ),
        `Type I error` = prop_pos_null,
        Power = prop_pos_alt,
        `Average N under the null` = mean_n_null,
        `Average N under the alternative` = mean_n_alt,
        `Distance to optimal efficiency` = n_dist_metric,
        `Distance to optimal accuracy` = ab_dist_metric
      )
  } else if (length(x$inputs$p_null) == 1) {
    opt_x <-
      rename(
        mutate(
          filter(
            x$res_summary,
            prop_pos_null >= type1_range[1] &
              prop_pos_null <= type1_range[2] &
              prop_pos_alt >= minimum_power
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

  opt_ab <-
    slice(
      group_by(
        arrange(
          opt_x,
          `Distance to optimal accuracy`, -pp_threshold, -ppp_threshold
        ),
        `Distance to optimal accuracy`
      ),
      1
    )

  opt_nn <-
    slice(
      group_by(
        arrange(
          opt_x,
          `Distance to optimal efficiency`,
          -pp_threshold, -ppp_threshold
        ),
        `Distance to optimal efficiency`
      ),
      1
    )


  list(
    "Optimal accuracy design:" =
      opt_ab[
        opt_ab$`Distance to optimal accuracy` == min(opt_ab$`Distance to optimal accuracy`),
        c(
          "pp_threshold", "ppp_threshold", "Type I error", "Power",
          "Average N under the null", "Average N under the alternative"
        )
      ],
    "Optimal efficiency design:" =
      opt_nn[
        opt_nn$`Distance to optimal efficiency` ==
          min(opt_nn$`Distance to optimal efficiency`),
        c(
          "pp_threshold", "ppp_threshold", "Type I error", "Power",
          "Average N under the null", "Average N under the alternative"
        )
      ]
  )
}
