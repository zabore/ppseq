#' Simulate a single dataset based on the response probability(ies), the total
#' sample size(s), and the interim look schedule(s)
#'
#' @description Helper function for calibrate_thresholds() function that
#' generates a single dataset of n and response count at each look based on the
#' response probability(ies)
#'
#' @param p vector of length two containing the probability of event in
#' the standard of care and experimental arm c(p0, p1) for the two-sample case;
#' integer of event probability for one-sample case
#' @param n matrix containing the total number of patients accrued so far at
#' each interim look in the standard of care (column 1) and experimental
#' (column 2) arms for two-sample case; vector of sample size accrued so far
#' at each interim look for one-sample case. The last value should be equal to
#' the total sample size at the end of the trial.
#' If only a single look will be done
#' at the end of the trial, this can be a vector specifying the total sample
#' size c(N0, N1) for the two-sample case or an integer specifying the total
#' sample size N for the one-sample case
#'
#' @return Returns a tibble with n0, n1, y0, y1 for the two-sample case and
#' a tibble with n1 and y1 for the one-sample case
#' 
#' @importFrom stats rbinom
#' @importFrom tibble tibble
sim_dat1 <- function(p, n) {
  if (length(p) == 2) {
    if (length(n) == 2 & is.matrix(n) == FALSE) {
      n <- matrix(n, nrow = 1)
    }
    y0 <- rbinom(n = 1, size = n[1, 1], prob = p[1])
    y1 <- rbinom(n = 1, size = n[1, 2], prob = p[2])
    if (length(n) > 2) {
      for (i in seq_len(nrow(n))[-1]) {
        y0 <- c(y0, y0[length(y0)] + rbinom(
          n = 1,
          size = n[i, 1] - n[i - 1, 1],
          prob = p[1]
        ))
        y1 <- c(y1, y1[length(y1)] + rbinom(
          n = 1,
          size = n[i, 2] - n[i - 1, 2],
          prob = p[2]
        ))
      }
    }
    return(tibble(
      n0 = n[, 1],
      n1 = n[, 2],
      y0 = y0,
      y1 = y1
    ))
  } else if (length(p) == 1) {
    y1 <- rbinom(n = 1, size = n[1], prob = p)
    if (length(n) > 1) {
      for (i in seq_along(n)[-1]) {
        y1 <- c(y1, y1[length(y1)] +
          rbinom(n = 1, size = n[i] - n[i - 1], prob = p))
      }
    }
    return(tibble(n1 = n, y1 = y1))
  }
}




#' Evaluate a single dataset for a single pp_threshold and ppp_threshold
#' combination
#'
#' @description Helper function for calibrate_thresholds() function that 
#' evaluates a single combination of a pp_threshold and a ppp_threshold for a 
#' single dataset
#'
#' @param data the name of the dataset
#' @param pp_threshold the posterior probability threshold of interest
#' @param ppp_threshold the posterior probability threshold of interest for
#' futility monitoring
#' @param p0 The target value to compare to in the one-sample case. Set to 
#' NULL for the two-sample case.
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param direction "greater" (default) if interest is in P(p1 > p0) and "less"
#' if interest is in P(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in P(p > p0) and "less" if interest is in P(p < p0).
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for the two-sample case. NULL for the one-sample case (default).
#' @param monitoring the type of interim monitoring to be performed. One of 
#' "futility" or "efficacy". Default is "futility".
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#'
#' @return Returns a tibble with the total sample size at the end of the
#' trial, the number of responses observed at the end of the trial, the
#' pp_threshold considered, the ppp_threshold considered, the observed
#' predictive probability generated from calc_predictive(), and an indicator
#' for whether the trial was positive or not at the end
#'
#' @importFrom tibble add_column
#' @importFrom dplyr mutate case_when
eval_thresh <- function(data, pp_threshold, ppp_threshold, p0, N, 
                        direction = "greater", delta = NULL,
                        monitoring = "futility",
                        prior = c(0.5, 0.5), S = 5000) {
  
  if (!monitoring %in% c("futility", "efficacy"))
    stop('monitoring must be either "futility" or "efficacy"')
  
  decision <- NULL
  ppp <- NULL
  for (i in 1:nrow(data)) {
    if (ncol(data) == 4) {
      ppp[i] <- calc_predictive(
        y = c(data$y0[i], data$y1[i]),
        n = c(data$n0[i], data$n1[i]),
        direction = direction, 
        p0 = p0, 
        delta = delta,
        prior = prior, 
        S = S, 
        N = N,
        theta = pp_threshold
      )
    } else if (ncol(data) == 2) {
      ppp[i] <- calc_predictive(
        y = data$y1[i], 
        n = data$n1[i],
        direction = direction, 
        p0 = p0, 
        delta = delta,
        prior = prior, 
        S = S, 
        N = N,
        theta = pp_threshold
      )
    }
    decision[i] <- ifelse(monitoring == "futility", 
                          ppp[i] < ppp_threshold,
                          ppp[i] > ppp_threshold)
    if (decision[i] == TRUE) break
  }
  
  res <- mutate(
    data[ifelse(any(decision == TRUE),
                which(decision == TRUE),
                length(decision)
    ), ],
    pp_threshold = pp_threshold,
    ppp_threshold = ppp_threshold,
    ppp = ppp[ifelse(any(decision == TRUE),
                     which(decision == TRUE),
                     length(decision)
    )],
    positive = ppp > pp_threshold
  )
  
  return(res)
}




#' Calibrate according to posterior probability threshold and predictive
#' probability threshold with interim futility monitoring
#'
#' @description This function is meant to be used in the context of a
#' clinical trial with a binary endpoint. For every combination of the provided
#' posterior thresholds and predictive thresholds, the function simulates many 
#' trials and then calculates the average number of times a trial was positive. 
#' In the null case, this is the type I error for the given thresholds. 
#' In the alternative case, this is the power for the given thresholds.
#'
#' @param p_null vector of length two containing the probability of event in
#' the standard of care and experimental arm c(p0, p1) for the two-sample case
#' for the null scenario;
#' integer of event probability for one-sample case
#' @param p_alt vector of length two containing the probability of event in
#' the standard of care and experimental arm c(p0, p1) for the two-sample case
#' for the alternative scenario;
#' integer of event probability for one-sample case
#' @param n matrix containing the total number of patients accrued so far at
#' each interim look in the standard of care (column 1) and experimental
#' (column 2) arms for two-sample case; vector of sample size accrued so far
#' at each interim look for one-sample case. The last value should be equal to
#' the total sample size at the end of the trial.
#' If only a single look will be done
#' at the end of the trial, this can be a vector specifying the total sample
#' size c(N0, N1) for the two-sample case or an integer specifying the total
#' sample size N for the one-sample case
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param pp_threshold the posterior probability threshold of interest
#' @param ppp_threshold the posterior predictive probability threshold of 
#' interest for futility monitoring
#' @param direction "greater" (default) if interest is in p(p1 > p0) and "less"
#' if interest is in p(p1 < p0) for two-sample case. For one-sample case,
#' "greater" if interest is in p(p > p0) and "less" if interest is in p(p < p0).
#' @param delta clinically meaningful difference between groups.
#' Typically 0 for the two-sample case. NULL for the one-sample case (default).
#' @param monitoring the type of interim monitoring to be performed. One of 
#' "futility" or "efficacy". Default is "futility".
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples drawn from the posterior. Default is 5000
#' @param nsim Number of simulated trial datasets.

#'
#' @return A list containing a
#' 1) a tibble 'res_summary' containing the posterior probability threshold
#' (pp_threshold), the predictive probability threshold (ppp_threshold),
#' the mean sample size under the null (mean_n0_null and mean_n1_null 
#' for two-sample case; mean_n1_null for one-sample case), the proportion of 
#' positive trials under the null (prop_pos_null), the proportion of trials 
#' stopped early under the null (prop_stopped_null), the mean sample 
#' size under the alternative (mean_n0_alt and mean_n1_alt
#' for two-sample case; mean_n1_alt for one-sample case), the proportion of 
#' positive trials under the alternative (prop_pos_alt), the proportion of 
#' trials stopped early under the alternative (prop_stopped_alt)
#' 2) 'call_list' containing the original function call
#' 3) 'calibrate_thresholds_inputs' a list containing the inputs to the
#' original function call
#'
#' The proportion of positive trials will be a measure of the type I error
#' for a null setting, and a measure of the power in the alternative setting.
#'
#' @examples
#' 
#' # One-sample case
#' set.seed(123)
#' 
#' calibrate_thresholds(
#'   p_null = 0.1, 
#'   p_alt = 0.4,
#'   n = seq(5, 15, 5), 
#'   N = 15,
#'   pp_threshold = c(0.85, 0.9),
#'   ppp_threshold = c(0.1, 0.2),
#'   S = 10, 
#'   nsim = 10
#'   )
#' 
#' # Two-sample case
#' set.seed(456)
#' 
#' calibrate_thresholds(
#'   p_null = c(0.1, 0.1), 
#'   p_alt = c(0.1, 0.5),
#'   n = cbind(seq(5, 15, 5), seq(5, 15, 5)), 
#'   N = c(15, 15),
#'   pp_threshold = c(0.8, 0.85),
#'   ppp_threshold = c(0.2, 0.3),
#'   delta = 0,
#'   S = 10, 
#'   nsim = 10
#'   )
#' 
#' @importFrom purrr pmap_dfr map cross_df
#' @importFrom furrr future_map furrr_options
#' @importFrom dplyr bind_rows full_join select rename ungroup summarize
#' @export

calibrate_thresholds <- function(p_null, p_alt, n, N,
                                 pp_threshold, ppp_threshold, 
                                 direction = "greater", delta = NULL, 
                                 monitoring = "futility",
                                 prior = c(0.5, 0.5),
                                 S = 5000, nsim = 1000) {
  sim_dat_null <-
    map(seq_len(nsim), ~ sim_dat1(p = p_null, n = n))
  
  sim_dat_alt <-
    map(seq_len(nsim), ~ sim_dat1(p = p_alt, n = n))
  
  cross_threshold <-
    cross_df(list(
      pp_threshold = pp_threshold,
      ppp_threshold = ppp_threshold
    ))
  
  p0 <- if(length(p_null) == 1) 
    p_null else if(length(p_null) == 2)
      NULL
  
  res_null <- 
    future_map(
      sim_dat_null,
      function(x) 
        pmap_dfr(cross_threshold,
                 function(pp_threshold, ppp_threshold) 
                   eval_thresh(x, 
                               pp_threshold, 
                               ppp_threshold,
                               direction = direction, 
                               p0 = p0, 
                               delta = delta, 
                               prior = prior, 
                               S = S, 
                               N = N,
                               monitoring = monitoring)), 
      .options = furrr_options(seed = TRUE)
    )
  
  res_alt <- 
    future_map(
      sim_dat_alt,
      function(x) 
        pmap_dfr(cross_threshold,
                 function(pp_threshold, ppp_threshold) 
                   eval_thresh(x, 
                               pp_threshold, 
                               ppp_threshold,
                               direction = direction, 
                               p0 = p0, 
                               delta = delta, 
                               prior = prior, 
                               S = S, 
                               N = N,
                               monitoring = monitoring)),
      .options = furrr_options(seed = TRUE)
    )
  
  res_df_null <-
    bind_rows(
      res_null,
      .id = "sim_num"
    )
  
  res_df_alt <-
    bind_rows(
      res_alt,
      .id = "sim_num"
    )
  
  if (length(p_null) == 2) {
    res_df <-
      full_join(
        select(
          rename(res_df_null,
                 n0_null = n0, 
                 n1_null = n1,
                 positive_null = positive
          ),
          -ppp, -y0, -y1
        ),
        select(
          rename(res_df_alt,
                 n0_alt = n0, 
                 n1_alt = n1,
                 positive_alt = positive
          ),
          -ppp, -y0, -y1
        )
      )
    
    res_summary <-
      ungroup(
        summarize(
          group_by(res_df, pp_threshold, ppp_threshold),
          mean_n0_null = mean(n0_null),
          mean_n1_null = mean(n1_null),
          prop_pos_null = mean(positive_null),
          prop_stopped_null = mean(sum(n0_null, n1_null) < sum(N)),
          mean_n0_alt = mean(n0_alt),
          mean_n1_alt = mean(n1_alt),
          prop_pos_alt = mean(positive_alt),
          prop_stopped_alt = mean(sum(n0_alt, n1_alt) < sum(N))
        )
      )
  } else if (length(p_null) == 1) {
    res_df <-
      full_join(
        select(
          rename(res_df_null, n1_null = n1, positive_null = positive),
          -ppp, -y1
        ),
        select(
          rename(res_df_alt, n1_alt = n1, positive_alt = positive),
          -ppp, -y1
        )
      )
    
    res_summary <-
      ungroup(
        summarize(
          group_by(res_df, pp_threshold, ppp_threshold),
          mean_n1_null = mean(n1_null),
          prop_pos_null = mean(positive_null),
          prop_stopped_null = mean(n1_null < N),
          mean_n1_alt = mean(n1_alt),
          prop_pos_alt = mean(positive_alt),
          prop_stopped_alt = mean(n1_alt < N)
        )
      )
  }
  
  # create empty list to return everything
  x <- list()
  
  # add the main results in
  x$res_summary <- res_summary
  
  # will return call, and all object passed to in table1 call
  # the object func_inputs is a list of every object passed to the function
  calibrate_thresholds_inputs <- list(
    p_null = p_null,
    p_alt = p_alt,
    n = n,
    direction = direction,
    delta = delta,
    prior = prior,
    S = S,
    N = N,
    nsim = nsim,
    pp_threshold = pp_threshold,
    ppp_threshold = ppp_threshold,
    monitoring = monitoring
  )
  
  # create other objects to return
  x$call_list <- list(calibrate_thresholds = match.call())
  x$inputs <- calibrate_thresholds_inputs
  
  # assign a custom class for S3 plotting and printing methods
  class(x) <- c("calibrate_thresholds", class(x))
  
  x
  
}