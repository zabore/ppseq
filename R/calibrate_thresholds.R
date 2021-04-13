#' Simulate a single dataset based on the response probability(ies), the total 
#' sample size(s), and the interim look schedule(s)
#' 
#' @description Helper function for calibrate_thresholds() function that 
#' generates a single dataset of n and response count at each look based on the 
#' response probability(ies)
#' 
#' @param prob vector of length two containing the probability of event in 
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
#' @examples 
#' 
#' \dontrun{
#' set.seed(123)
#' 
#' # One-sample case
#' sim_dat1(prob = 0.1, n = seq(5, 25, 5))
#' 
#' # Two-sample case
#' sim_dat1(prob = c(0.1, 0.3), n = cbind(seq(5, 25, 5), seq(5, 25, 5)))
#' }
sim_dat1 <- function(prob, n) {
  if(length(prob) == 2) {
    if(length(n) == 2 & is.matrix(n) == FALSE) {n <- matrix(n, nrow = 1)}
    y0 <- stats::rbinom(n = 1, size = n[1, 1], prob = prob[1]) 
    y1 <- stats::rbinom(n = 1, size = n[1, 2], prob = prob[2]) 
    if(length(n) > 2) {
      for(i in 2:nrow(n)) { 
        y0 <- c(y0, y0[length(y0)] + stats::rbinom(n = 1, 
                                                   size = n[i, 1] - n[i - 1, 1], 
                                                   prob = prob[1])) 
        y1 <- c(y1, y1[length(y1)] + stats::rbinom(n = 1, 
                                                   size = n[i, 2] - n[i - 1, 2], 
                                                   prob = prob[2])) 
      }
    }
    return(tibble::tibble(n0 = n[, 1],
                          n1 = n[, 2],
                          y0 = y0,
                          y1 = y1))
  } else if(length(prob) == 1){
    y1 <- stats::rbinom(n = 1, size = n[1], prob = prob) 
    if(length(n) > 1) {
      for(i in 2:length(n)) { 
        y1 <- c(y1,  y1[length(y1)] + 
                  stats::rbinom(n = 1, size = n[i] - n[i - 1], prob = prob))
      }
    }
    return(tibble::tibble(n1 = n, y1 = y1))
  }
}




#' Evaluate a single dataset for a single pp_threshold and ppp_threshold 
#' combination
#' 
#' @description Helper function for calibrate_thresholds() function that evaluates
#' a single combination of a pp_threshold and a ppp_threshold for a single 
#' dataset
#' 
#' @param data the name of the dataset
#' @param pp_threshold the posterior probability threshold of interest
#' @param ppp_threshold the posterior probability threshold of interest for 
#' futility monitoring
#' @param direction "greater" (default) if interest is in P(p1 > p0) and "less" 
#' if interest is in P(p1 < p0) for two-sample case. For one-sample case, 
#' "greater" if interest is in P(p > p0) and "less" if interest is in P(p < p0).
#' @param p0 The target value to compare to in the one-sample case
#' @param delta clinically meaningful difference between groups. 
#' Typically 0 (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples, default is 5000
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' 
#' @return Returns a tibble with the total sample size at the end of the 
#' trial, the number of responses observed at the end of the trial, the 
#' pp_threshold considered, the ppp_threshold considered, the observed 
#' predictive probability generated from calc_predictive(), and an indicator 
#' for whether the trial was positive or not at the end
#' 
#' @examples 
#' 
#' \dontrun{
#' set.seed(123)
#' 
#' # One-sample case
#' dat1 <- sim_dat1(prob = 0.1, n = seq(5, 25, 5))
#' eval_thresh(dat1, 0.95, 0.3, p0 = 0.1, delta = NULL, S = 500, N = 25)
#' 
#' # Two-sample case
#' dat2 <- sim_dat1(prob = c(0.1, 0.3), n = cbind(seq(5, 25, 5), seq(5, 25, 5)))
#' eval_thresh(dat2, 0.95, 0.3, S = 500, N = c(25, 25))
#' }
eval_thresh <- function(data, pp_threshold, ppp_threshold,
                        direction = "greater", p0 = NULL, delta = 0, 
                        prior = c(0.5, 0.5), S = 5000, N) {
  decision <- NULL
  ppp <- NULL
  for(i in 1:nrow(data)) {
    
    if(ncol(data) == 4) {
      ppp[i] <- calc_predictive(y = c(data$y0[i], data$y1[i]), 
                                n = c(data$n0[i], data$n1[i]),
                                direction = direction, p0 = p0, delta = delta, 
                                prior = prior, S = S, N = N, 
                                theta = pp_threshold) 
    } else if(ncol(data) == 2) {
      ppp[i] <- calc_predictive(y = data$y1[i], n = data$n1[i],
                                direction = direction, p0 = p0, delta = delta, 
                                prior = prior, S = S, N = N, 
                                theta = pp_threshold) 
    }
    decision[i] <- ppp[i] < ppp_threshold
    if(decision[i] == TRUE) break
  }
  res0 <- tibble::add_column(
    data[ifelse(any(decision == TRUE), 
                which(decision == TRUE), 
                length(decision)), ],
    pp_threshold = pp_threshold,
    ppp_threshold = ppp_threshold,
    ppp = ppp[ifelse(any(decision == TRUE), 
                     which(decision == TRUE), 
                     length(decision))])
  
  if(ncol(data) == 4) {
    res <- dplyr::mutate(
      res0,
      positive = dplyr::case_when(
        sum(n0, n1) == sum(N) & ppp > pp_threshold ~ TRUE,
        sum(n0, n1) == sum(N) & ppp < pp_threshold ~ FALSE,
        sum(n0, n1) != sum(N) ~ FALSE)
      )
  } else if(ncol(data) == 2) {
    res <- dplyr::mutate(
      res0,
      positive = dplyr::case_when(
        n1 == N & ppp > pp_threshold ~ TRUE,
        n1 == N & ppp < pp_threshold ~ FALSE,
        n1 != N ~ FALSE)
    )
  }
  return(res)
}




#' Calibrate according to posterior probability threshold and predictive 
#' probability threshold with interim futility monitoring
#'
#' @description This function is meant to be used in the context of a 
#' clinical trial with a binary endpoint. For a vector of possible posterior 
#' decision thresholds, the function simulates many trials and then calculates
#' the average number of times the posterior probability exceeds a given 
#' threshold. In a null case, this will result in the type I error at a given
#' threshold. In an alternative case, this will result in the power at a given
#' threshold.
#'
#' @param prob_null vector of length two containing the probability of event in 
#' the standard of care and experimental arm c(p0, p1) for the two-sample case
#' for the null scenario; 
#' integer of event probability for one-sample case
#' @param prob_alt vector of length two containing the probability of event in 
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
#' @param direction "greater" (default) if interest is in p(p1 > p0) and "less" 
#' if interest is in p(p1 < p0) for two-sample case. For one-sample case, 
#' "greater" if interest is in p(p > p0) and "less" if interest is in p(p < p0).
#' @param p0 The target value to compare to in the one-sample case
#' @param delta clinically meaningful difference between groups. 
#' Typically 0 (default).
#' @param prior hyperparameters of prior beta distribution.
#' Beta(0.5, 0.5) is default
#' @param S number of samples drawn from the posterior. Default is 5000
#' @param N the total planned sample size at the end of the trial, c(N0, N1)
#' for two-sample case; integer of total planned sample size at end of trial N
#' for one-sample case
#' @param nsim Number of simulated trial datasets.
#' @param pp_threshold the posterior probability threshold of interest
#' @param ppp_threshold the posterior probability threshold of interest for 
#' futility monitoring
#' 
#' @return A list containing a 
#' 1) a tibble 'res_summary' containing the posterior probability threshold 
#' (pp_threshold), the predictive probability threshold (ppp_threshold), 
#' the ceiling of the mean sample size (mean_n0 and mean_n1 for two-sample case; 
#' mean_n1 for one-sample case), and the proportion of positive trials.
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
#' \dontrun{
#' set.seed(123)
#' 
#' calibrate_thresholds(prob_null = 0.1, prob_alt = 0.3, 
#' n = seq(5, 25, 5), direction = "greater", p0 = 0.1, delta = NULL, 
#' prior = c(0.5, 0.5), S = 5000, N = 25, nsim = 1000, 
#' pp_threshold = c(0.9, 0.95, 0.96, 0.98), 
#' ppp_threshold = seq(0.05, 0.2, 0.05))
#' }
#' 
#' # Two-sample case
#'
#' @export

calibrate_thresholds <- function(prob_null, prob_alt, n,
                                       direction = "greater", p0 = NULL, 
                                       delta = 0, prior = c(0.5, 0.5), 
                                       S = 5000, N, nsim = 1000,
                                       pp_threshold, ppp_threshold) {

  sim_dat_null <- 
    purrr::map(1:nsim, ~sim_dat1(prob = prob_null, n = n))
  
  sim_dat_alt <- 
    purrr::map(1:nsim, ~sim_dat1(prob = prob_alt, n = n))
  
  cross_threshold <- 
    purrr::cross_df(list(pp_threshold = pp_threshold, 
                         ppp_threshold = ppp_threshold))
  
  res_null <- 
    furrr::future_map(
      sim_dat_null,
      function(x) 
        furrr::future_pmap_dfr(cross_threshold,
                        function(pp_threshold, ppp_threshold) 
                          eval_thresh(x, pp_threshold, ppp_threshold,
                                      direction = direction, p0 = p0, 
                                      delta = delta, prior = prior, 
                                      S = S, N = N), 
                        .options = furrr::furrr_options(seed = TRUE)), 
      .options = furrr::furrr_options(seed = TRUE)
      )
  
  res_alt <- 
    furrr::future_map(
      sim_dat_alt,
      function(x) 
        furrr::future_pmap_dfr(cross_threshold,
                               function(pp_threshold, ppp_threshold) 
                                 eval_thresh(x, pp_threshold, ppp_threshold,
                                             direction = direction, p0 = p0, 
                                             delta = delta, prior = prior, 
                                             S = S, N = N), 
                               .options = furrr::furrr_options(seed = TRUE)), 
      .options = furrr::furrr_options(seed = TRUE)
    )
  
  res_df_null <- 
    dplyr::bind_rows(
      res_null,
      .id = "sim_num"
    )
  
  res_df_alt <- 
    dplyr::bind_rows(
      res_alt,
      .id = "sim_num"
    )
  
  
  if(length(prob_null) == 2) {
    res_df <- 
      dplyr::full_join(
        dplyr::select(
          dplyr::rename(res_df_null, n0_null = n0, n1_null = n1, 
                        positive_null = positive),
          -ppp, -y0, -y1),
        dplyr::select(
          dplyr::rename(res_df_alt, n0_alt = n0, n1_alt = n1,
                        positive_alt = positive),
          -ppp, -y0, -y1)
      )
    
    res_summary <- 
      dplyr::ungroup(
        dplyr::summarize(
          dplyr::group_by(res_df, pp_threshold, ppp_threshold),
          mean_n0_null = ceiling(mean(n0_null)),
          mean_n1_null = ceiling(mean(n1_null)),
          prop_pos_null = mean(positive_null),
          prop_stopped_null = mean(sum(n0_null, n1_null) < sum(N)),
          mean_n0_alt = ceiling(mean(n0_alt)),
          mean_n1_alt = ceiling(mean(n1_alt)),
          prop_pos_alt = mean(positive_alt),
          prop_stopped_alt = mean(sum(n0_alt, n1_alt) < sum(N))
          )
        )
  } else if(length(prob_null) == 1) {
    res_df <- 
      dplyr::full_join(
        dplyr::select(
          dplyr::rename(res_df_null, n1_null = n1, positive_null = positive),
          -ppp, -y1),
        dplyr::select(
          dplyr::rename(res_df_alt, n1_alt = n1, positive_alt = positive),
          -ppp, -y1)
      )
    
    res_summary <- 
      dplyr::ungroup(
        dplyr::summarize(
          dplyr::group_by(res_df, pp_threshold, ppp_threshold),
          mean_n1_null = ceiling(mean(n1_null)),
          prop_pos_null = mean(positive_null),
          prop_stopped_null = mean(n1_null < N),
          mean_n1_alt = ceiling(mean(n1_alt)),
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
  calibrate_thresholds_inputs <- list(prob_null = prob_null, 
                                      prob_alt = prob_alt, 
                                      n = n,
                                      direction = direction, 
                                      p0 = p0,
                                      delta = delta, 
                                      prior = prior, 
                                      S = S, 
                                      N = N, 
                                      nsim = nsim,
                                      pp_threshold = pp_threshold, 
                                      ppp_threshold = ppp_threshold)
  
  # create other objects to return
  x$call_list <- list(calibrate_thresholds = match.call())
  x$inputs <- calibrate_thresholds_inputs
  
  # assign a custom class for S3 plotting methods
  class(x) <- c("calibrate_thresholds", class(x))
  
  x
}

# test <- calibrate_thresholds(prob_null = 0.1, prob_alt = 0.3, 
#                      n = seq(5, 25, 5), direction = "greater", p0 = 0.1, delta = NULL, 
#                      prior = c(0.5, 0.5), S = 500, N = 25, nsim = 10, 
#                      pp_threshold = c(0.9, 0.95, 0.96, 0.98), 
#                      ppp_threshold = seq(0.05, 0.2, 0.05))