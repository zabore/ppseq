## code to prepare `one_sample_cal_tbl` dataset goes here

# Note that this takes a long time to run and needs to be run on the server

library(ppseq)
library(tibble)
library(dplyr)
library(purrr)
library(furrr)

eval_thresh <- function(data, pp_threshold, ppp_threshold, p0, N, 
                        direction = "greater", delta = NULL,
                        prior = c(0.5, 0.5), S = 5000) {
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
    decision[i] <- ppp[i] < ppp_threshold
    if (decision[i] == TRUE) break
  }
  res0 <- add_column(
    data[ifelse(any(decision == TRUE),
                which(decision == TRUE),
                length(decision)
    ), ],
    pp_threshold = pp_threshold,
    ppp_threshold = ppp_threshold,
    ppp = ppp[ifelse(any(decision == TRUE),
                     which(decision == TRUE),
                     length(decision)
    )]
  )
  
  if (ncol(data) == 4) {
    res <- mutate(
      res0,
      positive = case_when(
        sum(n0, n1) == sum(N) & ppp > pp_threshold ~ TRUE,
        sum(n0, n1) == sum(N) & ppp <= pp_threshold ~ FALSE,
        sum(n0, n1) != sum(N) ~ FALSE
      )
    )
  } else if (ncol(data) == 2) {
    res <- mutate(
      res0,
      positive = case_when(
        n1 == N & ppp > pp_threshold ~ TRUE,
        n1 == N & ppp <= pp_threshold ~ FALSE,
        n1 != N ~ FALSE
      )
    )
  }
  return(res)
}


# Need a special function to simulate at every single value up to N so that I can get the protocol design interim look at n=14
sim_dat_all <- function(p, N) {
  y1 <- stats::rbinom(n = 1, size = 1, prob = p) 
  for(i in 2:N) { 
    y1 <- c(y1,  y1[length(y1)] + 
              stats::rbinom(n = 1, size = 1, prob = p))
  }
  return(tibble::tibble(n1 = seq(1:N), y1 = y1))
}


# This function does one futility look after the first 14 patients and then evaluates at the end
# This is based on a 20% alternative response rate - it wasn't actually clear to me if they were hypothesizing 20% or 30%
eval_protocol <- function(data) {
  data2 <- filter(data, n1 %in% c(14, 95))
  
  if(data2$y1[1] == 0) {
    res <- add_column(
      data2[1, ],
      positive = FALSE
    ) 
  } else if(data2$y1[1] >= 1) {
    res <- add_column(
      data2[2, ],
      positive = ifelse(data2$y1[2] >= 19, TRUE, FALSE)
    )
  }
}


# This setup works on the server and on lri-r0x and uses 76 cores
no_cores <- min(parallelly::availableCores() - 1, 76)
future::plan(future::multicore(workers = no_cores))

set.seed(123)

p_null <- 0.1
p_alt <- 0.2
n <- seq(5, 95, 5)
N <- 95 
pp_threshold <- c(0, 0.7, 0.74, 0.78, 0.82, 0.86, 
                  0.9, 0.92, 0.93, 0.94, 0.95,
                  0.96, 0.97, 0.98, 0.99, 0.999, 
                  0.9999, 0.99999, 1)
ppp_threshold <- seq(0.05, 0.2, 0.05)
direction <- "greater"
delta <- NULL
prior <- c(0.5, 0.5)
S <- 5000
nsim <- 1000


sim_dat_null <- 
  purrr::map(1:nsim, ~sim_dat_all(p = p_null, N = N))

sim_dat_alt <- 
  purrr::map(1:nsim, ~sim_dat_all(p = p_alt, N = N))

cross_threshold <-
  cross_df(list(
    pp_threshold = pp_threshold,
    ppp_threshold = ppp_threshold
  ))

p0 <- if(length(p_null) == 1) 
  p_null else if(length(p_null) == 2)
    NULL

# limit the data to the times when we do interim looks
look_dat_null <- 
  purrr::map(
    sim_dat_null,
    ~dplyr::filter(
      .x, n1 %in% n
    )
  )

look_dat_alt <- 
  purrr::map(
    sim_dat_alt,
    ~dplyr::filter(
      .x, n1 %in% n
    )
  )

res_null <- 
  future_map(
    look_dat_null,
    function(x) 
      pmap_dfr(cross_threshold,
               function(pp_threshold, ppp_threshold) 
                 eval_thresh(x, pp_threshold, ppp_threshold,
                             direction = direction, p0 = p0, 
                             delta = delta, prior = prior, 
                             S = S, N = N)), 
    .options = furrr_options(seed = TRUE)
  )

res_alt <- 
  future_map(
    look_dat_alt,
    function(x) 
      pmap_dfr(cross_threshold,
               function(pp_threshold, ppp_threshold) 
                 eval_thresh(x, pp_threshold, ppp_threshold,
                             direction = direction, p0 = p0, 
                             delta = delta, prior = prior, 
                             S = S, N = N)),
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

protocol_null <- 
  furrr::future_map_dfr(
    sim_dat_null, eval_protocol, 
    .options = furrr::furrr_options(seed = TRUE)
  )

protocol_alt <- 
  furrr::future_map_dfr(
    sim_dat_alt, eval_protocol, 
    .options = furrr::furrr_options(seed = TRUE)
  )

protocol_summary <- 
  dplyr::summarize(
    dplyr::bind_cols(
      dplyr::select(
        dplyr::rename(protocol_null, n1_null = n1, positive_null = positive),
        -y1
      ),
      dplyr::select(
        dplyr::rename(protocol_alt, n1_alt = n1, positive_alt = positive),
        -y1
      )
    ),
    mean_n1_null = mean(n1_null),
    prop_pos_null = mean(positive_null),
    prop_stopped_null = mean(n1_null < N),
    mean_n1_alt = mean(n1_alt),
    prop_pos_alt = mean(positive_alt),
    prop_stopped_alt = mean(n1_alt < N)
  )

# create empty list to return everything
x <- list()

# add the main results in
x$res_summary <- res_summary

# will return call, and all object passed to in table1 call
# the object func_inputs is a list of every object passed to the function
calibrate_thresholds_inputs <- list(p_null = p_null, 
                                    p_alt = p_alt, 
                                    n = n,
                                    N = N, 
                                    pp_threshold = pp_threshold, 
                                    ppp_threshold = ppp_threshold,
                                    direction = direction, 
                                    delta = delta, 
                                    prior = prior, 
                                    S = S, 
                                    nsim = nsim
)

# create other objects to return
x$call_list <- list(calibrate_thresholds = match.call())
x$inputs <- calibrate_thresholds_inputs

# add the Simon results to the list
x$protocol_res <- protocol_summary

# assign a custom class for S3 plotting methods
class(x) <- c("calibrate_thresholds", class(x))

one_sample_cal_tbl <- x

usethis::use_data(one_sample_cal_tbl, overwrite = TRUE)
