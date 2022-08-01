## code to prepare `one_sample_cal_tbl` dataset goes here
# Note that this was actually run on the server using file H:/ppseq-papers/one_sample_cal_tbl-server.R
library(ppseq)
library(future)

set.seed(123)

future::plan(future::multicore(workers = 56))

one_sample_cal_tbl <- 
  calibrate_thresholds(p_null = 0.1, 
                       p_alt = 0.2, 
                       n = seq(5, 95, 5),
                       N = 95, 
                       pp_threshold = c(0.7, 0.74, 0.78, 0.82, 0.86, 0.9, 
                                        0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 
                                        0.98, 0.99),
                       ppp_threshold = seq(0.05, 0.2, 0.05),
                       direction = "greater", 
                       delta = NULL, 
                       prior = c(0.5, 0.5), 
                       S = 5000, 
                       nsim = 1000
  )

usethis::use_data(one_sample_cal_tbl, overwrite = TRUE)