## code to prepare `two_sample_cal_tbl` dataset goes here
# Note that this was actually run on the server using file H:/ppseq-papers/two_sample_cal_tbl-server.R
library(ppseq)
library(future)

set.seed(123)

future::plan(future::multicore(workers = 40))

two_sample_cal_tbl <- 
  calibrate_thresholds(p_null = c(0.1, 0.1), 
                       p_alt = c(0.1, 0.25), 
                       n = cbind(seq(10, 50, 10), seq(10, 50, 10)),
                       N = c(50, 50), 
                       pp_threshold = seq(0.9, 0.99, 0.01),
                       ppp_threshold = seq(0.05, 0.2, 0.05),
                       direction = "greater", 
                       delta = 0, 
                       prior = c(0.5, 0.5), 
                       S = 5000, 
                       nsim = 1000
  )

usethis::use_data(two_sample_cal_tbl, overwrite = TRUE)