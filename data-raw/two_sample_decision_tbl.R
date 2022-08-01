## code to prepare `two_sample_decision_tbl` dataset goes here
# Note that this was actually run on the server using file H:/ppseq-papers/two_sample_decision_tbl-server.R
library(ppseq)

set.seed(123)

two_sample_decision_tbl <- 
  calc_decision_rules(
    n = cbind(seq(10, 50, 10), seq(10, 50, 10)),
    N = c(50, 50),
    theta = 0.92, 
    ppp = 0.05, 
    p0 = NULL, 
    direction = "greater", 
    delta = 0, 
    prior = c(0.5, 0.5), 
    S = 5000
  )

usethis::use_data(two_sample_decision_tbl, overwrite = TRUE)
