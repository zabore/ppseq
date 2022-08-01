## code to prepare `one_sample_decision_tbl` dataset goes here
# Note that this was actually run on the server using file H:/ppseq-papers/one_sample_decision_tbl-server.R
library(ppseq)

set.seed(123)

one_sample_decision_tbl <- 
  calc_decision_rules(
    n = seq(5, 95, 5),
    N = 95,
    theta = 0.92, 
    ppp = 0.1,
    p0 = 0.1,
    direction = "greater",
    delta = NULL,
    prior = c(0.5, 0.5),
    S = 5000)

usethis::use_data(one_sample_decision_tbl, overwrite = TRUE)
