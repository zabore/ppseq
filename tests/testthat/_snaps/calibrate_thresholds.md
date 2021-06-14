# one-sample simulate data

    Code
      sim_dat1(prob = 0.1, n = c(5, 10))
    Output
      # A tibble: 2 x 2
           n1    y1
        <dbl> <int>
      1     5     0
      2    10     1

# two-sample simulate data

    Code
      sim_dat1(prob = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)))
    Output
      # A tibble: 2 x 4
           n0    n1    y0    y1
        <dbl> <dbl> <int> <int>
      1     5     5     0     2
      2    10    10     0     5

# evaluate threshold one-sample case

    Code
      eval_thresh(dat1, 0.95, 0.3, p0 = 0.1, delta = NULL, S = 500, N = 25)
    Output
      # A tibble: 1 x 6
           n1    y1 pp_threshold ppp_threshold   ppp positive
        <dbl> <int>        <dbl>         <dbl> <dbl> <lgl>   
      1     5     0         0.95           0.3 0.094 FALSE   

# evaluate threshold two-sample case

    Code
      eval_thresh(dat2, 0.95, 0.3, S = 500, N = c(25, 25))
    Output
      # A tibble: 1 x 8
           n0    n1    y0    y1 pp_threshold ppp_threshold   ppp positive
        <dbl> <dbl> <int> <int>        <dbl>         <dbl> <dbl> <lgl>   
      1    10    10     0     5         0.95           0.3 0.988 FALSE   

# single-sample calibrate thresholds

    Code
      calibrate_thresholds(prob_null = 0.01, prob_alt = 0.9, n = c(5, 25), direction = "greater",
      p0 = 0.1, delta = NULL, prior = c(0.5, 0.5), S = 200, N = 100, nsim = 100,
      pp_threshold = c(0.98), ppp_threshold = c(0.01))
    Message <message>
      Joining, by = c("sim_num", "pp_threshold", "ppp_threshold")
    Output
      # A tibble: 0 x 8
      # ... with 8 variables: pp_threshold <dbl>, ppp_threshold <dbl>,
      #   mean_n1_null <dbl>, prop_pos_null <dbl>, prop_stopped_null <dbl>,
      #   mean_n1_alt <dbl>, prop_pos_alt <dbl>, prop_stopped_alt <dbl>

