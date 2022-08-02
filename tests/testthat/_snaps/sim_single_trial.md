# one-sample single trial simulation

    Code
      sim_single_trial(p = 0.3, n = c(5, 10), direction = "greater", p0 = 0.1, delta = NULL,
      prior = c(0.5, 0.5), S = 5000, N = 25, theta = 0.95)
    Output
      # A tibble: 2 x 5
        pp_threshold    y1    n1    pp   ppp
               <dbl> <int> <dbl> <dbl> <dbl>
      1         0.95     1     5 0.803 0.474
      2         0.95     3    10 0.966 0.800

# two-sample single trial simulation

    Code
      sim_single_trial(p = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)), direction = "greater",
      p0 = NULL, delta = 0, prior = c(0.5, 0.5), S = 5000, N = c(50, 50), theta = 0.95)
    Output
      # A tibble: 2 x 7
        pp_threshold    y0    y1    n0    n1    pp   ppp
               <dbl> <int> <int> <dbl> <dbl> <dbl> <dbl>
      1         0.95     0     2     5     5 0.944 0.872
      2         0.95     0     5    10    10 0.997 0.990

