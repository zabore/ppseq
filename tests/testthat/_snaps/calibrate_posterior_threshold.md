# calibrate_posterior_threshold works for two-sample case

    Code
      calibrate_posterior_threshold(p = c(0.1, 0.1), N = c(50, 50), direction = "greater",
      p0 = NULL, delta = 0, prior = c(0.5, 0.5), S = 50, theta = c(0.9, 0.95))
    Output
      # A tibble: 2 x 2
        pp_threshold prop_pos
               <dbl>    <dbl>
      1         0.9      0.14
      2         0.95     0.06

# calibrate_posterior_threshold works for one-sample case

    Code
      calibrate_posterior_threshold(p = 0.1, N = 50, direction = "greater", p0 = 0.1,
        delta = NULL, prior = c(0.5, 0.5), S = 50, theta = c(0.9, 0.95))
    Output
      # A tibble: 2 x 2
        pp_threshold prop_pos
               <dbl>    <dbl>
      1         0.9      0.18
      2         0.95     0.1 

