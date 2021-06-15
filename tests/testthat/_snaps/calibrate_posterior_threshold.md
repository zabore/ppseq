# calibrate_posterior_threshold works

    Code
      calibrate_posterior_threshold(prob = c(0.1, 0.1), N = c(50, 50), direction = "greater",
      p0 = NULL, delta = 0, prior = c(0.5, 0.5), S = 50, theta = c(0.9, 0.95))
    Output
      # A tibble: 2 x 2
        pp_threshold prop_pos
               <dbl>    <dbl>
      1         0.9      0.14
      2         0.95     0.06

