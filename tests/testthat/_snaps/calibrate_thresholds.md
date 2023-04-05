# one-sample simulate data

    Code
      sim_dat1(p = 0.1, n = c(5, 10))
    Output
      # A tibble: 2 x 2
           n1    y1
        <dbl> <int>
      1     5     0
      2    10     1

# two-sample simulate data

    Code
      sim_dat1(p = c(0.1, 0.3), n = cbind(c(5, 10), c(5, 10)))
    Output
      # A tibble: 2 x 4
           n0    n1    y0    y1
        <dbl> <dbl> <int> <int>
      1     5     5     0     2
      2    10    10     0     5

# two-sample vector argument to n

    Code
      sim_dat1(p = c(0.1, 0.3), n = c(5, 5))
    Output
      # A tibble: 1 x 4
           n0    n1    y0    y1
        <dbl> <dbl> <int> <int>
      1     5     5     0     2

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
      eval_thresh(dat2, 0.95, 0.3, p0 = NULL, delta = 0, S = 500, N = c(25, 25))
    Output
      # A tibble: 1 x 8
           n0    n1    y0    y1 pp_threshold ppp_threshold   ppp positive
        <dbl> <dbl> <int> <int>        <dbl>         <dbl> <dbl> <lgl>   
      1    10    10     0     5         0.95           0.3 0.988 TRUE    

# one-sample calibrate thresholds

      pp_threshold ppp_threshold mean_n1_null prop_pos_null prop_stopped_null
    1          0.9          0.05           25          0.05                 0
      mean_n1_alt prop_pos_alt prop_stopped_alt
    1          25         0.91                0

# two-sample calibrate thresholds

      pp_threshold ppp_threshold mean_n0_null mean_n1_null prop_pos_null
    1          0.9           0.2        15.25        15.25          0.07
      prop_stopped_null mean_n0_alt mean_n1_alt prop_pos_alt prop_stopped_alt
    1              0.65       23.95       23.95         0.88             0.07

