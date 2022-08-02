# one-sample calc_decision_rules works

    Code
      calc_decision_rules(n = c(10, 20), N = 25, theta = 0.86, ppp = 0.2, p0 = 0.1,
      direction = "greater", delta = NULL, prior = c(0.5, 0.5), S = 100)
    Output
      # A tibble: 2 x 3
            n     r   ppp
        <dbl> <int> <dbl>
      1    10     0  0.02
      2    20     2  0.06

# two-sample calc_decision_rules works

    Code
      calc_decision_rules(n = cbind(c(10, 20), c(10, 20)), N = c(25, 25), theta = 0.86,
      ppp = 0.2, p0 = NULL, direction = "greater", delta = 0, prior = c(0.5, 0.5), S = 100)
    Output
      # A tibble: 32 x 5
            n0    n1    r0    r1   ppp
         <dbl> <dbl> <int> <int> <dbl>
       1    10    10     0     0  0.14
       2    10    10     1     1  0.08
       3    10    10     2     2  0.13
       4    10    10     3     3  0.19
       5    10    10     4     4  0.15
       6    10    10     5     5  0.16
       7    10    10     6     5  0.07
       8    10    10     7     7  0.19
       9    10    10     8     8  0.15
      10    10    10     9     9  0.18
      # ... with 22 more rows

