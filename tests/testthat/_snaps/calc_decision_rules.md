# one-sample calc_decision_rules works

    # A tibble: 2 x 3
          n     r   ppp
      <dbl> <int> <dbl>
    1    10     0   0  
    2    20     2   0.1

# two-sample calc_decision_rules works

    # A tibble: 32 x 5
          n0    n1    r0    r1   ppp
       <dbl> <dbl> <int> <int> <dbl>
     1    10    10     0    NA  NA  
     2    10    10     1     1   0.2
     3    10    10     2     1   0  
     4    10    10     3     2   0  
     5    10    10     4     4   0.2
     6    10    10     5     4   0  
     7    10    10     6     6   0.2
     8    10    10     7     8   0.2
     9    10    10     8     8   0.1
    10    10    10     9    10   0.2
    # ... with 22 more rows

