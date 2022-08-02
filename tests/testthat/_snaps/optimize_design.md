# optimize_design works for one-sample case

    Code
      lapply(optimize_design(cal_tbl), as.data.frame)
    Output
      $`Optimal accuracy design:`
        pp_threshold ppp_threshold Type I error     Power Average N under the null
      1          0.9          0.05   0.08333333 0.9033333                       25
        Average N under the alternative
      1                              25
      
      $`Optimal efficiency design:`
        pp_threshold ppp_threshold Type I error     Power Average N under the null
      1          0.9          0.05   0.08333333 0.9033333                       25
        Average N under the alternative
      1                              25
      

# optimize_design works for two-sample case

    Code
      lapply(optimize_design(cal_tbl), as.data.frame)
    Output
      $`Optimal accuracy design:`
        pp_threshold ppp_threshold Type I error Power Average N under the null
      1          0.9           0.2         0.06  0.93                    14.95
        Average N under the alternative
      1                           24.25
      
      $`Optimal efficiency design:`
        pp_threshold ppp_threshold Type I error Power Average N under the null
      1          0.9           0.2         0.06  0.93                    14.95
        Average N under the alternative
      1                           24.25
      

