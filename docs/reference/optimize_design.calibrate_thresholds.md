# Custom optimization method for `calibrate_thresholds` objects

Determines the optimal designs based on a variety of criteria. The
optimal efficiency design is the one with the shortest Euclidean
distance to the upper left point on a plot of the average sample size
under the null by the average sample size under the alternative. The
optimal accuracy design is the one with the shortest Euclidean distance
to the upper left point on a plot of the type I error by the power.

## Usage

``` r
# S3 method for class 'calibrate_thresholds'
optimize_design(
  x,
  type1_range = c(0, 1),
  minimum_power = 0,
  w_type1 = 1,
  w_power = 1,
  w_Nnull = 1,
  w_Nalt = 1,
  ...
)
```

## Arguments

- x:

  an object of class 'calibrate_thresholds', usually returned by the
  `calibrate_thresholds` function

- type1_range:

  a vector specifying the minimum and maximum acceptable type I error.
  Specify NULL to return the full range of resulting type I error.
  Defaults to c(0, 1) to return all results.

- minimum_power:

  a numeric between 0 and 1 specifying the minimum acceptable power.
  Specify NULL to return the full range of resulting power. Defaults to
  0 to return all results.

- w_type1:

  a user-specified weight on the type 1 error. Defaults to 1 for no
  weighting.

- w_power:

  a user-specified weight on the power. Defaults to 1 for no weighting.

- w_Nnull:

  a user-specified weight on the average sample size under the null.
  Defaults to 1 for no weighting.

- w_Nalt:

  a user-specified weight on the average sample size under the
  alternative. Defaults to 1 for no weighting.

- ...:

  ignored

## Value

A list of length two containing details of the optimal efficiency and
optimal accuracy designs

## Examples

``` r

# Setting S = 50 and nsim = 50 for speed
# In practice you would want a much larger sample and more simulations

# One-sample case
set.seed(123)

cal_tbl1 <- calibrate_thresholds(
  p_null = 0.1, 
  p_alt = 0.4,
  n = seq(5, 15, 5), 
  N = 15,
  pp_threshold = c(0.85, 0.9),
  ppp_threshold = c(0.1, 0.2),
  S = 10, 
  nsim = 10
  )

optimize_design(cal_tbl1)
#> $`Optimal accuracy design:`
#> # A tibble: 1 × 6
#>   pp_threshold ppp_threshold `Type I error` Power `Average N under the null`
#>          <dbl>         <dbl>          <dbl> <dbl>                      <dbl>
#> 1         0.85           0.1            0.1     1                       13.5
#>   `Average N under the alternative`
#>                               <dbl>
#> 1                                15
#> 
#> $`Optimal efficiency design:`
#> # A tibble: 1 × 6
#>   pp_threshold ppp_threshold `Type I error` Power `Average N under the null`
#>          <dbl>         <dbl>          <dbl> <dbl>                      <dbl>
#> 1         0.85           0.2            0.1   0.9                       10.5
#>   `Average N under the alternative`
#>                               <dbl>
#> 1                                15
#> 


# Two-sample case
set.seed(456)

cal_tbl2 <- calibrate_thresholds(
  p_null = c(0.1, 0.1), 
  p_alt = c(0.1, 0.5),
  n = cbind(seq(5, 15, 5), seq(5, 15, 5)), 
  N = c(15, 15),
  pp_threshold = c(0.8, 0.85),
  ppp_threshold = c(0.2, 0.3),
  delta = 0,
  S = 10, 
  nsim = 10
  )

optimize_design(cal_tbl2)
#> $`Optimal accuracy design:`
#> # A tibble: 1 × 6
#>   pp_threshold ppp_threshold `Type I error` Power `Average N under the null`
#>          <dbl>         <dbl>          <dbl> <dbl>                      <dbl>
#> 1         0.85           0.3              0     1                          8
#>   `Average N under the alternative`
#>                               <dbl>
#> 1                                15
#> 
#> $`Optimal efficiency design:`
#> # A tibble: 1 × 6
#>   pp_threshold ppp_threshold `Type I error` Power `Average N under the null`
#>          <dbl>         <dbl>          <dbl> <dbl>                      <dbl>
#> 1          0.8           0.3              0     1                        7.5
#>   `Average N under the alternative`
#>                               <dbl>
#> 1                                15
#> 
```
