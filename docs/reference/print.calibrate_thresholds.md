# Print method for `calibrate_thresholds` objects

By default prints only the res_summary table from an object of class
'calibrate_thresholds'. The table can be limited to a range of type 1
error and a minimum value of power using the arguments 'type1_range' and
'minimum_power' respectively.

## Usage

``` r
# S3 method for class 'calibrate_thresholds'
print(x, type1_range = c(0, 1), minimum_power = 0, ...)
```

## Arguments

- x:

  an object of class 'calibrate_thresholds', usually returned by the
  `calibrate_thresholds` function

- type1_range:

  a vector specifying the minimum and maximum acceptable type I error.
  Specify c(0, 1) to return the full range of resulting type I error.
  Defaults to c(0, 1)

- minimum_power:

  a numeric between 0 and 1 specifying the minimum acceptable power.
  Specify 0 to return the full range of resulting power. Defaults to 0.

- ...:

  ignored

## Value

Returns a tibble

## Examples

``` r

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

print(cal_tbl1)
#> # A tibble: 4 × 8
#>   pp_threshold ppp_threshold mean_n1_null prop_pos_null prop_stopped_null
#>          <dbl>         <dbl>        <dbl>         <dbl>             <dbl>
#> 1         0.85           0.1         13.5           0.1               0.2
#> 2         0.85           0.2         10.5           0.1               0.6
#> 3         0.9            0.1         12.5           0                 0.3
#> 4         0.9            0.2         11             0.1               0.5
#>   mean_n1_alt prop_pos_alt prop_stopped_alt
#>         <dbl>        <dbl>            <dbl>
#> 1          15          1                  0
#> 2          15          0.9                0
#> 3          15          0.5                0
#> 4          15          0.6                0
print(cal_tbl1, type1_range = c(0.05, 0.1), minimum_power = 0.9)
#> # A tibble: 2 × 8
#>   pp_threshold ppp_threshold mean_n1_null prop_pos_null prop_stopped_null
#>          <dbl>         <dbl>        <dbl>         <dbl>             <dbl>
#> 1         0.85           0.1         13.5           0.1               0.2
#> 2         0.85           0.2         10.5           0.1               0.6
#>   mean_n1_alt prop_pos_alt prop_stopped_alt
#>         <dbl>        <dbl>            <dbl>
#> 1          15          1                  0
#> 2          15          0.9                0
```
