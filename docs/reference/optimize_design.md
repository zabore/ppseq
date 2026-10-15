# Function to setup usage of `optimize_design.calibrate_thresholds`

Function to setup usage of `optimize_design.calibrate_thresholds`

## Usage

``` r
optimize_design(x, type1_range = c(0, 1), minimum_power = 0, ...)
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

- ...:

  ignored
