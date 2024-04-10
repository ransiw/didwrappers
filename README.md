
<!-- README.md is generated from README.Rmd. Please edit that file -->

# didwrappers

<!-- badges: start -->
<!-- badges: end -->

The goal of didwrappers is to …

## Installation

You can install the development version of didwrappers:

``` r
# install.packages("devtools")
devtools::install_github("ransiw/didwrappers")
```

## Example

This is a basic example generating code with a sample dataset:

``` r
library(didwrappers)
```

Simulate sample data

``` r
simdata = sim_data()
head(simdata)
#>   unit treatg time posttreat baselevel tef cohort dosage      error timelag
#> 1    1     10    5         0        10  10      1      1  1.7043771      -5
#> 2    1     10    6         0        10  10      1      1  0.4750064      -4
#> 3    1     10    7         0        10  10      1      1  1.0733082      -3
#> 4    1     10    8         0        10  10      1      1 -2.0954244      -2
#> 5    1     10    9         0        10  10      1      1 -1.1245552      -1
#> 6    1     10   10         1        10  10      1      1 -1.3535047       0
#>           y
#> 1  1.704377
#> 2  2.475006
#> 3  5.073308
#> 4  3.904576
#> 5  6.875445
#> 6 18.646495
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual      lowci     highci
#> 1  1    10  6 -1.4435256 0.4224357 -1.4435256 0.04761905 -2.6365538 -0.2504975
#> 2  1    10  7  0.9612383 0.4117199  0.9612383 0.04761905 -0.2015267  2.1240033
#> 3  1    10  8 -3.3431486 0.3530095 -3.3431486 0.04761905 -4.3401057 -2.3461916
#> 4  1    10  9  0.9611202 0.3034882  0.9611202 0.04761905  0.1040194  1.8182209
#> 5  1    10 10  9.9380494 0.3514452  9.9380494 0.04761905  8.9455102 10.9305887
#> 6  1    10 11 10.2116630 0.3796539 10.2116630 0.04761905  9.1394575 11.2838684
```
