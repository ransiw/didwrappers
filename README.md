
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
head(simdata[,c("unit","time","treatg","dosage","y")])
#>   unit time treatg dosage          y
#> 1    1    5     10      1  1.4035594
#> 2    1    6     10      1  0.8609869
#> 3    1    7     10      1  4.0744607
#> 4    1    8     10      1  6.7694108
#> 5    1    9     10      1 10.0876894
#> 6    1   10     10      1 19.4150575
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual       lowci    highci
#> 1  1    10  6 -2.4682879 0.3955920 -2.4682879 0.04761905 -3.61528915 -1.321287
#> 2  1    10  7  1.4413852 0.3920755  1.4413852 0.04761905  0.30457972  2.578191
#> 3  1    10  8  0.9383865 0.3411411  0.9383865 0.04761905 -0.05073669  1.927510
#> 4  1    10  9  0.8583446 0.2748031  0.8583446 0.04761905  0.06156522  1.655124
#> 5  1    10 10  7.7116513 0.3097980  7.7116513 0.04761905  6.81340586  8.609897
#> 6  1    10 11  8.2764011 0.3651166  8.2764011 0.04761905  7.21776191  9.335040
```
