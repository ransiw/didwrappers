
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
#>   unit time treatg dosage         y
#> 1    1    5     10      1 -1.047722
#> 2    1    6     10      1  3.174111
#> 3    1    7     10      1  2.990559
#> 4    1    8     10      1  5.156149
#> 5    1    9     10      1  8.133341
#> 6    1   10     10      1 20.834095
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual      lowci    highci
#> 1  1    10  6  2.1946361 0.3686003  2.1946361 0.04761905  1.1486674  3.240605
#> 2  1    10  7 -2.3287000 0.3480624 -2.3287000 0.04761905 -3.3163887 -1.341011
#> 3  1    10  8  0.3634381 0.2712660  0.3634381 0.04761905 -0.4063272  1.133203
#> 4  1    10  9  0.9341750 0.2452204  0.9341750 0.04761905  0.2383187  1.630031
#> 5  1    10 10 10.5462608 0.2976297 10.5462608 0.04761905  9.7016840 11.390838
#> 6  1    10 11 10.1023958 0.3126995 10.1023958 0.04761905  9.2150555 10.989736
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.830435 0.1644191  9.495115 10.165756
#> 2    2  9.790290 0.2140320  9.353787 10.226793
#> 3    3  8.029329 0.1659425  7.690901  8.367757
#> 4    4  9.952411 0.2325503  9.478142 10.426681
#> 5    5 10.108504 0.1668766  9.768171 10.448837
#> 6    6  9.316662 0.2381747  8.830922  9.802402
#> 7    7 11.477014 0.1650254 11.140457 11.813572
#> 8    8  9.635968 0.2231480  9.180874 10.091062
#> 9    9 10.610166 0.1710056 10.261413 10.958920
#> 10  10  9.939365 0.2222042  9.486196 10.392534
```

Now sample a data frame with different dosage amounts and aggregate.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.949242 0.1987928  9.517417 10.381066
#> 2    2 11.415520 0.2000603 10.980943 11.850098
#> 3    3  9.490302 0.1881193  9.081663  9.898941
#> 4    4 11.536559 0.2017068 11.098405 11.974713
#> 5    5  9.607730 0.2057932  9.160700 10.054761
#> 6    6 19.904204 0.1940093 19.482770 20.325637
#> 7    7 19.021106 0.1987913 18.589284 19.452927
#> 8    8 19.107260 0.1976409 18.677938 19.536582
#> 9    9 19.044130 0.1976225 18.614847 19.473412
#> 10  10 22.098280 0.1922375 21.680695 22.515864
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt  att.egt    se.egt     lowci   highci
#> 1   1 10.28659 0.1703442  9.909286 10.66389
#> 2   2 19.74584 0.1575463 19.396885 20.09480
```
