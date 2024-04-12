
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
#> 1    1    5     10      1 -1.454967
#> 2    1    6     10      1  2.852471
#> 3    1    7     10      1  5.171669
#> 4    1    8     10      1  5.616401
#> 5    1    9     10      1  8.637986
#> 6    1   10     10      1 19.524796
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual       lowci     highci
#> 1  1    10  6  2.1839649 0.3436451  2.1839649 0.04761905  1.22058102  3.1473488
#> 2  1    10  7  0.4163615 0.4096696  0.4163615 0.04761905 -0.73211713  1.5648402
#> 3  1    10  8 -1.6594731 0.3650681 -1.6594731 0.04761905 -2.68291478 -0.6360315
#> 4  1    10  9  0.9588549 0.3727238  0.9588549 0.04761905 -0.08604892  2.0037588
#> 5  1    10 10  8.7128868 0.4392124  8.7128868 0.04761905  7.48158706  9.9441864
#> 6  1    10 11  7.0500808 0.2771826  7.0500808 0.04761905  6.27301972  7.8271419
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.611774 0.2341740  9.110378 10.113169
#> 2    2  9.166513 0.2513453  8.628351  9.704674
#> 3    3  7.631088 0.2409461  7.115193  8.146984
#> 4    4  9.637280 0.2651796  9.069497 10.205062
#> 5    5  8.463799 0.2444660  7.940367  8.987231
#> 6    6  8.860383 0.2329826  8.361538  9.359228
#> 7    7 11.481223 0.2384158 10.970745 11.991701
#> 8    8 10.328402 0.2433762  9.807303 10.849500
#> 9    9  8.520945 0.2463037  7.993578  9.048312
#> 10  10 11.692588 0.2587484 11.138575 12.246600
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  8.970797 0.2355177  8.436465  9.505129
#> 2    2 11.193846 0.2519456 10.622243 11.765448
#> 3    3 10.131051 0.2446061  9.576100 10.686002
#> 4    4 11.378626 0.2495497 10.812459 11.944793
#> 5    5 13.671997 0.2447711 13.116672 14.227323
#> 6    6 20.768162 0.2432417 20.216306 21.320018
#> 7    7 22.725881 0.2435312 22.173368 23.278393
#> 8    8 21.232717 0.2561457 20.651586 21.813849
#> 9    9 20.777605 0.2319255 20.251424 21.303787
#> 10  10 20.705576 0.2531611 20.131216 21.279936
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt  att.egt    se.egt    lowci   highci
#> 1   1 11.04642 0.1924713 10.66836 11.42449
#> 2   2 21.29863 0.1756917 20.95352 21.64374
```

The functions can also accommodate unbalancedness. We can impose
unbalancedness in simulated data. For example we could drop the first
unit before time period 11, which essentially eliminates the unit.

``` r
simdata = sim_data(tef=seq(1:10))
#simdata = simdata[!(simdata[,"unit"]==1 & simdata[,"time"]<11),]
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", panel = FALSE, data = simdata)
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
aggite_table(agtobject)
#>    egt    att.egt    se.egt     lowci    highci
#> 1    1 -0.3186647 0.2281154 -0.790232 0.1529027
#> 2    2  2.7453266 0.1842124  2.364517 3.1261364
#> 3    3  3.3001868 0.2328957  2.818737 3.7816361
#> 4    4  2.3994788 0.1799583  2.027463 2.7714944
#> 5    5  5.9280854 0.2266611  5.459524 6.3966464
#> 6    6  5.5566070 0.1676536  5.210028 5.9031860
#> 7    7  8.8051166 0.2256340  8.338679 9.2715544
#> 8    8  7.2908507 0.1831900  6.912154 7.6695469
#> 9    9  7.3485065 0.2065991  6.921418 7.7755947
#> 10  10  8.4884883 0.1785840  8.119314 8.8576627
```
