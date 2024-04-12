
<!-- README.md is generated from README.Rmd. Please edit that file -->

# didwrappers

<!-- badges: start -->
<!-- badges: end -->

The goal of didwrappers is to extend the functionality of the `did`
package, specifically for within treatment-group heterogeneity and
unbalanced data.

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
#>   unit time treatg dosage           y
#> 1    1    5     10      1  0.09704378
#> 2    1    6     10      1  0.72157023
#> 3    1    7     10      1  2.36605744
#> 4    1    8     10      1  7.31399137
#> 5    1    9     10      1  8.38592896
#> 6    1   10     10      1 20.11399648
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual     lowci      highci
#> 1  1    10  6 -1.0475233 0.3786252 -1.0475233 0.04761905 -2.109536  0.01448905
#> 2  1    10  7 -0.1166569 0.3295000 -0.1166569 0.04761905 -1.040877  0.80756318
#> 3  1    10  8  2.9354433 0.3025107  2.9354433 0.04761905  2.086926  3.78396089
#> 4  1    10  9 -0.7982317 0.3794871 -0.7982317 0.04761905 -1.862661  0.26619815
#> 5  1    10 10  9.3294149 0.2895122  9.3294149 0.04761905  8.517357 10.14147265
#> 6  1    10 11  9.0135349 0.2734146  9.0135349 0.04761905  8.246630  9.78044008
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.464130 0.2098869  9.038278  9.889981
#> 2    2 12.444767 0.2485189 11.940532 12.949001
#> 3    3  8.789862 0.2086015  8.366618  9.213105
#> 4    4  9.756398 0.2421116  9.265164 10.247632
#> 5    5  9.346977 0.2069996  8.926984  9.766970
#> 6    6 10.984647 0.2379694 10.501817 11.467477
#> 7    7 10.688954 0.2129108 10.256967 11.120941
#> 8    8  6.719797 0.2712157  6.169512  7.270082
#> 9    9 10.180374 0.2190712  9.735888 10.624860
#> 10  10 11.084473 0.2227763 10.632469 11.536476
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  8.524712 0.2362414  8.042604  9.006820
#> 2    2  7.569096 0.2498395  7.059238  8.078955
#> 3    3  9.418714 0.2219103  8.965852  9.871576
#> 4    4 10.083412 0.2324952  9.608949 10.557875
#> 5    5 10.367696 0.2350237  9.888072 10.847319
#> 6    6 19.193504 0.2407498 18.702195 19.684812
#> 7    7 18.776229 0.2392687 18.287943 19.264515
#> 8    8 18.408247 0.2501084 17.897840 18.918655
#> 9    9 20.526837 0.2249389 20.067794 20.985879
#> 10  10 18.722036 0.2398608 18.232541 19.211530
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt   att.egt    se.egt     lowci    highci
#> 1   1  9.231302 0.1862187  8.865107  9.597497
#> 2   2 19.183833 0.1757598 18.838205 19.529461
```

The functions can also accommodate unbalancedness. We can impose
unbalancedness in simulated data. For example we could drop the first
unit before time period 11, which essentially eliminates the unit.

``` r
simdata = sim_data()
simdata = simdata[!(simdata[,"unit"]==1 & simdata[,"time"]<11),]
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", panel = FALSE, data = simdata)
#> Warning in pre_process_did_i(yname = yname, tname = tname, idname = idname, : Be aware that there are some small groups in your dataset.
#>   Check groups: 10.
```

``` r
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
aggite_table(agtobject)
#>   egt   att.egt    se.egt     lowci   highci
#> 1   2 10.603792 0.2322058 10.110941 11.09664
#> 2   3 10.360666 0.2593564  9.810188 10.91114
#> 3   4 10.554211 0.2356693 10.054009 11.05441
#> 4   5 10.502988 0.2615913  9.947767 11.05821
#> 5   6  9.381730 0.2352910  8.882331  9.88113
#> 6   7 10.836976 0.2770554 10.248933 11.42502
#> 7   8  9.636501 0.2357263  9.136178 10.13682
#> 8   9 11.518715 0.2705198 10.944543 12.09289
#> 9  10  9.902476 0.2301457  9.413997 10.39095
```

We could also consider the effect of truncation of the outcome variable
at 100. Truncation will affect units whose baselevels are high.

``` r
simdata = sim_data()
simdata$y[simdata$y > 100] <- 100
unique(simdata[simdata$baselevel>75,c("unit","treatg", "baselevel")])
#>     unit treatg baselevel
#> 183    8     15        80
#> 209    9     10        90
#> 235   10     15       100
#> 651   26      0        80
#> 677   27      0        85
#> 703   28      0        90
#> 729   29      0        95
#> 755   30      0       100
```

We now calculate the unit level treatment effect for these.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", panel = FALSE, data = simdata)
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
#> Warning in compute.aggite(MP = MP, type = type, balance_e = balance_e, min_e =
#> min_e, : Simultaneous conf. band is somehow smaller than pointwise one using
#> normal approximation. Since this is unusual, we are reporting pointwise
#> confidence intervals
aggite_table(agtobject)
#>    egt    att.egt   se.egt      lowci    highci
#> 1    1  14.157554 1.292832  11.623650 16.691458
#> 2    2  11.199760 1.435605   8.386026 14.013494
#> 3    3  15.310027 1.270544  12.819806 17.800248
#> 4    4  13.222367 1.353793  10.568981 15.875752
#> 5    5  10.464601 1.278679   7.958436 12.970766
#> 6    6  13.176809 1.407119  10.418906 15.934712
#> 7    7   8.323136 1.329604   5.717161 10.929111
#> 8    8   6.494652 1.390846   3.768644  9.220659
#> 9    9  -5.687429 1.315356  -8.265478 -3.109379
#> 10  10 -12.249471 1.452175 -15.095682 -9.403260
```

Units 9 and 10 have negative estimates caused by the truncation. This
could be alleviated by matching units at the baselevels, which can be
done with matching units at the pre-treatment levels.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", panel = FALSE, xformla = ~y, data = simdata)
#> Warning in pre_process_did_i(yname = yname, tname = tname, idname = idname, : Be aware that there are some small groups in your dataset.
#>   Check groups: 10,15.
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
aggite_table(agtobject)
#>    egt    att.egt    se.egt      lowci     highci
#> 1    1 11.2952379 0.2489464 10.7186457 11.8718301
#> 2    2  7.7162431 0.3835555  6.8278787  8.6046076
#> 3    3 12.7950953 0.3496799 11.9851911 13.6049995
#> 4    4 10.4708341 0.4893991  9.3373222 11.6043460
#> 5    5 10.5082854 0.8971871  8.4302834 12.5862874
#> 6    6 12.9057595 0.8949041 10.8330451 14.9784739
#> 7    7 11.5547166 1.0601705  9.0992239 14.0102094
#> 8    8 10.4161731 0.9375617  8.2446580 12.5876881
#> 9    9  3.7691974 0.7950456  1.9277682  5.6106266
#> 10  10  0.2597346 0.2295181 -0.2718592  0.7913283
```
