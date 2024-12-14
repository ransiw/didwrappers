
<!-- README.md is generated from README.Rmd. Please edit that file -->

# didwrappers

<!-- badges: start -->
<!-- badges: end -->

The goal of didwrappers is to extend the functionality of the
[did](https://bcallaway11.github.io/did/) package, specifically for
within treatment-group heterogeneity and unbalanced data. It also allows
for additional type of aggregations. Components estimates can be
aggregated at the unit-level, or on any other time-invariant variable
(e.g: birth year of an individual, or the region of a country or state).
This allows for some types of non-binary treatments, if the dosage level
received by the treated unit is used as the aggregation level. See the
following examples.

## Installation

You can install the development version of didwrappers

``` r
# install.packages("devtools")
# devtools::install_github("ransiw/didwrappers", build_vignettes = TRUE)
```

Or install from CRAN

``` r
# install.packages("didwrappers")
# library(didwrappers)
```

Call for vignettes with `browseVignettes(package = "didwrappers")`

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
#> 1    1    5     10      1  0.9765028
#> 2    1    6     10      1  2.5040220
#> 3    1    7     10      1  4.1027499
#> 4    1    8     10      1  7.1109725
#> 5    1    9     10      1  8.3833244
#> 6    1   10     10      1 20.4952565
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf[,1:7])
#>   id group  t        att        se        lci         uci
#> 1  1    10  6 -0.3940472 0.2852624 -0.9531614  0.16506711
#> 2  1    10  7 -0.3538506 0.2263880 -0.7975711  0.08986987
#> 3  1    10  8  1.0170571 0.3313849  0.3675427  1.66657147
#> 4  1    10  9 -0.5391436 0.4444852 -1.4103345  0.33204729
#> 5  1    10 10 10.2803411 0.4146102  9.4677052 11.09297699
#> 6  1    10 11 10.0831698 0.4265082  9.2472138 10.91912586
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt   lci.egt   uci.egt
#> 1    1  9.595940 0.2222221  9.152127 10.058622
#> 2    2 11.364140 0.2669428 10.839662 11.924580
#> 3    3  8.988459 0.1963648  8.609782  9.379475
#> 4    4 10.282525 0.2718089  9.736423 10.807046
#> 5    5 10.479021 0.2401080 10.010325 10.942495
#> 6    6 10.149815 0.2251318  9.686151 10.592025
#> 7    7  9.940078 0.2575904  9.417874 10.436564
#> 8    8  9.296310 0.3063967  8.698213  9.914181
#> 9    9  8.834516 0.2861991  8.264489  9.413781
#> 10  10  8.999917 0.2523602  8.540064  9.494038
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", customnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt   lci.egt   uci.egt
#> 1    1 10.494341 0.3189246  9.849312 11.104125
#> 2    2 10.831962 0.2183098 10.411235 11.245508
#> 3    3 10.077112 0.3115077  9.457087 10.657892
#> 4    4  9.476786 0.2625175  8.955249  9.998167
#> 5    5 10.790680 0.2505775 10.336551 11.302131
#> 6    6 19.947197 0.2370833 19.444063 20.412442
#> 7    7 19.304729 0.2191709 18.882496 19.727221
#> 8    8 20.342717 0.2574521 19.843688 20.841611
#> 9    9 21.226676 0.2494786 20.782271 21.746952
#> 10  10 20.120987 0.1713363 19.785358 20.443224
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt  att.egt    se.egt  lci.egt  uci.egt
#> 1   1 10.35310 0.1364377 10.10538 10.62835
#> 2   2 20.19704 0.1223946 19.96002 20.43729
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
#>   egt   att.egt    se.egt   lci.egt  uci.egt
#> 1   2 11.491920 0.2909187 10.902494 12.04581
#> 2   3 10.550311 0.2094663 10.139954 10.98488
#> 3   4 11.303989 0.3319666 10.639628 11.93700
#> 4   5  9.783662 0.2384830  9.337403 10.28213
#> 5   6 10.500442 0.3010967  9.895773 11.07207
#> 6   7  6.939138 0.2554716  6.456224  7.43968
#> 7   8 10.332058 0.3093567  9.757034 10.95536
#> 8   9 10.451803 0.1841023 10.073357 10.79100
#> 9  10 11.044857 0.2762568 10.463495 11.55014
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
aggite_table(agtobject)
#>    egt    att.egt    se.egt    lci.egt   uci.egt
#> 1    1  13.465340 0.8844840  11.830112 15.253058
#> 2    2  15.224017 0.9524734  13.400294 17.119660
#> 3    3  14.060646 0.8611385  12.454377 15.826980
#> 4    4  13.215506 0.9698077  11.387653 15.204887
#> 5    5  12.749601 0.8282434  11.260808 14.527662
#> 6    6  12.115261 0.9067646  10.468479 13.962527
#> 7    7   8.837290 1.0430644   6.631224 10.784948
#> 8    8   7.044907 1.3059132   4.406511  9.480382
#> 9    9  -6.712105 2.1371959 -10.968982 -2.386859
#> 10  10 -11.057038 1.7962573 -14.591535 -7.820848
```

Units 9 and 10 have negative estimates caused by the truncation. This
could be alleviated by matching units at the baseline levels as follows.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", panel = FALSE, xformla = ~y, data = simdata)
#> Warning in pre_process_did_i(yname = yname, tname = tname, idname = idname, : Be aware that there are some small groups in your dataset.
#>   Check groups: 10,15.
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
aggite_table(agtobject)
#>    egt    att.egt    se.egt    lci.egt    uci.egt
#> 1    1  9.9382459 0.2743492  9.3913751 10.4813759
#> 2    2 11.4515737 0.3223047 10.8136277 12.0789291
#> 3    3 11.3520309 0.3155919 10.7395491 11.9782597
#> 4    4 10.4021520 0.4888054  9.4066020 11.3713841
#> 5    5 12.4120363 0.6277576 11.1652206 13.6474967
#> 6    6 12.0471588 0.8276022 10.5050054 13.6802863
#> 7    7 11.9523477 0.6637983 10.6512357 13.2781011
#> 8    8 10.7928201 0.7436780  9.2738863 12.1896832
#> 9    9  3.1847441 0.6297052  1.8826304  4.3465517
#> 10  10  0.5338706 0.1355085  0.2922493  0.8082448
```
