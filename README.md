
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
#>   unit time treatg dosage         y
#> 1    1    5     10      1 -1.682864
#> 2    1    6     10      1  2.823592
#> 3    1    7     10      1  4.233826
#> 4    1    8     10      1  5.557872
#> 5    1    9     10      1  7.037021
#> 6    1   10     10      1 19.481944
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf[,1:7])
#>   id group  t        att        se    attcalc    ipwqual
#> 1  1    10  6  2.2019412 0.3647034  2.2019412 0.04761905
#> 2  1    10  7 -0.7124275 0.3427465 -0.7124275 0.04761905
#> 3  1    10  8 -0.4978671 0.3699245 -0.4978671 0.04761905
#> 4  1    10  9 -0.5868508 0.3838006 -0.5868508 0.04761905
#> 5  1    10 10 10.1770026 0.2593637 10.1770026 0.04761905
#> 6  1    10 11  9.1755694 0.2872384  9.1755694 0.04761905
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1 10.913376 0.2231014 10.456225 11.370527
#> 2    2 13.097818 0.2376098 12.610938 13.584697
#> 3    3  8.442448 0.2247835  7.981851  8.903046
#> 4    4  9.468401 0.2490680  8.958043  9.978759
#> 5    5  7.924238 0.2407734  7.430876  8.417600
#> 6    6  8.617983 0.2309662  8.144716  9.091249
#> 7    7  9.499252 0.2453372  8.996539 10.001966
#> 8    8  9.724732 0.2579821  9.196108 10.253356
#> 9    9 13.078300 0.2407867 12.584911 13.571689
#> 10  10  9.673283 0.2333813  9.195068 10.151498
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.738682 0.2074943  9.320704 10.156660
#> 2    2 10.268772 0.2226722  9.820220 10.717325
#> 3    3  9.277046 0.1923937  8.889487  9.664605
#> 4    4  9.606105 0.2419563  9.118707 10.093504
#> 5    5  9.425413 0.1976163  9.027333  9.823492
#> 6    6 20.668759 0.2342785 20.196827 21.140691
#> 7    7 20.129250 0.1897136 19.747089 20.511410
#> 8    8 18.819633 0.2619434 18.291973 19.347294
#> 9    9 19.720983 0.2015420 19.314996 20.126971
#> 10  10 21.742343 0.2289798 21.281084 22.203601
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt   att.egt    se.egt     lowci    highci
#> 1   1  9.634337 0.1340298  9.351016  9.917658
#> 2   2 20.183852 0.1275047 19.914324 20.453380
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
#>   egt   att.egt    se.egt     lowci    highci
#> 1   2  9.599405 0.1902648  9.190005 10.008805
#> 2   3 10.540529 0.2443381 10.014778 11.066280
#> 3   4  9.942616 0.2110172  9.488563 10.396670
#> 4   5  8.529358 0.2544047  7.981946  9.076770
#> 5   6  9.246343 0.1933400  8.830326  9.662360
#> 6   7  7.755309 0.2534030  7.210052  8.300565
#> 7   8  9.904934 0.2041546  9.465647 10.344221
#> 8   9 10.738400 0.2728065 10.151392 11.325408
#> 9  10  9.714657 0.2123501  9.257735 10.171578
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
#> 1    1  14.370083 1.374298  11.676509 17.063657
#> 2    2  13.531035 1.420832  10.746255 16.315815
#> 3    3  15.911100 1.370176  13.225604 18.596596
#> 4    4  15.941756 1.431398  13.136269 18.747244
#> 5    5  12.895077 1.377833  10.194575 15.595580
#> 6    6  13.185333 1.388983  10.462976 15.907690
#> 7    7   8.431472 1.379588   5.727530 11.135414
#> 8    8   7.363971 1.400483   4.619076 10.108867
#> 9    9  -5.125133 1.445498  -7.958258 -2.292008
#> 10  10 -10.756602 1.439604 -13.578174 -7.935029
```

Units 9 and 10 have negative estimates caused by the truncation. This
could be alleviated by matching units at the baseline levels as follows.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", panel = FALSE, xformla = ~y, data = simdata)
#> Warning in pre_process_did_i(yname = yname, tname = tname, idname = idname, : Be aware that there are some small groups in your dataset.
#>   Check groups: 10,15.
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
aggite_table(agtobject)
#>    egt    att.egt    se.egt       lowci    highci
#> 1    1 11.3161207 0.2217000 10.81898111 11.813260
#> 2    2  9.3838194 0.2698139  8.77878937  9.988849
#> 3    3 13.2608120 0.3384556 12.50186000 14.019764
#> 4    4 12.9413845 0.4113505 12.01897316 13.863796
#> 5    5 12.6162939 0.8810554 10.64061755 14.591970
#> 6    6 12.9312819 0.8260141 11.07903003 14.783534
#> 7    7 11.8035206 1.1485902  9.22792507 14.379116
#> 8    8 11.0521524 1.0213119  8.76196527 13.342340
#> 9    9  4.3248216 0.8433345  2.43373054  6.215913
#> 10  10  0.7089733 0.2830933  0.07416559  1.343781
```
