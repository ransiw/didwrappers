
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
#>   unit time treatg dosage           y
#> 1    1    5     10      1 -0.06318495
#> 2    1    6     10      1  2.22046460
#> 3    1    7     10      1  4.21131837
#> 4    1    8     10      1  6.44301096
#> 5    1    9     10      1  9.72687332
#> 6    1   10     10      1 19.44497982
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t         att        se     attcalc    ipwqual      lowci    highci
#> 1  1    10  6 -0.04797318 0.3582682 -0.04797318 0.04761905 -1.0704969 0.9745505
#> 2  1    10  7  0.28811525 0.3070530  0.28811525 0.04761905 -0.5882365 1.1644671
#> 3  1    10  8  0.25868283 0.3500318  0.25868283 0.04761905 -0.7403336 1.2576993
#> 4  1    10  9  1.40103870 0.2972334  1.40103870 0.04761905  0.5527130 2.2493644
#> 5  1    10 10  7.34800732 0.3278986  7.34800732 0.04761905  6.4121606 8.2838540
#> 6  1    10 11  8.79452788 0.3113672  8.79452788 0.04761905  7.9058632 9.6831925
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  8.404469 0.2127174  7.934953  8.873986
#> 2    2  9.454916 0.1922370  9.030605  9.879228
#> 3    3 10.306823 0.2152879  9.831633 10.782013
#> 4    4 10.214076 0.1998244  9.773018 10.655135
#> 5    5  9.393001 0.2118405  8.925420  9.860581
#> 6    6 10.872406 0.2042690 10.421537 11.323274
#> 7    7  9.326678 0.2170932  8.847504  9.805853
#> 8    8 12.351097 0.2000748 11.909485 12.792708
#> 9    9  9.134421 0.2160083  8.657641  9.611201
#> 10  10  8.115082 0.2053938  7.661731  8.568433
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1 10.097536 0.2489815  9.578841 10.616232
#> 2    2  9.895154 0.3014619  9.267128 10.523180
#> 3    3  7.847296 0.2559204  7.314145  8.380447
#> 4    4 11.248098 0.2993426 10.624487 11.871710
#> 5    5  9.585272 0.2590297  9.045644 10.124901
#> 6    6 21.197248 0.2995467 20.573212 21.821284
#> 7    7 20.467182 0.2431943 19.960542 20.973821
#> 8    8 18.089222 0.3248534 17.412465 18.765979
#> 9    9 18.854548 0.2435407 18.347187 19.361909
#> 10  10 18.527270 0.2945913 17.913557 19.140983
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt   att.egt    se.egt     lowci   highci
#> 1   1  9.646571 0.1992635  9.251264 10.04188
#> 2   2 19.453069 0.2035187 19.049321 19.85682
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
#> 1   2  9.274623 0.2149594  8.812215  9.737030
#> 2   3 10.754198 0.2538130 10.208211 11.300185
#> 3   4 11.333543 0.2189885 10.862468 11.804618
#> 4   5  9.275298 0.2507500  8.735900  9.814696
#> 5   6  9.456229 0.2234667  8.975521  9.936937
#> 6   7  8.433401 0.2580149  7.878375  8.988427
#> 7   8 10.277068 0.2113411  9.822444 10.731693
#> 8   9 11.190208 0.2631521 10.624131 11.756284
#> 9  10 10.094596 0.2348203  9.589465 10.599727
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
#> 1    1  13.960746 1.277247  11.457388 16.464103
#> 2    2  13.283346 1.438058  10.464803 16.101888
#> 3    3  13.406250 1.350825  10.758682 16.053818
#> 4    4  12.809787 1.477327   9.914280 15.705295
#> 5    5  11.578988 1.329703   8.972819 14.185157
#> 6    6  13.437012 1.439722  10.615209 16.258814
#> 7    7   7.908859 1.390323   5.183877 10.633842
#> 8    8   6.396100 1.470667   3.513646  9.278554
#> 9    9  -6.285585 1.320529  -8.873774 -3.697396
#> 10  10 -11.360833 1.504928 -14.310437 -8.411228
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
#> 1    1 11.1548668 0.3097723 10.45098809 11.858745
#> 2    2  8.7570181 0.3557478  7.94867190  9.565364
#> 3    3 10.8288530 0.4946050  9.70498913 11.952717
#> 4    4  9.8540073 0.4345221  8.86666652 10.841348
#> 5    5 11.4457720 0.8256130  9.56977670 13.321767
#> 6    6 13.1382810 0.8803178 11.13798294 15.138579
#> 7    7 11.3160279 1.1273706  8.75436530 13.877690
#> 8    8 10.3944973 0.9976251  8.12764828 12.661346
#> 9    9  3.5383362 0.8069551  1.70473635  5.371936
#> 10  10  0.5079861 0.2199442  0.00821891  1.007753
```
