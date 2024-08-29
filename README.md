
<!-- README.md is generated from README.Rmd. Please edit that file -->

# didwrappers

<!-- badges: start -->
<!-- badges: end -->

The goal of didwrappers is to extend the functionality of the `did`
package, specifically for within treatment-group heterogeneity and
unbalanced data.

## Installation

You can install the development version of didwrappers, and call for
vignettes with `browseVignettes(package = "didwrappers")`

``` r
# install.packages("devtools")
devtools::install_github("ransiw/didwrappers")
#> Downloading GitHub repo ransiw/didwrappers@HEAD
#> RcppArmad... (0.12.8.4.0 -> 14.0.0-1) [CRAN]
#> cpp11        (0.4.7      -> 0.5.0   ) [CRAN]
#> Installing 2 packages: RcppArmadillo, cpp11
#> Installing packages into '/private/var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T/RtmpiBVQCY/temp_libpath47654f712b3'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T//Rtmpm1INC0/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T/Rtmpm1INC0/remotes3605660e0d29/ransiw-didwrappers-e4ba5aa/DESCRIPTION’ ... OK
#> * preparing ‘didwrappers’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘didwrappers_0.0.0.9000.tar.gz’
#> Installing package into '/private/var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T/RtmpiBVQCY/temp_libpath47654f712b3'
#> (as 'lib' is unspecified)
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
#> 1    1    5     10      1  0.7325122
#> 2    1    6     10      1  1.4964022
#> 3    1    7     10      1  4.3196122
#> 4    1    8     10      1  6.1426440
#> 5    1    9     10      1  8.8409572
#> 6    1   10     10      1 20.5316238
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual       lowci     highci
#> 1  1    10  6 -1.0845595 0.2788826 -1.0845595 0.04761905 -1.88532743 -0.2837915
#> 2  1    10  7  1.2593806 0.3463383  1.2593806 0.04761905  0.26492430  2.2538370
#> 3  1    10  8 -0.3222544 0.3208058 -0.3222544 0.04761905 -1.24339821  0.5988893
#> 4  1    10  9  0.8125945 0.2923179  0.8125945 0.04761905 -0.02675073  1.6519398
#> 5  1    10 10 10.0101777 0.3163351 10.0101777 0.04761905  9.10187075 10.9184846
#> 6  1    10 11  9.1042407 0.2937436  9.1042407 0.04761905  8.26080166  9.9476798
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.084122 0.1611146  8.719822  9.448422
#> 2    2 11.187077 0.2314551 10.663729 11.710426
#> 3    3 10.258636 0.1613267  9.893856 10.623415
#> 4    4  8.555250 0.2169538  8.064691  9.045809
#> 5    5  9.373920 0.1615097  9.008726  9.739113
#> 6    6  9.470111 0.2231336  8.965578  9.974643
#> 7    7  8.745954 0.1548380  8.395846  9.096061
#> 8    8 10.439071 0.2163197  9.949946 10.928197
#> 9    9 11.136871 0.1553856 10.785525 11.488216
#> 10  10 11.371770 0.2146882 10.886333 11.857206
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.692039 0.2043017  9.252623 10.131454
#> 2    2  9.379227 0.2643801  8.810594  9.947860
#> 3    3 11.713009 0.2089959 11.263497 12.162521
#> 4    4  8.092353 0.2820365  7.485744  8.698962
#> 5    5 10.257317 0.2010526  9.824889 10.689744
#> 6    6 21.980668 0.2811408 21.375986 22.585351
#> 7    7 19.638685 0.2203540 19.164744 20.112626
#> 8    8 18.809898 0.2851814 18.196525 19.423271
#> 9    9 20.174997 0.2115270 19.720041 20.629953
#> 10  10 19.345025 0.3006201 18.698447 19.991604
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt   att.egt    se.egt     lowci   highci
#> 1   1  9.941631 0.1707078  9.591145 10.29212
#> 2   2 19.980631 0.1816349 19.607710 20.35355
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
#> 1   2 10.980604 0.2217809 10.507390 11.453817
#> 2   3  8.749283 0.2618875  8.190494  9.308071
#> 3   4 10.292717 0.2243128  9.814101 10.771333
#> 4   5  9.489730 0.2481323  8.960291 10.019170
#> 5   6 10.914232 0.2268925 10.430112 11.398352
#> 6   7 10.424545 0.2563892  9.877488 10.971602
#> 7   8 10.544467 0.2342630 10.044621 11.044313
#> 8   9  7.409454 0.2395420  6.898343  7.920564
#> 9  10 10.976017 0.2396498 10.464677 11.487358
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
#> 1    1  13.317408 1.383562  10.605677 16.029139
#> 2    2  12.846260 1.457263   9.990077 15.702443
#> 3    3  14.225332 1.327287  11.623897 16.826766
#> 4    4  13.868734 1.427409  11.071064 16.666404
#> 5    5  12.685870 1.341219  10.057129 15.314610
#> 6    6  12.425729 1.492343   9.500790 15.350669
#> 7    7   9.835243 1.316555   7.254842 12.415643
#> 8    8   8.150247 1.484290   5.241092 11.059403
#> 9    9  -7.352430 1.242416  -9.787521 -4.917339
#> 10  10 -11.108553 1.379299 -13.811929 -8.405176
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
#> 1    1  9.8951048 0.8907955  7.77540548 12.014804
#> 2    2  9.1927351 0.2181543  8.67362440  9.711846
#> 3    3 11.5155404 0.4819641 10.36867892 12.662402
#> 4    4 11.0454343 0.3981465 10.09802177 11.992847
#> 5    5 12.4218746 0.7372082 10.66764514 14.176104
#> 6    6 12.3541783 0.8681447 10.28837783 14.419979
#> 7    7 12.7835317 1.0823860 10.20793169 15.359132
#> 8    8 11.6717003 0.9993713  9.29363847 14.049762
#> 9    9  2.9842023 0.6551317  1.42527850  4.543126
#> 10  10  0.6662568 0.2612576  0.04457921  1.287934
```
