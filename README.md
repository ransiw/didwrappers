
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
#> Downloading GitHub repo ransiw/didwrappers@HEAD
#> RcppArmad... (0.12.8.2.0 -> 0.12.8.2.1) [CRAN]
#> fs           (1.6.3      -> 1.6.4     ) [CRAN]
#> brio         (1.1.4      -> 1.1.5     ) [CRAN]
#> testthat     (3.2.1      -> 3.2.1.1   ) [CRAN]
#> lme4         (1.1-35.2   -> 1.1-35.3  ) [CRAN]
#> gtable       (0.3.4      -> 0.3.5     ) [CRAN]
#> ggplot2      (3.5.0      -> 3.5.1     ) [CRAN]
#> Installing 7 packages: RcppArmadillo, fs, brio, testthat, lme4, gtable, ggplot2
#> Installing packages into '/private/var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T/Rtmpb9jTHB/temp_libpath60cb39b828f8'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T//RtmpthC0Ti/downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T/RtmpthC0Ti/remotes61c21b7e5600/ransiw-didwrappers-bf706aa/DESCRIPTION’ ... OK
#> * preparing ‘didwrappers’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building ‘didwrappers_0.0.0.9000.tar.gz’
#> Installing package into '/private/var/folders/0r/jbdlc9211wl2wq7kl5m70l900000gn/T/Rtmpb9jTHB/temp_libpath60cb39b828f8'
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
#> 1    1    5     10      1  0.2098404
#> 2    1    6     10      1  1.8709051
#> 3    1    7     10      1  2.8901973
#> 4    1    8     10      1  4.9090820
#> 5    1    9     10      1  8.9893965
#> 6    1   10     10      1 20.1494063
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual     lowci      highci
#> 1  1    10  6 -0.1773470 0.2950703 -0.1773470 0.04761905 -1.015234  0.66053952
#> 2  1    10  7 -0.9921325 0.3444949 -0.9921325 0.04761905 -1.970366 -0.01389893
#> 3  1    10  8 -0.1464470 0.3932934 -0.1464470 0.04761905 -1.263249  0.97035544
#> 4  1    10  9  2.1544831 0.3629453  2.1544831 0.04761905  1.123857  3.18510877
#> 5  1    10 10  9.1150502 0.2996295  9.1150502 0.04761905  8.264217  9.96588304
#> 6  1    10 11  9.5332398 0.3862094  9.5332398 0.04761905  8.436553 10.62992639
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.204482 0.2330450  8.691479  9.717485
#> 2    2 10.484188 0.2261404  9.986384 10.981992
#> 3    3  9.171327 0.2371851  8.649210  9.693443
#> 4    4 10.786682 0.2273102 10.286304 11.287061
#> 5    5 10.526099 0.2287970 10.022447 11.029751
#> 6    6 10.272665 0.2431925  9.737324 10.808006
#> 7    7  9.544301 0.2307560  9.036337 10.052265
#> 8    8  9.628673 0.2350803  9.111190 10.146156
#> 9    9 10.522202 0.2267742 10.023003 11.021401
#> 10  10  9.515125 0.2387395  8.989587 10.040663
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1 10.209893 0.2415090  9.695456 10.724331
#> 2    2 10.081875 0.1424674  9.778406 10.385344
#> 3    3  8.245053 0.2511732  7.710030  8.780076
#> 4    4 11.209607 0.1477212 10.894947 11.524267
#> 5    5 10.873909 0.2555424 10.329579 11.418239
#> 6    6 20.418338 0.1460526 20.107233 20.729444
#> 7    7 18.559911 0.2445823 18.038927 19.080895
#> 8    8 20.500332 0.1404847 20.201086 20.799578
#> 9    9 19.166781 0.2613925 18.609990 19.723572
#> 10  10 18.827609 0.1496536 18.508833 19.146386
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt  att.egt    se.egt     lowci   highci
#> 1   1 10.06915 0.1877083  9.670849 10.46746
#> 2   2 19.42446 0.1603826 19.084134 19.76478
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
#> 1   2  8.986782 0.2141737  8.559868  9.413697
#> 2   3  8.211336 0.1817405  7.849071  8.573601
#> 3   4 10.607428 0.2144361 10.179991 11.034865
#> 4   5 10.325339 0.1753169  9.975878 10.674800
#> 5   6 10.289479 0.2161414  9.858642 10.720315
#> 6   7 10.684124 0.1666978 10.351844 11.016404
#> 7   8  6.992870 0.2144384  6.565428  7.420312
#> 8   9 10.944446 0.1785756 10.588490 11.300403
#> 9  10 10.172504 0.2001120  9.773619 10.571389
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
#> 1    1  14.306075 1.431440  11.500504 17.111646
#> 2    2  14.791837 1.398591  12.050650 17.533025
#> 3    3  12.717779 1.337275  10.096769 15.338790
#> 4    4  13.776468 1.392522  11.047175 16.505761
#> 5    5  15.101950 1.393449  12.370841 17.833059
#> 6    6  13.428498 1.370260  10.742839 16.114158
#> 7    7   9.373529 1.428941   6.572857 12.174202
#> 8    8   6.813258 1.337331   4.192136  9.434379
#> 9    9  -7.629540 1.377318 -10.329034 -4.930047
#> 10  10 -11.155771 1.341128 -13.784333 -8.527209
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
#>    egt    att.egt    se.egt       lowci    highci
#> 1    1 11.2226399 0.1985774 10.77168519 11.673595
#> 2    2 11.8853632 0.4522567 10.85832131 12.912405
#> 3    3 10.1262764 0.4190264  9.17469803 11.077855
#> 4    4 11.2769660 0.5291972 10.07519791 12.478734
#> 5    5 14.4438453 0.7180547 12.81319558 16.074495
#> 6    6 13.1524224 0.8117183 11.30906968 14.995775
#> 7    7 12.4985246 1.1579436  9.86891944 15.128130
#> 8    8 10.3730066 1.0377317  8.01639397 12.729619
#> 9    9  3.2345795 0.9536439  1.06892409  5.400235
#> 10  10  0.4975265 0.2514260 -0.07344347  1.068496
```
