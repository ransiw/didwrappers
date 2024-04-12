
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
#> 1    1    5     10      1  1.6954992
#> 2    1    6     10      1  0.6895626
#> 3    1    7     10      1  3.4967893
#> 4    1    8     10      1  4.8663595
#> 5    1    9     10      1  8.5575460
#> 6    1   10     10      1 21.4183516
```

Run an att_it() object and tabulate it

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", data = simdata)
attdf = attit_table(attobject)
head(attdf)
#>   id group  t        att        se    attcalc    ipwqual     lowci     highci
#> 1  1    10  6 -3.1970229 0.2833659 -3.1970229 0.04761905 -4.023224 -2.3708217
#> 2  1    10  7  0.9961634 0.4239372  0.9961634 0.04761905 -0.239897  2.2322237
#> 3  1    10  8 -0.7954356 0.3428678 -0.7954356 0.04761905 -1.795125  0.2042534
#> 4  1    10  9  1.8161446 0.2660011  1.8161446 0.04761905  1.040574  2.5917157
#> 5  1    10 10 10.6415818 0.2067880 10.6415818 0.04761905 10.038656 11.2445072
#> 6  1    10 11  9.1947591 0.2959749  9.1947591 0.04761905  8.331794 10.0577240
```

We can now aggregate all post-treatment effects to the unit level.

``` r
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1  9.246710 0.1832476  8.865402  9.628018
#> 2    2  9.014459 0.1925045  8.613888  9.415029
#> 3    3 10.593863 0.1878220 10.203036 10.984690
#> 4    4 12.000313 0.1979284 11.588456 12.412169
#> 5    5  9.424241 0.1931665  9.022293  9.826190
#> 6    6 10.325765 0.1847092  9.941416 10.710115
#> 7    7  8.615305 0.1888645  8.222309  9.008301
#> 8    8  9.290000 0.1925349  8.889366  9.690633
#> 9    9  9.553763 0.1915597  9.155159  9.952368
#> 10  10 11.188719 0.2063759 10.759285 11.618154
```

Now sample a data frame with different dosage amounts and aggregate to
the unit level.

``` r
attobject = att_it(yname = "y", tname = "time", gname = "treatg", idname ="unit", cohortnames = "dosage", data = sim_data(dosage = rep(c(1,2),each=5)))
agtobject = aggite(attobject,type="unit")
aggite_table(agtobject)
#>    egt   att.egt    se.egt     lowci    highci
#> 1    1 10.888983 0.2603020 10.338514 11.439452
#> 2    2  9.070811 0.1760926  8.698422  9.443199
#> 3    3 10.341061 0.2457369  9.821394 10.860729
#> 4    4 10.669375 0.1753193 10.298622 11.040128
#> 5    5 10.469362 0.2266298  9.990101 10.948624
#> 6    6 20.880034 0.1825029 20.494090 21.265979
#> 7    7 20.568871 0.2322928 20.077635 21.060108
#> 8    8 20.301514 0.1839135 19.912586 20.690441
#> 9    9 20.226121 0.2564376 19.683825 20.768418
#> 10  10 21.191232 0.1796282 20.811367 21.571097
```

We could also aggregate to the dosage level by specifying dosage as the
`type`.

``` r
agtobject = aggite(attobject,type="dosage")
aggite_table(agtobject)
#>   egt  att.egt    se.egt     lowci   highci
#> 1   1 10.33190 0.1713375  9.993805 10.67000
#> 2   2 20.60733 0.1398423 20.331379 20.88327
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
agtobject = aggite(attobject,type="unit", na.rm = TRUE)
aggite_table(agtobject)
#>   egt   att.egt    se.egt     lowci    highci
#> 1   2 10.962868 0.2551731 10.439999 11.485738
#> 2   3  9.878312 0.2246243  9.418039 10.338584
#> 3   4 10.474135 0.2606575  9.940028 11.008243
#> 4   5 11.375138 0.2361643 10.891219 11.859057
#> 5   6 11.604155 0.2642827 11.062620 12.145691
#> 6   7 11.060358 0.2134391 10.623005 11.497712
#> 7   8 10.740680 0.2403029 10.248281 11.233079
#> 8   9  9.945794 0.2039808  9.527822 10.363767
#> 9  10  9.190158 0.2508156  8.676217  9.704098
```
