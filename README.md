<!-- badges: start -->
[![CRAN version](http://www.r-pkg.org/badges/version/gesisdata)](https://cran.r-project.org/package=gesisdata) ![](http://cranlogs.r-pkg.org/badges/grand-total/gesisdata)
[![R-CMD-check](https://github.com/fsolt/gesisdata/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/fsolt/gesisdata/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

------------------------------------------------------------------------

gesisdata
=========

The [GESIS](https://search.gesis.org) Data Archive makes available thousands of invaluable social scientific surveys, including, among many others, the ALLBUS, the European Values Survey, the Eurobarometer, and the International Social Survey Program.  Researchers taking advantage of these datasets, however, are caught in a bind.  The archive's terms and conditions bar dissemination of downloaded datasets to third parties.  But to ensure that one's work can be reproduced, assessed, and built upon by others, one must provide access to the raw data one employed.  

The `gesisdata` package cuts this knot by providing programmatic, reproducible access to specified GESIS datasets from within R for [registered users](https://login.gesis.org/realms/gesis/account/). 


To install:

* the latest released version: `install.packages("gesisdata")`
* the latest development version:

```R
if (!require(remotes)) install.packages("remotes")
remotes::install_github("fsolt/gesisdata")
```

For more details, check out [the vignette](https://fsolt.org/gesisdata/articles/gesisdata-vignette.html).

Please recall that by using the GESIS Data Archive, you accept its terms and conditions.


