[![CRAN version](http://www.r-pkg.org/badges/version/gesisdata)](https://cran.r-project.org/package=gesisdata) ![](http://cranlogs.r-pkg.org/badges/grand-total/gesisdata) [![Travis-CI Build Status](https://travis-ci.org/fsolt/gesisdata.svg?branch=master)](https://travis-ci.org/fsolt/gesisdata)
------------------------------------------------------------------------

gesisdata
=========

The [GESIS](https://search.gesis.org) Data Archive provides an crucial repository of social scientific surveys, including, among many others, the ALLBUS, the European Values Survey, the Eurobarometer, and the International Social Survey Program.  Researchers taking advantage of these datasets, however, are caught in a bind.  The terms and conditions for downloading any GESIS dataset prohibit disseminating the data.  But to ensure that one's work can be reproduced, assessed, and built upon by others, one must provide access to the raw data one employed.  

The `gesisdata` package cuts this knot by providing programmatic, reproducible access to specified GESIS datasets from within R for [registered users](https://login.gesis.org/). 


To install:

* the latest released version: `install.packages("gesisdata")`
* the latest development version:

```R
if (!require(remotes)) install.packages("remotes")
remotes::install_github("fsolt/gesisdata")
```

For more details, check out [the vignette](https://cran.r-project.org/package=gesisdata/vignettes/gesisdata-vignette.html).

Please recall that by using the GESIS Data Archive, you accept its terms and conditions.


