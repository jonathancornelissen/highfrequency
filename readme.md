[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/highfrequency)](https://cran.r-project.org/package=highfrequency) 
[![Travis-CI Build Status](https://travis-ci.org/jonathancornelissen/highfrequency.svg?branch=master)](https://travis-ci.org/jonathancornelissen/highfrequency)
[![Codecov test coverage](https://codecov.io/gh/jonathancornelissen/highfrequency/branch/master/graph/badge.svg)](https://codecov.io/gh/jonathancornelissen/highfrequency?branch=master)
[![Downloads](https://cranlogs.r-pkg.org/badges/highfrequency)](https://cranlogs.r-pkg.org/badges/highfrequency)

## Highfrequency financial data in R

The package is still under development and is distributed without warranty.

Thanks to report bugs or make suggestions to <kris.boudt@ugent.be>.

## Installation
CRAN:
```r
install.packages("highfrequency")
```
Development version:
```r
# Install package via devtools
# install.packages("devtools")
library(devtools)
install_github("https://github.com/jonathancornelissen/highfrequency")
```

## Example
```r
library(highfrequency)
# Print raw quotes data to console
sampleQDataraw_microseconds
# Cleanup quotes leaves 46566 out of 464221 observations.
quotesCleanup(qdataraw = sampleQDataraw_microseconds, exchanges = "N")
```

## Special thanks

We would like to thank [Brian Peterson](https://github.com/braverock), [Chris Blakely](https://github.com/clisztian), [Eric Zivot](https://faculty.washington.edu/ezivot/) and Maarten Schermer. We are also grateful to [Dirk Eddelbuettel](https://github.com/eddelbuettel) for his support as a mentor during the Google Summmer of Code 2019. Moreover, we thank [Emil Sjørup](https://github.com/emilsjoerup) for implementing additional options in the harModel function.
