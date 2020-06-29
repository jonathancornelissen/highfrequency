Changes in version 0.7.0
 - New naming convention.
 - Bugfix in BNSjumpTest, JOjumpTest, AJjumpTest. These functions behaved in an unexpected and inconsistent manner when the input spanned more than one day
 - Bugfix in aggregateTS function which in edge cases returned data from AFTER the input data
 - Implement intradayJumpTest function which allows for flexible Lee-Mykland style jump tests
 - Implement rankJumpTest to test for the rank of the jump matrix
 - Implement new features in spotVol. Now the local volatility can be estimated with realized measures, they can also be used with pre-averaged realized measures.


Changes in version 0.6.5
 - Bug fix for kernelCov if cor = TRUE
 - Compatibility with lubridate 1.7.8

Changes in version 0.6.4
 - bug fix in refreshTime (affected MRC for n > 2)
 - one additional test for MRC
 - updated realized library file until end of 2019

Changes in version 0.6.3
 - aggregateTrades size aggregation bug fix
 
Changes in version 0.6.2
 - spotVol and spotDrift don not assume naming convention for univariate time series anymore
 - bug fix tpv and finite sample corrections
 
Changes in version 0.6.1
 - bug fix for Fedora compilation
 
Changes in version 0.6.0
 - all new backend
   - documentation via roxygen2
   - testing via test_that
   - covr integration on github
 - microsecond compatibility for WRDS files
 - improved doumentation
 - new options in harModel
 - updated data sets
 - updated references
 - cleanup of code basis
 
Changes in version 0.5
 - converted so that it would work with Cran
 - added missing data files
 - compressed data files

Changes in version 0.4
 - update package code github to version on rforge
 - to do: print more output in tradesCleanup about the different filters
 - correction to implementation AJjumptest by Giang


 



 
