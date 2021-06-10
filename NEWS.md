Changes in version 0.9.0

 - Fix typo where in spotVol, the stochastic periodicity was erroneously written as stochper instead of stochPer.
 
 - Better consistency with the choices in spot* functions
 
 - Add warning instead of print in detPer in spotVol. The periodic component is no longer set to 1 either, just warned that results may be weird.
 
 - Implement Beta-Adjusted realized covariance estimation in the rBACov function
 
 - Add plotTQData function to plot trades and quotes.
 
 - Add ICov and IVar documentation pages that work as lists of implemented estimators of the integrated covariance and the integrated variance, respectively.
 
 - More or less complete rework of HEAVY model and related functions
 
 - Fix bug in HARCJ model, thanks to @SKMASON (Zhixi Shao) for reporting the bug and providing pointers for the fix.
 
 - Better formatting of NEWS.md
 
 - aggregateTS now accept both data.table and xts objects
 

Changes in version 0.8.0

 - Various bugfixes and performance improvements
 
 - Added lowercase "r" in front of realized measures that did not have it, so it can easily be found in IDEs with highfrequency::
 
 - Added drift burst code
 
 - Added Backwards - Forwards matching algorithm to cleaning step. Thanks to Kim Christensen for providing his Matlab code.
 
 - removed various datasets, outdated TAQ format, simulated 5-minute prices, returns, and prices with jumps
 
 - Added European sample data, anonymized one-minute data with anonymous stock and market data, as well as home-grown dataset with realized measures
 
 - The arguments "on" and "k" in data handling is now "alignBy" and "alignPeriod" respectively. This means that the realized measures and data handling functions have similar notation
 
 - Improved UX in data cleaning functions by having more clear report on the trades when prompted and adding defaults that follow the standard in the literature
 
 - Added data.table support to (most) realized measures
 
 - Added functions makeRMFormat and makeOHLCV to convert data from a long format to a format that can be used for realized measures, and to make arbitrary period bars.
 
 - HARmodel has less 'RV' in the types - much easier to type and read, also it now supports an external regressor and has robust standard errors reported in the summary.
 
 - add asymptotic variance estimator for ReMeDI estimation - thanks to Merrick Li for contributing his Matlab code
 
 - In general, improved documentation. Also, better documentation of methods which were, in most cases undocumented and not clearly exported
 
 
Changes in version 0.7.0

 - New naming convention
 
 - Bugfix in BNSjumpTest, JOjumpTest, AJjumpTest. These functions behaved in an unexpected and inconsistent manner when the input spanned more than one day
 
 - Bugfix in aggregateTS function which in edge cases returned data from AFTER the input data
 
 - Implement intradayJumpTest function which allows for flexible Lee-Mykland style jump tests
 
 - Implement rankJumpTest to test for the rank of the jump matrix
 
 - Implement new features in spotVol. Now the local volatility can be estimated with realized measures, they can also be used with pre-averaged realized measures.
 
 - Implement a wrapper around quantmod's getSymbols.av function
 
 - harModel now includes Newey-West standard errors in the output
 
 - Bugfix for refreshTime function and large performance improvement
 
 - Implement CholCov estimator in rCholCov
 
 - Bugfixes in data handling functions, which sometimes produced different results depending on the options(digits.secs) setting. Most data handling functions now run considerably faster as a consequence of internally using numerics for timestamps.
 
 - Implemented new realized semi-covariance estimator in rSemiCov
 
 - Implemented new lead-lag estimation in leadLag
 
 - Implemented ReMeDI estimation in ReMeDI
 
 - More transparently handle the lagging of quotes when matching these with trades, now the user has control of this.
 
 - Add business time sampling
 
 - Changes to the included datasets. The microseconds quote datasets have been thinned out aggressively for exchanges != "N"
 

Changes in version 0.6.5

 - bug fix for kernelCov if cor = TRUE
 
 - compatibility with lubridate 1.7.8
 

Changes in version 0.6.4

 - bug fix in refreshTime (affected rMRC for n > 2)
 
 - one additional test for rMRC
 
 - updated realized library file until end of 2019
 

Changes in version 0.6.3

 - aggregateTrades size aggregation bug fix
 
 
Changes in version 0.6.2

 - spotVol and spotDrift do not assume naming convention for univariate time series anymore
 
 - bug fix tpv and finite sample corrections
 
 
Changes in version 0.6.1

 - bug fix for Fedora compilation
 
Changes in version 0.6.0

 - all new backend
   - documentation via roxygen2
   - testing via test_that
   - covr integration on github
   
 - microsecond compatibility for WRDS files
 
 - improved documentation
 
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
 


 



 
