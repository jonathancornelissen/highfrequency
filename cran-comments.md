## Test environments
* local OS X install, R 3.6.1
* ubuntu 14.04 (on travis-ci), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 1 notes

* checking installed package size ... NOTE
  installed size is  7.2Mb
  sub-directories of 1Mb or more:
    data   6.3Mb
    
The installed size is necessary due to the use of highfrequency intraday data.

On windows maybe:

Check: package dependencies, Result: NOTE
  Package in Depends/Imports which should probably only be in LinkingTo: 'RcppArmadillo'

It is actually necessary this way.

New maintainer email address