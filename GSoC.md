## GSoC 2019 Résumé

In the following, I would like to briefly describe my work done during GSoC 2019. All improvements to the highfrequency package are documented online by the corresponding pull requests that can be accessed here: <https://github.com/jonathancornelissen/highfrequency/pulls?utf8=✓&q=is%3Apr+is%3Aclosed+author%3Aonnokleen+merged%3A%3E%3D2019-05-01+>

### Improvements implemented

Most of the work was done to improve the foundations of the package at its core. The highfrequency package subsumed different packages throughout its lifetime and, hence, the underlying basis was not unified and even some functionalities were implemented twice. 

The changes at its core and new features are listed in the following:

#### All new backend:
- Automated testing via testthat and coverage analysis via covr
- Automated documentation via roxygen2
- Travis-ci for automated (linux) build checks
  - Already in use for two merged pull requests
  - Pull requests were automatilly checked for inconsistency/incompatibility with the main repository.
- Rcpp instead of per-hand C calls
  - Already used by a pull request for adding harModel features

#### New features:
- Totally rewritten cleaning functions for WRDS tick data. They now support microseconds data run by a data.table
- Microsecond data compatibility for the spot voltility and spot drift estimator
- Updated references and realized_library data set

### Postive experiences

- Working and communicating with great mentors
- During the time of GSoC there was a pull request from another author which got merged into the main repository through my fork. Hence, this was the first time I was able to "manage" an outside pull request. This was a great experience for further collaboration on this open source project.
- Setting up the automated test environment was of great help (and fun) for keeping the project almost continuatively to a quality standard similar to CRAN submissions. 

### Difficulties

- Porting the old C code to Rcpp needed way more time than I thought, costing me time that was reserved for improving the documentation. 
- Hence, open points on my agenda are adding more vignettes which will be added in late September to October.
