## Test environments
* local OS X install, R 4.3.1
* linux and windows server via devtools::check_rhub()
* windows via devtools::check_win_devel()

## R CMD check results

0 errors | 0 warnings | 0 notes

For some platforms we might get a note regarding the size of the package such as

* checking installed package size ... NOTE
  installed size is  7.2Mb
  sub-directories of 1Mb or more:
    data   6.3Mb
    
The installed size is necessary due to the use of highfrequency intraday data.

Sometimes there might be notes about misspelled words but our names are spelled like this.

Possibly misspelled words in DESCRIPTION:
  Boudt (20:8)
  Kleen (20:15)
  Sjoerup (20:26)