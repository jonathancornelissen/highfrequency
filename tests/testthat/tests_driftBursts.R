library("testthat")
library("data.table")
library("xts")

# DBH C++ error tests -----------------------------------------------------
test_that("DBH C++ error tests",{
  skip_on_cran()
  #these functions have broken before, hopefully they won't break again.
  expect_equal(highfrequency:::AsymptoticVarianceC(c(1:3), 3), NaN) 
  expect_equal(highfrequency:::AsymptoticVarianceC(c(1:3), 4), NaN)
  expect_equal(highfrequency:::AutomaticLagSelectionC(1:10, 30) , 7)
}
)


# DBH tests ---------------------------------------------------------------
test_that("DBH sim test", {
  skip_on_cran()
  set.seed(123)
  iT <- 23400
  meanBandwidth <- 300L
  timestamps <- seq(0, 23400, length.out = iT+1)
  testtimes  <- seq(60, 23400, 60L)
  r <- rnorm(iT, mean = 0.02, sd = 1) * sqrt(1/iT)
  p <- c(0,cumsum(r))
  dat <- data.table(DT = as.POSIXct(timestamps, tz = 'UTC', origin = as.POSIXct("1970-01-01", tz = 'UTC')), PRICE = exp(p))
  DBH <- driftBursts(dat, testtimes, preAverage = 1, ACLag = -1, meanBandwidth = meanBandwidth, varianceBandwidth = 5*meanBandwidth, parallelize = FALSE)
  DBH2 <- driftBursts(as.xts(dat), testtimes, preAverage = 1, ACLag = -1, meanBandwidth = meanBandwidth, varianceBandwidth = 5*meanBandwidth, parallelize = FALSE)
  
  expect_equal(as.numeric(DBH$tStat), as.numeric(DBH2$tStat))
  expect_equal(mean(DBH$tStat), 0.344636145)
  expect_equal(mean(DBH$sigma), 2.0548273e-05)
  expect_equal(mean(DBH$mu), 8.359077e-05)
  expect_equal(as.numeric(var(DBH$tStat)),0.774545510068)
  
  expect_equal(lapply(getCriticalValues(DBH, 0.99), round, digits = 4), list(normalizedQuantile = 3.9557, quantile = 4.1752))
  
  expect_output(print(DBH), regexp = NULL)
  
  

})
