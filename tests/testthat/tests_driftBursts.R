library(testthat)
library(data.table)
context("DBH C++ error tests")
test_that("DBH C++ error tests",{
  #these functions has broken before, hopefully they won't break again.
  expect_equal(highfrequency:::AsymptoticVarianceC(c(1:3), 3), NaN) 
  expect_equal(highfrequency:::AsymptoticVarianceC(c(1:3), 4), NaN)
  expect_equal(highfrequency:::AutomaticLagSelectionC(1:10, 30) , 7)
}
)

context("DBH test")
test_that("DBH sim test", {
  set.seed(123)
  iT <- 23400
  meanBandwidth <- 300L
  timestamps <- seq(0, 23400, length.out = iT+1)
  testtimes  <- seq(60, 23400, 60L)
  r <- rnorm(iT, mean = 0.02, sd = 1) * sqrt(1/iT)
  p <- c(0,cumsum(r))
  randomTZ  <-  sample(OlsonNames(), 1)
  dat <- data.table(DT = as.POSIXct(timestamps, tz = randomTZ, origin = as.POSIXct("1970-01-01", tz = randomTZ)), PRICE = exp(p))
  DBH <- driftBursts(dat, testtimes, preAverage = 1, ACLag = -1, meanBandwidth = meanBandwidth, varianceBandwidth = 5*meanBandwidth, parallelize = FALSE)
  
  
  plot(DBH)
  plot(DBH, price = p, timestamps = timestamps)
  expect_equal(mean(DBH$driftBursts), 0.34463614)
  expect_equal(mean(DBH$sigma), 2.054827e-05)
  expect_equal(mean(DBH$mu), 8.359077e-05)
  
  
  
  
})
