library(testthat)
library(highfrequency)

context("AJjumpTest unit test")
test_that("AJjumpTest unit test",{
  expect_equal(
    formatC(AJjumpTest(sampleTData$PRICE, p = 2, k = 3, alignBy = "seconds", alignPeriod = 5, makeReturns = TRUE)$ztest, digits = 10),
    "-2.903324467"
  )
})


context("JO jump test unit test")
test_that("JO jump test unit test",{
  expect_equal(
    formatC(JOjumpTest(sample5MinPricesJumps[,1], power = 6)$ztest, digits = 10),
    "8.998787842"
  )
  
  expect_equal(
    formatC(JOjumpTest(sample5MinPricesJumps[,1], power = 4)$ztest, digits = 4),
    "7.832"
  )
})

context("BNSjumpTest")
test_that("BNSjumpTest", {
  expect_equal(
    formatC(BNSjumpTest(sampleTData$PRICE, IVestimator= "minRV", 209, IQestimator = "medRQ", type= "linear", makeReturns = TRUE)$pvalue, digits = 0),
    c(PRICE = "0")
  )
})

context("intradayJumpTest")
test_that("LM test",{
  ## Extract the prices and set the time-zone to the local time-zone
  library(xts)
  dat <- sampleTDataMicroseconds[as.Date(DT) == "2018-01-02", .(DT, PRICE)]
  
  jumpTest <- intradayJumpTest(pData = dat, volEstimator = "RM", driftEstimator = "none", alpha = 0.95, RM = "bipower", 
                               lookBackPeriod = 10, dontIncludeLast = TRUE, on = "minutes", k = 5,
                               marketOpen = "9:30:00", marketClose = "16:00:00", tz = "GMT")
  
  P1 <- plot(jumpTest)
  lims <- P1$get_xlim()
  expect_equal(
    lims[1], as.numeric(as.POSIXlt(dat[1,DT]))  
  )
  expect_equal(
    lims[2], as.numeric(as.POSIXlt(dat[nrow(dat),DT]))  
  )
  
  
})

test_that("FoF test",{
  dat <- sampleTData$PRICE
  tzone(dat) <- "GMT"
  storage.mode(dat) <- "numeric"
  FoFtest <- intradayJumpTest(pData = dat, volEstimator = "PARM", driftEstimator = "none", alpha = 0.95, RM = "bipower", 
                              theta = 1, lookBackPeriod = 50, marketOpen = "9:30:00", marketClose = "16:00:00", tz = "GMT")
  
  
  P1 <- plot(FoFtest)
  lims <- P1$get_xlim()
  
  expect_equal(
    lims[1], as.numeric(index(dat))[1]
  )
  expect_equal(
    lims[2], as.numeric(index(dat))[nrow(dat)]
  )
  
})