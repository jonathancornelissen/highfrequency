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
test_that("intradayJumpTest",{
  ## Extract the prices and set the time-zone to the local time-zone
  ## thus we should be able to run this test on computers in other countires with reproducible results
  library(xts)
  dat <- sample5MinPricesJumps["2010-01-04", 1]
  tzone(dat) = Sys.timezone()
  storage.mode(dat) <- "numeric"
  plot(dat)
  jumpTest <- intradayJumpTest(pData = dat, testType = "LM", windowSize = 5, K = 10, testingTimes = seq(34200 + 10 * 300, 57600, 300))
  P1 <- plot(jumpTest)
  lims <- P1$get_xlim()
  expect_equal(
    lims[1], as.numeric(as.POSIXlt("2010-01-04 10:30:00"))  
  )
  expect_equal(
    lims[2], as.numeric(as.POSIXlt("2010-01-04 17:00:00"))  
  )
  # In the five minute jump case we expect the data itself to be returned
  expect_true(all(jumpTest$tests[,1] == dat))
  
})