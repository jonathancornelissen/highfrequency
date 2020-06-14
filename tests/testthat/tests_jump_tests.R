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
expect_equal(
  formatC(BNSjumpTest(sampleTData$PRICE, IVestimator= "minRV", 209, IQestimator = "medRQ", type= "linear", makeReturns = TRUE)$pvalue, digits = 0),
  c(PRICE = "0")
)


