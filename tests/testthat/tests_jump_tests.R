library(testthat)
library(highfrequency)

context("AJjumpTest unit test")
test_that("AJjumpTest unit test",{
  expect_equal(
    formatC(AJjumpTest(sampleTData$PRICE, p = 2, k = 3, alignBy = "seconds", alignPeriod = 5, makeReturns = TRUE)$ztest, digits = 10),
    "-2.462012017"
  )
})


context("JO jump test unit test")
test_that("JO jump test unit test",{
  expect_equal(
    formatC(as.numeric(JOjumpTest(sample5MinPricesJumps[,1], power = 6)$ztest), digits = 10),
    c("0.2237433282","0.8492610224", "8.714153635", "-18.43875721",  "33.00286362",   "-1.12530156",   
      "-0.1693194718", "1.946341487", "-0.8662709953", "14.27443109", " 2.90820588", "5.505960335","-1.437705957",
      "-0.07068737283","0.7935449771", "-10.81545189", "1.577474946", "-0.09450737237","-3.262432421")
  )
  
  expect_equal(
    formatC(as.numeric(JOjumpTest(sample5MinPricesJumps[,1], power = 4)$ztest), digits = 4),
    c("0.2345","1.043", "6.265", "-13.97", " 29.4", "-0.9539", "-0.1651", "1.328", "-0.9089", "   17", "2.782", "4.522", "-1.585", "-0.0738", "0.636",
    "-11.33", "1.072", "-0.09093", "-2.554")
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
  dat <- sampleTDataMicroseconds[as.Date(DT) == "2018-01-02", list(DT, PRICE)]
  
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

