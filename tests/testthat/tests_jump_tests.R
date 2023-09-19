library(testthat)
library(highfrequency)
library(xts)

# AJjumpTest ----------------------------------------------------
test_that("AJjumpTest unit test",{
  skip_on_cran()
  expect_equal(
    as.numeric(AJjumpTest(as.xts(sampleTData[,list(DT, PRICE)]), p = 2, k = 3, alignBy = "seconds", alignPeriod = 5, makeReturns = TRUE)[,1]),
    c(-0.63154307241, -0.03660544638)
  )
})



# JO jump test ------------------------------------------------------------
test_that("JO jump test unit test",{
  skip_on_cran()
  expect_equal(
    as.numeric(JOjumpTest(as.xts(sampleOneMinuteData)[,1], power = 6)$ztest),
    c(1.58839272530, -2.23596371826, 1.43940624574, -1.62044376089, -0.16071177517, 0.42242944882, -0.51764968793, -3.84231423189, 6.11221556860, 0.42155384715,
      -0.23981850002, 1.89611556565, -1.18434897661, 3.04518285659, -0.07857489704, 0.18955107699, -1.08415509224, -0.29925793964, 1.11070405246, 1.80433849107,
      1.59206104645, 1.95829451255)
  )
  
  expect_equal(
    as.numeric(JOjumpTest(as.xts(sampleOneMinuteData)[,1], power = 4)$ztest),
    c(1.82173389154, -2.39176475648, 1.01422852838, -1.43946546204, -0.19394671424, 0.29908656815, -0.37650298919, -2.66443521869, 2.80829134982, 0.43105071484, -0.26540328125, 
      1.38399907441, -1.44534259783, 3.20191265277, -0.05577322167, 0.18755879172, -1.21634307408, -0.31449501972, 1.22257863035, 1.70472241982, 1.51144383711, 1.49120113899)
  )
})


# BNSjumpTest -------------------------------------------------------------
test_that("BNSjumpTest", {
  skip_on_cran()
  expect_equal(
    as.numeric(BNSjumpTest(as.xts(sampleTData[, list(DT, PRICE)]), IVestimator= "rMinRVar", IQestimator = "rMedRQuar", type= "linear", makeReturns = TRUE)[, "p.value"]),
    c(1.322998188e-01, 2.816329921e-05)
  )
  expect_equal(
    as.numeric(BNSjumpTest(as.xts(sampleTData[, list(DT, PRICE)]), IVestimator= "rOWCov", IQestimator = "rMedRQuar", makeReturns = TRUE, type ="linear")[, "p.value"]),
    c(0,0) # Test statistic of 50+ - very zero
  )
  expect_equal(
    sum(as.numeric(BNSjumpTest(as.xts(sampleTData[, list(DT, PRICE)]), IVestimator= "CTBV", IQestimator = "CTTPV", makeReturns = TRUE, type ="ratio")[, "p.value"])),
    0
  )
  
})

# intradayJumpTest --------------------------------------------------------
test_that("LM test",{
  skip_on_cran()
  ## Extract the prices and set the time-zone to the local time-zone
  library(xts)
  dat <- sampleTData[as.Date(DT) == "2018-01-02", list(DT, PRICE)]
  
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
  skip_on_cran()
  dat <- sampleTData[, list(DT, PRICE)]
  FoFtest <- intradayJumpTest(pData = dat, volEstimator = "PARM", driftEstimator = "none", alpha = 0.95, RM = "bipower", 
                              theta = 1, lookBackPeriod = 50, marketOpen = "9:30:00", marketClose = "16:00:00", tz = "GMT")
  
  expect_equal(sum(FoFtest$ztest), -0.10230471)
  expect_equal(sum(FoFtest$vol$spot), 0.25932179)
  expect_equal(sum(FoFtest$drift), 0)
  
})


test_that("multiple intradayJumpTest", {
  skip_on_cran()
  jumpTest <- intradayJumpTest(pData = sampleOneMinuteData[, list(DT, PRICE = MARKET)], volEstimator = "RM", driftEstimator = "none", alpha = 0.95, RM = "bipower", 
                               lookBackPeriod = 10, dontIncludeLast = TRUE, on = "minutes", k = 5,
                               marketOpen = "9:30:00", marketClose = "16:00:00", tz = "GMT")
  
  p1 <- plot(jumpTest)
  expect_equal(p1$get_xlim(), c(999509400, 999532800))
  
  expect_true(jumpTest$isMultiDay)
  
})


# Rank jump test ----------------------------------------------------------
test_that("rank jump test", {
  skip_on_cran()
  expect_error(rankJumpTest(marketPrice = as.xts(sampleOneMinuteData)[,1], stockPrices = list(as.xts(sampleOneMinuteData)[,2])), "Singular value decomposition cannot be calculated")
  rjt <- rankJumpTest(marketPrice = as.xts(sampleOneMinuteData)[,1], stockPrices = list(as.xts(sampleOneMinuteData)[,2], as.xts(sampleOneMinuteData)[,2] * 1.05))
  
  expect_equal(rjt$jumpIndices, c(124, 448, 1284, 1537))
  
})