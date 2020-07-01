library(testthat)
library(highfrequency)
context("medRV")
test_that("", {
expect_equal(
  formatC(as.numeric(medRV(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[1]), digits = 5),
  "0.013105"
)
})
# medRQ
context("medRQ")
test_that("", {
  expect_equal(
    formatC(medRQ(sampleTData$PRICE,alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000, digits = 5),
    "0.23645"
  )
})
# minRV
context("minRV")
test_that("minRV", {  
  expect_equal(
    formatC(as.numeric(minRV(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[1,1]), digits = 5),
    "0.013259"
  )
})
# minRQ
context("minRQ")
test_that("minRQ", {
  expect_equal(
    minRQ(sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000,
    0.13061613812613856456
  )
})

# MRC
context("MRC")
test_that("MRC", {
  expect_equal({
    formatC(sum(MRC(list(sample5MinPricesJumps["2010-01-04",1], sample5MinPricesJumps["2010-01-04",2]), pairwise = TRUE, makePsd = TRUE)), digits = 5)
    },
    "0.031692"
  )
  expect_equal({
    formatC(sum(MRC(list(sample5MinPricesJumps["2010-01-04",1], sample5MinPricesJumps["2010-01-04",2]), pairwise = FALSE, makePsd = TRUE)), digits = 5)
    },
    "0.034393"
  )
})

# rBeta
context("rBeta")
test_that("rBeta", {
  expect_equal({
    a <- sample5MinPricesJumps['2010-01-04', 1]
    b <- sample5MinPricesJumps['2010-01-04', 2]
    formatC(rBeta(a,b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE), digits = 5)
    },
    c(stock2 = "1.4318")
  )
  expect_equal({
    a <- sample5MinPricesJumps['2010-01-04', 1]
    b <- sample5MinPricesJumps['2010-01-04', 2]
    formatC(rBeta(a,b, RCOVestimator = "rOWCov", RVestimator = "medRV", makeReturns = TRUE), digits = 5)},
    c("1.2885")
  )
})

# rBPCov
context("rBPCov")
test_that("rBPCov", {
  expect_equal(
    formatC(sum(rBPCov(rData = sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns=TRUE)[[1]][1:2,1:2]) * 1000000, digits = 5),
    formatC(sum(rBPCov(rData = sample5MinPricesJumps[c('2010-01-04'), 1:2], makeReturns=TRUE)[1:2,1:2]) * 1000000, digits = 5),
    "29778"
  )
})
# RBPVar
context("RBPVar")
test_that("RBPVar", {
  expect_equal(
    formatC(sum(RBPVar(rData = sample5MinPricesJumps['2010-01-04', 1])), digits = 5),
    "118.93"
  )
})
# rCov
context("rCov")
test_that("rCov", {
  expect_equal(
    formatC(sum(rCov(rData = sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns = TRUE)[[1]][1:2,1:2]), digits = 5),
    "0.028614"
  )
})

# rKurt
context("rKurt")
test_that("rKurt", {
  expect_equal(
    formatC(as.numeric(rKurt(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1], digits = 5),
    "2.8936"
  )
})

# rMPV
context("rMPV")
test_that("rMPV", {
  expect_equal(
    formatC(rMPV(sampleTData$PRICE, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE), digits = 5),
    "0.00042328"
  )
})
# rOWCov
context("rOWCov")
test_that("rOWCov", {
  expect_equal(
    formatC(rOWCov(rData = sample5MinPricesJumps['2010-01-04'], makeReturns = TRUE)[1,1], digits = 5),
    "0.010039"
  )
  expect_equal(
    formatC(rOWCov(rData = sample5MinPricesJumps['2010-01-04'], makeReturns = TRUE, wFunction = "SR")[1,1], digits = 3),
    "0.0101"
  )
})
# rRTSCov
context("rRTSCov")
test_that("rRTSCov", {
  expect_equal(
    formatC(rRTSCov(pData = sampleTData$PRICE) * 10000, digits = 5),
    "3.5276"
  )
  expect_equal(
    formatC(sum(rRTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 1000000, digits = 5),
    "0.24729"
  )
})

# rKernel
context("rKernelCov")
test_that("rKernelCov", {
  expect_equal(
    formatC(rKernelCov(rData = sampleTData$PRICE, alignBy = "minutes",  alignPeriod = 5, makeReturns = TRUE), digits = 5),
    "0.00059605"
  )
  expect_equal(
    formatC(sum(rKernelCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)) * 1000, digits = 5),
    "0.021378"
  )
})

# rSkew
context("rSkew")
test_that("rSkew", {
  expect_equal(
    formatC(as.numeric(rSkew(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1], digits = 5),
    "0.078047"
  )
})
# rSV
context("rSV")
test_that("rSV", {
  expect_equal({
    rSV(sampleTData$PRICE,alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)},
    list(rSVdownside = 0.00029125752237359241398, rSVupside = 0.00016310984360246060573)
  )
})
# rThresholdCov
context("rThresholdCov")
test_that("rThresholdCov", {
  expect_equal(
    formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1)) * 10000, digits = 5),
    "0.59455"
  )
  expect_equal(
    formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1), cor = TRUE), digits = 1),
    " 1"
  )
})
# rTPVar
context("rTPVar")
test_that("rTPVar", {
  expect_equal(
    formatC(rTPVar(sampleTData$PRICE,alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000, digits = 5),
    "0.41334"
  )
})
# rTSCov univariate
context("rTSCov")
test_that("rTSCov", {
  expect_equal(
    formatC(rTSCov(pData = sampleTData$PRICE), digits = 5),
    "0.00052769"
  )
})
# rTSCov multivariate
context("rTSCov")
test_that("rTSCov", {
  expect_equal(
    formatC(sum(rTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 10000, digits = 5),
    "0.0024262"
  )
})
# RV
context("RV")
test_that("RV", {
  expect_equal(
    formatC(RV(makeReturns(sampleTData$PRICE)), digits = 5),
    "0.00064381"
  )
})

# rQPVar
context("rQPVar")
test_that("rQPVar", {
  expect_equal(
    formatC(as.numeric(rQPVar(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
    "115.62"
  )
})

# rQuar
context("rQuar")
test_that("rQuar", {
  expect_equal(
    formatC(as.numeric(rQuar(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
    "116.57"
  )
})
  
  
context("ivInference")
test_that("ivInference", {
  expect_equal(
    formatC(ivInference(sampleTData$PRICE, IVestimator= "minRV", IQestimator = "medRQ", confidence = 0.95, makeReturns = TRUE)$cb * 10000, digits = 5),
    c("4.7594", "5.7472")
  )
})


context("refreshTime")
test_that("refreshTime", {
  
  # Unit test for the refreshTime algorithm based on Kris' example in http://past.rinfinance.com/agenda/2015/workshop/KrisBoudt.pdf
  #suppose irregular timepoints: 
  start = as.POSIXct("2010-01-01 09:30:00") 
  ta = start + c(1,2,4,5,9,14); 
  tb = start + c(1,3,6,7,8,9,10,11,15); 
  tc = start + c(1,2,3,5,7,8,10,13); 
  a = as.xts(1:length(ta),order.by=ta); 
  b = as.xts(1:length(tb),order.by=tb);
  c = as.xts(1:length(tc),order.by=tc); 
  #Calculate the synchronized timeseries: 
  
  expect_equal(refreshTime(list(a,b,c)),
  xts(matrix(c(1,1,1,
               2,2,3,
               4,3,4,
               5,6,6,
               6,8,8), ncol = 3, byrow = TRUE), order.by = start + c(1,3,6,9,14)))
  
  
})


