library(testthat)
library(xts)
library(data.table)
library(highfrequency)
set.seed(123)
start <- strptime("1970-01-01", format = "%Y-%m-%d", tz = "UTC")
timestamps <- start + rep(seq(34200, 57600, length.out = 23401), 3) + rep(0:2 * 86400, each = 23401)

dat <- rbind(cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401)),
             cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401)),
             cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401)))

dat <- exp(cumsum(xts(dat, timestamps)))
colnames(dat) <- c("PRICE1", "PRICE2", "PRICE3")
datDT <- as.data.table(dat)
setnames(datDT, old = "index", new = "DT")


returnDat <- NULL
for (date in c("1970-01-01", "1970-01-02", "1970-01-03")) {
  returnDat <- rbind(returnDat, makeReturns(dat[date]))
}

returnDatDT <- as.data.table(returnDat)
setnames(returnDatDT, old = "index", new = "DT")





##### medRV #####
context("medRV")
test_that("medRV", {
  expect_equal(
    as.numeric(colSums(medRV(sampleOneMinuteData, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[,!"DT"])),
    c(0.003387052289,0.001451676499)
  )
  
  expect_equal(lapply(medRV(returnDat), sum), list("PRICE1" = 3.022290125, "PRICE2" = 3.003088657, "PRICE3" = 3.013543419))
  expect_equal(lapply(medRV(returnDat), sum), lapply(medRV(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(medRV(returnDat), ncol = 3), matrix(as.matrix(medRV(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(medRV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(medRV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(medRV(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(medRV(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  

})
##### medRQ ##### 
context("medRQ")
test_that("", {
  expect_equal(
    as.numeric(medRQ(as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]),alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000),
    c(0.010922500356, 0.003618836787)
  )
  expect_true(all.equal(medRQ(returnDatDT, alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE), medRQ(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)))
  expect_true(all.equal(medRQ(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) , medRQ(returnDat, alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)))
  expect_equal(lapply(medRQ(returnDat), sum), list("PRICE1" = 3.06573359, "PRICE2" = 3.010144579, "PRICE3" = 3.030828633))
  expect_equal(lapply(medRQ(returnDat), sum), lapply(medRQ(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(medRQ(returnDat), ncol = 3), matrix(as.matrix(medRQ(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(medRQ(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(medRQ(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(medRQ(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(medRQ(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
})
##### minRV ##### 
context("minRV")
test_that("minRV", {  
  expect_equal(
    as.numeric(colSums(minRV(sampleOneMinuteData, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, !"DT"])),
    c(0.003344205602,0.001438168861)
  )
  
  expect_equal(lapply(minRV(returnDat), sum), list("PRICE1" = 3.027820575, "PRICE2" = 2.99975133, "PRICE3" = 3.001113006))
  expect_equal(lapply(minRV(returnDat), sum), lapply(minRV(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(minRV(returnDat), ncol = 3), matrix(as.matrix(minRV(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(minRV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(minRV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(minRV(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(minRV(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
  
  
})
##### minRQ ##### 
context("minRQ")
test_that("minRQ", {
  expect_equal(
    as.numeric(minRQ(as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]), alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000),
    c(0.011852089820, 0.002546123569)
  )
  expect_equal(lapply(minRQ(returnDat), sum), list("PRICE1" = 3.0696895, "PRICE2" = 2.977093559, "PRICE3" = 3.01211734))
  expect_equal(lapply(minRQ(returnDat), sum), lapply(minRQ(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(minRQ(returnDat), ncol = 3), matrix(as.matrix(minRQ(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(minRQ(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(minRQ(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(minRQ(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(minRQ(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
})

##### MRC #####  
context("MRC")
test_that("MRC", {
  expect_equal({
    formatC(sum(MRC(list(as.xts(sampleOneMinuteData)["2001-08-04","MARKET"], as.xts(sampleOneMinuteData)["2001-08-04","STOCK"]), pairwise = TRUE, makePsd = TRUE)), digits = 5)
  },
  "0.00061674"
  )
  expect_equal({
    formatC(sum(MRC(list(as.xts(sampleOneMinuteData)["2001-08-04","MARKET"], as.xts(sampleOneMinuteData)["2001-08-04","STOCK"]), pairwise = FALSE, makePsd = TRUE)), digits = 5)
  },
  "0.00065676"
  )
  
  
})

##### rBeta #####
context("rBeta")
test_that("rBeta", {
  expect_equal({
    a <- as.xts(sampleOneMinuteData)["2001-08-04",1]
    b <- as.xts(sampleOneMinuteData)["2001-08-04",2]
    formatC(rBeta(a,b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE), digits = 5)
  },
  c(MARKET = "0.97877")
  )
  expect_equal({
    a <- as.xts(sampleOneMinuteData)["2001-08-04",1]
    b <- as.xts(sampleOneMinuteData)["2001-08-04",2]
    formatC(rBeta(a,b, RCOVestimator = "rOWCov", RVestimator = "medRV", makeReturns = TRUE), digits = 5)},
    c("1.0577")
  )
})

##### rBPCov ##### 
context("rBPCov")
test_that("rBPCov", {
  
  expect_equal(lapply(rBPCov(returnDat), sum), list("1970-01-01" = 2.98622886, "1970-01-02" = 3.026269417, "1970-01-03" = 2.99293847))
  expect_equal(lapply(rBPCov(returnDat), sum), lapply(rBPCov(dat, makeReturns = TRUE), sum))
  
  expect_equal(rBPCov(returnDat),  rBPCov(returnDatDT))
  expect_equal(rBPCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rBPCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rBPCov(sampleOneMinuteData, makeReturns = TRUE), rBPCov(as.xts(sampleOneMinuteData), makeReturns = TRUE))
  
  
})
##### RBPVar ##### 
context("RBPVar")
test_that("RBPVar", {
  
  if(!interactive()){ ## I don't want to test this everytime I manually run this script interactively
    expect_equal(
      formatC(sum(RBPVar(rData = diff(as.xts(sampleOneMinuteData))[-1,])), digits = 5),
      "150.48"
    )
  }
})
##### rCov #####
context("rCov")
test_that("rCov", {
  expect_equal(
    formatC(sum(rCov(rData = sampleOneMinuteData, makeReturns = TRUE)[[1]][1:2,1:2]), digits = 5),
    "0.00081828"
  )
  
  expect_true(all.equal(rCov(returnDatDT, alignBy = "minutes", alignPeriod = 5) ,rCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)))
  expect_true(all.equal(rCov(returnDat, alignBy = "minutes", alignPeriod = 5) ,rCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)))
  
  expect_equal(lapply(rCov(returnDat), sum), list("1970-01-01" = 2.971967832, "1970-01-02" = 3.032179598, "1970-01-03" = 3.012844796))
  expect_equal(lapply(rCov(returnDat), sum), lapply(rCov(dat, makeReturns = TRUE), sum))
  expect_equal(rCov(returnDat),  rCov(returnDatDT))
  expect_equal(rCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rCov(sampleOneMinuteData, makeReturns = TRUE), rCov(as.xts(sampleOneMinuteData), makeReturns = TRUE))
})

##### rKurt ##### 
context("rKurt")
test_that("rKurt", {
  expect_equal(
    as.numeric(colMeans(rKurt(sampleOneMinuteData, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)[, !"DT"])),
    c(5.357363079, 5.186198676)
  )
  expect_equal(sum(rKurt(returnDat[,1])), sum(rKurt(returnDat)[,1]))
  expect_equal(lapply(rKurt(returnDat), sum), list("PRICE1" = 8.963908299, "PRICE2" = 9.029035031, "PRICE3" = 9.063772367))
  expect_equal(lapply(rKurt(returnDat), sum), lapply(rKurt(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rKurt(returnDat), ncol = 3), matrix(as.matrix(rKurt(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rKurt(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rKurt(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rKurt(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rKurt(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
})

##### rMPV ##### 
context("rMPV")
test_that("rMPV", {
  expect_equal(
    as.numeric(rMPV(as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]), alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)),
    c(9.393123822e-05, 5.623885699e-05)
  )
  
  expect_equal(lapply(rMPV(returnDat), sum), list("PRICE1" = 3.016532757, "PRICE2" = 3.004313242, "PRICE3" = 3.002691466))
  expect_equal(lapply(rMPV(returnDat), sum), lapply(rMPV(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMPV(returnDat), ncol = 3), matrix(as.matrix(rMPV(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMPV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMPV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMPV(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rMPV(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
})
##### rOWCov ##### 
context("rOWCov")
test_that("rOWCov", {
  expect_equal(
    formatC(rOWCov(rData = as.xts(sampleOneMinuteData)["2001-08-04"], makeReturns = TRUE)[1,1], digits = 5),
    "0.00027182"
  )
  expect_equal(
    formatC(rOWCov(rData = as.xts(sampleOneMinuteData)["2001-08-04"], makeReturns = TRUE, wFunction = "SR")[1,1], digits = 3),
    "0.000276"
  )
})
##### rRTSCov ##### 
context("rRTSCov")
test_that("rRTSCov", {
  expect_equal(
    as.numeric(rRTSCov(pData = as.xts(sampleTDataMicroseconds[as.Date(DT) == "2018-01-02", list(DT, PRICE)])) * 10000),
    0.3681962867
  )
  expect_equal(
    formatC(sum(rRTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 1000000, digits = 5),
    "0.24729"
  )
})

##### rKernelCov ##### 
context("rKernelCov")
test_that("rKernelCov", {
  expect_equal(
    as.numeric(rKernelCov(rData = as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]), alignBy = "minutes",  alignPeriod = 5, makeReturns = TRUE)),
    c(1.253773e-04, 6.087867e-05)
  )
  expect_equal(
    formatC(sum(rKernelCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)), digits = 5),
    "0.0022276"
  )
  expect_equal(length(listAvailableKernels()) , 12)
  
  expect_equal(lapply(rKernelCov(returnDat), sum), list("1970-01-01" = 2.941858941, "1970-01-02" = 3.009886185, "1970-01-03" = 3.009261885))
  expect_equal(lapply(rKernelCov(returnDat), sum), lapply(rKernelCov(dat, makeReturns = TRUE), sum))
  expect_equal(rKernelCov(returnDat),  rKernelCov(returnDatDT))
  expect_equal(rKernelCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rKernelCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  
})

##### rSkew ##### 
context("rSkew")
test_that("rSkew", {
  expect_equal(
    as.numeric(colMeans(rSkew(sampleOneMinuteData, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)[,!"DT"])),
    c(0.3159246000,0.3930917059)
  )
  expect_equal(sum(rSkew(returnDat[,1])), sum(rSkew(returnDat)[,1]))
  expect_equal(lapply(rSkew(returnDat), sum), list("PRICE1" = 0.03781808902, "PRICE2" = 0.0146462315, "PRICE3" = 0.03046798416))
  expect_equal(lapply(rSkew(returnDat), sum), lapply(rSkew(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rSkew(returnDat), ncol = 3), matrix(as.matrix(rSkew(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rSkew(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rSkew(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rSkew(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rSkew(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
})
##### rSV ##### 
context("rSV")
test_that("rSV", {
  expect_equal(
    sum(rSV(as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]), alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)),
    0.000166891
  )
  expect_equal(lapply(rSV(returnDat), function(x) lapply(x, sum))[[1]], list("rSVdownside" = 1.501497027, "rSVupside" = 1.50206646))
  expect_equal(rSV(returnDat), rSV(dat, makeReturns = TRUE))
  
  expect_equal(rSV(returnDat), rSV(returnDatDT))
  expect_true(all.equal(rSV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rSV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), check.attributes = FALSE))

  
})
##### rThresholdCov ##### 
context("rThresholdCov")
test_that("rThresholdCov", {
  expect_equal(
    formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1)), digits = 5),
    "0.0015979"
  )
  expect_equal(
    formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1, cor = TRUE)), digits = 5),
    "3.2939"
  )
  
  expect_equal(lapply(rThresholdCov(returnDat), sum), list("1970-01-01" = 2.943885754, "1970-01-02" = 2.992550689, "1970-01-03" = 2.963583828))
  expect_equal(lapply(rThresholdCov(returnDat), sum), lapply(rThresholdCov(dat, makeReturns = TRUE), sum))
  expect_equal(rThresholdCov(returnDat),  rThresholdCov(returnDatDT))
  expect_equal(rThresholdCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rThresholdCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  expect_equal(rThresholdCov(as.xts(sampleOneMinuteData), makeReturns = TRUE),  rThresholdCov(sampleOneMinuteData, makeReturns = TRUE))
  
  
  
  
})



##### rTPQuar ##### 
context("rTPQuar")
test_that("rTPQuar", {
  expect_equal(
    as.numeric(rTPQuar(as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]),alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000),
    c(0.013510877, 0.002967281)
  )
  expect_equal(lapply(rTPQuar(returnDat), sum), list("PRICE1" = 3.023117658, "PRICE2" = 3.003898984, "PRICE3" = 2.966162109))
  expect_equal(lapply(rTPQuar(returnDat), sum), lapply(rTPQuar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rTPQuar(returnDat), ncol = 3), matrix(as.matrix(rTPQuar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rTPQuar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rTPQuar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rTPQuar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rTPQuar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
})

##### rTSCov univariate ##### 
context("rTSCov")
test_that("rTSCov univariate", {
  expect_equal(
    as.numeric(rTSCov(pData = as.xts(sampleTDataMicroseconds[as.Date(DT) == "2018-01-02", list(DT, PRICE)]))),
    0.0001097988
  )
})
##### rTSCov multivariate ##### 
context("rTSCov")
test_that("rTSCov multivariate", {
  expect_equal(
    formatC(sum(rTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 10000, digits = 5),
    "0.0024262"
  )
})
##### RV  #####
context("RV")
test_that("RV", {
  expect_equal(
    formatC(RV(makeReturns(as.xts(sampleTDataMicroseconds[as.Date(DT) == "2018-01-02", list(DT, PRICE)]))), digits = 5),
    "0.0001032"
  )
})

##### rQPVar  ##### 
context("rQPVar")
test_that("rQPVar", {
  expect_equal(
    as.numeric(colMeans(rQPVar(sampleOneMinuteData, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)[, -"DT"])) * 1000000,
    c(0.046268513319,0.007675935052)
  )
  
  expect_equal(lapply(rQPVar(returnDat), sum), list("PRICE1" = 3.018535259, "PRICE2" = 3.006780968, "PRICE3" = 2.95202662))
  expect_equal(lapply(rQPVar(returnDat), sum), lapply(rQPVar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rQPVar(returnDat), ncol = 3), matrix(as.matrix(rQPVar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rQPVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rQPVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rQPVar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rQPVar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
})

##### rQuar  ##### 
context("rQuar")
test_that("rQuar", {
  expect_equal(
    as.numeric(colMeans(rQuar(sampleOneMinuteData, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)[, -"DT"])) * 1000000,
    c(0.05486143300,0.01361880467)
  )
  
  expect_equal(lapply(rQuar(returnDat), sum), list("PRICE1" = 2.997549025, "PRICE2" = 3.030800189, "PRICE3" = 3.055064757))
  expect_equal(lapply(rQuar(returnDat), sum), lapply(rQuar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rQuar(returnDat), ncol = 3), matrix(as.matrix(rQuar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rQuar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rQuar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rQuar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rQuar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
})

##### ivInference #####
context("ivInference")
test_that("ivInference", {
  expect_equal(
    formatC(ivInference(as.xts(sampleTDataMicroseconds[, list(DT, PRICE)]), IVestimator= "minRV", IQestimator = "medRQ", 
                        confidence = 0.95, makeReturns = TRUE)[[1]]$cb * 10000, digits = 5),
    c("0.84827", "1.0328")
  )
})

##### rAVGCov #####
context("rAVGCov")
test_that("rAVGCov",{
  rcovSub <- rAVGCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes",alignPeriod = 5, k = 1, makeReturns = FALSE)
  expect_equal(as.numeric(rcovSub), c(0.0005884480, 0.0004312966, 0.0004312966, 0.0006857147))
  # Correct handling of seconds?
  rcovSubSeconds <- rAVGCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "seconds",alignPeriod = 5 * 60 , k = 60 , makeReturns = FALSE)
  expect_equal(rcovSub , rcovSubSeconds)
  rcovSubUnivariate <- rAVGCov(rData = cbind(lltc, sbux, fill = 0)[,1], alignBy = "minutes",alignPeriod = 5, makeReturns = FALSE)
  expect_equal(rcovSub[[1]], rcovSubUnivariate)
  rcovSub_makeReturns <- rAVGCov(rData = exp(cumsum(cbind(lltc, sbux, fill = 0))), alignBy = "minutes",alignPeriod = 5, k = 1, makeReturns = TRUE)
  expect_equal(as.numeric(rcovSub_makeReturns) , c(0.0005881636, 0.0004307105,0.0004307105, 0.0005710761))
  # Correct handling of fractional minute specification.
  rcovSub <- rAVGCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes",alignPeriod = 2.5, k = 0.5, makeReturns = FALSE)
  expect_equal(as.numeric(rcovSub), c(0.0005945100, 0.0003883607, 0.0003883607, 0.0006608860))
  # We the fast alignment is not a factor of the slow alignment period
  expect_error(rAVGCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes",alignPeriod = 2.75, k = 0.5, makeReturns = FALSE))
  
  
  expect_equal(lapply(rAVGCov(returnDat), sum), list("1970-01-01" = 2.6334856, "1970-01-02" = 2.491597803, "1970-01-03" = 2.974218965))
  expect_equal(rAVGCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rAVGCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  
  
  
  
})
##### rCholCov #####
context("rCholCov")
test_that("rCholCov", {
  
  set.seed(123)
  iT <- 23400
  
  rets <- mvtnorm::rmvnorm(iT * 3 + 1, mean = rep(0,4), 
                           sigma = matrix(c(1, -0.5 , 0.7, 0.8,
                                            -0.5, 3, -0.4, 0.7,
                                            0.7, -0.4, 2, 0.6,  
                                            0.8, 0.7, 0.6, 4), ncol = 4))
  
  w1 <- rets[,1]
  w2 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.75)), 2]
  w3 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.65)), 3]
  w4 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.8)), 4] # Here we make asset 4 the second most liquid asset, which will function to test the ordering
  
  timestamps1 <- seq(34200, 57600, length.out =  length(w1))
  timestamps2 <- seq(34200, 57600, length.out =  length(w2))
  timestamps3 <- seq(34200, 57600, length.out =  length(w3))
  timestamps4 <- seq(34200, 57600, length.out =  length(w4))
  
  
  p1  <- xts(cumsum(w1) * c(0,sqrt(diff(timestamps1) / (max(timestamps1) - min(timestamps1)))), as.POSIXct(timestamps1, origin = "1970-01-01"))
  p2  <- xts(cumsum(w2) * c(0,sqrt(diff(timestamps2) / (max(timestamps2) - min(timestamps2)))), as.POSIXct(timestamps2, origin = "1970-01-01"))
  p3  <- xts(cumsum(w3) * c(0,sqrt(diff(timestamps3) / (max(timestamps3) - min(timestamps3)))), as.POSIXct(timestamps3, origin = "1970-01-01"))
  p4  <- xts(cumsum(w4) * c(0,sqrt(diff(timestamps4) / (max(timestamps4) - min(timestamps4)))), as.POSIXct(timestamps4, origin = "1970-01-01"))
  
  rCC <- rCholCov(list("market" = p1, "stock1" = p2, "stock2" =p3 , "stock3" = p4))
  
  expect_equal(colnames(rCC$CholCov) , c("market", "stock3", "stock1", "stock2"))
  expect_equal(round(as.numeric(rCC$CholCov), 6) , round(c(0.9719097, 0.7821389, -0.3605819,  0.5238333, 0.7821389, 4.5218663,  0.5850890,
                                                           0.3575620, -0.3605819, 0.5850890,  2.4545129, -0.4106370, 0.5238333, 0.3575620,
                                                           -0.4106370,  1.6889769) , 6))
  
  
  
  expect_equal(colnames(rCC$L), colnames(rCC$G))
  
  
})

##### rSemiCov #####
context("rSemiCov")
test_that("rSemiCov", {
  rSC <- rSemiCov(sampleOneMinuteData, makeReturns = TRUE)
  mixed <- do.call(rbind, lapply(rSC, function(x) x[["mixed"]][1,2]))
  neg <- do.call(rbind, lapply(rSC, function(x) x[["negative"]][1,2]))
  pos <- do.call(rbind, lapply(rSC, function(x) x[["positive"]][1,2]))
  concordant <- do.call(rbind, lapply(rSC, function(x) x[["concordant"]][1,2]))
  # Test whether we have zeros on the diagonal of the mixed covariance matrix
  expect_equal(as.numeric(diag((rSC[[1]][['mixed']]))) , numeric(nrow(rSC[[1]][['mixed']])))
  
  
  rCv <- rCov(sampleOneMinuteData, makeReturns = TRUE)
  realized <- do.call(rbind, lapply(rCv, function(x) x[1,2]))
  # Test whether the realized covariance is equal to the sum of the decomposed realized semicovariances.
  expect_equal(realized , (mixed + neg + pos))
  
  
  expect_equal(lapply(rSemiCov(returnDat) , function(x) sum(as.numeric(lapply(x[1:3], sum)))),  lapply(rCov(returnDat), sum))
  expect_equal(rSemiCov(returnDat), rSemiCov(dat, makeReturns = TRUE))
  expect_equal(rSemiCov(returnDat),  rSemiCov(returnDatDT))
  expect_equal(rSemiCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rSemiCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  
  expect_equal(rSemiCov(as.xts(sampleOneMinuteData), makeReturns = TRUE),
               rSemiCov(sampleOneMinuteData, makeReturns = TRUE))
  
  
})




##### ReMeDI #####
context("ReMeDI")
test_that("ReMeDI Estimation matches expected output", { # We thank Merrick li for contributing Matlab code.
  # print("Make sure to implement tests for correctTime = TRUE") ## When it becomes relevant.
  #remed <- ReMeDI(sampleTDataMicroseconds, correctTime = FALSE, lags = 0:25, kn = 2) ##Changed due to correctTime bug
  remed <- ReMeDI(sampleTDataMicroseconds[, list(DT, PRICE = log(PRICE))], lags = 0:25, kn = 2)

  expected <- c(5.391986e-10,  3.873739e-09,  4.261547e-09,  3.118519e-09,  1.538245e-09,  6.805792e-10, -3.835125e-10, -2.232302e-10, -1.157490e-10, -1.110401e-09, -1.934303e-09,
  -2.685536e-09, -3.174416e-09, -2.839272e-09, -1.163387e-09, -4.468208e-10,  7.741021e-11,  1.093390e-09,  1.071914e-09,  1.360021e-09,  1.237765e-09, -1.382685e-10,
  -9.406390e-10, -1.695860e-09, -1.667980e-09, -1.982078e-10)
  
  
  expect_equal(remed, expected)

  # Same data-set but xts format
  dat <- sampleTDataMicroseconds[, list(DT, PRICE = log(PRICE))]
  #remed <- ReMeDI(dat, correctTime = FALSE, jumpsIndex = NULL, lags = 0:25, kn = 4) ##Changed due to correctTime bug
  remed <- ReMeDI(as.xts(dat), lags = 0:25, kn = 2)
  
  

  expect_equal(remed, expected)


})

test_that("ReMeDI lag choosing algorithm chooses the correct values", {

  # optimalKn <- knChooseReMeDI(sampleTDataMicroseconds, correctTime = FALSE, jumpsIndex = NULL, knMax = 10, tol = 0.05, size = 3, lower = 1, upper = 10, plot = FALSE)##Changed due to correctTime bug
  optimalKn <- knChooseReMeDI(sampleTDataMicroseconds, knMax = 10, tol = 0.05, size = 3, lower = 1, upper = 10, plot = FALSE)
  expect_equal(optimalKn, 1L)

  
  
  # optimalKn <- knChooseReMeDI(dat, correctTime = FALSE, jumpsIndex = NULL, knMax = 10, tol = 0.05, size = 3, lower = 3, upper = 5, plot = FALSE) ##Changed due to correctTime bug
  optimalKn <- knChooseReMeDI(sampleTDataMicroseconds[as.Date(DT) == "2018-01-02"], knMax = 10, tol = 0.05, size = 3, lower = 3, upper = 5, plot = FALSE)
  expect_equal(optimalKn, 5L)

})


test_that("ReMeDI asymptotic variance gives same result as Merrick Li's code", {
  dat <- sampleTDataMicroseconds[, list(DT, PRICE = log(PRICE))]
  avar <- ReMeDIAsymptoticVariance(dat, phi = 0.5, lags = 0:10, kn = 3, i = 1)
  remed <- ReMeDI(dat, 3, 0:10)
  expected <- c(4.420651e-13, 2.083090e-13, 6.915452e-14, 1.867677e-14, 5.907554e-14, 5.129904e-14,
                 1.399077e-14, 1.433821e-14, 3.475069e-14, 2.972763e-14, 2.250913e-14)
  expect_equal(avar$asympVar, expected)
  ## We correctly calculate the remedi
  expect_equal(avar$ReMeDI, remed)
  
  
  avar <- ReMeDIAsymptoticVariance(dat, phi = 1, lags = 0:10, kn = 6, i = 2)  
  expected <- c(2.637164e-12, 1.613527e-12, 1.057763e-12, 6.532975e-13, 4.462853e-13, 3.568027e-13, 2.155630e-13, 
                2.985189e-13, 1.176854e-13, 6.341883e-14, 7.769585e-16)
  expect_equal(avar$asympVar, expected)
  
})
