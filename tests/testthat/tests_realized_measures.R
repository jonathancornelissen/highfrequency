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
    formatC(as.numeric(medRV(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[1]), digits = 5),
    "0.013105"
  )
  
  expect_equal(lapply(medRV(returnDat), sum), list("PRICE1" = 3.022290125, "PRICE2" = 3.003088657, "PRICE3" = 3.013543419))
  expect_equal(lapply(medRV(returnDat), sum), lapply(medRV(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(medRV(returnDat), ncol = 3), matrix(as.matrix(medRV(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(medRV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(medRV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(medRV(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
    matrix(medRV(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  

})
##### medRQ ##### 
context("medRQ")
test_that("", {
  expect_equal(
    formatC(medRQ(sampleTData$PRICE,alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000, digits = 5),
    "0.23952"
  )
  
  expect_equal(lapply(medRQ(returnDat), sum), list("PRICE1" = 3.06573359, "PRICE2" = 3.010144579, "PRICE3" = 3.030828633))
  expect_equal(lapply(medRQ(returnDat), sum), lapply(medRQ(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(medRQ(returnDat), ncol = 3), matrix(as.matrix(medRQ(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(medRQ(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(medRQ(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(medRQ(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(medRQ(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
})
##### minRV ##### 
context("minRV")
test_that("minRV", {  
  expect_equal(
    formatC(as.numeric(minRV(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[1,1]), digits = 5),
    "0.013259"
  )
  
  expect_equal(lapply(minRV(returnDat), sum), list("PRICE1" = 3.027820575, "PRICE2" = 2.99975133, "PRICE3" = 3.001113006))
  expect_equal(lapply(minRV(returnDat), sum), lapply(minRV(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(minRV(returnDat), ncol = 3), matrix(as.matrix(minRV(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(minRV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(minRV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(minRV(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(minRV(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
  
})
##### minRQ ##### 
context("minRQ")
test_that("minRQ", {
  expect_equal(
    minRQ(sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000,
    0.1325120781
  )
  expect_equal(lapply(minRQ(returnDat), sum), list("PRICE1" = 3.0696895, "PRICE2" = 2.977093559, "PRICE3" = 3.01211734))
  expect_equal(lapply(minRQ(returnDat), sum), lapply(minRQ(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(minRQ(returnDat), ncol = 3), matrix(as.matrix(minRQ(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(minRQ(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(minRQ(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(minRQ(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(minRQ(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
})

##### MRC #####  
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

##### rBeta #####
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

##### rBPCov ##### 
context("rBPCov")
test_that("rBPCov", {
  expect_equal(
    formatC(sum(rBPCov(rData = sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns=TRUE)[[1]][1:2,1:2]) * 1000000, digits = 5),
    formatC(sum(rBPCov(rData = sample5MinPricesJumps[c('2010-01-04'), 1:2], makeReturns=TRUE)[1:2,1:2]) * 1000000, digits = 5),
    "29778"
  )
  
  expect_equal(lapply(rBPCov(returnDat), sum), list("1970-01-01" = 2.98622886, "1970-01-02" = 3.026269417, "1970-01-03" = 2.99293847))
  expect_equal(lapply(rBPCov(returnDat), sum), lapply(rBPCov(dat, makeReturns = TRUE), sum))
  
  expect_equal(rBPCov(returnDat),  rBPCov(returnDatDT))
  expect_equal(rBPCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rBPCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rBPCov(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE),  rBPCov(sample5MinPrices, makeReturns = TRUE))
  
  
  
})
##### RBPVar ##### 
context("RBPVar")
test_that("RBPVar", {
  
  if(!interactive()){ ## I don't want to test this everytime I manually run this script interactively
    expect_equal(
      formatC(sum(RBPVar(rData = sample5MinPricesJumps['2010-01-04', 1])), digits = 5),
      "118.93"
    )
  }
})
##### rCov #####
context("rCov")
test_that("rCov", {
  expect_equal(
    formatC(sum(rCov(rData = sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns = TRUE)[[1]][1:2,1:2]), digits = 5),
    "0.028614"
  )
  
  
  expect_equal(lapply(rCov(returnDat), sum), list("1970-01-01" = 2.971967832, "1970-01-02" = 3.032179598, "1970-01-03" = 3.012844796))
  expect_equal(lapply(rCov(returnDat), sum), lapply(rCov(dat, makeReturns = TRUE), sum))
  expect_equal(rCov(returnDat),  rCov(returnDatDT))
  expect_equal(rCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rCov(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE),  rCov(sample5MinPrices, makeReturns = TRUE))
  
})

##### rKurt ##### 
context("rKurt")
test_that("rKurt", {
  expect_equal(
    formatC(as.numeric(rKurt(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1], digits = 5),
    "2.8936"
  )
  expect_equal(sum(rKurt(returnDat[,1])), sum(rKurt(returnDat)[,1]))
  expect_equal(lapply(rKurt(returnDat), sum), list("PRICE1" = 8.963908299, "PRICE2" = 9.029035031, "PRICE3" = 9.063772367))
  expect_equal(lapply(rKurt(returnDat), sum), lapply(rKurt(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rKurt(returnDat), ncol = 3), matrix(as.matrix(rKurt(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rKurt(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rKurt(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rKurt(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(rKurt(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
  
})

##### rMPV ##### 
context("rMPV")
test_that("rMPV", {
  expect_equal(
    formatC(rMPV(sampleTData$PRICE, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE), digits = 5),
    "0.00042925"
  )
  
  expect_equal(lapply(rMPV(returnDat), sum), list("PRICE1" = 3.016532757, "PRICE2" = 3.004313242, "PRICE3" = 3.002691466))
  expect_equal(lapply(rMPV(returnDat), sum), lapply(rMPV(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMPV(returnDat), ncol = 3), matrix(as.matrix(rMPV(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMPV(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMPV(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMPV(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(rMPV(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
})
##### rOWCov ##### 
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
##### rRTSCov ##### 
context("rRTSCov")
test_that("rRTSCov", {
  expect_equal(
    formatC(rRTSCov(pData = sampleTData$PRICE) * 10000, digits = 5),
    "3.5676"
  )
  expect_equal(
    formatC(sum(rRTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 1000000, digits = 5),
    "0.24729"
  )
})

##### rKernel ##### 
context("rKernelCov")
test_that("rKernelCov", {
  expect_equal(
    formatC(rKernelCov(rData = sampleTData$PRICE, alignBy = "minutes",  alignPeriod = 5, makeReturns = TRUE), digits = 5),
    "0.00060559"
  )
  expect_equal(
    formatC(sum(rKernelCov(rData = cbind(lltc, sbux, fill = 0), alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)) * 1000, digits = 5),
    "0.021281"
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
    formatC(as.numeric(rSkew(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1], digits = 5),
    "0.078047"
  )
  expect_equal(sum(rSkew(returnDat[,1])), sum(rSkew(returnDat)[,1]))
  expect_equal(lapply(rSkew(returnDat), sum), list("PRICE1" = 0.03781808902, "PRICE2" = 0.0146462315, "PRICE3" = 0.03046798416))
  expect_equal(lapply(rSkew(returnDat), sum), lapply(rSkew(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rSkew(returnDat), ncol = 3), matrix(as.matrix(rSkew(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rSkew(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rSkew(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rSkew(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(rSkew(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
})
##### rSV ##### 
context("rSV")
test_that("rSV", {
  expect_equal(
    rSV(sampleTData$PRICE,alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE),
    list(rSVdownside = c("PRICE" = 0.0002913423039), rSVupside = c("PRICE" = 0.0001642838323))
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
    formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1)) * 10000, digits = 5),
    "0.59423"
  )
  expect_equal(
    formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1), cor = TRUE), digits = 1),
    " 1"
  )
  
  expect_equal(lapply(rThresholdCov(returnDat), sum), list("1970-01-01" = 2.943885754, "1970-01-02" = 2.992550689, "1970-01-03" = 2.963583828))
  expect_equal(lapply(rThresholdCov(returnDat), sum), lapply(rThresholdCov(dat, makeReturns = TRUE), sum))
  expect_equal(rThresholdCov(returnDat),  rThresholdCov(returnDatDT))
  expect_equal(rThresholdCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rThresholdCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rThresholdCov(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE),  rThresholdCov(sample5MinPrices, makeReturns = TRUE))
  
  
  
  
})
##### rTPVar ##### 
context("rTPVar")
test_that("rTPVar", {
  expect_equal(
    formatC(rTPVar(sampleTData$PRICE,alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000, digits = 5),
    "0.42755"
  )
  expect_equal(lapply(rTPVar(returnDat), sum), list("PRICE1" = 3.023117658, "PRICE2" = 3.003898984, "PRICE3" = 2.966162109))
  expect_equal(lapply(rTPVar(returnDat), sum), lapply(rTPVar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rTPVar(returnDat), ncol = 3), matrix(as.matrix(rTPVar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rTPVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rTPVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rTPVar(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(rTPVar(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
})
##### rTSCov univariate ##### 
context("rTSCov")
test_that("rTSCov univariate", {
  expect_equal(
    formatC(rTSCov(pData = sampleTData$PRICE), digits = 5),
    "0.00052838"
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
    formatC(RV(makeReturns(sampleTData$PRICE)), digits = 5),
    "0.00064815"
  )
})

##### rQPVar  ##### 
context("rQPVar")
test_that("rQPVar", {
  expect_equal(
    formatC(as.numeric(rQPVar(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
    "115.62"
  )
  
  expect_equal(lapply(rQPVar(returnDat), sum), list("PRICE1" = 3.018535259, "PRICE2" = 3.006780968, "PRICE3" = 2.95202662))
  expect_equal(lapply(rQPVar(returnDat), sum), lapply(rQPVar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rQPVar(returnDat), ncol = 3), matrix(as.matrix(rQPVar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rQPVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rQPVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rQPVar(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(rQPVar(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
  
  
})

##### rQuar  ##### 
context("rQuar")
test_that("rQuar", {
  expect_equal(
    formatC(as.numeric(rQuar(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
    "116.57"
  )
  
  expect_equal(lapply(rQuar(returnDat), sum), list("PRICE1" = 2.997549025, "PRICE2" = 3.030800189, "PRICE3" = 3.055064757))
  expect_equal(lapply(rQuar(returnDat), sum), lapply(rQuar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rQuar(returnDat), ncol = 3), matrix(as.matrix(rQuar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rQuar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rQuar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rQuar(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE)[,-1]), ncol = ncol(sample5MinPrices)),
               matrix(rQuar(sample5MinPrices, makeReturns = TRUE), ncol = ncol(sample5MinPrices)))
})

##### ivInference #####
context("ivInference")
test_that("ivInference", {
  expect_equal(
    formatC(ivInference(sampleTData$PRICE, IVestimator= "minRV", IQestimator = "medRQ", confidence = 0.95, makeReturns = TRUE)$cb * 10000, digits = 5),
    c("4.8088", "5.7948")
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
  rSC <- rSemiCov(sample5MinPrices, makeReturns = TRUE)
  mixed <- do.call(rbind, lapply(rSC, function(x) x[["mixed"]][1,2]))
  neg <- do.call(rbind, lapply(rSC, function(x) x[["negative"]][1,2]))
  pos <- do.call(rbind, lapply(rSC, function(x) x[["positive"]][1,2]))
  concordant <- do.call(rbind, lapply(rSC, function(x) x[["concordant"]][1,2]))
  # Test whether we have zeros on the diagonal of the mixed covariance matrix
  expect_equal(as.numeric(diag((rSC[[1]][['mixed']]))) , numeric(nrow(rSC[[1]][['mixed']])))
  
  
  rCv <- rCov(sample5MinPrices, makeReturns = TRUE)
  realized <- do.call(rbind, lapply(rCv, function(x) x[1,2]))
  # Test whether the realized covariance is equal to the sum of the decomposed realized semicovariances.
  expect_equal(realized , (mixed + neg + pos))
  
  
  expect_equal(lapply(rSemiCov(returnDat) , function(x) sum(as.numeric(lapply(x[1:3], sum)))),  lapply(rCov(returnDat), sum))
  expect_equal(rSemiCov(returnDat), rSemiCov(dat, makeReturns = TRUE))
  expect_equal(rSemiCov(returnDat),  rSemiCov(returnDatDT))
  expect_equal(rSemiCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rSemiCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rSemiCov(as.data.table(sample5MinPrices)[,DT := index][,!"index"], makeReturns = TRUE),
               rSemiCov(sample5MinPrices, makeReturns = TRUE))
  
  
})




##### ReMeDI #####
context("ReMeDI")
test_that("ReMeDI Estimation matches Merrick Li's code", { # We thank Merrick li for contributing Matlab code.
  # print("Make sure to implement tests for correctTime = TRUE") ## When it becomes relevant.
  #remed <- ReMeDI(sampleTDataMicroseconds, correctTime = FALSE, lags = 0:25, kn = 2) ##Changed due to correctTime bug
  remed <- ReMeDI(sampleTDataMicroseconds, lags = 0:25, kn = 2)

  expected <- c(1.431069e-05, 9.651728e-05, 1.060440e-04, 7.762527e-05, 3.823010e-05, 1.709553e-05, -9.089181e-06, -4.940754e-06, -2.070274e-06, 
                -2.682253e-05, -4.762974e-05, -6.670733e-05, -7.923285e-05, -7.106233e-05, -2.953979e-05, -1.200891e-05, 1.211180e-06,  2.670063e-05, 
                2.620544e-05, 3.368025e-05, 3.087709e-05, -3.367455e-06, -2.308410e-05, -4.196142e-05, -4.182684e-05, -5.326772e-06)
  
  expect_equal(remed, expected)

  # Different data-set
  dat <- sampleTData$PRICE
  storage.mode(dat) <- "numeric"
  #remed <- ReMeDI(dat, correctTime = FALSE, jumpsIndex = NULL, lags = 0:25, kn = 4) ##Changed due to correctTime bug
  remed <- ReMeDI(dat, lags = 0:25, kn = 4)
  
  expected <- c(1.946686e-04, -3.945028e-04, -5.868124e-04,  -8.010601e-04, -8.411491e-04, -9.121411e-04, -8.848654e-04, -8.964487e-04,-7.963612e-04,
                -6.367172e-04, -4.734131e-04, -2.349435e-04, -1.945647e-04,  -1.113470e-04, -4.221193e-05,  9.638518e-06, 8.949410e-05, 
                7.872398e-05,-1.349880e-05, -1.493591e-04, -1.288487e-04, -3.294210e-05,  1.014176e-04, 2.808128e-04,  2.813373e-04,  1.856106e-04)
  

  expect_equal(remed, expected)


})

test_that("ReMeDI lag choosing algorithm chooses the correct values", {

  # optimalKn <- knChooseReMeDI(sampleTDataMicroseconds, correctTime = FALSE, jumpsIndex = NULL, knMax = 10, tol = 0.05, size = 3, lower = 1, upper = 10, plot = FALSE)##Changed due to correctTime bug
  optimalKn <- knChooseReMeDI(sampleTDataMicroseconds, knMax = 10, tol = 0.05, size = 3, lower = 1, upper = 10, plot = FALSE)
  expect_equal(optimalKn, 1L)

  dat <- sampleTData$PRICE
  storage.mode(dat) <- "numeric"
  # optimalKn <- knChooseReMeDI(dat, correctTime = FALSE, jumpsIndex = NULL, knMax = 10, tol = 0.05, size = 3, lower = 3, upper = 5, plot = FALSE) ##Changed due to correctTime bug
  optimalKn <- knChooseReMeDI(dat, knMax = 10, tol = 0.05, size = 3, lower = 3, upper = 5, plot = FALSE)
  expect_equal(optimalKn, 4L)

})


test_that("ReMeDI asymptotic variance gives same result as Merrick Li's code", {
  dat <- sampleTData$PRICE
  storage.mode(dat) <- "numeric"
  dat <- log(dat)
  avar <- ReMeDIAsymptoticVariance(dat, phi = 0.5, lags = 0:10, kn = 3, int = 1)
  remed <- ReMeDI(dat, 3, 0:10)
  expected <- c(4.420651e-13, 2.083090e-13, 6.915452e-14, 1.867677e-14, 5.907554e-14, 5.129904e-14,
                 1.399077e-14, 1.433821e-14, 3.475069e-14, 2.972763e-14, 2.250913e-14)
  expect_equal(avar$asympVar, expected)
  ## We correctly calculate the remedi
  expect_equal(avar$ReMeDI, remed)
  
  
  avar <- ReMeDIAsymptoticVariance(dat, phi = 1, lags = 0:10, kn = 6, int = 2)  
  expected <- c(2.637164e-12, 1.613527e-12, 1.057763e-12, 6.532975e-13, 4.462853e-13, 3.568027e-13, 2.155630e-13, 
                2.985189e-13, 1.176854e-13, 6.341883e-14, 7.769585e-16)
  expect_equal(avar$asympVar, expected)
  
})