library(testthat)
library(xts)
library(data.table)
library(highfrequency)

data.table::setDTthreads(2)

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





##### rMedRVar #####
test_that("rMedRVar", {
  skip_on_cran()
  
  expect_equal(
    as.numeric(colSums(rMedRVar(sampleOneMinuteData, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[,!"DT"])),
    c(0.003387052289,0.001451676499)
  )
  
  expect_equal(lapply(rMedRVar(returnDat), sum), list("PRICE1" = 3.022290125, "PRICE2" = 3.003088657, "PRICE3" = 3.013543419))
  expect_equal(lapply(rMedRVar(returnDat), sum), lapply(rMedRVar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMedRVar(returnDat), ncol = 3), matrix(as.matrix(rMedRVar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMedRVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMedRVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMedRVar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rMedRVar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))

})
##### rMedRQuar ##### 
test_that("rMedRQuar", {
  skip_on_cran()
  
  expect_equal(
    as.numeric(rMedRQuar(as.xts(sampleTData[, list(DT, PRICE)]),alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000),
    c(0.015057721369, 0.003094834848)
  )
  expect_true(all.equal(rMedRQuar(returnDatDT, alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE), rMedRQuar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)))
  expect_true(all.equal(rMedRQuar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) , rMedRQuar(returnDat, alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)))
  expect_equal(lapply(rMedRQuar(returnDat), sum), list("PRICE1" = 3.06573359, "PRICE2" = 3.010144579, "PRICE3" = 3.030828633))
  expect_equal(lapply(rMedRQuar(returnDat), sum), lapply(rMedRQuar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMedRQuar(returnDat), ncol = 3), matrix(as.matrix(rMedRQuar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMedRQuar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMedRQuar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMedRQuar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rMedRQuar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
})
##### rMinRVar ##### 
test_that("rMinRVar", {  
  skip_on_cran()
  
  expect_equal(
    as.numeric(colSums(rMinRVar(sampleOneMinuteData, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, !"DT"])),
    c(0.003344205602,0.001438168861)
  )
  
  expect_equal(lapply(rMinRVar(returnDat), sum), list("PRICE1" = 3.027820575, "PRICE2" = 2.99975133, "PRICE3" = 3.001113006))
  expect_equal(lapply(rMinRVar(returnDat), sum), lapply(rMinRVar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMinRVar(returnDat), ncol = 3), matrix(as.matrix(rMinRVar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMinRVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMinRVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMinRVar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rMinRVar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
  
  
})
##### rMinRQuar ##### 
test_that("rMinRQuar", {
  
  skip_on_cran()
  
  expect_equal(
    as.numeric(rMinRQuar(as.xts(sampleTData[, list(DT, PRICE)]), alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000),
    c(0.016174147040, 0.002659484761)
  )
  expect_equal(lapply(rMinRQuar(returnDat), sum), list("PRICE1" = 3.0696895, "PRICE2" = 2.977093559, "PRICE3" = 3.01211734))
  expect_equal(lapply(rMinRQuar(returnDat), sum), lapply(rMinRQuar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMinRQuar(returnDat), ncol = 3), matrix(as.matrix(rMinRQuar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMinRQuar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMinRQuar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMinRQuar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rMinRQuar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
  
})

##### rMRCov #####  
test_that("rMRCov", {
  
  skip_on_cran()
  
  expect_equal({
    formatC(sum(rMRCov(list(as.xts(sampleOneMinuteData)["2001-08-04","MARKET"], as.xts(sampleOneMinuteData)["2001-08-04","STOCK"]), pairwise = TRUE, makePsd = TRUE)), digits = 5)
  },
  "0.00061674"
  )
  expect_equal({
    formatC(sum(rMRCov(list(as.xts(sampleOneMinuteData)["2001-08-04","MARKET"], as.xts(sampleOneMinuteData)["2001-08-04","STOCK"]), pairwise = FALSE, makePsd = TRUE)), digits = 5)
  },
  "0.00065673"
  )
  
  
})

##### rBeta #####
test_that("rBeta", {
  
  skip_on_cran()
  
  expect_equal({
    a <- as.xts(sampleOneMinuteData)["2001-08-04",1]
    b <- as.xts(sampleOneMinuteData)["2001-08-04",2]
    formatC(rBeta(a,b, RCOVestimator = "rBPCov", RVestimator = "rMinRVar", makeReturns = TRUE), digits = 5)
  },
  c(MARKET = "0.97877")
  )
  expect_equal({
    a <- as.xts(sampleOneMinuteData)["2001-08-04",1]
    b <- as.xts(sampleOneMinuteData)["2001-08-04",2]
    formatC(rBeta(a,b, RCOVestimator = "rOWCov", RVestimator = "rMedRVar", makeReturns = TRUE), digits = 5)},
    c("1.0577")
  )
})

##### rBPCov ##### 
test_that("rBPCov", {
  
  expect_equal(lapply(rBPCov(returnDat), sum), list("1970-01-01" = 2.98622886, "1970-01-02" = 3.026269417, "1970-01-03" = 2.99293847))
  expect_equal(lapply(rBPCov(returnDat), sum), lapply(rBPCov(dat, makeReturns = TRUE), sum))
  
  expect_equal(rBPCov(returnDat),  rBPCov(returnDatDT))
  expect_equal(rBPCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rBPCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  expect_equal(rBPCov(sampleOneMinuteData, makeReturns = TRUE), rBPCov(as.xts(sampleOneMinuteData), makeReturns = TRUE))
  
  
})
##### RBPVar ##### 
test_that("RBPVar", {
  
  skip_on_cran()
  
  if(!interactive()){ ## I don't want to test this everytime I manually run this script interactively
    expect_equal(
      formatC(sum(RBPVar(rData = diff(as.xts(sampleOneMinuteData))[-1,])), digits = 5),
      "150.48"
    )
  }
})
##### rCov #####
test_that("rCov", {
  
  skip_on_cran()
  
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
  
  expect_equal(lapply(rCov(returnDat, alignBy = "ticks", alignPeriod = 5, makeReturns = FALSE), sum), 
               list("1970-01-01" = 2.96428172300159, "1970-01-02" = 2.97075659500349, "1970-01-03" = 3.01890683034766))
  
  expect_equal(rCov(returnDat, alignBy = "ticks", alignPeriod = 5, makeReturns = FALSE),
               rCov(returnDatDT, alignBy = "ticks", alignPeriod = 5, makeReturns = FALSE))
  
  
  
})

##### rHYCov ##### 
test_that("rHYCov gives correct results", {
  
  skip_on_cran()
  
  hy <- rHYCov(rData = as.xts(sampleOneMinuteData)["2001-08-05"],
              period = 5, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
  expect_equal(hy, matrix(c(0.0003355498, 0.0001639014,
                            0.0001639014, 0.0002603934), ncol = 2))
  
})

##### rKurt ##### 
test_that("rKurt", {
  
  skip_on_cran()
  
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

##### rMPVar ##### 
test_that("rMPVar", {
  
  skip_on_cran()
  
  expect_equal(
    as.numeric(rMPVar(as.xts(sampleTData[, list(DT, PRICE)]), alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)),
    c(9.352083621e-05, 5.789397118e-05)
  )
  
  expect_equal(lapply(rMPVar(returnDat), sum), list("PRICE1" = 3.016532757, "PRICE2" = 3.004313242, "PRICE3" = 3.002691466))
  expect_equal(lapply(rMPVar(returnDat), sum), lapply(rMPVar(dat, makeReturns = TRUE), sum))
  
  expect_equal(matrix(rMPVar(returnDat), ncol = 3), matrix(as.matrix(rMPVar(returnDatDT)[,-1]), ncol = 3))
  expect_equal(matrix(rMPVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), ncol = 3),
               matrix(as.matrix(rMPVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[, -1]), ncol = 3))
  
  expect_equal(matrix(as.matrix(rMPVar(sampleOneMinuteData, makeReturns = TRUE)[,-1]), ncol = 2),
               matrix(rMPVar(as.xts(sampleOneMinuteData), makeReturns = TRUE), ncol = 2))
  
})
##### rOWCov ##### 
test_that("rOWCov", {
  
  skip_on_cran()
  
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
test_that("rRTSCov", {
  
  skip_on_cran()
  
  expect_equal(
    as.numeric(rRTSCov(pData = as.xts(sampleTData[as.Date(DT) == "2018-01-02", list(DT, PRICE)])) * 10000),
    0.4500523428
  )
  expect_equal(
    formatC(sum(rRTSCov(pData = list(dat["1970-01-01",1], dat["1970-01-01",2])), digits = 5)),
    "6.597"
  )
  
  foo <- spreadPrices(sampleMultiTradeData)[is.na(BBB), !"BBB"]
  
  expect_equal(sum(rRTSCov(pData = list(as.xts(foo[!is.na(ETF), list(DT,ETF)]), as.xts(foo[!is.na(AAA), list(DT,AAA)])), eta = 3)),
               0.0008762014)
  
})

##### rKernelCov ##### 
test_that("rKernelCov", {
  
  skip_on_cran()
  
  expect_equal(
    as.numeric(rKernelCov(rData = as.xts(sampleTData[, list(DT, PRICE)]), alignBy = "minutes",  alignPeriod = 5, makeReturns = TRUE)),
    c(1.313672470e-04,  6.263569337e-05)
  )
  expect_equal(
    formatC(sum(rKernelCov(rData = cbind(returnDat["1970-01-01",1], returnDat["1970-01-01",2]), alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)), digits = 5),
    " 1.708"
  )
  expect_equal(length(listAvailableKernels()) , 12)
  
  expect_equal(lapply(rKernelCov(returnDat), sum), list("1970-01-01" = 2.941858941, "1970-01-02" = 3.009886185, "1970-01-03" = 3.009261885))
  expect_equal(lapply(rKernelCov(returnDat), sum), lapply(rKernelCov(dat, makeReturns = TRUE), sum))
  
  expect_equal(rKernelCov(returnDat),  rKernelCov(returnDatDT))
  expect_equal(rKernelCov(returnDatDT), rKernelCov(datDT, makeReturns = TRUE))
  
  expect_equal(rKernelCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rKernelCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  
})

##### rSkew ##### 
test_that("rSkew", {
  
  skip_on_cran()
  
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
##### rSVar ##### 
test_that("rSVar", {
  
  skip_on_cran()
  
  expect_equal(
    sum(rSVar(as.xts(sampleTData[, list(DT, PRICE)]), alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)),
    0.0001657447672
  )
  expect_equal(lapply(rSVar(returnDat), function(x) lapply(x, sum))[[1]], list("rSVardownside" = 1.501497027, "rSVarupside" = 1.50206646))
  expect_equal(rSVar(returnDat), rSVar(dat, makeReturns = TRUE))
  
  expect_equal(rSVar(returnDat), rSVar(returnDatDT))
  expect_true(all.equal(rSVar(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rSVar(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE), check.attributes = FALSE))

  
})
##### rThresholdCov ##### 
test_that("rThresholdCov", {
  
  skip_on_cran()
  
  expect_equal(
    formatC(sum(rThresholdCov(cbind(returnDat["1970-01-01",1], returnDat["1970-01-01",2]), alignBy = "minutes", alignPeriod = 1)), digits = 5),
    "1.7277"
  )
  expect_equal(
    formatC(sum(rThresholdCov(cbind(returnDat["1970-01-01",1], returnDat["1970-01-01",2]), alignBy = "minutes", alignPeriod = 1, cor = TRUE)), digits = 5),
    "1.9694"
  )
  
  expect_equal(lapply(rThresholdCov(returnDat), sum), list("1970-01-01" = 2.943885754, "1970-01-02" = 2.992550689, "1970-01-03" = 2.963583828))
  expect_equal(lapply(rThresholdCov(returnDat), sum), lapply(rThresholdCov(dat, makeReturns = TRUE), sum))
  expect_equal(rThresholdCov(returnDat),  rThresholdCov(returnDatDT))
  expect_equal(rThresholdCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rThresholdCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  expect_equal(rThresholdCov(as.xts(sampleOneMinuteData), makeReturns = TRUE),  rThresholdCov(sampleOneMinuteData, makeReturns = TRUE))
  
  
})



##### rTPQuar ##### 
test_that("rTPQuar", {
  
  skip_on_cran()
  
  expect_equal(
    as.numeric(rTPQuar(as.xts(sampleTData[, list(DT, PRICE)]),alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000),
    c(0.014641358869, 0.003225971764)
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
test_that("rTSCov univariate", {
  
  skip_on_cran()
  
  expect_equal(
    as.numeric(rTSCov(pData = as.xts(sampleTData[as.Date(DT) == "2018-01-02", list(DT, PRICE)]))),
    0.0001157509218
  )
})
##### rTSCov multivariate ##### 
test_that("rTSCov multivariate", {
  
  skip_on_cran()
  
  expect_equal(
    formatC(sum(rTSCov(pData = list(dat["1970-01-01",1], dat["1970-01-01",2]))), digits = 5),
    "1.6068"
  )
})
##### rRVar  #####
test_that("rRVar", {
  
  skip_on_cran()
  
  expect_equal(
    formatC(as.numeric(rRVar(makeReturns(as.xts(sampleTData[as.Date(DT) == "2018-01-02", list(DT, PRICE)])))), digits = 5),
    "0.0001086"
  )
})

##### rQPVar  ##### 
test_that("rQPVar", {
  
  skip_on_cran()
  
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
test_that("rQuar", {
  
  skip_on_cran()
  
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
test_that("ivInference", {
  
  skip_on_cran()
  
  expect_equal(
    formatC(IVinference(as.xts(sampleTData[, list(DT, PRICE)]), IVestimator= "rMinRVar", IQestimator = "rMedRQuar", 
                        confidence = 0.95, makeReturns = TRUE)[[1]]$cb * 10000, digits = 5),
    c("0.93557", "1.1201")
  )
})

##### rAVGCov #####
test_that("rAVGCov",{
  
  skip_on_cran()
  
  rcovSub <- rAVGCov(rData = cbind(dat["1970-01-01",1], dat["1970-01-01",2]), alignBy = "minutes",alignPeriod = 5, k = 1, makeReturns = TRUE)
  expect_equal(as.numeric(rcovSub), c(0.78573656425, 0.06448478596, 0.06448478596, 0.73770313284))
  # Correct handling of seconds?
  rcovSubSeconds <- rAVGCov(rData = cbind(dat["1970-01-01",1], dat["1970-01-01",2]), alignBy = "seconds",alignPeriod = 5 * 60 , k = 60 , makeReturns = TRUE)
  expect_equal(rcovSub , rcovSubSeconds)
  rcovSubUnivariate <- rAVGCov(rData = cbind(dat["1970-01-01",1], dat["1970-01-01",2])[,1], alignBy = "minutes",alignPeriod = 5, makeReturns = TRUE)
  expect_equal(rcovSub[[1]], rcovSubUnivariate)

  # When the fast alignment is not a factor of the slow alignment period we throw an error
  expect_error(rAVGCov(rData = cbind(dat["1970-01-01",1], dat["1970-01-01",2]), alignBy = "minutes",alignPeriod = 2.75, k = 0.5, makeReturns = FALSE))
  
  
  expect_equal(lapply(rAVGCov(returnDat), sum), list("1970-01-01" = 2.6334856, "1970-01-02" = 2.491597803, "1970-01-03" = 2.974218965))
  expect_equal(rAVGCov(dat, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE),
               rAVGCov(datDT, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE))
  
  
  
  
  
  
})
##### rCholCov #####
test_that("rCholCov", {
  skip_on_cran()
  print("Skipping cholcov test on CRAN until ARM macs are available for testing.")
  
  
  if(!require("mvtnorm")){skip("mvtnorm isn't installed")}
  set.seed(123)
  iT <- 23400
  
  rets <- rmvnorm(iT * 3 + 1, mean = rep(0,4), 
                           sigma = matrix(c(1, -0.5 , 0.7, 0.8,
                                            -0.5, 3, -0.4, 0.7,
                                            0.7, -0.4, 2, 0.6,  
                                            0.8, 0.7, 0.6, 4), ncol = 4))
  ## YES, I do realize this is not the correct way to construct these returns - doesn't matter it's just for a test
  w1 <- rets[,1]
  w2 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.75)), 2]
  w3 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.65)), 3]
  w4 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.8)), 4] # Here we make asset 4 the second most liquid asset, which will function to test the ordering
  
  timestamps1 <- seq(34200, 57600, length.out =  length(w1))
  timestamps2 <- seq(34200, 57600, length.out =  length(w2))
  timestamps3 <- seq(34200, 57600, length.out =  length(w3))
  timestamps4 <- seq(34200, 57600, length.out =  length(w4))
  
  
  p1  <- xts(cumsum(w1) * c(0,sqrt(diff(timestamps1) / (max(timestamps1) - min(timestamps1)))), as.POSIXct(timestamps1, origin = "1970-01-01", tz = "UTC"), tz = "UTC")
  p2  <- xts(cumsum(w2) * c(0,sqrt(diff(timestamps2) / (max(timestamps2) - min(timestamps2)))), as.POSIXct(timestamps2, origin = "1970-01-01", tz = "UTC"), tz = "UTC")
  p3  <- xts(cumsum(w3) * c(0,sqrt(diff(timestamps3) / (max(timestamps3) - min(timestamps3)))), as.POSIXct(timestamps3, origin = "1970-01-01", tz = "UTC"), tz = "UTC")
  p4  <- xts(cumsum(w4) * c(0,sqrt(diff(timestamps4) / (max(timestamps4) - min(timestamps4)))), as.POSIXct(timestamps4, origin = "1970-01-01", tz = "UTC"), tz = "UTC")
  
  rCC <- rCholCov(list("market" = exp(p1), "stock1" = exp(p2), "stock2" = exp(p3) , "stock3" = exp(p4)))
  
  expect_equal(colnames(rCC$CholCov) , c("market", "stock3", "stock1", "stock2"))
  expect_equal(round(as.numeric(rCC$CholCov), 6) , round(c(0.9719097, 0.7821389, -0.3605819,  0.5238333, 0.7821389, 4.5218663,  0.5850890,
                                                           0.3575620, -0.3605819, 0.5850890,  2.4545129, -0.4106370, 0.5238333, 0.3575620,
                                                           -0.4106370,  1.6889769) , 6))
  
  expect_equal(colnames(rCC$L), colnames(rCC$G))
  
  
})

##### rSemiCov #####
test_that("rSemiCov", {
  
  skip_on_cran()
  
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
test_that("ReMeDI Estimation matches expected output", { # We thank Merrick li for contributing Matlab code.
  
  skip_on_cran()
  
  remed <- ReMeDI(sampleTData[, list(DT, PRICE = log(PRICE))], lags = 0:25, kn = 2)

  expected <- c(5.391986e-10,  3.873739e-09,  4.261547e-09,  3.118519e-09,  1.538245e-09,  6.805792e-10, -3.835125e-10, -2.232302e-10, -1.157490e-10, -1.110401e-09, -1.934303e-09,
  -2.685536e-09, -3.174416e-09, -2.839272e-09, -1.163387e-09, -4.468208e-10,  7.741021e-11,  1.093390e-09,  1.071914e-09,  1.360021e-09,  1.237765e-09, -1.382685e-10,
  -9.406390e-10, -1.695860e-09, -1.667980e-09, -1.982078e-10)
  
  
  expect_equal(remed, expected)

  # Same data-set but xts format
  dat <- sampleTData[, list(DT, PRICE = log(PRICE))]
  #remed <- ReMeDI(dat, correctTime = FALSE, jumpsIndex = NULL, lags = 0:25, kn = 4) ##Changed due to correctTime bug
  remed <- ReMeDI(as.xts(dat), lags = 0:25, kn = 2)
  
  

  expect_equal(remed, expected)


})

test_that("ReMeDI lag choosing algorithm chooses the correct values", {
  
  skip_on_cran()

  optimalKn <- knChooseReMeDI(sampleTData, knMax = 10, tol = 0.05, size = 3, lower = 1, upper = 10, plot = FALSE)
  expect_equal(optimalKn, 5L)
  
  optimalKn <- knChooseReMeDI(sampleTData[as.Date(DT) == "2018-01-02"], knMax = 10, tol = 0.05, size = 3, lower = 3, upper = 5, plot = FALSE)
  expect_equal(optimalKn, 5L)

})


test_that("ReMeDI asymptotic variance gives same result as Merrick Li's code", {
  
  skip_on_cran()
  
  dat <- sampleTData[, list(DT, PRICE = log(PRICE))]
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


#### rBACov ####
test_that("rBACov returns correct values", {
  
  skip_on_cran()
  
  set.seed(123)
  iT <- 23400
  if(!require("mvtnorm")){skip("mvtnorm isn't installed")}
  rets <- rmvnorm(iT * 3 + 1, mean = rep(0,4), 
                           sigma = matrix(c(0.1, -0.03 , 0.02, 0.04,
                                            -0.03, 0.05, -0.03, 0.02,
                                            0.02, -0.03, 0.05, -0.03,  
                                            0.04, 0.02, -0.03, 0.08), ncol = 4))
  w1 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.5)), 1]
  w2 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.75)), 2]
  w3 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.65)), 3]
  w4 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.8)), 4]
  w5 <- rnorm(nrow(rets) * 0.9, mean = 0, sd = 0.005)
  
  timestamps1 <- cumsum(abs(rnorm(length(w1) - 1)))
  disturbance <- runif(1)
  timestamps1 <- c(34200, 34200 + (timestamps1 - timestamps1[1] + disturbance)/(timestamps1[length(timestamps1)] + 2 * disturbance - timestamps1[1]) * (57600  - 34200))
  
  disturbance <- runif(1)
  timestamps2 <- cumsum(abs(rnorm(length(w2) - 1)))
  timestamps2 <- c(34200 ,34200 + (timestamps2 - timestamps2[1] + disturbance)/(timestamps2[length(timestamps2)] + 2 * disturbance- timestamps2[1]) * (57600  - 34200))
  
  disturbance <- runif(1)
  timestamps3 <- cumsum(abs(rnorm(length(w3) - 1)))
  timestamps3 <- c(34200, 34200 + (timestamps3 - timestamps3[1] + disturbance)/(timestamps3[length(timestamps3)] + 2 * disturbance - timestamps3[1]) * (57600  - 34200))
  
  disturbance <- runif(1)
  timestamps4 <- cumsum(abs(rnorm(length(w4) - 1)))
  timestamps4 <- c(34200, 34200 + (timestamps4 - timestamps4[1] + disturbance)/(timestamps4[length(timestamps4)] + 2 * disturbance - timestamps4[1]) * (57600  - 34200))
  
  disturbance <- runif(1)
  timestamps5 <- cumsum(abs(rnorm(length(w5))))
  timestamps5 <- 34200 + (timestamps5 - timestamps5[1] + disturbance)/(timestamps5[length(timestamps5)] + 2 * disturbance - timestamps5[1]) * (57600  - 34200)
  
  w1 <- xts(w1 * c(1,sqrt(diff(timestamps1) / (max(timestamps1) - min(timestamps1)))), as.POSIXct(timestamps1, origin = "1970-01-01"), tz = "UTC")
  w2 <- xts(w2 * c(1,sqrt(diff(timestamps2) / (max(timestamps2) - min(timestamps2)))), as.POSIXct(timestamps2, origin = "1970-01-01"), tz = "UTC")
  w3 <- xts(w3 * c(1,sqrt(diff(timestamps3) / (max(timestamps3) - min(timestamps3)))), as.POSIXct(timestamps3, origin = "1970-01-01"), tz = "UTC")
  w4 <- xts(w4 * c(1,sqrt(diff(timestamps4) / (max(timestamps4) - min(timestamps4)))), as.POSIXct(timestamps4, origin = "1970-01-01"), tz = "UTC")
  w5 <- xts(w5 * c(1,sqrt(diff(timestamps5) / (max(timestamps5) - min(timestamps5)))), as.POSIXct(timestamps5, origin = "1970-01-01"), tz = "UTC")
  
  
  p1  <- cumsum(w1)
  p2  <- cumsum(w2)
  p3  <- cumsum(w3)
  p4  <- cumsum(w4)
  
  
  weights <- runif(4) * 1:4
  weights <- weights / sum(weights)
  p5 <- xts(rowSums(cbind(w1 * weights[1], w2 * weights[2], w3 * weights[3], w4 * weights[4]), na.rm = TRUE), index(cbind(p1, p2, p3, p4)))
  p5 <- xts(cumsum(rowSums(cbind(p5, w5), na.rm = TRUE)), index(cbind(p5, w5)))
  
  p5 <- p5[sort(sample(1:length(p5), size = nrow(rets) * 0.9))]
  p1 <- p1[-1]
  p2 <- p2[-1]
  p3 <- p3[-1]
  p4 <- p4[-1]
  p5 <- p5[-1]
  
  lDT <- lapply(list("ETF" = p5, "STOCK 1" = p1, "STOCK 2" = p2, "STOCK 3" = p3, "STOCK 4" = p4),
         function(x){
           x <- as.data.table(exp(x))
           setnames(x, "index","DT")
           return(x)
         })
  BAC <- rBACov(lDT, shares = 1:4, outstanding = 1, nonEquity = 0, ETFNAME = "ETF", unrestricted = FALSE,
              preEstimator = "rCov", noiseCorrection = FALSE, returnL = FALSE, K = 2, J = 1)
  expect_equal(
  matrix(c(0.098589281, -0.002606114, -0.006960383, -0.01323153, -0.002606114, 0.050470789, 
           -0.005729020, -0.01868274, -0.006960383, -0.005729020, 0.050596758, -0.04860079, 
           -0.013231527, -0.018682744, -0.048600791, 0.08091408), ncol = 4, dimnames = list(c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"), 
                                                                                            c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"))),
  BAC
  )
  
  
  
  
  unrestrictedBAC <- rBACov(lDT, shares = 1:4, outstanding = 1, nonEquity = 0, ETFNAME = "ETF", unrestricted = TRUE, 
                          preEstimator = "rCov", noiseCorrection = FALSE, returnL = FALSE, K = 2, J = 1)

  expect_equal(
    matrix(c(0.095798183, -0.004477446, -0.008222493, -0.009909461, -0.004477446, 0.046707724, -0.008796183, -0.012393680, 
             -0.008222493, -0.008796183, 0.032547848, -0.025785512, -0.009909461, -0.012393680, -0.025785512, 0.049417920), ncol = 4,
           dimnames = list(c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"), 
                           c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"))),
    unrestrictedBAC
  )
  
  
  noisyBAC <- rBACov(lDT, shares = 1:4, outstanding = 1, nonEquity = 0, ETFNAME = "ETF", unrestricted = FALSE,
                   preEstimator = "rCov", noiseCorrection = TRUE, returnL = FALSE, K = 2, J = 1)
   
  expect_equal(
  matrix(c(0.097813865, -0.002561477, -0.006875550, -0.01314578,
           -0.002561477,  0.050470789, -0.005747408, -0.01867366,
           -0.006875550, -0.005747408,  0.050596758, -0.04861247,
           -0.013145784, -0.018673661, -0.048612469,  0.08089841), ncol = 4,
         dimnames = list(c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"), 
                         c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"))),
  noisyBAC
  )
  
  unrestrictedNoisyBAC <- rBACov(lDT, shares = 1:4, outstanding = 1, nonEquity = 0, ETFNAME = "ETF", unrestricted = TRUE,
                               preEstimator = "rCov", noiseCorrection = TRUE, returnL = FALSE, K = 2, J = 1)
  
  expect_equal(
    matrix(c(0.095064006, -0.004436407, -0.008136801, -0.009833758,
             -0.004436407,  0.046704739, -0.008802415, -0.012395926,
             -0.008136801, -0.008802415,  0.032534836, -0.025790203,
             -0.009833758, -0.012395926, -0.025790203,  0.049403869), ncol = 4, dimnames = list(c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"), 
                                                                                       c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"))),
    unrestrictedNoisyBAC
  )
  
  varianceAdjustedBAC <- rBACov(lDT, shares = 1:4, outstanding = 1, nonEquity = 0, ETFNAME = "ETF", unrestricted = FALSE, targetBeta = "VAB",
                      preEstimator = "rCov", noiseCorrection = FALSE, returnL = FALSE, K = 2, J = 1)
  expect_equal(
  matrix(c(
    0.09858928, -0.002640660, -0.008127390, -0.01333734,
    -0.00264066,  0.050470789, -0.008125928, -0.01892530,
    -0.00812739, -0.008125928,  0.050596758, -0.05311512,
    -0.01333734, -0.018925299, -0.053115115,  0.08091408
    ), ncol = 4, dimnames = list(c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"), 
                                 c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"))),
  varianceAdjustedBAC)
  
  
  bachy <- rBACov(lDT, shares = 1:4, outstanding = 1, nonEquity = 0, ETFNAME = "ETF", unrestricted = FALSE, targetBeta = "VAB",
         preEstimator = "rHYCov", noiseCorrection = FALSE, returnL = FALSE, K = 2, J = 1)
  
  expect_equal(
   matrix(c(0.09858928144971504, -0.0020233069260868, -0.00828194003776822, -0.0136038269799748, -0.00202330692608680, 0.0504707888931908, -0.00817577069472840,
     -0.0189714896369624, -0.00828194003776822, -0.0081757706947284, 0.05059675814898044, -0.0530529319960292, -0.01360382697997483, -0.0189714896369624,
     -0.05305293199602916, 0.0809140841505641), ncol = 4, dimnames = list(c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"), 
                                                                          c("STOCK 1", "STOCK 2", "STOCK 3", "STOCK 4"))),
   bachy
  )
  
  
})
