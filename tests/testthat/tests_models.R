library("testthat")
library("xts")
rets <- as.xts(sampleOneMinuteData)[, 1]

for (date in unique(as.character(as.Date(index(rets))))) {
  rets[date] <- makeReturns(rets[date])
}


context("HARmodel")
test_that("HARModel",{
  
  expect_equal(
    formatC(sum(HARmodel(rets, periods = c(1, 5, 10), periodsJ = c(1),
                         RVest = c("rCov", "rBPCov"), type = "HARCJ", transform = "sqrt", inputType = "returns")$coefficients, na.rm = TRUE), 
            digits = 5),
    "0.58084"
  )
  
  expect_identical(
    {blub <- HARmodel(rets, periods = c(1, 5, 10), periodsJ = c(1),
                      RVest = c("rCov", "rBPCov"), type = "HARCJ", transform = "sqrt", inputType = "returns")
    blub2 <- plot(blub)
    blub2$get_xlim()},
    c(998150400, 999532800)
  )
  
  expect_identical(
    {blub <- HARmodel(rets, periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                      RVest = c("rCov", "rBPCov"), type = "HARCJ", transform = "sqrt", inputType = "returns")
    blub2 <- plot(blub)
    blub2$get_xlim()},
    c(998150400, 999532800)
  )
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5)]))
  info <- summary(model)
  expect_equal(info$call, "RV1 = beta0  +  beta1 * RV1 +  beta2 * RV5 +  beta3 * RV22")
  ## Testing with BPQ's data 
  expect_equal(model$coefficients, c("(Intercept)" = 1.160000921e-05 ,"RV1" = 2.953165771e-01,"RV5" = 2.813334173e-01, "RV22" = 1.471632893e-01 ))
  expect_output(print(model), NULL)
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5, RQ5)]), type = "HARQ")
  expect_equal(model$coefficients, c("(Intercept)" = 3.285615865e-06, "RV1" = 9.754440119e-01, "RV5" = 7.909932136e-03, "RV22" = 2.366579823e-02, "RQ1" = -3.881445184e-01 ))
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5, RQ5)]), type = "HARQ", periodsQ = c(1,5,22))
  expect_equal(model$coefficients,  
               c("(Intercept)" = -6.413188e-07, "RV1" = 9.163777832e-01, "RV5" = 1.609612721e-01, "RV22" = 6.218964e-02, 
                 "RQ1" = -3.581803794e-01 , "RQ5" = -1.695874367e-01  , "RQ22" = -2.373133e-01))
  expect_equal(summary(model)$r.squared, 0.3205499342)
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5, BPV5, RQ5)]), type = "HARQJ", periodsJ = c(1))
  expect_equal(model$coefficients,
               c("(Intercept)" = 3.278421768e-06, "RV1" = 9.738665838e-01, "RV5" = 7.578418611e-03, "RV22" = 2.352647045e-02 , 
                 "J1" = 2.665076387e-02, "RQ1" = -3.874289127e-01 )
  )
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5)]), periods = c(1,5,22), externalRegressor = xts(1:nrow(SPYRM), order.by = SPYRM$DT), periodsExternal = c(1))
  expect_true(all(model$model[,"externalRegressor"] == 22:(nrow(SPYRM)-1)))
  
  model <- HARmodel(as.xts(SPYRM[1:1000 , list(DT, RV5)]))
  expect_equal(predict(model), sum(coefficients(model) * tail(cbind(1, model$model[,-1]),1)))
  
  model <- HARmodel(as.xts(SPYRM[1:1000 , list(DT, RV5)]), transform = "sqrt")
  expect_equal(predict(model, backtransform = "simple"), (sum(coefficients(model) * tail(cbind(1, model$model[,-1]),1))^2))

  testStats <- sqrt(390) * (SPYRM$RV1 - SPYRM$BPV1)/sqrt((pi^2/4+pi-3 - 2) * SPYRM$medRQ1) ## BNSJumptest
  model <- HARmodel(cbind(as.xts(SPYRM[, list(DT, RV5, BPV5)]), testStats), type = "HARCJ")
  
  expect_equal(sum(summary(model)[[4]][,2]), 5.145151472487521)
  
  dat <- as.xts(sampleOneMinuteData[, makeReturns(STOCK), by = list(DATE = as.Date(DT))])
  x <- HARmodel(dat, periods = c(1,3), RVest = c("rCov"), type="HAR", inputType = "returns", leverage = c(1,3))
  expect_equal(sum(coef(x)), 0.5175878)
  
})

context("HEAVYmodel")
test_that("HEAVYmodel",{
  
  logReturns <- 100 * makeReturns(SPYRM$CLOSE)[-1]
  logReturns <- logReturns - mean(logReturns)
  dataSPY <- xts(cbind(logReturns, SPYRM$BPV5[-1] * 10000), order.by = SPYRM$DT[-1])
  output <- HEAVYmodel(dataSPY)
  expect_equal(round(sum(output$coefficients), 3), 1.981)
  
  
  p1 <- plot(output)
  p2 <- plot(output, type = 'RM')
  expect_equal(p1$get_xlim(),  p2$get_xlim()) # Make sure we plot the same range
  expect_equal(as.numeric(p1$get_ylim()[[2]]), range(range(dataSPY[,1]^2),  range(output$varCondVariances)))
  expect_equal(round(as.numeric(p1$get_ylim()[[2]]),4), c(0.0000, 17.9841))
  expect_equal(as.numeric(p2$get_ylim()[[2]]), range(range(dataSPY[,2]),  range(output$RMCondVariances)))
  expect_equal(round(as.numeric(p2$get_ylim()[[2]]),4), c(0.0186, 26.2201))
  
  pred <- predict(output, stepsAhead = 12)
  expect_equal(as.numeric(round(pred, 5)), c(0.24265, 0.26215, 0.27462, 0.28815, 0.30235, 0.31691, 0.33162, 0.34633, 0.36090, 0.37527,
                                             0.38937, 0.40315, 0.14614, 0.19149, 0.21304, 0.23387, 0.25402, 0.27349, 0.29232, 0.31052,
                                             0.32812, 0.34514, 0.36159, 0.37749))
  coeffs <- output$coefficients
  uncondRM <- as.numeric(coeffs[4] / (1 - coeffs[5] - coeffs[6]))
  uncondVar <- as.numeric((coeffs[1] + coeffs[2] * uncondRM) / (1 - coeffs[3]))
  expect_equal(as.numeric(round(predict(output, stepsAhead = 400)[400,], 4)),
               round(c(uncondVar, uncondRM), 4))
  
  
  expect_output(print(output), NULL)
  
})
