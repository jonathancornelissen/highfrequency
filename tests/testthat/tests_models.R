library(testthat)
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
  
})

context("HEAVYmodel")
test_that("HEAVYmodel",{
  
  logReturns <- 100 * makeReturns(SPYRM$CLOSE)[-1]
  logReturns <- logReturns - mean(logReturns)
  output <- HEAVYmodel(logReturns, SPYRM$BPV5[-1] * 10000)
  expect_identical(
    formatC(sum(output$coefficients), digits = 6),
    "2.67386"
  )
})