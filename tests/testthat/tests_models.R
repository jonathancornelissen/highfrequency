library(xts)
library(testthat)
rets <- sample5MinPricesJumps[, 1]

for (date in unique(as.character(as.Date(index(rets))))) {
 rets[date] <- makeReturns(rets[date])
}


context("HARmodel")
test_that("HARModel",{
  
  expect_equal(
    formatC(sum(sum(HARmodel(rets, periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                             RVest = c("rCov", "rBPCov"), type = "HARCJ", transform = "sqrt", inputType = "returns")$coefficients)), 
            digits = 5),
    "7.6069"
  )
  
  expect_identical(
    {blub <- HARmodel(rets, periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                      RVest = c("rCov", "rBPCov"), type = "HARCJ", transform = "sqrt", inputType = "returns")
    blub2 <- plot(blub)
    blub2$get_xlim()},
    c(1263916800, 1264780800)
  )
  
  expect_identical(
    {blub <- HARmodel(rets, periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                      RVest = c("rCov", "rBPCov"), type = "HARCJ", transform = "sqrt", inputType = "returns")
    blub2 <- plot(blub)
    blub2$get_xlim()},
    c(1263916800, 1264780800)
  )
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5)]))
  info <- summary(model)
  expect_equal(info$call, "RV1 = beta0  +  beta1 * RV1 +  beta2 * RV5 +  beta3 * RV22")
  ## Testing with BPQ's data 
  expect_equal(model$coefficients, c("(Intercept)" = 1.0657152785,"RV1" = 0.5542609958,"RV5" = 0.2194697795, "RV22" = 0.1041612492))
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5, RQ5)]), type = "HARQ")
  expect_equal(model$coefficients, c("(Intercept)" = 0.43766717, "RV1" = 0.72960614, "RV5" = 0.15875296, "RV22" = 0.06894572, "RQ1" = -0.17082535))
                
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5, RQ5)]), type = "HARQ", periodsQ = c(1,5,22))
  expect_equal(model$coefficients,  
               c("(Intercept)" = -0.14999372, "RV1" = 0.70939522, "RV5" = 0.20844754, "RV22" = 0.11054066, 
                 "RQ1" = -0.15272757, "RQ5" = -0.06202333, "RQ22" = -0.13730876))
  expect_equal(summary(model)$r.squared, 0.59853454)
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5, BPV5, RQ5)]), type = "HARQJ", periodsJ = c(1))
  expect_equal(model$coefficients,
               c("(Intercept)" = 0.47391062, "RV1" = 0.73820120, "RV5" = 0.15866817, "RV22" = 0.07063117, "J1" = -0.31028495, "RQ1" = -0.17198537)
               )
  
  model <- HARmodel(as.xts(SPYRM[, list(DT, RV5)]), periods = c(1,5,22), externalRegressor = xts(1:nrow(SPYRM), order.by = SPYRM$DT), periodsExternal = c(1))
  expect_true(all(model$model[,"externalRegressor"] == 22:(nrow(SPYRM)-1)))
  
  model <- HARmodel(as.xts(SPYRM[1:1000 , list(DT, RV5)]))
  expect_equal(predict(model), sum(coefficients(model) * tail(cbind(1, model$model[,-1]),1)))
  
  
  
})




context("HEAVYmodel")
test_that("HEAVYmodel",{
  
  returns <-  makeReturns(SPYRM$CLOSE)
  bv      <-  SPYRM$BPV5
  returns <- returns[!is.na(bv)]
  bv <- bv[!is.na(bv)] # Remove NA's
  data <- cbind( returns^2, bv) # Make data matrix with returns and realized measures
  backCast <- matrix(c(var(returns), mean(bv)), ncol = 1)
  
  #For traditional (default) version:
  output <- HEAVYmodel(data = as.matrix(data,ncol=2), compConst = FALSE, backCast = backCast)
  expect_identical(
      formatC(sum(output$estparams), digits = 7),
    "1.485103"
  )
})
