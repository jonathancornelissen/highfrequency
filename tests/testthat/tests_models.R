library(xts)

context("HARmodel")
test_that("HARModel",{
  expect_equal(
    formatC(sum(sum(HARmodel(makeReturns(sample5MinPricesJumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                             RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")$coefficients)), 
            digits = 5),
    "7.6406"
  )
  
  expect_identical(
    {blub <- HARmodel(makeReturns(sample5MinPricesJumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                      RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")
    blub2 <- plot(blub)
    blub2$get_xlim()},
    c(1263916800, 1264780800)
  )
  
  expect_identical(
    {blub <- HARmodel(makeReturns(sample5MinPricesJumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10),
                      RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")
    blub2 <- plot(blub)
    blub2$get_xlim()},
    c(1263916800, 1264780800)
  )
  model <- HARmodel(SP500RM$RV)
  info <- summary(model)
  expect_equal(info$call, "RV1 = beta0  +  beta1 * RV1 +  beta2 * RV5 +  beta3 * RV22")
  ## Testing with BPQ's data 
  expect_equal(model$coefficients, c("(Intercept)" = 0.1123142,"RV1" = 0.22734364,"RV5" = 0.49034938, "RV22" = 0.18637662))
  
  model <- HARmodel(SP500RM[, c("RV", "RQ")], type = "HARRVQ")
  expect_equal(model$coefficients, c("(Intercept)" = -0.009805735, "RV1" = 0.576823481, "RV5" = 0.358626466, "RV22" = 0.097615353, "RQ1" = -0.360196901))
                
  model <- HARmodel(SP500RM[, c("RV", "RQ")], type = "HARRVQ", periodsQ = c(1,5,22))
  expect_equal(model$coefficients,  
               c("(Intercept)" = -0.01868119, "RV1" = 0.55739295, "RV5" = 0.43113396, "RV22" = 0.05390772, 
                 "RQ1" = -0.33898605, "RQ5" = -0.14063204, "RQ22" = 0.08558724))
  expect_equal(summary(model)$r.squared, 0.56229227)
  
  model <- HARmodel(SP500RM[, c("RV", "BPV", "RQ")], type = "HARRVQJ", periodsJ = c(1))
  expect_equal(model$coefficients,
               c("(Intercept)" = 0.004452336, "RV1" = 0.589002342, "RV5" = 0.351905075, "RV22" = 0.105652537, "J1" = -0.339255775, "RQ1" = -0.326558434)
               )
  
  
  
  
  
  
  
  
  
  
  
  
  
})




context("HEAVYmodel")
test_that("HEAVYmodel",{
  data("realizedLibrary")
  returns <-  realizedLibrary$open_to_close
  bv      <-  realizedLibrary$bv
  returns <- returns[!is.na(bv)]
  bv <- bv[!is.na(bv)] # Remove NA's
  data <- cbind( returns^2, bv) # Make data matrix with returns and realized measures
  backCast <- matrix(c(var(returns), mean(bv)), ncol = 1)
  
  #For traditional (default) version:
  startValues <- c(0.004,0.02,0.44,0.41,0.74,0.56) # Initial values
  output <- HEAVYmodel(data = as.matrix(data,ncol=2), startingValues = startValues, compConst = FALSE, backCast = backCast)
  expect_identical(
      formatC(sum(output$estparams), digits = 7),
    "2.207789"
  )
})
