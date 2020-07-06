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
