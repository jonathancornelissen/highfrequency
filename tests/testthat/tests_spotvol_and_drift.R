library(highfrequency)
library(testthat)


context("spotDrift")
test_that("spotDrift",{
  expect_identical(
    {price <- sampleTData$PRICE
    storage.mode(price) <- "numeric"
    kerneldrift <- spotDrift(price, method = "driftKernel", on = "minutes", k = 1)
    formatC(kerneldrift$mu[1:6], digits = 5)},
    c("-0.070876", "-0.1145", "-0.072902", "0.020546", "0.078481", "0.089241")
  )
  
  dat <- data.table::copy(sampleTDataMicroseconds)
  dat[, SYMBOL := NULL]
  meandrift <- spotDrift(data = dat, k = 1, tz = "EST")
  expect_identical(formatC(meandrift$mu[1:10], digits = 5),
    c("    NA", "    NA", "    NA", "    NA", "0.00040961", "0.000454", "0.00060476", "0.00051623", "0.00050264", "-2.5183e-05")
  )
  
  expect_identical(
    {dat <- data.table::copy(sampleTDataMicroseconds)
    dat[, SYMBOL := NULL]
    meandrift1 <- spotDrift(data = dat, k = 10, on =  "seconds", tz = "EST")
    formatC(meandrift1$mu[11:40], digits = 5)},
    {dat <- data.table::copy(sampleTDataMicroseconds)
    dat[, SYMBOL := NULL]
    meandrift2 <- spotDrift(data = dat, k = 10000, on =  "milliseconds", tz = "EST")
    formatC(meandrift2$mu[11:40], digits = 5)}
  )
})


context("spotVol")
test_that("spotVol", {
  expect_identical(
    formatC(as.numeric(spotVol(sampleReal5MinPrices)$spot[1:10]), digits = 3),
    c("0.004", "0.00373", "0.00348", "0.00325", "0.00306", "0.00288", "0.00274", "0.00262", "0.00251", "0.00241")
  )
  init <- list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.007,
               sigma_k = 0.06, phi = 0.194, rho = 0.986, mu = c(1.87,-0.42),
               delta_c = c(0.25, -0.05, -0.2, 0.13, 0.02),
               delta_s = c(-1.2, 0.11, 0.26, -0.03, 0.08))
  expect_identical(
    formatC(as.numeric(spotVol(sampleReal5MinPrices["2005-03-04/2005-03-05"], method = "stochper", init = init)$spot[1:10]), digits = 3),
    c("0.00227", "0.00223", "0.00218", "0.00214", "0.00209", "0.00205", "0.00202", "0.00198", "0.00195", "0.00193")
  )
  

})