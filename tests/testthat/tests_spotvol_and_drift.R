expect_identical(
  {price <- sample_tdata$PRICE
  storage.mode(price) = "numeric"
  kerneldrift <- spotDrift(price, method = "driftKernel", on = "minutes", k = 1)
  formatC(kerneldrift$mu[1:3], digits = 5)},
  c("     0", "-0.070876", "-0.1145")
)