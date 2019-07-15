expect_identical(
  {price <- sample_tdata$PRICE
  storage.mode(price) = "numeric"
  kerneldrift <- spotDrift(price, method = "driftKernel", on = "minutes", k = 1)
  formatC(kerneldrift$mu[1:3], digits = 5)},
  c("     0", "-0.070876", "-0.1145")
)


expect_identical(
  {dat <- sample_tdata_microseconds[, SYMBOL := NULL]
  meandrift <- spotDrift(data = dat, k = 1, tz = "EST")
  formatC(meandrift$mu[1:10], digits = 5)},
  c("    NA", "    NA", "    NA", "    NA", "0.00040961", "0.000454", "0.00060476", "0.00051623", "0.00050264", "-2.5183e-05")
)