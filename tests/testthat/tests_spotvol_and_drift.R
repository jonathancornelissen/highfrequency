expect_identical(
  {price <- sample_tdata$PRICE
  storage.mode(price) <- "numeric"
  kerneldrift <- spotDrift(price, method = "driftKernel", on = "minutes", k = 1)
  formatC(kerneldrift$mu[1:6], digits = 7)},
  c("-0.0708763", "-0.1144962",  "-0.07290184",  "0.02054575",  "0.07848068",  "0.08924061")
)


expect_identical(
  {dat <- data.table::copy(sample_tdata_microseconds)
  dat[, SYMBOL := NULL]
  meandrift <- spotDrift(data = dat, k = 1, tz = "EST")
  formatC(meandrift$mu[1:10], digits = 5)},
  c("    NA", "    NA", "    NA", "    NA", "0.00040961", "0.00046663", "0.00060476", "0.00051623", "0.00050264", "-2.5183e-05")
)

expect_identical(
  {dat <- data.table::copy(sample_tdata_microseconds)
  dat[, SYMBOL := NULL]
  meandrift1 <- spotDrift(data = dat, k = 10, on =  "seconds", tz = "EST")
  formatC(meandrift1$mu[11:40], digits = 5)},
  {dat <- data.table::copy(sample_tdata_microseconds)
  dat[, SYMBOL := NULL]
  meandrift2 <- spotDrift(data = dat, k = 10000, on =  "milliseconds", tz = "EST")
  formatC(meandrift2$mu[11:40], digits = 5)}
)
