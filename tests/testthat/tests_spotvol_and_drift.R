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

expect_identical(
  formatC(as.numeric(spotvol(sample_real5minprices)$spot[1:10]), digits = 3),
  c("0.004", "0.00373", "0.00348", "0.00325", "0.00306", "0.00288", "0.00274", "0.00262", "0.00251", "0.00241")
)

expect_identical(
  formatC(as.numeric(spotvol(sample_real5minprices, method = "stochper", init = init)$spot[1:10]), digits = 3),
  c("0.00315", "0.00331", "0.00303", "0.00305", "0.0028", "0.00268", "0.00238", "0.00261", "0.00233", "0.00223")
)

formatC(as.numeric(spotvol(sample_tdata_microseconds, on = "minutes", k = 5)$spot[1:10]), digits = 3)



