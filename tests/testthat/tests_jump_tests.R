
expect_equal(
  formatC(AJjumptest(sampletData$PRICE, p = 2, k = 3, align.by = "seconds", align.period = 5, makeReturns = TRUE)$ztest, digits = 10),
  "-2.903324467"
)

expect_equal(
  formatC(JOjumptest(sample5MinPricesJumps[,1], power = 6)$ztest, digits = 10),
  "8.998787842"
)

expect_equal(
  formatC(JOjumptest(sample5MinPricesJumps[,1], power = 4)$ztest, digits = 4),
  "7.832"
)

expect_equal(
  formatC(BNSjumptest(sampletData$PRICE, IVestimator= "minRV", 209, IQestimator = "medRQ", type= "linear", makeReturns = TRUE)$pvalue, digits = 0),
  c(PRICE = "0")
)


