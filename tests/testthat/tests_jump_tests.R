
expect_equal(
  formatC(AJjumptest(sample_tdata$PRICE, p = 2, k = 3, align.by = "seconds", align.period = 5, makeReturns = TRUE)$ztest, digits = 10),
  "3.392275872"
)

expect_equal(
  formatC(JOjumptest(sample_5minprices_jumps[,1], power = 6)$ztest, digits = 10),
  "8.998787842"
)