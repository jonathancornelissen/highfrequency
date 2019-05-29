expect_equal(
  formatC(sum(sum(harModel(makeReturns(sample_5minprices_jumps[, 1]), periods = c(1, 5, 10), periodsJ = c(1, 5, 10), 
                           RVest = c("rCov", "rBPCov"), type = "HARRVCJ", transform = "sqrt", inputType = "returns")$coefficients)), 
          digits = 5),
  "7.6406"
)

