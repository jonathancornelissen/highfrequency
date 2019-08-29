
# medRV
expect_equal(
  formatC(as.numeric(medRV(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by = "minutes", align.period = 5, makeReturns = TRUE)[1]), digits = 5),
  "0.013105"
)

# medRQ
expect_equal(
  formatC(medRQ(sample_tdata$PRICE,align.by = "minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 5),
  "0.23645"
)

# minRV
expect_equal(
  formatC(as.numeric(minRV(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1:2], align.by = "minutes", align.period = 5, makeReturns = TRUE)[1,1]), digits = 5),
  "0.013259"
)

# minRQ
expect_equal(
  minRQ(sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE) * 1000000,
  0.13061613812613856456
)

# MRC
expect_equal({
  formatC(sum(MRC(list(sample_5minprices_jumps["2010-01-04",1], sample_5minprices_jumps["2010-01-04",2]), pairwise = TRUE, makePsd = TRUE)), digits = 5)
  },
  "0.031718"
)

# rBeta
expect_equal({
  a <- sample_5minprices_jumps['2010-01-04', 1]
  b <- sample_5minprices_jumps['2010-01-04', 2]
  formatC(rBeta(a,b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE), digits = 5)
  },
  c(stock2 = "1.4318")
)
expect_equal({
  a <- sample_5minprices_jumps['2010-01-04', 1]
  b <- sample_5minprices_jumps['2010-01-04', 2]
  formatC(rBeta(a,b, RCOVestimator = "rOWCov", RVestimator = "medRV", makeReturns = TRUE), digits = 5)},
  c("1.2885")
)

# rBPCov
expect_equal(
  formatC(sum(rBPCov(rdata = sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns=TRUE)[[1]][1:2,1:2]) * 1000000, digits = 5),
  formatC(sum(rBPCov(rdata = sample_5minprices_jumps[c('2010-01-04'), 1:2], makeReturns=TRUE)[1:2,1:2]) * 1000000, digits = 5),
  "29778"
)

# RBPVar
expect_equal(
  formatC(sum(RBPVar(rdata = sample_5minprices_jumps['2010-01-04', 1])), digits = 5),
  "118.93"
)

# rCov
expect_equal(
  formatC(sum(rCov(rdata = sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns = TRUE)[[1]][1:2,1:2]), digits = 5),
  "0.028614"
)

# rKurt
expect_equal(
  formatC(as.numeric(rKurt(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1], digits = 5),
  "2.8936"
)

# rMPV
expect_equal(
  formatC(rMPV(sample_tdata$PRICE, align.by ="minutes", align.period = 5, makeReturns = TRUE), digits = 5),
  "0.00042328"
)

# rOWCov
expect_equal(
  formatC(rOWCov(rdata = sample_5minprices_jumps['2010-01-04'], makeReturns = TRUE)[1,1], digits = 5),
  "0.010039"
)
expect_equal(
  formatC(rOWCov(rdata = sample_5minprices_jumps['2010-01-04'], makeReturns = TRUE, wfunction = "SR")[1,1], digits = 3),
  "0.0101"
)

# rRTSCov
expect_equal(
  formatC(rRTSCov(pdata = sample_tdata$PRICE) * 10000, digits = 5),
  "3.5276"
)
expect_equal(
  formatC(sum(rRTSCov(pdata = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 1000000, digits = 5),
  "0.24693"
) 

# rKernel
expect_equal(
  formatC(rKernelCov(rdata = sample_tdata$PRICE, align.by = "minutes",  align.period = 5, makeReturns = TRUE), digits = 5),
  "0.00059605"
)
expect_equal(
  formatC(sum(rKernelCov(rdata = cbind(lltc, sbux, fill = 0), align.by = "minutes", align.period = 5, makeReturns = FALSE)) * 1000, digits = 5),
  "0.021378"
)

# rSkew
expect_equal(
  formatC(as.numeric(rSkew(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1], digits = 5),
  "0.078047"
)

# rSV
expect_equal({
  rSV(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE)},
  list(rSVdownside = 0.00029125752237359241398, rSVupside = 0.00016310984360246060573)
)

# rThresholdCov
expect_equal(
  formatC(sum(rThresholdCov(cbind(lltc, sbux), align.by = "minutes", align.period = 1)) * 10000, digits = 5),
  "0.59455"
)
expect_equal(
  formatC(sum(rThresholdCov(cbind(lltc, sbux), align.by = "minutes", align.period = 1), cor = TRUE), digits = 1),
  " 1"
)

# rTPVar
expect_equal(
  formatC(rTPVar(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 5),
  "0.0027737"
)

# rTSCov univariate
expect_equal(
  formatC(rTSCov(pdata = sample_tdata$PRICE), digits = 5),
  "0.00052769"
)
# rTSCov multivariate
expect_equal(
  formatC(sum(rTSCov(pdata = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 10000, digits = 5),
  "0.0024259"
)

# RV
expect_equal(
  formatC(RV(makeReturns(sample_tdata$PRICE)), digits = 5),
  "0.00064381"
)

# rQPVar
expect_equal(
  formatC(as.numeric(rQPVar(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
  "1.4635"
)

# rQuar
expect_equal(
  formatC(as.numeric(rQuar(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
  "116.57"
)


expect_equal(
  formatC(ivInference(sample_tdata$PRICE, IVestimator= "minRV", IQestimator = "medRQ", confidence = 0.95, makeReturns = TRUE)$cb * 10000, digits = 5),
  c("4.7594", "5.7472")
)


