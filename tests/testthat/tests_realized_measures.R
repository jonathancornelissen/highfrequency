
# medRV
expect_equal(
  formatC(as.numeric(medRV(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[1]), digits = 5),
  "0.013105"
)

# medRQ
expect_equal(
  formatC(medRQ(sampleTData$PRICE,alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000, digits = 5),
  "0.23645"
)

# minRV
expect_equal(
  formatC(as.numeric(minRV(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)[1,1]), digits = 5),
  "0.013259"
)

# minRQ
expect_equal(
  minRQ(sampleTData$PRICE, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000,
  0.13061613812613856456
)

# MRC
expect_equal({
  formatC(sum(MRC(list(sample5MinPricesJumps["2010-01-04",1], sample5MinPricesJumps["2010-01-04",2]), pairwise = TRUE, makePsd = TRUE)), digits = 5)
  },
  "0.031718"
)
expect_equal({
  formatC(sum(MRC(list(sample5MinPricesJumps["2010-01-04",1], sample5MinPricesJumps["2010-01-04",2]), pairwise = FALSE, makePsd = TRUE)), digits = 5)
  },
  "0.034418"
)

# rBeta
expect_equal({
  a <- sample5MinPricesJumps['2010-01-04', 1]
  b <- sample5MinPricesJumps['2010-01-04', 2]
  formatC(rBeta(a,b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE), digits = 5)
  },
  c(stock2 = "1.4318")
)
expect_equal({
  a <- sample5MinPricesJumps['2010-01-04', 1]
  b <- sample5MinPricesJumps['2010-01-04', 2]
  formatC(rBeta(a,b, RCOVestimator = "rOWCov", RVestimator = "medRV", makeReturns = TRUE), digits = 5)},
  c("1.2885")
)

# rBPCov
expect_equal(
  formatC(sum(rBPCov(rdata = sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns=TRUE)[[1]][1:2,1:2]) * 1000000, digits = 5),
  formatC(sum(rBPCov(rdata = sample5MinPricesJumps[c('2010-01-04'), 1:2], makeReturns=TRUE)[1:2,1:2]) * 1000000, digits = 5),
  "29778"
)

# RBPVar
expect_equal(
  formatC(sum(RBPVar(rdata = sample5MinPricesJumps['2010-01-04', 1])), digits = 5),
  "118.93"
)

# rCov
expect_equal(
  formatC(sum(rCov(rdata = sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns = TRUE)[[1]][1:2,1:2]), digits = 5),
  "0.028614"
)

# rKurt
expect_equal(
  formatC(as.numeric(rKurt(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1], digits = 5),
  "2.8936"
)

# rMPV
expect_equal(
  formatC(rMPV(sampleTData$PRICE, alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE), digits = 5),
  "0.00042328"
)

# rOWCov
expect_equal(
  formatC(rOWCov(rdata = sample5MinPricesJumps['2010-01-04'], makeReturns = TRUE)[1,1], digits = 5),
  "0.010039"
)
expect_equal(
  formatC(rOWCov(rdata = sample5MinPricesJumps['2010-01-04'], makeReturns = TRUE, wfunction = "SR")[1,1], digits = 3),
  "0.0101"
)

# rRTSCov
expect_equal(
  formatC(rRTSCov(pData = sampleTData$PRICE) * 10000, digits = 5),
  "3.5276"
)
expect_equal(
  formatC(sum(rRTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 1000000, digits = 5),
  "0.24693"
) 

# rKernel
expect_equal(
  formatC(rKernelCov(rdata = sampleTData$PRICE, alignBy = "minutes",  alignPeriod = 5, makeReturns = TRUE), digits = 5),
  "0.00059605"
)
expect_equal(
  formatC(sum(rKernelCov(rdata = cbind(lltc, sbux, fill = 0), alignBy = "minutes", alignPeriod = 5, makeReturns = FALSE)) * 1000, digits = 5),
  "0.021378"
)

# rSkew
expect_equal(
  formatC(as.numeric(rSkew(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1], digits = 5),
  "0.078047"
)

# rSV
expect_equal({
  rSV(sampleTData$PRICE,alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE)},
  list(rSVdownside = 0.00029125752237359241398, rSVupside = 0.00016310984360246060573)
)

# rThresholdCov
expect_equal(
  formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1)) * 10000, digits = 5),
  "0.59455"
)
expect_equal(
  formatC(sum(rThresholdCov(cbind(lltc, sbux), alignBy = "minutes", alignPeriod = 1), cor = TRUE), digits = 1),
  " 1"
)

# rTPVar
expect_equal(
  formatC(rTPVar(sampleTData$PRICE,alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE) * 1000000, digits = 5),
  "0.41334"
)

# rTSCov univariate
expect_equal(
  formatC(rTSCov(pData = sampleTData$PRICE), digits = 5),
  "0.00052769"
)
# rTSCov multivariate
expect_equal(
  formatC(sum(rTSCov(pData = list(cumsum(lltc) + 100, cumsum(sbux) + 100))) * 10000, digits = 5),
  "0.0024259"
)

# RV
expect_equal(
  formatC(RV(makeReturns(sampleTData$PRICE)), digits = 5),
  "0.00064381"
)

# rQPVar
expect_equal(
  formatC(as.numeric(rQPVar(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
  "115.62"
)

# rQuar
expect_equal(
  formatC(as.numeric(rQuar(sample5MinPricesJumps[c('2010-01-04', '2010-01-05'), 1], alignBy ="minutes", alignPeriod = 5, makeReturns = TRUE))[1] * 1000000, digits = 5),
  "116.57"
)


expect_equal(
  formatC(ivInference(sampleTData$PRICE, IVestimator= "minRV", IQestimator = "medRQ", confidence = 0.95, makeReturns = TRUE)$cb * 10000, digits = 5),
  c("4.7594", "5.7472")
)


