


# medRQ
expect_equal(
  formatC(medRQ(sample_tdata$PRICE,align.by = "minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "0.23645192756991520455"
)
# minRQ
expect_equal(
  minRQ(sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE) * 1000000,
  0.13061613812613856456
)

# MRC
expect_equal({
  formatC(sum(MRC(list(sample_5minprices_jumps["2010-01-04",1], sample_5minprices_jumps["2010-01-04",2]), pairwise = TRUE, makePsd = TRUE)), digits = 10)
  },
  "0.03169161677"
)

# rBeta
expect_equal({
  a <- sample_5minprices_jumps['2010-01-04', 1]
  b <- sample_5minprices_jumps['2010-01-04', 2]
  formatC(rBeta(a,b, RCOVestimator = "rBPCov", RVestimator = "minRV", makeReturns = TRUE), digits = 10)
  },
  c(stock2 = "1.431818232" )
)

# medRV
expect_equal(
  formatC(as.numeric(medRV(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by = "minutes", align.period = 5, makeReturns = TRUE)[1]), digits = 20),
  "0.013105073468934913539"
)
# minRV
expect_equal(
  formatC(as.numeric(minRV(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1:2], align.by = "minutes", align.period = 5, makeReturns = TRUE)[1,1]), digits = 20),
  "0.013259308626346031496"
)

# rBPCov
expect_equal(
  formatC(sum(rBPCov(rdata = sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns=TRUE)[[1]][1:2,1:2]) * 1000000, digits = 20),
  formatC(sum(rBPCov(rdata = sample_5minprices_jumps[c('2010-01-04'), 1:2], makeReturns=TRUE)[1:2,1:2]) * 1000000, digits = 20),
  "29778.176450595357892"
)

# RBPVar
expect_equal(
  formatC(sum(RBPVar(rdata = sample_5minprices_jumps['2010-01-04', 1])) * 1000000, digits = 20),
  "118932449.13193686306"
)

# rCov
expect_equal(
  formatC(sum(rCov(rdata = sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1:2], makeReturns = TRUE)[[1]][1:2,1:2]) * 1000000, digits = 20),
  "28614.139463211860857"
)

# rKurt
expect_equal(
  formatC(as.numeric(rKurt(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1] * 1000000, digits = 20),
  "2893553.4691817406565"
)

# rMPV
expect_equal(
  formatC(rMPV(sample_tdata$PRICE, align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "423.27734518675470099"
)

# rOWCov
expect_equal(
  formatC(rOWCov(rdata = sample_5minprices_jumps['2010-01-04'], makeReturns = TRUE)[1,1] * 1000000, digits = 10),
  "10038.65677"
)

# rSkew
expect_equal(
  formatC(as.numeric(rSkew(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1] * 1000000, digits = 20),
  "78047.167443186961464"
)

# rSV
expect_equal({
  rSV(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE)},
  list(rSVdownside = 0.00029125752237359241398, rSVupside = 0.00016310984360246060573)
)

# rTPVar
expect_equal(
  formatC(rTPVar(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "0.0027736636298646998901"
)

# RV
expect_equal(
  formatC(RV(sample_tdata$PRICE) / 10000, digits = 10),
  "29766.53635"
)

# rQPVar
expect_equal(
  formatC(as.numeric(rQPVar(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1] * 1000000, digits = 20),
  "1.4635490211980626274"
)

# rQuar
expect_equal(
  formatC(as.numeric(rQuar(sample_5minprices_jumps[c('2010-01-04', '2010-01-05'), 1], align.by ="minutes", align.period = 5, makeReturns = TRUE))[1] * 1000000, digits = 20),
  "116.56733664733448563"
)
