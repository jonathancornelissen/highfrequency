


# medRQ
expect_equal(
  formatC(medRQ(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "0.23645192756991520455"
)
# minRQ
expect_equal(
  minRQ(sample_tdata$PRICE, align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000,
  0.13061613812613856456
)

# MRC

# # rBeta
# expect_equal({
#   a <- sample_5minprices_jumps['2010-01-04',1]
#   b <- sample_5minprices_jumps['2010-01-04',2]
#   formatC(rBeta(a,b,RCOVestimator="rBPCov", RVestimator="minRV", makeReturns=TRUE), digits = 20)
#   },
#   "1.4318182315337431021"
# )

# # rKurt
# expect_equal(
#   formatC(rKurt(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
#   "6830574.3464303314686"
# )
# 
# # rMPV
# expect_equal(
#   formatC(rMPV(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
#   "423.27734518675470099"
# )

# # rSkew
# expect_equal(
#   formatC(rSkew(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
#   " -1197639.06461360096"
# )

# rSV
expect_equal({
  rSV(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE)},
  list(rSVdownside = 0.00029125752237359241398, rSVupside = 0.00016310984360246060573)
)

# # rTPVar
# expect_equal(
#   formatC(rTPVar(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
#   "0.0027736636298646998901"
# )

# RV
expect_equal(
  formatC(RV(sample_tdata$PRICE) / 10000, digits = 10),
  "29766.53635"
)

# # rQPVar
# expect_equal(
#   formatC(rQPVar(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
#   "0.005940444309252677739"
# )

# rQuar
expect_equal(
  formatC(rQuar(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "0.47608305003642853448"
)
