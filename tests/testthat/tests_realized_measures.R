

# rSV
expect_equal({
  data(sample_tdata)
  rSV(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE)},
  list(rSVdownside = 0.00029125752237359241398, rSVupside = 0.00016310984360246060573)
)
# medRQ
expect_equal(
  formatC(medRQ(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "0.23645192756991520455"
)
# minRQ
expect_equal(
  minRQ(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000,
  0.13061613812613856456
)
# RV
expect_equal(
  formatC(RV(sample_tdata$PRICE) / 10000, digits = 10),
  "29766.53635"
)

# rQuar
expect_equal(
  formatC(rQuar(sample_tdata$PRICE,align.by ="minutes", align.period = 5, makeReturns = TRUE) * 1000000, digits = 20),
  "0.47608305003642853448"
)