
expect_equal(
  unique(autoSelectExchangeTrades(sample_tdataraw)$EX),
  "N"
)

expect_equal(
  unique(autoSelectExchangeQuotes(sampleQDataraw)$EX),
  "N"
)

expect_equal(
  quotesCleanup(qdataraw = sampleQDataraw, exchanges = "N")$report["remove_outliers"],
  c(remove_outliers = 7706)
)

expect_equal(
  formatC(sum(head(aggregatePrice(sample_tdata$PRICE, on = "secs", k = 30))), digits = 10),
  "   1157.465"
)

expect_equal(
  unique(selectExchange(sampleQDataraw, c("N", "W"))$EX),
  c("N", "W")
)

expect_equal(
  dim(rmOutliersQuotes(selectExchange(sampleQDataraw, "W"))),
  dim(rmOutliersQuotes(selectExchange(sampleQDataraw, "W"), type = "standard"))
)

expect_equal(
  dim(rmTradeOutliersUsingQuotes(selectExchange(sample_tdataraw, "W"), selectExchange(sampleQDataraw, "W"))),
  c(203, 8)
)

expect_equal(
  dim(rmLargeSpread(selectExchange(sampleQDataraw, "N"))),
  c(9794, 7)
)

expect_equal(
  dim(mergeQuotesSameTimestamp(selectExchange(sampleQDataraw, "N"), selection = "max.volume")),
  c(7707, 5)
)

expect_equal(
  dim(mergeQuotesSameTimestamp(selectExchange(sampleQDataraw, "N"), selection = "weighted.average")),
  c(7707, 5)
)

expect_equal(
  dim(noZeroQuotes(selectExchange(sampleQDataraw, "N"))),
  c(9792, 7)
)

expect_equal(
  dim(tradesCleanupUsingQuotes(tdata = sample_tdata, qdata = sampleQData)),
  c(8153, 8)
)

expect_equal(
  dim(tradesCleanup(tdataraw = sample_tdataraw, exchanges = "N", report = FALSE)),
  c(9104, 3)
)



# Test edge cases of aggregatets
expect_equal(index(aggregatets(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))),
                   index(aggregatets(xts(1:23400, as.POSIXct(seq(34200, 57599, length.out = 23400), origin = '1970-01-01'))))
)

expect_true(
  max(index(aggregatets(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))))<
  max(index(aggregatets(xts(1:23400, as.POSIXct(seq(34200, 57601, length.out = 23400), origin = '1970-01-01')))))
)

