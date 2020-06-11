
expect_equal(
  unique(autoSelectExchangeTrades(sampletDataRaw)$EX),
  "N"
)

expect_equal(
  unique(autoSelectExchangeQuotes(sampleqDataRaw)$EX),
  "N"
)

expect_equal(
  quotesCleanup(qDataraw = sampleqDataRaw, exchanges = "N")$report["remove_outliers"],
  c(remove_outliers = 7706)
)

expect_equal(
  formatC(sum(head(aggregatePrice(sampletData$PRICE, on = "secs", k = 30))), digits = 10),
  "   1157.465"
)

expect_equal(
  unique(selectExchange(sampleqDataRaw, c("N", "W"))$EX),
  c("N", "W")
)

expect_equal(
  dim(rmOutliersQuotes(selectExchange(sampleqDataRaw, "W"))),
  dim(rmOutliersQuotes(selectExchange(sampleqDataRaw, "W"), type = "standard"))
)

expect_equal(
  dim(rmTradeOutliersUsingQuotes(selectExchange(sampletDataRaw, "W"), selectExchange(sampleqDataRaw, "W"))),
  c(203, 8)
)

expect_equal(
  dim(rmLargeSpread(selectExchange(sampleqDataRaw, "N"))),
  c(9794, 7)
)

expect_equal(
  dim(mergeQuotesSameTimestamp(selectExchange(sampleqDataRaw, "N"), selection = "max.volume")),
  c(7707, 5)
)

expect_equal(
  dim(mergeQuotesSameTimestamp(selectExchange(sampleqDataRaw, "N"), selection = "weighted.average")),
  c(7707, 5)
)

expect_equal(
  dim(noZeroQuotes(selectExchange(sampleqDataRaw, "N"))),
  c(9792, 7)
)

expect_equal(
  dim(tradesCleanupUsingQuotes(tData = sampletData, qData = sampleqData)),
  c(8153, 8)
)

expect_equal(
  dim(tradesCleanup(tDataraw = sampletDataRaw, exchanges = "N", report = FALSE)),
  c(9104, 3)
)



# Test edge cases of aggregateTS
expect_equal(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))),
                   index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57599, length.out = 23400), origin = '1970-01-01'))))
)

expect_true(
  max(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))))<
  max(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57601, length.out = 23400), origin = '1970-01-01')))))
)

