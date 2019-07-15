

expect_equal(
  quotesCleanup(qdataraw = sample_qdataraw, exchanges = "N")$report["remove_outliers"],
  c(remove_outliers = 7706)
)

expect_equal(
  formatC(sum(head(aggregatePrice(sample_tdata$PRICE, on = "secs", k = 30))), digits = 10),
  "   1157.465"
)

expect_equal(
  unique(selectExchange(sample_qdataraw, c("N", "W"))$EX),
  c("N", "W")
)

expect_equal(
  dim(rmOutliersQuotes(selectExchange(sample_qdataraw, "W"))),
  dim(rmOutliersQuotes(selectExchange(sample_qdataraw, "W"), type = "standard"))
)

expect_equal(
  dim(rmTradeOutliersUsingQuotes(selectExchange(sample_tdataraw, "W"), selectExchange(sample_qdataraw, "W"))),
  c(203, 8)
)

expect_equal(
  dim(rmLargeSpread(selectExchange(sample_qdataraw, "N"))),
  c(9794, 7)
)

expect_equal(
  dim(mergeQuotesSameTimestamp(selectExchange(sample_qdataraw, "N"), selection = "max.volume")),
  c(7707, 3)
)

expect_equal(
  dim(mergeQuotesSameTimestamp(selectExchange(sample_qdataraw, "N"), selection = "weighted.average")),
  c(7707, 3)
)

expect_equal(
  dim(noZeroQuotes(selectExchange(sample_qdataraw, "N"))),
  c(9792, 7)
)

expect_equal(
  dim(tradesCleanupUsingQuotes(tdata = sample_tdata, qdata = sample_qdata)),
  c(8153, 8)
)

expect_equal(
  dim(tradesCleanup(tdataraw = sample_tdataraw, exchanges = "N", report = FALSE)),
  c(9104, 3)
)




