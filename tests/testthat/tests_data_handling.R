

expect_equal(
  quotesCleanup(qdataraw = sample_qdataraw, exchanges = "N")$report["remove_outliers"],
  c(remove_outliers = 7706)
)

expect_equal(
  unique(selectExchange(sample_qdataraw, c("N", "W"))$EX),
  c("N", "W")
)

expect_equal(
  dim(rmOutliersQuotes(selectExchange(sample_qdataraw, "W"))),
  c(4394, 7)
)

expect_equal(
  dim(rmOutliersTrades(selectExchange(sample_tdataraw, "W"))),
  c(190, 7)
)

expect_equal(
  dim(rmLargeSpread(selectExchange(sample_qdataraw, "N"))),
  c(9794, 10)
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

