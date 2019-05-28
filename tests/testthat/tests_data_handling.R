

expect_equal(
  quotesCleanup(qdataraw = sample_qdataraw, exchanges = "N")$report["remove_outliers"],
  c(remove_outliers = 7706)
)