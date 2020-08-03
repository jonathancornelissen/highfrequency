context("autoSelectExchangeTrades")
test_that("autoSelectExchangeTrades", {
  expect_equal(
    unique(autoSelectExchangeTrades(sampleTDataRaw)$EX),
    "N"
  )
  
  expect_equal(
    unique(autoSelectExchangeQuotes(sampleQDataRaw)$EX),
    "N"
  )

})


context("quotesCleanup")
test_that("quotesCleanup", {
  expect_equal(
    quotesCleanup(qDataRaw = sampleQDataRaw, exchanges = "N")$report["remove_outliers"],
    c(remove_outliers = 7706)
  )
})


context("aggregatePrice")
test_that("aggregatePrice", {
  expect_equal(
    formatC(sum(head(aggregatePrice(sampleTData$PRICE, on = "secs", k = 30))), digits = 10),
    "   1157.465"
  )
})

context("selectExchange and data cleaning functions")
test_that("selectExchange and data cleaning functions", {
  expect_equal(
    unique(selectExchange(sampleQDataRaw, c("N", "W"))$EX),
    c("N", "W")
  )
  
  expect_equal(
    dim(rmOutliersQuotes(selectExchange(sampleQDataRaw, "W"))),
    dim(rmOutliersQuotes(selectExchange(sampleQDataRaw, "W"), type = "standard"))
  )
  
  expect_equal(
    dim(rmTradeOutliersUsingQuotes(selectExchange(sampleTDataRaw, "W"), selectExchange(sampleQDataRaw, "W"))),
    c(203, 8)
  )
  
  expect_equal(
    dim(rmLargeSpread(selectExchange(sampleQDataRaw, "N"))),
    c(9794, 7)
  )
  
  expect_equal(
    dim(mergeQuotesSameTimestamp(selectExchange(sampleQDataRaw, "N"), selection = "max.volume")),
    c(7707, 7)
  )
  
  expect_equal(
    dim(mergeQuotesSameTimestamp(selectExchange(sampleQDataRaw, "N"), selection = "weighted.average")),
    c(7707, 7)
  )
  
  expect_equal(
    dim(noZeroQuotes(selectExchange(sampleQDataRaw, "N"))),
    c(9792, 7)
  )
  
  
  expect_equal(
  dim(tradesCleanupUsingQuotes(tData = sampleTData, qData = sampleQData)),
  c(8153, 8)
  )
  
  expect_equal(
  dim(tradesCleanup(tDataRaw = sampleTDataRaw, exchanges = "N", report = FALSE)),
  c(9104, 3)
  )
})


context("tradesCleanup")
test_that("tradesCleanup on-disk functionality", {
  skip_on_cran()
  library(data.table)
  options(digits = 6)
  trades1 <- sampleTDataRaw
  trades2 <- sampleTDataRawMicroseconds
  currentWD <- getwd()
  tempDir <- tempdir(check = TRUE)
  setwd(tempDir)
  dir.create("rawTradeData")
  dir.create("rawQuoteData")

  fwrite(trades1, "rawTradeData/trades1.csv")
  fwrite(trades2, "rawTradeData/trades2.csv")

  tradesCleanup(dataSource = "rawTradeData", dataDestination = "cleanedTradeData", exchanges = "N", saveAsXTS = FALSE)

  cleanedTrades1 <- tradesCleanup(tDataRaw = trades1, exchanges = "N", report = FALSE)
  cleanedTrades2 <- tradesCleanup(tDataRaw = trades2, exchanges = "N", report = FALSE)
  cleanedTrades2Day1 <- cleanedTrades2[as.character(as.Date(DT)) == "2018-01-02",]
  cleanedTrades2Day2 <- cleanedTrades2[as.character(as.Date(DT)) == "2018-01-03",]


  onDisk1 <- readRDS("cleanedTradeData/trades1.csv/2008-01-04.rds")
  onDisk2Day1 <- readRDS("cleanedTradeData/trades2.csv/2018-01-02.rds")
  onDisk2Day2 <- readRDS("cleanedTradeData/trades2.csv/2018-01-03.rds")

  ### CLEANUP!
  setwd(currentWD)
  unlink(tempDir, recursive = TRUE, force = TRUE)

  expected2 <- tradesCleanup(tDataRaw = sampleTDataRawMicroseconds, exchanges = "N", report = FALSE)

  expect_equal(cleanedTrades2Day1[,2:4], onDisk2Day1[,2:4])
  
  expect_equal(expected2[as.Date(DT) == "2018-01-02",], cleanedTrades2Day1)
  expect_equal(expected2[as.Date(DT) == "2018-01-03",], cleanedTrades2Day2)

  

})




context("aggregateTS edge cases")
test_that("aggregateTS edge cases", {
  # Test edge cases of aggregateTS
  expect_equal(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))),
                     index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57599, length.out = 23400), origin = '1970-01-01'))))
  )
  
  expect_true(
    max(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))))<
    max(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57601, length.out = 23400), origin = '1970-01-01')))))
  )

})



context("aggregatePrice edge cases")
test_that("aggregatePrice edge cases", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34500, 34500 + 1e-9, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "GMT"), PRICE = 0:7)
  output <- aggregatePrice(dat, on = "minutes", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00", fill = FALSE)
  
  target <- data.table(DT = as.POSIXct(c(34200, 34500, 34800, 35100), origin = "1970-01-01", tz = "GMT"), PRICE = c(1,2,4,7))
  setkeyv(target, c("DT", "PRICE"))
  expect_equal(output, target)
})


context("aggregatePrice milliseconds vs seconds")
test_that("aggregatePrice milliseconds vs seconds", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34500, 34500 + 1e-9, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "GMT"), PRICE = 0:7)
  expect_equal(aggregatePrice(dat, on = "milliseconds", k = 5000, marketOpen = "09:30:00", marketClose = "16:00:00", fill = TRUE),
               aggregatePrice(dat, on = "secs", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00", fill = TRUE))
  
})

context("aggregatePrice filling correctly")
test_that("aggregatePrice filling correctly", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34800, 45500, 45799, 50801, 50803, 57599.01, 57601), origin = "1970-01-01", tz = "GMT"), PRICE = 0:8)
  output <- aggregatePrice(dat, on = "milliseconds", k = 1000, marketOpen = "09:30:00", marketClose = "16:00:00", fill = TRUE)
  expect_equal(sum(output$PRICE == 0), 0) # This should be removed since it happens before the market opens
  expect_equal(sum(output$PRICE == 1), 60 * 10) # 1 is the prevaling price for 10 minutes (It is also the opening price!!!!!)
  expect_equal(sum(output$PRICE == 2), 2 * 60 * 60 + 58 * 60 + 20) # 2 is the prevailing price for 2 hours, 58 minutes and 20 seconds 
  expect_equal(sum(output$PRICE == 3), 4 * 60 + 59) #3 is the prevailiing price for 4 min 59 sec
  expect_equal(sum(output$PRICE == 4), 1 * 60 * 60 + 23 * 60 + 22) # 4 is the prevailiing price for 1 hour, 23 minutes and 22 seconds
  expect_equal(sum(output$PRICE == 5), 2) # 5 is the prevailing price for 2 sec
  expect_equal(sum(output$PRICE == 6), 1 * 60 * 60 + 53 * 60 + 17) # 6 is the prevailing price for 1 hour, 53 mins and 17 sec
  expect_equal(sum(output$PRICE == 7), 1) # 7 only happens once
  expect_equal(sum(output$PRICE == 8), 0) # 8 should be removed since it happens 1 sec after market closes.
  expect_equal(nrow(output), 23401)
})


context("aggregateQuotes edge cases")
test_that("aggregateQuotes edge cases", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34500, 34500 + 1e-9, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "GMT"), 
                    SYMBOL = "XXX", BID = as.numeric(0:7), BIDSIZ = as.numeric(1), OFR = as.numeric(1:8), OFRSIZ = as.numeric(2))
  output <- aggregateQuotes(dat, on = "minutes", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00")
  
  target <- data.table(DT = as.POSIXct(c(34200, 34500, 34800, 35100), origin = "1970-01-01", tz = "GMT"),
                       SYMBOL = "XXX", BID = c(1,2,4,7), BIDSIZ = c(1,1,2,3), OFR = c(1,2,4,7) + 1, OFRSIZ = c(1,1,2,3) * 2)
  
  expect_equivalent(output, target)
  
  expect_true(all.equal(output$BIDSIZ * 2 , output$OFRSIZ))
})


context("aggregateQuotes milliseconds vs seconds")
test_that("aggregateQuotes milliseconds vs seconds", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34500, 34500 + 1e-12, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "GMT"), 
                    SYMBOL = "XXX", BID = as.numeric(0:7), BIDSIZ = as.numeric(1), OFR = as.numeric(1:8), OFRSIZ = as.numeric(2))
  expect_equal(aggregateQuotes(dat, on = "milliseconds", k = 5000, marketOpen = "09:30:00", marketClose = "16:00:00"),
               aggregateQuotes(dat, on = "secs", k = 5, marketOpen = "09:30:00", marketClose = "16:00:00"))
  
})



