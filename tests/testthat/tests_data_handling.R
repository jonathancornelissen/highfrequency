library(xts)
library(testthat)
library(data.table)
context("autoSelectExchangeTrades")
test_that("autoSelectExchangeTrades", {
  expect_equal(
    unique(autoSelectExchangeTrades(sampleTDataRawMicroseconds, printExchange = FALSE)$EX),
    "D"
  )
  
  expect_equal(
    unique(autoSelectExchangeQuotes(sampleQDataRawMicroseconds, printExchange = FALSE)$EX),
    "N"
  )

})


context("quotesCleanup")
test_that("quotesCleanup", {
  expect_equal(
    quotesCleanup(qDataRaw = sampleQDataRawMicroseconds, exchanges = "N")$report["remove_outliers"],
    c(remove_outliers = 46566)
  )
})


context("aggregatePrice")
test_that("aggregatePrice", {
  expect_equal(
    formatC(sum(head(aggregatePrice(sampleTDataMicroseconds[, list(DT, PRICE)], alignBy = "secs", alignPeriod = 30))$PRICE), digits = 10),
    "     950.73"
  )
})

context("selectExchange and data cleaning functions")
test_that("selectExchange and data cleaning functions", {
  expect_equal(
    unique(selectExchange(sampleQDataRawMicroseconds, c("N", "P"))$EX),
    c("P", "N")
  )
  
  expect_equal(
    dim(rmOutliersQuotes(selectExchange(sampleQDataRawMicroseconds, "N"))),
    dim(rmOutliersQuotes(selectExchange(sampleQDataRawMicroseconds, "N"), type = "standard"))
  )
  
  expect_equal(
    dim(rmTradeOutliersUsingQuotes(selectExchange(sampleTDataRawMicroseconds, "P"), selectExchange(sampleQDataRawMicroseconds, "N"))),
    c(5502, 22)
  )
  
  expect_equal(
    dim(rmLargeSpread(selectExchange(sampleQDataRawMicroseconds, "N"))),
    c(94422, 13)
  )
  
  expect_equal(
    dim(mergeQuotesSameTimestamp(selectExchange(sampleQDataRawMicroseconds, "N"), selection = "max.volume")),
    c(46566, 13)
  )
  
  expect_equal(
    dim(mergeQuotesSameTimestamp(selectExchange(sampleQDataRawMicroseconds, "N"), selection = "weighted.average")),
    c(46566, 13)
  )
  
  expect_equal(
    dim(noZeroQuotes(selectExchange(sampleQDataRawMicroseconds, "N"))),
    c(94422, 13)
  )
  
  
  expect_equal(
  dim(tradesCleanupUsingQuotes(tData = sampleTDataRawMicroseconds, qData = sampleQDataMicroseconds)),
  c(72035, 23)
  )
  
  expect_equal(
  dim(tradesCleanup(tDataRaw = sampleTDataRawMicroseconds, exchanges = "N", report = FALSE)),
  c(6140, 12)
  )
})

context("tradesCleanup")

test_that("tradesCleanup gives same data as the shipped data", {
  
  cleanedMicroseconds <-
    tradesCleanupUsingQuotes(
      tData = tradesCleanup(tDataRaw = sampleTDataRawMicroseconds, exchanges = "N", report = FALSE),
      qData = quotesCleanup(qDataRaw = sampleQDataRawMicroseconds, exchanges = "N", type = "standard", report = FALSE),
      lagQuotes = 0
    )[, c("DT", "SYMBOL", "PRICE", "SIZE")]
  
  setkey(cleanedMicroseconds, SYMBOL, DT)
  expect_equal(cleanedMicroseconds, sampleTDataMicroseconds)
  
  
})

test_that("tradesCleanup on-disk functionality", {
  skip_on_cran()
  if(Sys.getenv("USER") != "emil"){
    skip("Skipped to not mess with other people's files")
  }
  library(data.table)
  DT <- SYMBOL <- NULL
  trades2 <- sampleTDataRawMicroseconds
  quotes2 <- sampleQDataRawMicroseconds
  trades2[,DT := as.POSIXct(DT, tz = "EST")]
  quotes2[,DT := as.POSIXct(DT, tz = "EST")]
  setwd("/home/emil/tmp/")
  
  rawDataSource <- paste0(LETTERS[sample(1:26, size = 10)], collapse = "")
  tradeDataSource <- paste0(LETTERS[sample(1:26, size = 10)], collapse = "")
  quoteDataSource <- paste0(LETTERS[sample(1:26, size = 10)], collapse = "")
  dataDestination <- paste0(LETTERS[sample(1:26, size = 10)], collapse = "")
  dir.create(rawDataSource)
  fwrite(quotes2, paste0(rawDataSource, "/quotes2.csv"))
  fwrite(trades2, paste0(rawDataSource, "/trades2.csv"))
  tradesCleanup(dataSource = rawDataSource, dataDestination = tradeDataSource, exchanges = "N", saveAsXTS = FALSE, tz = "EST")
  quotesCleanup(dataSource = rawDataSource, dataDestination = quoteDataSource, exchanges = "N", saveAsXTS = FALSE, type = "standard", tz = "EST")
  tradesCleanupUsingQuotes(tradeDataSource = tradeDataSource, quoteDataSource = quoteDataSource, dataDestination = dataDestination,
                           lagQuotes = 0)
  
  onDiskDay1 <- readRDS(paste0(dataDestination, "/", "trades2.csv/2018-01-02tradescleanedbyquotes.rds"))
  onDiskDay2 <- readRDS(paste0(dataDestination, "/", "trades2.csv/2018-01-03tradescleanedbyquotes.rds"))
  

  ### CLEANUP!
  setwd("/home/emil/tmp")  ## Emil Sjoerup's computer
  unlink(rawDataSource, recursive = TRUE, force = TRUE)
  unlink(tradeDataSource, recursive = TRUE, force = TRUE)
  unlink(quoteDataSource, recursive = TRUE, force = TRUE)
  unlink(dataDestination, recursive = TRUE, force = TRUE)
  
  sampleTDataMicrosecondsDay1 <-
    tradesCleanupUsingQuotes(
      tData = tradesCleanup(tDataRaw = sampleTDataRawMicroseconds[as.Date(DT) == "2018-01-02"], exchanges = "N", report = FALSE),
      qData = quotesCleanup(qDataRaw = sampleQDataRawMicroseconds[as.Date(DT) == "2018-01-02"], exchanges = "N", type = "advanced", report = FALSE),
      lagQuotes = 0
    )[, c("DT", "SYMBOL", "PRICE", "SIZE")]
  
  
  sampleTDataMicrosecondsDay2 <-
    tradesCleanupUsingQuotes(
      tData = tradesCleanup(tDataRaw = sampleTDataRawMicroseconds[as.Date(DT) == "2018-01-03"], exchanges = "N", report = FALSE),
      qData = quotesCleanup(qDataRaw = sampleQDataRawMicroseconds[as.Date(DT) == "2018-01-03"], exchanges = "N", type = "standard", report = FALSE),
      lagQuotes = 0
    )[, c("DT", "SYMBOL", "PRICE", "SIZE")]
  
  
  
  onDiskDay1 <- onDiskDay1[as.Date(DT, tz = "EST") == "2018-01-02",c("DT", "SYMBOL", "PRICE", "SIZE")][, DT := DT - 18000]
  onDiskDay2 <- onDiskDay2[as.Date(DT, tz = "EST") == "2018-01-03",c("DT", "SYMBOL", "PRICE", "SIZE")][, DT := DT - 18000]
  setkey(onDiskDay1, SYMBOL, DT)
  setkey(onDiskDay2, SYMBOL, DT)
  expect_equal(onDiskDay1[,-"DT"], sampleTDataMicrosecondsDay1[,-"DT"])
  expect_equal(onDiskDay2[,-"DT"], sampleTDataMicrosecondsDay2[,-"DT"])
  ## Test that they are equal to the shipped data
  cleanedMicroseconds <-  rbind(sampleTDataMicrosecondsDay1, sampleTDataMicrosecondsDay2)
  setkey(cleanedMicroseconds, SYMBOL, DT)
  expect_equal(sampleTDataMicroseconds, cleanedMicroseconds) 
  
})



# test_that("sampleTData matches cleaned sampleTDataRaw", {
#   
#   cleaned <- tradesCleanup(tDataRaw = sampleTDataRaw, exchanges = "N", report = FALSE)  
#   
#   
#   cleaned <- tradesCleanupUsingQuotes(tData = tradesCleanup(tDataRaw = sampleTDataRaw, exchanges = "N", report = FALSE),
#                                       qData = quotesCleanup(qDataRaw = sampleQDataRaw, exchanges = "N", type = "advanced", report = FALSE))
#   
#   
#   cleaned <- cleaned$PRICE
#   old <- sampleTData$PRICE
#   storage.mode(cleaned) <- storage.mode(old) <- "numeric"
#   plot(cleaned, lwd = 1)
#   plot(na.locf(cbind(cleaned, old)), col = 2:1)
#   lines(old, col = "red", lwd = 1)
# })




context("aggregateTS edge cases")
test_that("aggregateTS edge cases", {
  # Test edge cases of aggregateTS
  expect_equal(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01')))),
                     index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57599, length.out = 23400), origin = '1970-01-01'))))
  )
  
  expect_true(
    max(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57600, length.out = 23400), origin = '1970-01-01', alignBy = "minutes", alignPeriod = 1)))))<
    max(index(aggregateTS(xts(1:23400, as.POSIXct(seq(34200, 57601, length.out = 23400), origin = '1970-01-01', alignBy = "minutes", alignPeriod = 1)))))
    # The last one will have an extra minute in this case!
  )

})


context("aggregatePrice time zones")
test_that("aggregatePrice time zones", {
  dat <- data.table(DT = as.POSIXct(c(34150, 34201, 34201, 34500, 34500 + 1e-6, 34799, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "EST"), PRICE = 0:9)
  
  output <- aggregatePrice(dat, alignBy = "minutes", alignPeriod = 5, marketOpen = "04:30:00", marketClose = "11:00:00", fill = FALSE)
  target <- data.table(DT = as.POSIXct(c(34200, 34500, 34800, 35100), origin = "1970-01-01", tz = "EST"), PRICE = c(1,3,6,9))
  expect_equal(output, target)
  
  dat <- as.xts(dat)
  
  output <- aggregatePrice(dat, alignBy = "minutes", alignPeriod = 5, marketOpen = "04:30:00", marketClose = "11:00:00", fill = FALSE, tz = "EST")
  target <- xts(c(1,3,6,9), as.POSIXct(c(34200, 34500, 34800, 35100), origin = "1970-01-01", tz = "EST")) 
  colnames(target) <- "PRICE"
  expect_equal(output, target)
  
  dat <- data.table(DT = as.POSIXct(c(34150, 34201, 34201, 34500, 34500 + 1e-6, 34799, 34799, 34801, 34803, 35099) + 86400 * c(rep(1,10), rep(200,10)),
                                    origin =  as.POSIXct("1970-01-01", tz = "EST"), tz = "EST"), PRICE = rep(0:9, 2))
  
  output <- aggregatePrice(dat, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", fill = FALSE)
  target <- data.table(DT = as.POSIXct(c(34200, 34500, 34800, 35100) + 86400 * c(rep(1,4), rep(200,4)), origin = as.POSIXct("1970-01-01", tz = "EST"), tz = "EST"), PRICE = rep(c(1,3,6,9), 2))
  expect_equal(output, target)
  
  
  
})


context("aggregatePrice edge cases")
test_that("aggregatePrice edge cases", {
  dat <- data.table(DT = as.POSIXct(c(34150, 34201, 34201, 34500, 34500 + 1e-9, 34799, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "UTC"), PRICE = 0:9)
  output <- aggregatePrice(dat, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", fill = FALSE)
  target <- data.table(DT = as.POSIXct(c(34200, 34500, 34800, 35100), origin = "1970-01-01", tz = "UTC"), PRICE = c(1,3,6,9))
  expect_equal(output, target)
})


context("aggregatePrice milliseconds vs seconds")
test_that("aggregatePrice milliseconds vs seconds", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34500, 34500 + 1e-9, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "GMT"), PRICE = 0:7)
  expect_equal(aggregatePrice(dat, alignBy = "milliseconds", alignPeriod = 5000, marketOpen = "09:30:00", marketClose = "16:00:00", fill = TRUE),
               aggregatePrice(dat, alignBy = "secs", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00", fill = TRUE))
  
})

context("aggregatePrice filling correctly")
test_that("aggregatePrice filling correctly", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34800, 45500, 45799, 50801, 50803, 57599.01, 57601), origin = "1970-01-01", tz = "GMT"), PRICE = 0:8)
  output <- aggregatePrice(dat, alignBy = "milliseconds", alignPeriod = 1000, marketOpen = "09:30:00", marketClose = "16:00:00", fill = TRUE)
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
  output <- aggregateQuotes(dat, alignBy = "minutes", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00")
  
  target <- data.table(DT = as.POSIXct(c(34200, 34500, 34800, 35100), origin = "1970-01-01", tz = "GMT"),
                       SYMBOL = "XXX", BID = c(1,2,4,7), BIDSIZ = c(1,1,2,3), OFR = c(1,2,4,7) + 1, OFRSIZ = c(1,1,2,3) * 2)
  
  expect_equivalent(output, target)
  
  expect_true(all.equal(output$BIDSIZ * 2 , output$OFRSIZ))
})


context("aggregateQuotes milliseconds vs seconds")
test_that("aggregateQuotes milliseconds vs seconds", {
  dat <- data.table(DT = as.POSIXct(c(34150 ,34201, 34500, 34500 + 1e-12, 34799, 34801, 34803, 35099), origin = "1970-01-01", tz = "GMT"), 
                    SYMBOL = "XXX", BID = as.numeric(0:7), BIDSIZ = as.numeric(1), OFR = as.numeric(1:8), OFRSIZ = as.numeric(2))
  expect_equal(aggregateQuotes(dat, alignBy = "milliseconds", alignPeriod = 5000, marketOpen = "09:30:00", marketClose = "16:00:00"),
               aggregateQuotes(dat, alignBy = "secs", alignPeriod = 5, marketOpen = "09:30:00", marketClose = "16:00:00"))
  
})



context("business time aggregation")
test_that("business time aggregation",{
  skip_if_not(capabilities('long.double'), 'Skip tests when long double is not available')
  pData <- sampleTDataMicroseconds
  agged1 <- businessTimeAggregation(pData, measure = "intensity", obs = 390, bandwidth = 0.075)
  expect_equal(nrow(agged1$pData), 780) # We return the correct number of observations
  
  
  expect_warning(businessTimeAggregation(pData, measure = "volume", obs = 390), "smaller")
  agged2 <- suppressWarnings(businessTimeAggregation(pData, measure = "volume", obs = 390))
  expect_equal(nrow(agged2$pData), 748)
  
  agged3 <- suppressWarnings(businessTimeAggregation(pData, measure = "vol", obs = 39, method = "PARM", RM = "rv", lookBackPeriod = 5))
  expect_equal(nrow(agged3$pData), 76)
  
  # pData <- sampleTData[,c("PRICE", "SIZE")]
  # storage.mode(pData) <- "numeric"
  # agged4 <- businessTimeAggregation(pData, measure = "intensity", obs = 390, bandwidth = 0.075)
  # expect_equal(nrow(agged4$pData), 390) # We return the correct number of observations
  # 
  # 
  # agged5 <- suppressWarnings(businessTimeAggregation(pData, measure = "volume", obs = 78))
  # expect_equal(nrow(agged5$pData), 78)
  # 
  # agged6 <- suppressWarnings(businessTimeAggregation(pData, measure = "vol", obs = 39, method = "PARM", RM = "rv", lookBackPeriod = 5))
  # expect_equal(nrow(agged6$pData), 39)
  
})




context("refreshTime")
test_that("refreshTime", {
  
  # Unit test for the refreshTime algorithm based on Kris' example in http://past.rinfinance.com/agenda/2015/workshop/KrisBoudt.pdf
  #suppose irregular timepoints: 
  start = as.POSIXct("2010-01-01 09:30:00", tz = "GMT") 
  ta = start + c(1, 2, 4, 5, 9, 14)
  tb = start + c(1, 3, 6, 7, 8, 9, 10, 11, 15)
  tc = start + c(1, 2, 3, 5, 7, 8, 10, 13)
  a = as.xts(1:length(ta), order.by = ta) 
  b = as.xts(1:length(tb), order.by = tb)
  c = as.xts(1:length(tc), order.by = tc) 
  #Calculate the synchronized timeseries: 
  expected <- xts(matrix(c(1,1,1, 2,2,3, 4,3,4, 5,6,6, 6,8,8), ncol = 3, byrow = TRUE), order.by = start + c(1,3,6,9,14))
  colnames(expected) <- c("a", "b", "c")
  expect_equal(refreshTime(list("a" = a, "b" = b, "c" = c)), expected)
  
  squaredDurationCriterion <- function(x) sum(as.numeric(diff(index(x)))^2)
  durationCriterion <- function(x) sum(as.numeric(diff(index(x))))
  sqDur <- sort(sapply(list("a" = a, "b" = b, "c" = c), squaredDurationCriterion), index.return = TRUE)$ix
  dur <- sort(sapply(list("a" = a, "b" = b, "c" = c), durationCriterion), index.return = TRUE)$ix
  
  expect_equal(refreshTime(list("b" = b, "a" = a, "c" = c), sort = TRUE, criterion = "squared duration"), expected[, sqDur])
  expect_equal(refreshTime(list("b" = b, "a" = a, "c" = c), sort = TRUE, criterion = "duration"), expected[, dur])
  
  
  aDT <- data.table(index(a), a)
  colnames(aDT) <- c("DT", "PRICE")
  bDT <- data.table(index(b), b)
  colnames(bDT) <- c("DT", "PRICE")
  cDT <- data.table(index(c), c)
  colnames(cDT) <- c("DT", "PRICE")
  
  RT <- refreshTime(list("a" = aDT, "b" = bDT, "c" = cDT))
  
  expected <- data.table(DT =start + c(1,3,6,9,14), 
                         matrix(c(1,1,1, 2,2,3, 4,3,4, 5,6,6, 6,8,8), ncol = 3, byrow = TRUE, dimnames = list(NULL, c("a", "b", "c"))))
  
  expect_equal(RT, expected)
  
  expect_equal(refreshTime(list("a" = aDT, "b" = bDT, "c" = cDT), sort = TRUE, criterion = "squared duration"), 
               expected[, names(expected)[c(1, sqDur + 1)], with = FALSE])
  expect_equal(refreshTime(list("a" = aDT, "b" = bDT, "c" = cDT), sort = TRUE, criterion = "duration"),
               expected[, names(expected)[c(1, dur + 1)], with = FALSE])
  
})


library(data.table)

context("makeRMFormat")
test_that("makeRMFormat",{
  set.seed(1)
  PRICE <- DT <- .N <- NULL
  data1 <- copy(sampleTDataMicroseconds)[,  `:=`(PRICE = PRICE * runif(.N, min = 0.99, max = 1.01),
                                                 DT = DT + runif(.N, 0.01, 0.02))]
  data2 <- copy(sampleTDataMicroseconds)[, SYMBOL := 'XYZ']
  
  dat <- rbind(data1, data2)
  setkey(dat, "DT")
  dat <- makeRMFormat(dat)
  
  res <- rCov(dat, alignBy = 'minutes', alignPeriod = 5, makeReturns = TRUE, cor = TRUE)
  target <- list("2018-01-02" = matrix(c(1, 0.05400510115,
                                         0.05400510115, 1), ncol = 2),
                 "2018-01-03" = matrix(c(1, 0.171321754,
                                         0.171321754, 1), ncol = 2)
                 )
  expect_equal(res, target)
  
  
})
