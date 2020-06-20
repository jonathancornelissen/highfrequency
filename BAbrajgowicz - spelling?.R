library(reticulate)
library(highfrequency)
source_python("pickle_reader.py")
files <- list.files("/media/emil/Elements SE/massdownload", full.names = TRUE)
prices <- NULL
for (date in seq.Date(as.Date("2006-01-01"), as.Date("2008-12-31"), by = "day")) {
  file <- paste0("/media/emil/Elements SE/massdownload/BA" , gsub("-", "", as.Date(date)),"quotes.pickle")
  quotes <- trades <- NULL
  if(file %in% files){
    # load quotes
    quotes <- read_pickle_file_gzipped(file)
    quotes["SYMBOL"] <- "BA"
    quotes["CR"] <- "0"

    # Convert to XTS
    quotes <- xts(quotes[,-c(1,2)], as.POSIXct(quotes[,1], origin = as.Date(date)), tzone = "GMT")

    colnames(quotes) <- c("BID", "BIDSIZE", "OFR", "OFRSIZE", "MODE", "EX", "SYMBOL", "CR")
    # Clean quotes

    quotes <- quotesCleanup(qDataRaw = quotes,
                            exchanges = unique(autoSelectExchangeQuotes(quotes)[,"EX"]), report = FALSE)



    trades <- read_pickle_file_gzipped(gsub("quotes", "trades", file))

    trades["SYMBOL"] <- "BA"

    trades <- xts(trades[,-c(1,2)], as.POSIXct(trades[,1], origin = as.Date(date)), tzone = "GMT")
    colnames(trades) <- c("PRICE", "SIZE", "COND", "CORR", "G127", "EX", "SYMBOL")

    
    trades <- mergeTradesSameTimestamp(trades)
    trades <- tradesCleanupUsingQuotes(tData = trades, qData = quotes)
    trades <- exchangeHoursOnly(trades)
    if(is.null(prices)){
      prices <- trades[,"PRICE"]
    } else {
      prices <- rbind(prices, trades[,"PRICE"])
    }
  }

}

storage.mode(prices) <- "numeric"
save(prices, file="BAprices.rda")

rm(list = ls())
load("BAprices.rda")
library(highfrequency)
FoFtest <- intradayJumpTest(pData = prices["2007-01-03"], testType = "FoF", K = 20, theta = 0.5)
plot(FoFtest)


foo <- numeric(length(FoFtest$tests))

for (i in 1:length(FoFtest$tests)) {
  foo[i] <- FoFtest$tests[[i]][which.max(abs(FoFtest$tests[[i]][,"L"])), "L"]
}





BNStest <- BNSjumpTest(rData = prices, makeReturns = TRUE, IQestimator = "QP",alignBy = "minutes", alignPeriod = 2, type = "ratio", alpha = 0.95)
plot(as.numeric(BNStest$ztest), ylim = c(min(BNStest, qnorm(universalThreshold)), max(BNStest, -qnorm(universalThreshold))), cex = 0.5)
lines(as.numeric(BNStest$upper), col = "red")
lines(as.numeric(BNStest$lower), col = "red")
lines(rep(-as.numeric(qnorm(universalThreshold)), nrow(BNStest)), col = "blue")
lines(rep(as.numeric(qnorm(universalThreshold)), nrow(BNStest)), col = "blue")



N <- ndays(prices)
1/(N * sqrt(pi * log(N)))


universalThreshold <- pnorm(- sqrt(2 * log(N)))
qnorm(universalThreshold)
