
#################################################
################# VIGNETTE CODE #################
#################################################

###### DESCRIPTION ######

### This code is used in the vignette paper 'Analyzing intraday financial data in R: The highfrequency package' (Boudt, Kleen, and Sjoerup, 2020).
### See the paper for the results and setup details.
### Download the package and its dependencies first before you run this script.
### install.packages("highfrequency", dependencies = TRUE) # from CRAN (version 0.9.1),

###### SESSION INFO ######

# Session used to create the JSS submission

# R version 4.1.1 (2021-08-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Catalina 10.15.7
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] shape_1.4.6         data.table_1.14.2   xts_0.12.1          zoo_1.8-9           highfrequency_0.9.1
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.7               lattice_0.20-45          RcppRoll_0.3.0           truncnorm_1.0-8         
# [5] grid_4.1.1               RcppArmadillo_0.10.7.0.0 robustbase_0.93-9        sandwich_3.0-1          
# [9] tools_4.1.1              DEoptimR_1.0-9           numDeriv_2016.8-1.1      parallel_4.1.1          
# [13] compiler_4.1.1           Rsolnp_1.16           


remove(list = ls())
options(prompt = "R> ", continue = "+  ", width = 120, digits = 4, max.print = 90, useFancyQuotes = FALSE)
sink(file = "output_vignette.txt", append = FALSE, split = TRUE) # output printed in .txt file

library("highfrequency")
library("xts")
library("data.table")

options(scipen = 1, digits = 3)
Sys.setenv(LANG = 'en')

# RNG initialization
set.seed(42)

## Set colors for bids and offer that will be used throughout
bidColor <- "green" 
offerColor <- "blue"

##################
###### CODE ######
##################

cat("### SECTION 2 ####################### \n \n")

cat("# 2.1. Raw tick-by-tick datasets \n \n")
cat("R> print(sampleTDataRaw, topn = 4) \n")
print(sampleTDataRaw, topn = 4)
cat("\n \n")

cat("R> print(exchangeHoursOnly(sampleQDataRaw), topn = 4) \n")
print(exchangeHoursOnly(sampleQDataRaw), topn = 4)
cat("\n \n")

cat("# 2.2. Preparing raw price and quote data for analysis \n \n")

cat("# Figure_1.pdf and Figure_2.pdf are generated. \n \n")
sink()

pdf("Figure_1.pdf")
rawDat <- sampleTDataRaw[as.Date(DT, tz = "EST") == "2018-01-03"]
rawQDat <- sampleQDataRaw[as.Date(DT, tz = "EST") == "2018-01-03"]
rawQDat <- exchangeHoursOnly(rawQDat)
rawDat <- exchangeHoursOnly(rawDat)
xLim <- range(as.POSIXct("2018-01-03 11:00:00", tz = "EST"), as.POSIXct("2018-01-03 12:00:00", tz = "EST"))
par(mgp = c(3,1,0), mai = c(0.4, 0.6, 0.35, 0.35))
yLim <- range(c(rawDat[DT %between% xLim]$PRICE, rawQDat[DT %between% xLim & BID!=0 & OFR != 0][, list(BID, OFR)]))

rawDat <- rawDat[DT %between% xLim]

rawQDat <- rawQDat[DT %between% xLim]
rawQDat <- rawQDat[BID != 0 & OFR != 0]
rawQDat <- rawQDat[BID < 155 | OFR > 157]

plot(rawQDat$DT, rawQDat$BID, ylim = yLim,  main = "",
     ylab = "Price", xlab = "Time", xlim = xLim, xaxs = "i", lwd = 1, pch = 19, xaxt='n', las = 2)
points(rawQDat$DT, rawQDat$OFR, ylim = yLim, xlim = xLim, pch = 19)
lines(rawDat$DT, rawDat$PRICE, xlim = xLim,  col = "red", lwd = 2)
axis(1, at = seq(xLim[1], xLim[2], by = 60*10), labels = format(seq(xLim[1], xLim[2], by = 60*10), format = "%H:%M"))
dev.off()

pdf("Figure_2.pdf")
cleanedDat <- tradesCleanupUsingQuotes(tData = tradesCleanup(tDataRaw = sampleTDataRaw,exchange = "N", report = FALSE, printExchange = FALSE),
                                       qData = quotesCleanup(qDataRaw = sampleQDataRaw, exchange = "N", report = FALSE, printExchange = FALSE))[as.Date(DT) == "2018-01-03"]
par(mfrow = c(2, 1), mai = c(0.4, 0.6, 0.35, 0.35), mgp = c(3,0.5,0))
xLim <- range(as.POSIXct("2018-01-03 09:30:00", tz = "EST"), as.POSIXct("2018-01-03 16:00:00", tz = "EST"))
rawDat <- sampleTDataRaw[as.Date(DT, tz = "EST") == "2018-01-03"]
rawDat <- exchangeHoursOnly(rawDat)
## Top plot:
plot(0, 0, main = "", ylab = "price", xlab = "Time", xlim = xLim, xaxs = "i", type = "l", xaxt = "n", ylim = range(rawDat$PRICE), las = 2)
xLim2 <- range(as.POSIXct(c("2018-01-03 11:45:00 EST", "2018-01-03 12:00:00 EST"), tz = "EST"))
rect(xLim2[1], min(rawDat$PRICE), xLim2[2], max(rawDat$PRICE), border = FALSE, col = "darkgray")
lines(rawDat$DT, rawDat$PRICE, xlim = xLim, type = "l")
lines(cleanedDat$DT, cleanedDat$PRICE, col = "red")
legend(xLim[2]-8000, 159, legend = c("raw","cleaned"), lty = c(1, 1), col = c("black", "red"), pt.cex = 2, box.lwd = 0, bty="n")
axis(1, at = seq(xLim[1], xLim[2], by = 60*30)[c(1,2,4,6,8,10,12,14)], labels = format(seq(xLim[1], xLim[2], by = 60*30)[c(1,2,4,6,8,10,12,14)], format = "%H:%M"), cex = 0.9)
## Bottom plot:
plotTQData(cleanedDat, sampleQData, xLim2, main = "", cex = 0.5, las = 2, xlab = "time", ylab = "price")
dev.off()

sink(file = "output_vignette.txt", append = TRUE, split = TRUE)

cat("R> sampleQData <- quotesCleanup(qDataRaw = sampleQDataRaw, \n")
cat("+                             exchanges = \"N\", \n")
cat("+                             type = \"standard\", report = FALSE) \n \n")
sampleQData <- quotesCleanup(qDataRaw = sampleQDataRaw,
                             exchanges = "N", 
                             type = "standard", report = FALSE)
cat("R> tradesAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, \n")
cat("+                                          exchanges = \"N\", report = FALSE) \n")
tradesAfterFirstCleaning <- tradesCleanup(tDataRaw = sampleTDataRaw, 
                                          exchanges = "N", report = FALSE)
cat("R> sampleTData <- tradesCleanupUsingQuotes(tData = tradesAfterFirstCleaning, \n")
cat("+                                        qData = sampleQData, \n")
cat("+                                        lagQuotes = 0)[, c(\"DT\", \"EX\", \"SYMBOL\", \"PRICE\", \"SIZE\")] \n")
sampleTData <- tradesCleanupUsingQuotes(tData = tradesAfterFirstCleaning,
                                        qData = sampleQData,
                                        lagQuotes = 0)[, c("DT", "EX",
                                                           "SYMBOL",
                                                           "PRICE", "SIZE")]
cat("R> print(sampleTData, topn = 4) \n")
print(sampleTData, topn = 4)
cat("\n \n")

cat("# 2.3. Aggregating high-frequency data \n \n")

cat("R> agg <- aggregateTrades(sampleTData[, list(DT, PRICE, SIZE, SYMBOL)], \n")
cat("+                       marketOpen = \"09:30:00\", marketClose = \"16:00:00\", \n")
cat("+                       alignBy = \"minutes\", alignPeriod = 5) \n ")      
agg <- aggregateTrades(sampleTData[, list(DT, PRICE, SIZE, SYMBOL)],
                       marketOpen = "09:30:00", marketClose = "16:00:00",
                       alignBy = "minutes", alignPeriod = 5)
cat("R> print(agg, digits = 6, topn = 4) \n")
print(agg, digits = 6, topn = 4)

cat("\n")
cat("# Figure_3.pdf is generated. \n \n")
sink()

### 

pdf("Figure_3.pdf")

start = as.POSIXct("1970-01-01", tz = "GMT")
ta = start + c(0.1, 1.2, 4, 5, 9.2, 14)
tb = start + c(0, 3.25, 6, 6.5, 6.9, 7.3, 7.7, 11, 15)
tc = start + c(0.7, 1.3, 2, 5, 7, 8.3, 10, 13)
a = as.xts(1:length(ta), order.by = ta)
b = as.xts(1:length(tb), order.by = tb)
c = as.xts(1:length(tc), order.by = tc)
refreshTime(list("a" = a, "b" = b, "c" = c))
par(mfrow = c(2, 1), mai = c(0, 0.3, 0.2, 0), mar = c(2.5, 3.5, 1.5, 0))
plot(x = seq(range(ta,tb,tc)[1], range(ta,tb,tc)[2],length.out = max(length(ta), length(tb), length(tc))), 
     y = seq(1,4,length.out = max(length(ta), length(tb), length(tc))), col = 0, ylab = "", xlab = "", axes = FALSE,
     main = "", las = 2)
title(xlab = "time", mgp = c(1.25,0,0))
axis(1, at = seq(1,15, 2), mgp = c(0,0.5,0))
axis(2, at = c(1.5, 2.5, 3.5), labels = paste("asset", c("A", "B", "C")), las = 2, mgp = c(0,0.5,0))
box()

lines(c(ta[1], ta[1]), c(1,2), col = 1, lty = 2)
lines(c(tb[1], tb[1]), c(2,3), col = "red", lty = 2)
lines(c(tc[1], tc[1]), c(3,4), col = "blue", lty = 2)
points(ta[1], c(1.5), pch = "1", cex = 2)
points(tb[1], c(2.5), pch = "1", cex = 2)
points(tc[1], c(3.5), pch = "1", cex = 2)

idx <- 2
for (i in ta[-1]) {
  lines(c(i, i), c(1,2), col = 1, lty = 2)
  points(c(i), c(1.5), col = 1, pch = paste(as.numeric(a[idx])), cex = 2)
  idx <- idx + 1
}

idx <- 2
for (i in tb[-1]) {
  lines(c(i, i), c(2,3), col = "red", lty = 2)
  points(c(i), c(2.5), col = 1, pch = paste(as.numeric(b[idx])), cex = 2)
  idx <- idx + 1
}
idx <- 2
for (i in tc[-1]) {
  lines(c(i, i), c(3,4), col = "blue", lty = 2)
  points(c(i), c(3.5), col = 1, pch = paste(as.numeric(c[idx])), cex = 2)
  idx <- idx + 1
}
## Add arrows:
shape::Arrows(0.25, 1.5, 0.65, 1.5, code =2, arr.adj = 0, arr.type = "simple", col = "black")
shape::Arrows(0.15, 2.5, 0.65, 2.5, code =2, arr.adj = 0, arr.type = "simple", col = "red")
shape::Arrows(1.5, 1.5, 3, 1.5, code =2, arr.adj = 0, arr.type = "simple")
shape::Arrows(2.25, 3.5, 3, 3.5, code =2, arr.adj = 0, arr.type = "simple", col = "blue")
shape::Arrows(5.25, 1.5, 5.75, 1.5, code =2, arr.adj = 0, arr.type = "simple")
shape::Arrows(11.25, 2.5, 13.75, 2.5, code =2, arr.adj = 0, arr.type = "simple", col = "red")

shape::Arrows(5.25, 3.5, 5.75, 3.5, code =2, arr.adj = 0, arr.type = "simple", col = "blue")
shape::Arrows(8.35, 3.5, 9.15, 3.5, code =2, arr.adj = 0, arr.type = "simple", col = "blue")
shape::Arrows(7.85, 2.5, 9.15, 2.5, code =2, arr.adj = 0, arr.type = "simple", col = "red")
shape::Arrows(13.25, 3.5, 13.75, 3.5, code =2, arr.adj = 0, arr.type = "simple", col = "blue")

RT <- refreshTime(list("a" = a, "b" = b, "c" = c))
plot(x = seq(range(ta,tb,tc)[1], range(ta,tb,tc)[2],length.out = max(length(ta), length(tb), length(tc))), 
     y = seq(1,4,length.out = max(length(ta), length(tb), length(tc))), col = 0, ylab = "", xlab = "", axes = FALSE,
     main = "", las = 2)
title(xlab = "time", mgp = c(1.25,0,0))
axis(1, at = seq(1,15, 2), mgp = c(0,0.5,0))
axis(2, at = c(1.5, 2.5, 3.5), labels = paste("asset", c("A", "B", "C")), las = 2, mgp = c(0,0.5,0))
box()

RTstart <- as.numeric(index(RT)[1])
lines(c(RTstart, RTstart), c(1,2), col = 1, lty = 2)
lines(c(RTstart, RTstart), c(2,3), col = "red", lty = 2)
lines(c(RTstart, RTstart), c(3,4), col = "blue", lty = 2)
points(RTstart, c(1.5), pch = "1", cex = 2)
points(RTstart, c(2.5), pch = "1", cex = 2)
points(RTstart, c(3.5), pch = "1", cex = 2)

idx <- 2
for (i in as.numeric(index(RT))[-1]) {
  lines(c(i, i), c(1,2), col = 1, lty = 2)
  points(c(i), c(1.5), col = 1, pch = paste(RT[idx,1]), cex = 2)
  lines(c(i, i), c(2,3), col = "red", lty = 2)
  points(c(i), c(2.5), col = 1, pch = paste(RT[idx,2]), cex = 2)
  lines(c(i, i), c(3,4), col = "blue", lty = 2)
  points(c(i), c(3.5), col = 1, pch = paste(RT[idx,3]), cex = 2)
  idx <- idx + 1
}

dev.off()


sink(file = "output_vignette.txt", append = TRUE, split = TRUE)

cat("### SECTION 5 ####################### \n \n")

cat("# 5.1. Univariate realized measures \n \n")
cat("# Figure_4.pdf is generated. \n \n")
sink()

pdf("Figure_4.pdf")

par(mfrow = c(2,2), mai = c(0, 0, 0, 0), mar = c(3, 3, 1.1, 3.1))
oneMinute <- sampleOneMinuteData[as.Date(DT) > "2001-08-30"]
ylim1 <- range(oneMinute$MARKET)
ylim2 <- range(oneMinute$STOCK)
for (date in unique(as.Date(oneMinute$DT))) {
  plot(oneMinute[as.Date(DT) == date, DT], oneMinute[as.Date(DT) == date, MARKET], type = "l", ylab = "Price", xlab = "hour:minute", xaxs = "i", main = as.Date(date, origin = "1970-01-01"), ylim = ylim1, axes = FALSE)
  axis(2, las = 2)
  box()
  par(new = TRUE)
  plot(oneMinute[as.Date(DT) == date, DT], oneMinute[as.Date(DT) == date, STOCK], type = "l", ylab = "Price", xlab = "hour:minute", xaxs = "i", axes = FALSE, col = "red", lty = 4, ylim = ylim2, las = 2)
  axis(4, col.axis = "red", las = 2)
  xLim <- range(as.POSIXct(paste(as.Date(date), "09:30:00"), tz = "UTC"), as.POSIXct(paste(as.Date(date), "16:00:00"), tz = "UTC"))
  axis(1, at = seq(xLim[1], xLim[2], by = 60*30)[c(1,2,4,6,8,10,12,14)], labels = format(seq(xLim[1], xLim[2], by = 60*30)[c(1,2,4,6,8,10,12,14)], format = "%H:%M"), cex = 0.9)
}

dev.off()

sink(file = "output_vignette.txt", append = TRUE, split = TRUE)

cat("R> oneMinute <- sampleOneMinuteData[as.Date(DT) > \"2001-08-30\"] \n ")
oneMinute <- sampleOneMinuteData[as.Date(DT) > "2001-08-30"]
cat("R> RV1 <- rCov(oneMinute[, list(DT, MARKET)], makeReturns = TRUE,\n")
cat("+                 alignBy = \"minutes\", alignPeriod = 1) \n")
RV1 <- rCov(oneMinute[, list(DT, MARKET)], makeReturns = TRUE,
            alignBy = "minutes", alignPeriod = 1)
cat("R> print(RV1) \n")
print(RV1)
cat("\n")

cat("R> BPV1 <- rBPCov(oneMinute[, list(DT, MARKET)], makeReturns = TRUE) \n")
BPV1 <- rBPCov(oneMinute[, list(DT, MARKET)], makeReturns = TRUE)
cat("R> print(BPV1) \n \n")
print(BPV1)
cat("\n\n")

### Volatility signature plot #######

cat("R> nums <- c(1, 2, 5, 10, 20, 30, 60, 90, 180, 360, 600) \n")
nums <- c(1, 2, 5, 10, 20, 30, 60, 90, 180, 360, 600)
cat("R> rvAgg <- matrix(nrow = length(nums), ncol = 2)")
rvAgg <- matrix(nrow = length(nums), ncol = 2)
cat("R> for(i in 1:nrow(rvAgg)) { \n")
cat("+ rvAgg[i, 1] <- rCov(sampleMultiTradeData[SYMBOL == 'AAA', list(DT, PRICE)], \n")
cat("+                    alignBy = \"ticks\", alignPeriod = nums[i], \n")
cat("+                      makeReturns = TRUE)[[2]] \n")
cat("+ rvAgg[i, 2] <- rCov(sampleMultiTradeData[SYMBOL == 'AAA', list(DT, PRICE)], \n")
cat("+                    alignBy = \"seconds\", alignPeriod = nums[i],\n")
cat("+                    makeReturns = TRUE)[[2]] \n \n")
cat("+} \n \n")
for(i in 1:nrow(rvAgg)) {
  rvAgg[i, 1] <- rCov(sampleMultiTradeData[SYMBOL == 'AAA', list(DT, PRICE)],
                      alignBy = "ticks", alignPeriod = nums[i], 
                      makeReturns = TRUE)[[2]]
  rvAgg[i, 2] <- rCov(sampleMultiTradeData[SYMBOL == 'AAA', list(DT, PRICE)],
                      alignBy = "seconds", alignPeriod = nums[i],
                      makeReturns = TRUE)[[2]]
}

cat("# Figure_5.pdf is generated. \n \n")

sink()

pdf("Figure_5.pdf")
par(mfrow = c(2,1), cex.axis = 0.8, mai = c(0,0, 0, 0), mar = c(3, 4, 1, 2))
plot(x = factor(nums), rep(0, length(nums)), type = "l", axes = FALSE, xlab = "", ylab = "", 
     ylim = c(18,50))
lines(factor(nums), sqrt(rvAgg[,1] * 252) * 100)
axis(1, at = factor(nums), labels = nums)
axis(2, las = 2)
title(xlab = "ticks", mgp = c(1.75,0,0))
box()
plot(x = factor(nums), rep(0, length(nums)), type = "l", axes = FALSE, xlab = "", ylab = "", 
     ylim = c(18,50))
lines(factor(nums), sqrt(rvAgg[,2] * 252) * 100)
axis(1, at = factor(nums), labels = nums)
title(xlab = "seconds", mgp = c(1.75,0,0))
axis(2, las = 2)
box()
dev.off()

sink(file = "output_vignette.txt", append = TRUE, split = TRUE)

### 

cat("R> listAvailableKernels() \n")
listAvailableKernels()
cat("\n \n")

### 

cat("R> RK1 <- rKernelCov(oneMinute[, list(DT, MARKET)], kernelType = \"Parzen\",\n")
cat("+                  makeReturns = TRUE)) \n")
RK1 <- rKernelCov(oneMinute[, list(DT, MARKET)], kernelType = "Parzen",
                  makeReturns = TRUE)
cat("R> print(RK1)")
print(RK1)

cat("\n\n")

cat("# 5.2 Multivariate realized measures \n \n")

cat("R> rCov(oneMinute, makeReturns = TRUE)[1:2] \n")
rCov(oneMinute, makeReturns = TRUE)[1:2]

### 

cat("R> rMRCov(list(\"ETF\" = sampleMultiTradeData[SYMBOL == \"ETF\", list(DT, PRICE)], \n")
cat("+             \"AAA\" = sampleMultiTradeData[SYMBOL == \"AAA\", list(DT, PRICE)]), \n")
cat("+             theta = 0.1) \n")
rMRCov(list("ETF" = sampleMultiTradeData[SYMBOL == "ETF", list(DT, PRICE)],
            "AAA" = sampleMultiTradeData[SYMBOL == "AAA", list(DT, PRICE)]), 
       theta = 0.1)
cat("\n ")
cat("R> rCov(spreadPrices(sampleMultiTradeData[SYMBOL %chin% c(\"ETF\", \"AAA\")]), \n") 
cat("+                  alignBy = \"ticks\", alignPeriod = 1, makeReturns = TRUE) \n")
rCov(spreadPrices(sampleMultiTradeData[SYMBOL %chin% c("ETF", "AAA")]), 
     alignBy = "ticks", alignPeriod = 1, makeReturns = TRUE) 
cat("\n \n")

### 

cat("# 5.3. Noise variance estimation \n\n")

cat("R> stockData <- sampleMultiTradeData[\n")
cat("+     SYMBOL == \"AAA\", list(DT, PRICE = log(PRICE))] \n")
stockData <- sampleMultiTradeData[
  SYMBOL == "AAA", list(DT, PRICE = log(PRICE))]

cat("R> kn <- knChooseReMeDI(stockData) \n")
kn <- knChooseReMeDI(stockData)

cat("R> remedi <- ReMeDI(stockData, kn = kn, lags = 0:15) \n")
remedi <- ReMeDI(stockData, kn = kn, lags = 0:15)
cat("R> remedi \n \n")
remedi
cat("\n")

cat("R> asympVar <- ReMeDIAsymptoticVariance( \n")
cat("R>    stockData, kn = kn, lags = 0:15, phi = 0.2, i = 2) \n")
asympVar <- ReMeDIAsymptoticVariance(
  stockData, kn = kn, lags = 0:15, phi = 0.2, i = 2)

cat("R> asympVar \n \n")
asympVar
cat("\n \n")

cat("### SECTION 6 ####################### \n \n")

### Spotvol ###########

cat("R> parametric <- spotVol( \n")
cat("+     data = sampleOneMinuteData[, list(DT, PRICE = MARKET)], \n")
cat("+     periodicVol = \"TML\", P1 = 2, P2 = 2, alignPeriod = 1) \n \n")
parametric <- spotVol(
  data = sampleOneMinuteData[, list(DT, PRICE = MARKET)],
  periodicVol = "TML", P1 = 2, P2 = 2, alignPeriod = 1)

cat("R> nonParametric <- spotVol( \n")
cat("+     data = sampleOneMinuteData[, list(DT, PRICE = MARKET)], \n")
cat("+     periodicVol = \"WSD\", alignPeriod = 1) \n \n")
nonParametric <- spotVol(
  data = sampleOneMinuteData[, list(DT, PRICE = MARKET)],
  periodicVol = "WSD", alignPeriod = 1)
cat("\n \n")

cat("### SECTION 7 ####################### \n \n")

cat("# Figure 6 is generated. \n\n")
sink()

pdf("Figure_6.pdf")
plot.ts(cbind(as.numeric(parametric$periodic), as.numeric(nonParametric$periodic)), main = "", plot.type = "single", col = c("black", "red"), xaxt = "n", ylab = "diurnality effect", xaxs = "i", las = 2, axes = FALSE)
labs <- format(index(parametric$periodic), "%H:%M")
at <- 1:nrow(parametric$periodic)
axis(2)
axis(labels = labs[seq(1,length(labs), length.out = 7)], at = at[seq(1,length(labs), length.out = 7)], side = 1)
legend(x = 150, y = 2.5, legend = c("parametric", "non-parametric"), col = c("black", "red"), lty = 1, box.lwd = 0)
box()
dev.off()

sink(file = "output_vignette.txt", append = TRUE, split = TRUE)


cat("R> daily <- BNSjumpTest(\n")
cat("+    sampleOneMinuteData[, list(DT, MARKET)],\n")
cat("+    makeReturns = TRUE)\n")
daily <- BNSjumpTest(
  sampleOneMinuteData[, list(DT, MARKET)],
  makeReturns = TRUE)
cat("R> pValues <- sapply(daily, function(x) x[[\"pvalue\"]])) \n\n")
pValues <- sapply(daily, function(x) x[["pvalue"]])
cat("R> pValues[which.min(pValues)] \n")
pValues[which.min(pValues)]

cat("R> intraday <- intradayJumpTest( \n")
cat("+   sampleOneMinuteData[as.Date(sampleOneMinuteData$DT) == \"2001-08-26\", \n")
cat("+                       list(DT, PRICE = MARKET)],\n")
cat("+   makeReturns = TRUE, alignBy = \"minutes\", alignPeriod = 1)\n")
intraday <- intradayJumpTest(
  sampleOneMinuteData[as.Date(sampleOneMinuteData$DT) == "2001-08-26", 
                      list(DT, PRICE = MARKET)],
  makeReturns = TRUE, alignBy = "minutes", alignPeriod = 1)
cat("\n")

cat("# Figure_7.pdf is generated. \n \n")

sink()

pdf("Figure_7.pdf")
par(mfrow = c(2, 1), mai = c(0.4, 0.4, 0.35, 0.2), mar = c(3, 3, 0.5, 1))
plot(x = as.Date(names(daily)) , y = abs(sapply(daily, function(x) x[["ztest"]])), ylab = "test statistic", xlab = "date", type = "l", col = "white", axes = FALSE)
rect(xleft =  as.Date(names(daily))[which.min(as.numeric(sapply(daily, function(x) x[["pvalue"]])))] - 0.5, xright =  as.Date(names(daily))[which.min(sapply(daily, function(x) x[["pvalue"]]))] + 0.5, ybottom = min(abs(sapply(daily, function(x) x[["ztest"]]))), ytop = max(abs(sapply(daily, function(x) x[["ztest"]]))), col = "darkgrey", border = "darkgrey")
lines(x = as.Date(names(daily)) , y = abs(sapply(daily, function(x) x[["ztest"]])), main ="",
      ylab = "test statistic", xlab = "date", type = "l")
foo <- c(1,6,12, 16, 21)
axis(1, at = as.Date(names(sapply(daily, function(x) x[["ztest"]])))[foo], labels = as.Date(names(sapply(daily, function(x) x[["ztest"]])))[foo])
axis(2, las = 2)
box()

plot(x = intraday$pData[[1]], intraday$pData[[2]], type = "l", col = "white", xlab = "time of day", ylab = "price", axes = FALSE)
rect(xleft = index(intraday$ztest)[which(abs(intraday$ztest) > intraday$criticalValue)] - 45, xright = index(intraday$ztest)[which(abs(intraday$ztest) > intraday$criticalValue)] + 45, ybottom = min(intraday$pData[[2]]), ytop = max(intraday$pData[[2]]), col = 2, border = 2)
lines(x = intraday$pData[[1]], intraday$pData[[2]], type = "l")
xLim <- range(as.POSIXct("2001-08-26 9:30:00", tz = "UTC"), as.POSIXct("2001-08-26 16:00:00", tz = "UTC"))
axis(1, at = seq(xLim[1], xLim[2], by = 60*30)[c(1,2,4,6,8,10,12,14)], labels = format(seq(xLim[1], xLim[2], by = 60*30)[c(1,2,4,6,8,10,12,14)], format = "%H:%M"), cex = 0.9)
axis(2, las = 2, mgp = c(0,0.65,0))
box()
dev.off()

sink(file = "output_vignette.txt", append = TRUE, split = TRUE)

cat("### SECTION 8 ####################### \n \n")

### HAR ##########

cat("# 8.1 HAR Model \n\n")

cat("R> RV <- as.xts(SPYRM[, list(DT, RV5)]) * 10000 \n")
RV <- as.xts(SPYRM[, list(DT, RV5)]) * 10000
cat("R> model <- HARmodel(data = RV, periods = c(1, 5, 22), \n")
cat("+                  type = \"HAR\", inputType = \"RM\")\n")
model <- HARmodel(data = RV, periods = c(1, 5, 22),
                  type = "HAR", inputType = "RM")
cat("R> summary(model)")
summary(model)

cat("\n")
cat("# 8.1 HEAVY Model \n\n")

# Calculate returns in percentage log returns
cat("R> logReturns <- 100 * makeReturns(SPYRM$CLOSE)[-1] \n")
logReturns <- 100 * makeReturns(SPYRM$CLOSE)[-1]

# Combine both returns and realized measures into one xts
dataSPY <- xts(cbind(logReturns, RV = SPYRM$RV5[-1] * 10000),
               order.by = SPYRM$DT[-1])

cat("# Estimate the full sample HEAVY model with RV as the realized measure \n")
cat("R> model <- HEAVYmodel(data = dataSPY[, c(\"logReturns\",\"RV\")]) \n")
model <- HEAVYmodel(data = dataSPY[, c("logReturns","RV")])
cat("R> summary(model) \n")
summary(model)

cat("\n\n")
cat("# 8.3 Forecasting results \n\n")
cat("R> # Create columns to populate with our forecasts \n")
cat("R> dataSPY$HARfcst <- NA")
dataSPY$HARfcst <- NA
cat("R> dataSPY$HEAVYfcst <- NA")
dataSPY$HEAVYfcst <- NA

cat("R> for (i in 1:494) {\n")
cat("+  HAREstimated <- HARmodel(data = dataSPY[i:(i + 999), \"RV\"], \n")
cat("+                           periods = c(1, 5, 22), type = \"HAR\", \n")
cat("+                           inputType = \"RM\") \n")
cat("+  dataSPY$HARfcst[i+1000] <- predict(HAREstimated) \n")
cat("+  HEAVYEstimated <- HEAVYmodel(data = dataSPY[i:(i + 999), c(\"logReturns\", \"RV\")]) \n")
cat("+  dataSPY$HEAVYfcst[i+1000] <- \n")
cat("+    predict(HEAVYEstimated, stepsAhead = 1)[,\"CondRM\"] \n")
cat("+}\n \n")
for (i in 1:494) {
  HAREstimated <- HARmodel(data = dataSPY[i:(i + 999), "RV"], 
                           periods = c(1, 5, 22), type = "HAR",
                           inputType = "RM")
  dataSPY$HARfcst[i+1000] <- predict(HAREstimated)
  HEAVYEstimated <- HEAVYmodel(data = dataSPY[i:(i + 999), c("logReturns", "RV")])
  dataSPY$HEAVYfcst[i+1000] <- 
    predict(HEAVYEstimated, stepsAhead = 1)[,"CondRM"]
}

cat("R> dfSPY <- as.data.frame(dataSPY)\n")
dfSPY <- as.data.frame(dataSPY)
cat("R> dfSPY$dates <- as.Date(index(dataSPY)) \n")
dfSPY$dates <- as.Date(index(dataSPY))
cat("R> dfSPY <- dfSPY[!is.na(dfSPY$HEAVYfcst),]\n\n")
dfSPY <- dfSPY[!is.na(dfSPY$HEAVYfcst),]

cat("# Figure_8.pdf is generated. \n\n")
sink()

pdf("Figure_8.pdf")

plot(dfSPY$dates, dfSPY$RV, type = "l", xlab = "Date", ylab = "", lty = 3, las = 2)
lines(dfSPY$dates, dfSPY$HARfcst, col = "blue", lty = 1)
lines(dfSPY$dates, dfSPY$HEAVYfcst, col = "red", lty = 1)
legend("topright", c("RV", "HAR", "HEAVY"),
       lty = c(3, 1, 1),
       col = c("black", "blue", "red"))

dev.off()

sink(file = "output_vignette.txt", append = TRUE, split = TRUE)

cat("# Calculate RMSE of the two models\n")
cat("# RMSE of HEAVY model \n")
cat("R> sqrt(mean((dataSPY$RV - dataSPY$HEAVYfcst)^2, na.rm = TRUE)) \n")
sqrt(mean((dataSPY$RV - dataSPY$HEAVYfcst)^2, na.rm = TRUE))
# RMSE of HAR model
cat("R> sqrt(mean((dataSPY$RV - dataSPY$HARfcst)^2, na.rm = TRUE)) \n")
sqrt(mean((dataSPY$RV - dataSPY$HARfcst)^2, na.rm = TRUE))

cat("# Forecast evaluation")
cat("R> idx <- dataSPY$RV > quantile(dataSPY$RV[-c(1:1000)], 0.90)")
idx <- dataSPY$RV > quantile(dataSPY$RV[-c(1:1000)], 0.90)
cat("# RMSE of HEAVY model \n")
cat("R> sqrt(mean((dataSPY$RV[idx] - dataSPY$HEAVYfcst[idx])^2, na.rm = TRUE)) \n")
sqrt(mean((dataSPY$RV[idx] - dataSPY$HEAVYfcst[idx])^2, na.rm = TRUE))
cat("# RMSE of HAR model\n")
cat("R> sqrt(mean((dataSPY$RV[idx] - dataSPY$HARfcst[idx])^2, na.rm = TRUE)) \n")
sqrt(mean((dataSPY$RV[idx] - dataSPY$HARfcst[idx])^2, na.rm = TRUE))
cat("\n\n")
######

cat("############################## \n")
cat("###### SESSION INFO \n \n")
cat("############################## \n")
info <- sessionInfo()
print(info)
cat("\n")

######

sink()


