rm(list = ls())
gc() # force garbage collection
####### GSoC 2020 playground #######
library(data.table)
library(highfrequency)
library(quantmod)
library(anytime)
options(error = recover)
####### Alpha Vantage #######

# Because this will be on github I will read my key from a file on my desktop.
# The API key will have to be set in each session
apiKey = read.table("/home/emil/Desktop/alphavantage.txt")[[1]]
# Set my apiKey in defaults.
setDefaults(getSymbols.av, api.key = as.character(apiKey))
# download some data
if(FALSE)
  data <- getHFData(symbols = c("MMM", "GS", "SPY"), outputType = "DT")

####### Simulations #######
volatilityModel <- list(modelType = "constant", variance = 0.0391, burstModel = list(burstMultiplier = 3, burstInterval = c(15/32, 17/32)),
                        includeDiurnality = FALSE, diurnalModel = list(C = 0.88929198, A = 0.75, B = 0.25, a = 10, b = 10))
driftModel <- list(modelType = "constant", drift = 0)
nSeries <- 30
nDays <- 5
nObs <- 23401 
timeSettings  <- list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01" , sampling = "equidistant")
discretize <- FALSE

jumpModel  <- list(modelType = "PA", jumpComponent = 1 / 2, jumpTime = c(320/640, 321/640), includeJumps = TRUE) #includeJumps should be automated in the creation of the spec

hfSimSpec <- createHFSimSpec(volatilityModel = volatilityModel, driftModel = driftModel, jumpModel = jumpModel, nDays = nDays, nSeries = nSeries, nObs = nObs)
sim <- hfsim.do(hfSimSpec)
# hatreturn <- highfrequency:::hatreturn
# gfunction <- highfrequency:::gfunction
# theta <- 0.1
# testingTimes <- seq(34200 + 10*300, 57600 - 3600, 300) + 3600
# gc()
# 
# rv <- rCov(exp(sim$prices), makeReturns = TRUE, alignBy = "secs", alignPeriod = 1)
# bpv <- rBPCov(exp(sim$prices), makeReturns = TRUE, alignBy = "secs", alignPeriod = 1)
# 
# jumpVariation <- matrix(0, nrow = nDays, ncol = nSeries)
# jumpComponent <- 0
# for(i in 1:nDays){
#   if(length(rv[[i]]) > 1){ # We have covariance matrix
#     jumpVariation[i,] <- diag((rv[[i]]-bpv[[i]])/rv[[i]])
#   } else { # we only have variance estimates
#     jumpVariation <- pmax((rv - bpv)/rv, 0) # We can just use the XTS's and do it vectorized, truncating at 0
#     jumpComponent <- rv-bpv
#     break 
#   }
# }
# 
# if(nSeries == 1){
#   print(paste("Realized Variance:", mean(rv)))
#   print(paste("Realized Bipower Variation:", mean(bpv)))
# }
####### LM jump test #######
# or equivalently we can specify the jumps in seconds after midnight.
# testingTimes <- seq(34200 + 10*300, 57600 - 3600, 300) + 3600
# LMtest <- intradayJumpTest(pData = exp(sim$prices[,1]), testType = "LM", testingTimes = testingTimes, windowSize = 5, K = 10)
# 
# LMtest1Day <- intradayJumpTest(pData = exp(sim$prices)["1970-01-01",1], testType = "LM", testingTimes = testingTimes, windowSize = 5, K = 10, RM = "rBPCov")







options(error = recover)
pData <- exp(sim$prices)[,1]
pData <- setnames(as.data.table(pData), old = "index", new = "DT")
names(pData) <- c("DT", "PRICE")
pData[, PRICE := as.numeric(PRICE)]
jumpTest <- intradayJumpTest(pData, volEstimator = "RM",  # PRE-AVERAGED REALIZED MEASURE
                           driftEstimator = "driftMean", on = "minutes", alpha = 0.95, k = 5, 
                           RM = "bipower", lookBackPeriod = 10, tz = "GMT", marketOpen = "10:30:00", marketClose = "17:00:00",
                           dontIncludeLast = TRUE)



#plot(jumpTest)

library(data.table)
library(xts)
library(anytime)
### Rank test at jump events
# rm(list = ls())
# dat <- fread("/home/emil/Dropbox/GSOC/code_from_papers/OnlineSoftware/dz.csv")
# dx <- as.matrix(fread("/home/emil/Dropbox/GSOC/code_from_papers/OnlineSoftware/dx.csv"))
# 
# 
# 
# nDays <- nrow(dat)/390
# timestamps <- rep(seq(34260, 57600, 60), nDays) + rep(0:(nDays-1) * 86400, each = 390)
# marketReturns <- xts(as.numeric(dat$V1), anytime(timestamps))
# 
# 
# stockReturns <- xts(dx, anytime(timestamps))
# 
# alpha = c(7, 4)
# 
# alpha = c(7,4)
# 
# K = 10
# kn = 30
# r = 1
# BoxCox = 1
# nBoot = 1000
# dontTestAtBoundaries = FALSE

marketPrice <- exp(sim$prices)[,1]

stockPrice <- {}
for (i in 1:(ncol(sim$prices)-1)) {
  stockPrice[[i]] <- exp(sim$prices)[,i+1]
}

#stockPrice <- exp(sim$prices)[,-1]

options(error = recover)
rankTest <- rankJumpTest(marketPrice, stockPrice, K = 30, k = 1, alpha = c(5,3), tz = "GMT", marketOpen = "10:30:00", marketClose = "17:00:00")



