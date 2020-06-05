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
volatilityModel <- list(modelType = "constant burst", variance = 0.2, burstModel = list(burstMultiplier = 3, burstInterval = c(16/32, 17/32)),
                        includeDiurnality = FALSE, diurnalModel = list(C = 0.88929198, A = 0.75, B = 0.25, a = 10, b = 10))
driftModel <- list(modelType = "constant", drift = 0)
nSeries <- 1
nDays <- 5
nObs <- 23400 
timeSettings  <- list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01" , sampling = "equidistant")
discretize <- FALSE

jumpModel  <- list(modelType = "none", jumpComponent = 1 / 5, jumpTime = c(0.25, 0.75)) #includeJumps should be automated in the creation of the spec

hfSimSpec <- createHFSimSpec(volatilityModel = volatilityModel, driftModel = driftModel, jumpModel = jumpModel, nDays = nDays, nSeries = nSeries, nObs = nObs)
sim <- hfsim.do(hfSimSpec)

gc()

rv <- rCov(exp(sim$prices), makeReturns = TRUE, align.by = "secs", align.period = 1)
bpv  <- rBPCov(exp(sim$prices), makeReturns = TRUE, align.by = "secs", align.period = 1)

jumpVariation <- matrix(0, nrow = nDays, ncol = nSeries)
jumpComponent <- 0
for(i in 1:nDays){
  if(length(rv[[i]]) > 1){ # We have covariance matrix
    jumpVariation[i,] <- diag((rv[[i]]-bpv[[i]])/rv[[i]])
  } else { # we only have variance estimates
    jumpVariation <- pmax((rv - bpv)/rv, 0) # We can just use the XTS's and do it vectorized, truncating at 0
    jumpComponent <- rv-bpv
    break 
  }
}

if(nSeries == 1){
  print(paste("Realized Variance:", mean(rv)))
  print(paste("Realized Bipower Variation:", mean(bpv)))
}
####### LM jump test #######
# or equivalently we can specify the jumps in seconds after midnight.
testingTimes <- seq(34200 + 10*300, 57600 - 3600, 300) + 3600
LMtest <- intradayJumpTest(pData = exp(sim$prices), testType = "LM", testingTimes = testingTimes, windowSize = 5, K = 10)

LMtest1Day <- intradayJumpTest(pData = exp(sim$prices)["1970-01-01"], testType = "LM", testingTimes = testingTimes, windowSize = 5, K = 10)

### Testing place

plot(LMtest1Day)
plot(LMtest)



#### Reminder: Seek help with the shading of xts plots when observation.based = TRUE
timestamps <- rep(0:(10-1), each = 23400) * 86400 + seq(34200, 57600, length.out = 23400)
x <- xts(cumsum(rnorm(23400 * 10) * sqrt(1/23400)), as.POSIXct(timestamps, origin = "1970-01-01"))
plot(x)




