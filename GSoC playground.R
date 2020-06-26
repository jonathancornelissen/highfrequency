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


options(error = recover)
pData <- exp(sim$prices)[,1]
pData <- setnames(as.data.table(pData), old = "index", new = "DT")
names(pData) <- c("DT", "PRICE")
pData[, PRICE := as.numeric(PRICE)]
jumpTest <- intradayJumpTest(pData, volEstimator = "RM",  # PRE-AVERAGED REALIZED MEASURE
                           driftEstimator = "driftMean", on = "minutes", alpha = 0.95, k = 5, 
                           RM = "bipower", lookBackPeriod = 10, tz = "GMT", marketOpen = "10:30:00", marketClose = "17:00:00",
                           dontIncludeLast = TRUE)

if(!jumpTest$isMultiDay){
  plot(jumpTest)
}



rm(list = ls())
gc()
library(highfrequency)
library(data.table)
library(xts)
library(anytime)
volatilityModel <- list(modelType = "constant", variance = 0.0391)
driftModel <- list(modelType = "constant", drift = 0)
nSeries <- 30
nDays <- 150
nObs <- 4861 
timeSettings  <- list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01" , sampling = "equidistant")
discretize <- FALSE

jumpModel  <- list(modelType = "PA", jumpComponent = 1 / 5, jumpTime = c(640/1280, 641/1280)) #includeJumps should be automated in the creation of the spec

hfSimSpec <- createHFSimSpec(volatilityModel = volatilityModel, driftModel = driftModel, jumpModel = jumpModel, nDays = nDays, nSeries = nSeries, nObs = nObs)
sim <- hfsim.do(hfSimSpec)



marketPrice <- exp(sim$prices)[,1]

stockPrice <- list()
for (i in 1:(ncol(sim$prices)-1)) {
  stockPrice[[i]] <- exp(sim$prices)[,i+1]
}

rankTest <- rankJumpTest(marketPrice, stockPrice, K = 10, k = 1, alpha = c(5,3), tz = "GMT", marketOpen = "10:30:00", marketClose = "17:00:00")

