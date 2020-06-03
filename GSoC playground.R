rm(list = ls())
####### GSoC 2020 playground #######
library(highfrequency)
library(quantmod)
library(anytime)

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

# defaults so it's easy to play with
model = list(modelType = "BMJ", driftModel = "constant", volatilityModel = "constant",  drift = 0,
             volatility = 1, alpha = NULL, beta = NULL, sampling = "equidistant")
nSeries = 3
nDays = 3
nObs = 23400
misc = list(trading.start = 34200, trading.end = 57600, origin = "1970-01-01")
discretize = FALSE

jumpModel  <- list(modelType = "pre announced", jump.variation = 1 / 4, jump.time = c(0.25, 0.75))

hfSimSpec <- createHFSimSpec(model = model, jumpModel = jumpModel, nDays = nDays, nSeries = nSeries, nObs = nObs)
sim <- hfsim.do(hfSimSpec)


RV <- rCov(exp(sim), makeReturns = TRUE, align.by = 'mins', align.period = 1)
BPV  <- rBPCov(exp(sim), makeReturns = TRUE, align.by = "mins", align.period = 1)

ratio <- matrix(0, nrow = nDays, ncol = nSeries)
for(i in 1:nDays){
  ratio[i,] <- diag(RV[[i]]/BPV[[i]])
}

colMeans(ratio)^-1


plot(sim, observation.based = TRUE)


####### LM jump test #######

pdata <- as.xts.data.table(sample_tdata_microseconds)$PRICE["2018-01-02"]
alpha <- 0.01
testing.times <- c("09:30", "09:45", "10:30","10:35","10:40", "10:45", "10:50", "16:00")

LMtest <- intradayJumpTest(pdata, testType = "LM", testing.times = testing.times, window.size = 5, K = 10)

# or equivalently we can specify the jumps in seconds after midnight.
testing.times <- c(34200, 35100, seq(37800, 39000, 5 * 60), 57600) - 3600
LMtest <- intradayJumpTest(pdata, testType = "LM", testing.times = testing.times, window.size = 5, K = 10)


### Testing place
prices <- LMtest[[1]]
jumps <- na.omit(LMtest[[2]])
plot(prices)
eventlines <- rep('Jump', length(jumps))
eventlines <- xts(eventlines[jumps], index(jumps[jumps]))
addEventLines(eventlines)

sample.xts <- as.xts(sample_matrix)
events <- xts(letters[1:3], 
              as.Date(c("2007-01-12", "2007-04-22", "2007-06-13")))
plot(sample.xts[,4])
addEventLines(events, srt=90, pos=2)
window.size <- 1
K <- 50
