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
  data = getHFData(symbols = c("MMM", "GS", "SPY"), outputType = "DT")

####### Simulations #######
n.obs = 23400
BM = cumsum(rnorm(n.obs) * sqrt(1/n.obs))
sum(diff(BM)^2)
plot.ts(BM)

####### LM jump test #######

pdata <- as.xts.data.table(sample_tdata_microseconds)$PRICE["2018-01-02"]
alpha <- 0.01
testing.times <- c("09:30", "09:45", "10:30","10:35","10:40", "10:45", "10:50", "16:00")

LMtest <- intradayjumptest(pdata, test.type = "LM", testing.times = testing.times, window.size = 5, K = 10)

# or equivalently we can specify the jumps in seconds after midnight.
testing.times <- c(34200, 35100, seq(37800, 39000, 5 * 60), 57600) - 3600
LMtest <- intradayjumptest(pdata, test.type = "LM", testing.times = testing.times, window.size = 5, K = 10)


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
