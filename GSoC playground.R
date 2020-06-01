####### GSoC 2020 playground #######
library(highfrequency)
library(quantmod)

####### Alpha Vantage #######

# Because this will be on github I will read my key from a file on my desktop.
# The API key will have to be set in each session
apiKey = read.table("/home/emil/Desktop/alphavantage.txt")[[1]]
# Set my apiKey in defaults.
setDefaults(getSymbols.av, api.key = as.character(apiKey))
# download some data
data = getHFData(symbols = c("MMM", "GS", "SPY"), outputType = "DT")




