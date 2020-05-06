### GSoC 2020 playground 
library(highfrequency)
library(quantmod)
## Get some high frequency data from alphavantage

# Because this will be on github I will read my key from a file.
# The API key will have to be set in each session
apiKey = read.table('/home/emil/Desktop/alphavantage.txt')[[1]]
setDefaults(getSymbols.av, api.key = as.character(apiKey))

stopifnot(is.character(getDefaults(getSymbols.av, 'api.key')$api.key))
data = getHFData(symbols = c('MMM', 'GS', 'SPY'))
stopifnot(all.equal(data, getHFData(symbols = c('MMM', 'GS', 'SPY'), apiKey = as.character(apiKey))))
