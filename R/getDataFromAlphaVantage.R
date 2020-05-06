#' @importFrom quantmod getSymbols.av getDefaults
#' @importFrom data.table as.data.table setnames
#' @keywords data
#' 
#' ##### Examples should NOT test this function as it will require an internet connection and connection to the AV server
#' ##### Also, the output size may change which may lead to problems (this will happen when there has been half trading days.)
#' @export




getHFData = function(symbols = NULL, interval = '5min', outputType = 'xts', apiKey = NULL, ...){
  # Check for API key set in quantmod if it is not present, we kindly remind the user to get it.
  if(is.null(getDefaults(getSymbols.av, 'api.key')$api.key) & is.null(apiKey)){
    stop('Your AlphaVantage API key is not set in the quantmod package and no API key was provided. 
          An API key is required but can be obtained for free.
          Please see the getSymbols.av function in the quantmod package for details on how to set a default API key for the current session.
          Alternatively, use the apiKey argument to provide the key.')
  }
  if(is.null(apiKey)){
    apiKey = getDefaults(getSymbols.av, 'api.key')$api.key #quantMod handles the extra quotes.
  }
  
  
  # Check if we have more than one symbol
  multiSymbols = ifelse(length(symbols) > 1, TRUE, FALSE)
  
  # We have more than one symbol
  if(multiSymbols){
    data = vector("list", length = length(symbols))
    names(data) = symbols
    for (symbol in symbols) {
      # load the data for the symbol into dat.
      singleSymbol = getSymbols.av(symbol, periodicity = 'intraday', interval = interval,
                                   api.key = apiKey, auto.assign = FALSE, output.size = 'full')
      
      
      if(outputType == 'DT'){ #Here the user wants a data table out. Thus we set the class DT and we change the names.
        singleSymbol = as.data.table(singleSymbol)
        setnames(singleSymbol, colnames(singleSymbol), new = c('DT', 'Open', 'High', 'Low' , 'Close', 'Volume'))
      } else{
        # Change the names to OHLC-V abbreviations
        colnames(singleSymbol) = c('Open', 'High', 'Low', 'Close', 'Volume')
      }
      data[[symbol]] = singleSymbol
    }
  }
  # We have one symbols
  else{
    data = getSymbols.av(symbols, periodicity = 'intraday', interval = interval,
                         api.key = apiKey, auto.assign = FALSE, output.size = 'full', ...)  
    if(outputType == 'DT'){ #Here the user wants a data table out. Thus we set the class DT and we change the names.
      data = as.data.table(data)
      setnames(data, colnames(data), new = c('DT', 'Open', 'High', 'Low' , 'Close', 'Volume'))
    } else{
      # Change the names to OHLC-V abbreviations
      colnames(data) = c('Open', 'High', 'Low', 'Close', 'Volume')
    }
  }
  
  return(data)
  
}


