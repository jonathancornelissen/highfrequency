#' Lead-Lag estimations
#' Function that


#' @export
leadLag <- function(price1, price2, lags, resolution = "seconds", normalize = TRUE, parallelize = FALSE, nCores = NA){
  
  # Make adjustments if we are doing millisecond precision
  timestampsCorrectionFactor <- 1
  if(resolution %in% c("ms", "milliseconds", "milli-seconds")){
    timestampsCorrectionFactor <- 1000
  }
  # Make sure we have xts inputs
  if(!is.xts(price1) | !is.xts(price2) | isMultiXts(price1)| isMultiXts(price2)){
    stop("lead-lag estimation requires the input to be single day xts objects")
  }
  
  # Convert timestamps to denote time after open (denoted by first trade in either asset)
  timestampsX <- as.numeric(index(price1)) * timestampsCorrectionFactor 
  timestampsY <- as.numeric(index(price2)) * timestampsCorrectionFactor 
  origin <- min(timestampsY[1], timestampsX[1])
  timestampsX <- timestampsX - origin
  timestampsY <- timestampsY - origin
  
  x <- as.numeric(price1)
  y <- as.numeric(price2)
  
  if(parallelize && is.numeric(nCores) && (nCores %% 1) == 0){
    contrasts <- as.numeric(leadLagCppPAR(x, timestampsX, y, timestampsY, lags, normalize, nCores))    
  } else {
    contrasts <- as.numeric(leadLagCpp(x, timestampsX, y, timestampsY, lags, normalize))
  }
  
  # Calculate the likelihood ratio
  posIDX <- which(lags>0)
  negIDX <- which(lags<0)
  if( length(negIDX) != 0 ){
    likelihoodRatio <- sum(contrasts[posIDX]) / sum(contrasts[negIDX])
  } else {
    likelihoodRatio <- NA
  }
  
  res <- list("contrasts" = contrasts, "likelihoodRatio" = likelihoodRatio, "lags" = lags)
  class(res) <- "leadLag"
  return(res)
  
}


#' @export 
plot.leadLag <- function(x, ...){
  plot(x$lags, x$contrasts, type = "l", main = "lead-lag estimation", xlab = "lag", ylab = "contrast")
}