#' Lead-Lag estimation
#' @description Function that estimates whether one series leads (or lags) another.
#' 
#' @param price1 xts or data.table containing prices in levels, in case of data.table,
#'  use a column DT to denote the date-time in POSIXct format, and a column PRICE to denote the price
#' @param price2 xts or data.table containing prices in levels, in case of data.table,
#'  use a column DT to denote the date-time in POSIXct format, and a column PRICE to denote the price
#' @param lags a numeric denoting which lags (in units of \code{resolution}) should be tested as leading or lagging
#' @param resolution the resolution at which the lags is measured. 
#' The default is "seconds", use this argument to gain 1000 times resolution by setting it to either "ms", "milliseconds", or "milli-seconds".
#' @param normalize logical denoting whether the contrasts should be normalized by the product of the L2 norms of both the prices. Default = TRUE. 
#' This does not change the value of the lead-lag-ratio.
#' @param parallelize logical denoting whether to use a parallelized version of the C++ code (parallelized using OPENMP). Default = FALSE
#' @param nCores integer valued numeric denoting how many cores to use for the lead-lag estimation procedure in case parallelize is TRUE. 
#' Default is NA, which does not parallelize the code.
#' 
#' @return An list with class "leadLag" which containing 'contrasts', 'lead-lag-ratio', and 'lags', denoting the estimated values for each lag calculated,
#' the lead-lag-ratio, and the tested lags respectively.
#' 
#' @details The lead-lag-ratio (LLR) can be used to see if one asset leads the other. If LLR < 1, then price1 MAY be leading price2 and vice versa if LLR > 1.
#' 
#' @references M. Hoffmann, M. Rosenbaum, and N. Yoshida (2013) : Estimation of the lead-lag parameter from non-synchronous data. Bernoulli 19, pages 1-37
#' 
#' @examples 
#' \dontrun{
#' ll <- leadLag(100 + cumsum(sbux), 100 + cumsum(lltc), lags = seq(-50,50))
#' plot(ll)
#' }
#' 
#' @export
leadLag <- function(price1 = NULL, price2 = NULL, lags = NULL, resolution = "seconds", normalize = TRUE, parallelize = FALSE, nCores = NA){
  PRICE <- DT <- timestampsX <- timestampsY <- x <- y <- NULL # initialization
  # Make adjustments if we are doing millisecond precision
  timestampsCorrectionFactor <- 1
  if(resolution %in% c("ms", "milliseconds", "milli-seconds")){
    timestampsCorrectionFactor <- 1000
  }
  if(!is.numeric(lags) | (length(lags) < 1)){
    stop(" lags must be a numeric with a length greater than 0")
  }
  if(!all(class(price1) == class(price2))){
    stop("price1 and price2 must be of same class")
  }
  
  if(parallelize && is.numeric(nCores) && (nCores %% 1) == 0 && nCores < 1){
    parallelize <- FALSE
    warning("nCores is not an integer valued numeric greater than 0. Using non-parallelized code")
  }
  
  ## Test if we have data.table input
  if(is.data.table(price1)){ # We can check one because we check that the classes are the same above
    
    if (!("DT" %in% colnames(price1))) {
      stop("price1 neeeds DT column (date-time ).")
    }
    if (!("PRICE" %in% colnames(price1))) {
      stop("price1 neeeds PRICE column.")
    }
    if (!("DT" %in% colnames(price2))) {
      stop("price2 neeeds DT column (date-time).")
    }
    if (!("PRICE" %in% colnames(price2))) {
      stop("price2 neeeds PRICE column.")
    }
    
    timestampsX <- as.numeric(price1[, DT]) * timestampsCorrectionFactor
    timestampsY <- as.numeric(price2[, DT]) * timestampsCorrectionFactor
    x <- as.numeric(price1[, PRICE])
    y <- as.numeric(price2[, PRICE])
  } else { # Here we have XTS input
    # Make sure we have correct xts inputs
    if(!is.xts(price1) | !is.xts(price2) | isMultiXts(price1)| isMultiXts(price2)){
      stop(paste0("lead-lag estimation requires the input to be contain single day input. The input must be either xts or data.table, not ", 
                  class(price1)))
    } 
    timestampsX <- as.numeric(index(price1)) * timestampsCorrectionFactor
    timestampsY <- as.numeric(index(price2)) * timestampsCorrectionFactor
    
    if(ncol(price1) > 1){
      x <- as.numeric(price1[, "PRICE"])
      y <- as.numeric(price2[, "PRICE"])
    } else {
      x <- as.numeric(price1)
      y <- as.numeric(price2)
    }
  }
  
  
  # When y is longer than x, it is often faster to swap the variables than run with a longer y than x.
  isSwapped <- FALSE
  if(length(x) < length(y)){
    tmp <- x
    x <- y
    y <- tmp
    tmp <- timestampsX
    timestampsX <- timestampsY
    timestampsY <- tmp
    isSwapped <- TRUE
  }

  # Convert timestamps to denote time after open (denoted by first trade in either asset)
  origin <- min(timestampsY[1], timestampsX[1])
  timestampsX <- timestampsX - origin
  timestampsY <- timestampsY - origin
  
  
  # (1 + -2 * isSwapped) is 1 if isSwapped is FALSE, and -1 if isSwapped is TRUE
  if(parallelize){
    contrasts <- as.numeric(leadLagCppPAR(x, timestampsX, y, timestampsY, lags * (1 + -2 * isSwapped), normalize, nCores))    
  } else {
    contrasts <- as.numeric(leadLagCpp(x, timestampsX, y, timestampsY, lags * (1 + -2 * isSwapped), normalize))
  }
  
  
  # Calculate the likelihood ratio
  posIDX <- which(lags>0)
  negIDX <- which(lags<0)
  if( length(negIDX) != 0 ){
    LLR <- sum(contrasts[posIDX]) / sum(contrasts[negIDX])
  } else {
    LLR <- NA
  }
  
  res <- list("contrasts" = contrasts, "lead-lag-ratio" = LLR, "lags" = lags)
  class(res) <- c("leadLag", "list")
  return(res)
  
}


#' @export 
plot.leadLag <- function(x, ...){
  plot(x$lags, x$contrasts, type = "l", main = "lead-lag estimation", xlab = "lag", ylab = "contrast")
}