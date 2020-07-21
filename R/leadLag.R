
#' @export
leadLag <- function(price1, price2, lagRange, normalize = TRUE, parallelize = FALSE, nCores = NA){
  
  timestampsX <- as.numeric(index(price1))
  timestampsY <- as.numeric(index(price2))
  origin <- min(timestampsY[1], timestampsX[1])
  timestampsX <- timestampsX - origin
  timestampsY <- timestampsY - origin
  
  x <- as.numeric(price1)
  y <- as.numeric(price2)
  
  if(parallelize && is.numeric(nCores) && (nCores %% 1) == 0){
    contrasts <- as.numeric(leadLagCppPAR(x, timestampsX, y, timestampsY, lagRange, normalize, nCores))    
  } else {
    contrasts <- as.numeric(leadLagCpp(x, timestampsX, y, timestampsY, lagRange, normalize))
  }
  
  
  posIDX <- which(lagRange>0)
  negIDX <- which(lagRange<0)
  if( length(negIDX) != 0 ){
    likelihoodRatio <- sum(contrasts[posIDX]) / sum(contrasts[negIDX])
  } else {
    likelihoodRatio <- NA
  }
  
  res <- list("contrasts" = contrasts, "likelihoodRatio" = likelihoodRatio)
  class(res) <- "leadLag"
  return(res)
  
}