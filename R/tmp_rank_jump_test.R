library(data.table)
library(xts)
library(anytime)
### Rank test at jump events

dat <- fread("/home/emil/Dropbox/GSOC/code_from_papers/OnlineSoftware/dz.csv")


nDays <- nrow(dat)/390
timestamps <- rep(seq(34260, 57600, 60), nDays) + rep(0:(nDays-1) * 86400, each = nrow(dat)/nDays)
returns <- xts(dat$V1, anytime(timestamps))

alpha = c(7, 4)


timeOfDayAdjustments <- function(returns, n, m, polyOrder){
  returns <- matrix(returns, nrow = n, ncol = m, byrow = FALSE) # remap returns
  timePolyMatrix <- matrix(rep(1:nrow(returns), each = polyOrder + 1)^(0:polyOrder), nrow = nrow(returns), ncol = polyOrder + 1, byrow = TRUE)
  
  # 1.249531 is a constant from the author's implementation.
  
  timeOfDayScatter <- matrix(1.249531 * rowMeans(abs(returns[,1:(m-2)]) ^ (2/3) * abs(returns[,2:(m-1)])^ (2/3) * abs(returns[,3:m])^ (2/3)))
  
  timeOfDayBeta <- as.numeric(solve(t(timePolyMatrix) %*% timePolyMatrix) %*% t(timePolyMatrix) %*% timeOfDayScatter)
  
  timeOfDayFit <- timePolyMatrix %*% timeOfDayBeta
  
  # Normalize the fit
  timeOfDayFit <- timeOfDayFit / mean(timeOfDayFit)
  
  
  timeOfDayScatter <- timeOfDayScatter/mean(timeOfDayScatter)
  out <- list("timeOfDayScatter" = timeOfDayScatter, "timeOfDayFit" = timeOfDayFit, "timeOfDayBeta" = timeOfDayBeta, "timePolyMatrix" = timePolyMatrix) 
  
  return(out)
  
  # plot(timeOfDayScatter)
  # lines(timeOfDayFit, col = 2)
}



jumpDetection <- function(returns, alpha){
  nDays <- ndays(returns)
  
  bpv <- as.numeric(rBPCov(returns))
  rv <- as.numeric(rCov(returns))
  
  TODadjustments <- timeOfDayAdjustments(returns, n=nrow(returns)/nDays,  m = nDays, polyOrder = 2)
  
  
  Un <- alpha * sqrt(kronecker(pmin(bpv,rv), TODadjustments$timeOfDayFit)) * (1/(nrow(returns)/nDays))^0.49
  
  jumpIndices <- which(abs(as.numeric(returns)) > Un) # Where does a jump in the market occur?
  
  out <- list("jumpIndices" = jumpIndices, "Un" = Un, "timeOfDayADJ" = TODadjustments$timeOfDayFit)
  return(out)
  
}





rankJumpTest <- function(marketReturns, stockReturns, alpha = c(7,4)){
 
  if(length(alpha) == 1){
    alpha = rep(alpha,2)
  } else if(length(alpha) > 2){
    warning("alpha should be a numeric of length one or two")
    alpha = alpha[1:2]
  }
  
  marketJumpDetection <- jumpDetection(marketReturns, alpha[1])
   
}