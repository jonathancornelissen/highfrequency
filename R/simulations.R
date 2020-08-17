
#' simulate hf prices
#' @author Emil Sjoerup
#' @export
hfsim.do <- function(hfSimSpec){
  # Unpacking the spec
  includeJumps <- hfSimSpec$jumpModel$includeJumps
  driftModel <- hfSimSpec$driftModel
  volatilityModel <- hfSimSpec$volatilityModel
  
  diurnallModel <- hfSimSpec$diurnalModel
  burstModel <- hfSimSpec$burstModel
  jumpModel <- hfSimSpec$jumpModel
  noiseModel <- hfSimSpec$noiseModel
  timeSettings <- hfSimSpec$timeSettings
  nObs <- hfSimSpec$nObs
  nSeries <- hfSimSpec$nSeries
  nDays <- hfSimSpec$nDays
  volatilityModel$sigma <- as.matrix(volatilityModel$sigma)
  
  if(hfSimSpec$timeSettings$sampling != "equidistant"){
    timestamps <- rep(0:(nDays-1), each = nObs) * 86400 
    
    
  } else {
    timestamps <- rep(0:(nDays-1), each = nObs) * 86400 + seq(hfSimSpec$timeSettings$tradingStart, hfSimSpec$timeSettings$tradingEnd, length.out = nObs)
    dt <- matrix(rep(1/nObs, nDays * nObs * nSeries), ncol= nSeries)
  }
  
  # Here, we lower the volatility of the volatility process to account for the jump variation
  if (jumpModel$modelType != "none" && includeJumps) {
    diag(jumpModel$jumpVolatility) <- diag(volatilityModel$sigma * jumpModel$jumpComponent) # We create a variable to contain the jump volatility which will be used in the RNG
    diag(volatilityModel$sigma) <- diag(volatilityModel$sigma * (1-jumpModel$jumpComponent))
  }

  # Adjust the volatility such that the average stays the same.
  if(burstModel$volModel$modelType == "constantBurst"){
    diag(volatilityModel$sigma) <- diag(volatilityModel$sigma) * (1/(1+((burstModel$volModel$burstInterval[2] - burstModel$volModel$burstInterval[1]) * burstModel$volModel$burstMultiplier)))
    #(1/(1 + (volatilityModel$burstModel$burstInterval[2] - volatilityModel$burstModel$burstInterval[1]) * volatilityModel$burstModel$burstMultiplier))
  }
  
  
  ### Returns coming from mu(t)
  driftReturns <- switch(driftModel$modelType,
                         constant = list("drift" = driftModel$drift * dt),
                         oneFactor = vasicekModel(driftModel, nObs, nSeries, nDays, dt)
                         )
  # par(mfrow = c(2,1))
  # plot.ts(driftReturns$drift[,1])
  # abline(h = driftModel$drift[1])
  # plot.ts(driftReturns$drift[,2])
  # abline(h = driftModel$drift[2])
  # browser()
  ### Returns coming from sigma(t)
  volatilityReturns <- switch(volatilityModel$modelType,
                              constant = constantVolatilitySim(volatilityModel, nDays, nSeries, nObs, dt)
                              )
  
  ### Returns from drift bursts
  driftBursts <- switch(burstModel$driftModel$modelType,
                        none = 0,
                        singularityBurst = singularityDriftBurst(burstModel$driftModel, nDays, nSeries, nObs, dt))
  
  ### What we need to multiply to the volatility returns
  volBursts <- switch(burstModel$volModel$modelType,
                      none = 1,
                      constantBurst = FoFVolatilitySim(burstModel$volModel, nDays, nSeries, nObs, dt),
                      singularityBurst = singularityVolBurst(burstModel$volModel, nDays, nSeries, nObs, dt))
  
  ### Returns coming from J(t)
  jumps <- switch (jumpModel$modelType,
    none = 0,
    PA = preAnnouncedJumpSim(hfSimSpec$jumpModel, nDays, nSeries, nObs)
  )
  #Construct our price process
  returns <- driftReturns$drift + driftBursts + volatilityReturns$returns * volBursts
  
  # If we need to include jumps, then we do it here
  if(includeJumps){
    jumps$jumpIndices <- jumps$jumpIndices + 0:(nDays-1) * nObs
    for (j in 1:nSeries){
      returns[jumps$jumpIndices[, j], j] <- returns[jumps$jumpIndices[, j], j] + jumps$jumps[,j]
    }
  }
  
  
  out <- vector(mode = "list", length = 4L)
  names(out) <- c("prices", "drift", "volatility", "jumps")

  out$prices <- xts(colCumsum(returns), as.POSIXct(timestamps, origin = hfSimSpec$timeSettings$origin))
  if(hfSimSpec$discretize){
   out$prices <- log(round(100 * exp(out$prices)) / 100)
  }
  
  out$jumps <- jumps
  return(out)
  
  


}

#' List the available volatility models for simulations
#' @export
#' @author Emil Sjoerup
#' @return This function returns the available volatility models in a matrix
listAvailableVolatilityModels <- function(){
  
  models <- matrix(
    c("constant", "constant volatility"), ncol = 2, byrow=TRUE
                 )
  
  colnames(models) <- c("Abbreviation", "Description")
  
  return(models)
}

#' List the available jump models for simulations
#' @export
#' @author Emil Sjoerup
#' @return This function returns the available Jump models in a matrix
listAvailableJumpModels <- function(){
  models <- matrix(
    c("none", "No jumps",
      "PA", "Single daily pre announced jump"), ncol = 2, byrow=TRUE
  )
  
  colnames(models) <- c("Abbreviation", "Description")
  return(models)
}





#' @importFrom mvtnorm rmvnorm
#' @keywords internal
constantVolatilitySim <- function(model, nDays, nSeries, nObs, dt){
  # This is simply a brownian motion
  returns <- rmvnorm(nDays * nObs, mean = rep(0, nrow(model$sigma)), model$sigma) * sqrt(dt)
  return(list("returns" = returns))
}

#' @importFrom data.table between
#' @keywords internal
FoFVolatilitySim <- function(model, nDays, nSeries, nObs, dt){
  burstIndices <- round(nObs * model$burstInterval)
  #returns <- rmvnorm(nDays * nObs, mean = rep(0, nrow(model$sigma)), model$sigma)
  #returns[between(1:nObs %% nObs, burstIndices[1], burstIndices[2]),] <- returns[between(1:nObs %% nObs, burstIndices[1], burstIndices[2]),] * sqrt(model$burstModel$burstMultiplier)
  #returns <- returns * sqrt(dt)
  returns <- matrix(rep(rep(1, nSeries), nDays * nObs), ncol = nSeries)
  returns[between(1:nObs %% nObs, burstIndices[1], burstIndices[2]),] <- sqrt(model$burstMultiplier)
  return(returns)
  
}

#' @importFrom data.table between
#' @keywords internal
singularityVolBurst <- function(model, nDays, nSeries, nObs, dt){
  if(length(dt) == 1){
    burstIndices <- round(nObs * model$burstInterval)
    timestamps <- 1:nObs/nObs
    pivot <- mean(timestamps[burstIndices]) # we use this pivot so we can move our drift burst around on the day
  } else {
    print("need to make driftBursts compatible with non-equidistant sampling")
    timestamps <- (colCumsum(dt[1:nObs,])) 
    burstIndices <- which(between(dt, model$burstInterval[1], model$burstInterval[2]))
    pivot <- timestamps[round(mean(round(nObs * model$burstInterval)))] ## Find the pivot and make sure it's actually an obseration.
  }
  volBurst <- matrix(rep(model$b * 1/abs(pivot - timestamps)^model$beta, nDays), ncol = nSeries)
  infs <- which(is.infinite(volBurst))
  volBurst[infs] <- volBurst[infs-1]
  return(sqrt(volBurst))
  
}



#' @importFrom data.table between
#' @keywords internal
singularityDriftBurst <- function(model, nDays, nSeries, nObs, dt){
  if(length(dt) == 1){
    burstIndices <- round(nObs * model$burstInterval)
    timestamps <- 1:nObs/nObs
    pivot <- mean(timestamps[burstIndices]) # we use this pivot so we can move our drift burst around on the day
  } else {
    print("need to make driftBursts compatible with non-equidistant sampling")
    timestamps <- (colCumsum(dt[1:nObs,])) 
    burstIndices <- which(between(timestamps, model$burstInterval[1], model$burstInterval[2]))
    pivot <- timestamps[round(mean(round(nObs * model$burstInterval)))] ## Find the pivot and make sure it's actually an obseration.
  }
  driftDB <- matrix(rep(model$a * sign(timestamps - pivot)/(abs(pivot - timestamps)^model$alpha), nDays), ncol = nSeries)
  driftDB[is.na(driftDB)] <- 0
  return(driftDB * dt)
  
}







############# Simulate jumps
#' @keywords internal
preAnnouncedJumpSim <- function(model, nDays, nSeries, nObs){
  jumps <- matrix(rnorm(nDays * nSeries, sd = model$jumpVolatility), ncol = nSeries)
  
  jumpIndices <- round(matrix(sample((model$jumpTime[1] * nObs):(model$jumpTime[2] * nObs), nSeries * nDays, replace = TRUE), nrow = nDays, ncol = nSeries))
  # make sure the jumps don't happen during trading and not after (i.e. we try to put it in to indices that dont exits)
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmin.int(x, nObs)), ncol = nSeries)
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmax.int(x, 1)), ncol = nSeries)
  
  out <- list("jumps" = jumps, "jumpIndices" = jumpIndices)
  return(out)
}

