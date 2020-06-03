
#' simulate hf prices
#' @export
hfsim.do <- function(hfSimSpec){
  # Unpacking the spec
  nObs <- hfSimSpec$nObs
  nSeries <- hfSimSpec$nSeries
  nDays <- hfSimSpec$nDays
  
  model <- hfSimSpec$model
  jumpModel <- hfSimSpec$jumpModel
  misc <- hfSimSpec$misc
  
  # Here, we must lower the volatility of the volatility process to account for the jump variation
  if (jumpModel$modelType != "none") {
    model$volatility <- model$volatility * (1-jumpModel$jumpVariation)
  }
  
  if(jumpModel$modelType == "pre announced"){
    jumpModel$modelType <- "PA" # We will use the abbreviation in the switch statement
  }
  
  
  ## First we generate a drift path
  driftPath <- switch (model$driftModel,
                       constant = matrix(model$drift,1,1)
  )
  
  ## We generate a volatility path
  volatilityModel <- switch (model$volatilityModel,
                             constant = matrix(model$volatility,1,1)
  )
  
  ## We generate a jump
  jumpPath <- switch (jumpModel$modelType,
                       none = matrix(0,1,nSeries),
                       PA = matrix(rnorm(nDays * nSeries) * sqrt(model$volatility * jumpModel$jumpVariation) , ncol = nSeries)
  )
  
  lower <- jumpModel$jumpTime[1] * nObs
  upper <- jumpModel$jumpTime[2] * nObs
  
  jumpIndices <- switch(jumpModel$modelType,
                         none = matrix(1, 1, nSeries),
                         PA = round(matrix(sample((jumpModel$jumpTime[1] * nObs):(jumpModel$jumpTime[2] * nObs),
                                                  nSeries * nDays, replace = TRUE), nDays))
  )
  # make sure the jumps don't happen 
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmin.int(x, nObs)), ncol = nSeries)
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmax.int(x, 1)), ncol = nSeries)
 
  
  
  out <- switch (model$modelType,
                 BM = BrownianMotion(nDays, nObs, driftModel, volatilityModel),
                 BMJ = BrownianMotionWithJumps(nDays, nObs, nSeries, driftModel, volatilityModel, jumpPath, jumpIndices)
  )
  
  ## Convert to xts
  if(model$sampling == "equidistant"){
    timestamps <- numeric(ifelse(is.matrix(out), nrow(out), length(out))) # If we have a multivariate output, we will use nrow.
    timestamps <- rep(0:(nDays-1), each = nObs) * 86400 + seq(misc$trading.start, misc$trading.end, length.out = nObs)
    out <- xts(out, as.POSIXct(timestamps, origin = misc$origin))  
  }
  
  
  return(out)
}


#' @importFrom anytime anytime
#' @export
createHFSimSpec <- function(model = list(modelType = "BM", driftModel = "constant", volatilityModel = "constant",  drift = 0,
                                   volatility = 0.05, alpha = NULL, beta = NULL, sampling = "equidistant"),
                      jumpModel = list(modelType = "none", jumpVariation = 1 / 5, jumpTime = c(0.5, 17/32)),
                      noiseModel = list(noiseType = "constant with interday change", signaltonoiseratio = 2),
                      nSeries = 1, nDays = 1, nObs = 23401, 
                      discretize = FALSE, time.settings = list(trading.start = 34200, trading.end = 57600, origin = "1970-01-01" , sampling = "equidistant")){

  
  

  
  
  
  simSpec <- list(model = model, jumpModel = jumpModel, noiseModel = noiseModel, nSeries = nSeries, misc = misc,
                  nDays = nDays, nObs = nObs, discretize = discretize, time.settings = time.settings)
  
  return(simSpec)
  
}

#' @keywords internal
BrownianMotion <- function(nDays, nObs, nSeries, driftModel, volatilityModel){
  
  out <- matrix(0, nrow = nDays * nObs, ncol = nSeries)
  # Vector denoting which observations to switch between days on.
  obs.vec <- seq(1, nObs * (nDays + 1), nObs)

  
  driftPath <- simulateDrift(nObs, driftModel)
  volatilityPath <- simulateVolatility(nObs, volatilityModel)
  
  out[1:nObs, ] <- apply(matrix(rnorm(nObs * nSeries, driftPath, volatilityPath) * sqrt(1/nObs), ncol = nSeries, byrow = TRUE), 2, cumsum)
  for (i in 2:nDays) {
    out[obs.vec[i]:(obs.vec[(i+1)]-1), ] = colCumsum(matrix(c(out[obs.vec[i]-1,] + rnorm(1, driftPath, volatilityPath) * sqrt(1/nObs),
                                                              rnorm(nObs-1, driftPath, volatilityPath) * sqrt(1/nObs), ), byrow = TRUE))
      
  }
  
  return(out)
  
}


#' @keywords internal
BrownianMotionWithJumps <- function(nDays, nObs, nSeries, driftModel, volatilityModel, jumpPath, jumpIndices){
  
  out <- matrix(0, nrow = nDays * nObs, ncol = nSeries)
  # Vector denoting which observations to switch between days on.
  obs.vec <- seq(1, nObs * (nDays + 1), nObs)
  
  ### TODO: Fix the volatility and drift paths such that we can actually use these.
  returns <- matrix(rnorm(nObs * nSeries, driftModel[1,], volatilityModel[1,]) * sqrt(1/nObs), ncol = nSeries)
  # Here, we insert the jumps
  for (j in 1:nSeries) {
    returns[jumpIndices[1,j], j] <- returns[jumpIndices[1,j], j] + jumpPath[1,j]
  }
  
  out[1:nObs, ] <- apply(returns,2, cumsum)
  for (i in 2:nDays) {
    returns <- matrix(rnorm((nObs) * nSeries, driftModel[1,], volatilityModel[1,]) * sqrt(1/nObs), ncol = nSeries)
    returns[1,] <- out[obs.vec[i]-1,] # set the initial values of the price
    # Here, we insert the jumps
    for (j in 1:nSeries) {
      returns[jumpIndices[i,j], j] <- returns[jumpIndices[i,j], j] + jumpPath[i,j]
    }
    
    
    out[obs.vec[i]:(obs.vec[(i+1)]-1),] <- apply(returns, 2, cumsum)
  }
  
  
  return(out)
}


