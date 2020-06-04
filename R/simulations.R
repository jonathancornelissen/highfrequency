
#' simulate hf prices
#' @export
hfsim.do <- function(hfSimSpec){
  # Unpacking the spec
  nObs <- hfSimSpec$nObs
  nSeries <- hfSimSpec$nSeries
  nDays <- hfSimSpec$nDays
  includeJumps <- hfSimSpec$jumpModel$includeJumps
  model <- hfSimSpec$model
  jumpModel <- hfSimSpec$jumpModel
  misc <- hfSimSpec$misc
  
  # Here, we must lower the volatility of the volatility process to account for the jump variation
  if (jumpModel$modelType != "none") {
    jumpVolatility <- model$volatility * jumpModel$jumpVariation # We create a variable to contain the jump volatility which will be used in the RNG
    
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
                       PA = matrix(rnorm(nDays * nSeries) * sqrt(jumpVolatility) , ncol = nSeries)
  )
  
  jumpIndices <- switch(jumpModel$modelType,
                         none = matrix(1, 1, nSeries),
                         PA = round(matrix(sample((jumpModel$jumpTime[1] * nObs):(jumpModel$jumpTime[2] * nObs),
                                                  nSeries * nDays, replace = TRUE), nDays))
  )
  # make sure the jumps don't happen 
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmin.int(x, nObs)), ncol = nSeries)
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmax.int(x, 1)), ncol = nSeries)
 
  
  sim <- switch (model$modelType,
                 BM = BrownianMotion(nDays, nObs, nSeries, drift = model$drift, volatility = model$volatility),
                 BMJ = BrownianMotionWithJumps(nDays, nObs, nSeries, drift = model$drift, volatility = model$volatility, jumpPath, jumpIndices)
  )
  
  out <- vector(mode = "list", length = 4L)
  names(out) <- c("prices", "drift", "volatility", "jumps")
  ## Convert to xts
  if(model$sampling == "equidistant"){
    timestamps <- numeric(ifelse(is.matrix(sim$prices), nrow(sim$prices), length(sim$prices))) # If we have a multivariate output, we will use nrow.
    timestamps <- rep(0:(nDays-1), each = nObs) * 86400 + seq(misc$tradingStart, misc$tradingEnd, length.out = nObs)
    out$prices <- xts(sim$prices, as.POSIXct(timestamps, origin = misc$origin))  
    
    # Here, we add timestamps to the jumps
    if(includeJumps){
      out$jumps <- xts(matrix(diag(nSeries), length(jumpIndices), ncol = nSeries, byrow = FALSE), as.POSIXct(timestamps[jumpIndices + rep(0:(nDays-1) * nObs, each = nSeries)], origin = misc$origin))
    } else {
      out$jumps <- NULL
    }
    
  }
  
  
  return(out)
}


#' @importFrom anytime anytime
#' @export
createHFSimSpec <- function(model = list(modelType = "BM", driftModel = "constant", volatilityModel = "constant",  drift = 0,
                                   volatility = 0.05, alpha = NULL, beta = NULL, sampling = "equidistant"),
                      jumpModel = list(modelType = "none", jumpVariation = 1 / 5, jumpTime = c(0.5, 17/32), includeJumps = FALSE),
                      noiseModel = list(noiseType = "constant with interday change", signaltonoiseratio = 2),
                      nSeries = 1, nDays = 1, nObs = 23401, 
                      discretize = FALSE, time.settings = list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01" , sampling = "equidistant")){

  
  simSpec <- list(model = model, jumpModel = jumpModel, noiseModel = noiseModel, nSeries = nSeries, misc = misc,
                  nDays = nDays, nObs = nObs, discretize = discretize, time.settings = time.settings)
  
  return(simSpec)
  
}

#' @keywords internal
BrownianMotion <- function(nDays, nObs, nSeries, drift, volatility){
  
  prices <- matrix(0, nrow = nDays * nObs, ncol = nSeries)
  # Vector denoting which observations to switch between days on.
  obs.vec <- seq(1, nObs * (nDays + 1), nObs)

  # simulate the drift and volatility paths that will serve as the basis of our price simulation
  
  returns <- matrix(rnorm(nObs * nSeries, drift, volatility) * sqrt(1/nObs), ncol = nSeries)
  prices[1:nObs, ] <- colCumsum(returns)
  for (i in 2:nDays) {
    returns <- matrix(rnorm((nObs) * nSeries, drift, volatility) * sqrt(1/nObs), ncol = nSeries)
    returns[1,] <- prices[obs.vec[i]-1,] # set the initial values of the price
    prices[obs.vec[i]:(obs.vec[(i+1)]-1),] <- colCumsum(returns)
  }
  
  out <- list(prices = prices)
  
  
  return(out)
  
}

#' @keywords internal
BrownianMotionWithJumps <- function(nDays, nObs, nSeries, drift, volatility, jumpPath, jumpIndices){
  
  
  
  prices <- matrix(0, nrow = nDays * nObs, ncol = nSeries)
  # Vector denoting which observations to switch between days on.
  obs.vec <- seq(1, nObs * (nDays + 1), nObs)
  
  
  returns <- matrix(rnorm(nObs * nSeries, drift, volatility) * sqrt(1/nObs), ncol = nSeries)
  # Here, we insert the jumps
  for (j in 1:nSeries) {
    returns[jumpIndices[1,j], j] <- returns[jumpIndices[1,j], j] + jumpPath[1,j]
  }
  
  prices[1:nObs, ] <- colCumsum(returns)
  for (i in 2:nDays) {
    returns <- matrix(rnorm((nObs) * nSeries, drift, volatility) * sqrt(1/nObs), ncol = nSeries)
    returns[1,] <- returns[1, ] + prices[obs.vec[i]-1,] # set the initial values of the price
    # Here, we insert the jumps
    for (j in 1:nSeries) {
      returns[jumpIndices[i,j], j] <- returns[jumpIndices[i,j], j] + jumpPath[i,j]
    }
    
    prices[obs.vec[i]:(obs.vec[(i+1)]-1),] <- colCumsum(returns)
  }
  
  out <- list(prices = prices)
  
  return(out)
}


simulateDrift <- function(model){
  
  if(model$driftModel == "constant"){
    out <- model$drift
  }
  
  
  return(out)
  
  
}

simulateVolatility <- function(model){
  
  if(model$driftModel == "constant"){
    out <- model$volatilityModel
  }
  
  return(out)
}

