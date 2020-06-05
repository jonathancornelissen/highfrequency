
#' simulate hf prices
#' 
#' @export
hfsim.do <- function(hfSimSpec){
  # Unpacking the spec
  nObs <- hfSimSpec$nObs
  nSeries <- hfSimSpec$nSeries
  nDays <- hfSimSpec$nDays
  includeJumps <- hfSimSpec$jumpModel$includeJumps
  hfSimSpec$volatilityModel$volatility <- sqrt(hfSimSpec$volatilityModel$variance)
  # Here, we lower the volatility of the volatility process to account for the jump variation
  if (jumpModel$modelType != "none" && includeJumps) {
    hfSimSpec$jumpModel$jumpVolatility <- sqrt(hfSimSpec$volatilityModel$variance * jumpModel$jumpComponent) # We create a variable to contain the jump volatility which will be used in the RNG
    
    hfSimSpec$volatilityModel$volatility <- sqrt(hfSimSpec$volatilityModel$variance * (1-hfSimSpec$jumpModel$jumpComponent)) # volatility is standard deviation!
    
  }

  if(hfSimSpec$volatilityModel$modelType == "constant burst"){
    
  }
  
  
  ### Returns coming from mu(t)
  
  driftReturns <- switch(hfSimSpec$driftModel$modelType,
                         constant = hfSimSpec$driftModel$drift
                         )
  
  ### Returns coming from sigma(t)
  
  volatilityReturns <- switch(hfSimSpec$volatilityModel$modelType,
                              constant = constantVolatilitySim(hfSimSpec$volatilityModel, nDays, nSeries, nObs),
                              `constant burst` = FoFVolatilitySim(hfSimSpec$volatilityModel, nDays, nSeries, nObs)
                              )  
  
  
  ### Returns coming from J(t)
  jumps <- switch (hfSimSpec$jumpModel$modelType,
    none = 0,
    PA = preAnnouncedJumpSim(hfSimSpec$jumpModel, nDays, nSeries, nObs)
  )
  
  #Construct our price process
  returns <- driftReturns + volatilityReturns
  
  # If we need to include jumps, then we do it here
  if(includeJumps){
    jumps$jumpIndices <- jumps$jumpIndices + 0:(nDays-1) * nObs
    returns[jumps$jumpIndices] <- returns[jumps$jumpIndices] + jumps$jumps
  }
  
  
  out <- vector(mode = "list", length = 4L)
  names(out) <- c("prices", "drift", "volatility", "jumps")
  ## Convert to xts
  if(hfSimSpec$timeSettings$sampling == "equidistant"){
    timestamps <- numeric(ifelse(is.matrix(returns), nrow(returns), length(returns))) # If we have a multivariate output, we will use nrow.
    timestamps <- rep(0:(nDays-1), each = nObs) * 86400 + seq(hfSimSpec$timeSettings$tradingStart, hfSimSpec$timeSettings$tradingEnd, length.out = nObs)
    out$prices <- xts(colCumsum(returns), as.POSIXct(timestamps, origin = hfSimSpec$timeSettings$origin))  
  }
  
  return(out)
  
  
  ### NO longer used.
  
  ## First we generate a drift path
  driftPath <- switch (model$driftModel,
                       constant = matrix(model$drift,1,1)
  )
  
  ## We generate a volatility path
  volatilityModel <- switch (model$volatilityModel,
                             constant = matrix(model$variance,1,1)
  )
  
  ## We generate a jump
  jumpPath <- switch (jumpModel$modelType,
                       none = matrix(0,1,nSeries),
                       PA = matrix(rnorm(nDays * nSeries, sd = jumpVolatility) , ncol = nSeries)
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
                 BM = BrownianMotion(nDays, nObs, nSeries, drift = model$drift, variance = model$variance),
                 BMJ = BrownianMotionWithJumps(nDays, nObs, nSeries, drift = model$drift, variance = model$variance, jumpPath, jumpIndices)
  )
  

}

#' List the available volatility models for simulations
#' @export
#' @return This function returns the available volatility models in a matrix
listAvailableVolatilityModels <- function(){
  
  models <- matrix(
    c("constant", "constant volatility",
      "constant burst", "Piecewise constant volatility with a constant burst of volatility in a pre-defined interval (FoF)"), ncol = 2, byrow=TRUE
                 )
  
  colnames(models) <- c("Abbreviation", "Description")
  
  return(models)
}

#' List the available jump models for simulations
#' @export
#' @return This function returns the available Jump models in a matrix
listAvailableJumpModels <- function(){
  models <- matrix(
    c("none", "No jumps",
      "PA", "Single daily pre announced jump"), ncol = 2, byrow=TRUE
  )
  
  colnames(models) <- c("Abbreviation", "Description")
  return(models)
}



#' List the available drift models for simulations
#' @export
#' @return This function returns the available drift models in a matrix
listAvailableDriftModels <- function(){
  models <- matrix(
    c("constant", "constant drift",
      "singularityBurst", "Drift burst that approaches a singularity as in CRO(2018)"), ncol = 2, byrow=TRUE
  )
  
  colnames(models) <- c("Abbreviation", "Description")
  return(models)
}


#' create high frequency simulation spec
#' @param model Object of type list with the entries: \enumerate{
#'    \item modelType A string denoting which model type to use, available models are available through the \code{listAvailableVolatilityModels} function
#'    \item driftModel A strig denoting which (if any) drift model type to use, available models are available through the \code{listAvailableDriftModels} function
#'    \item drift
#'    \item volatility the total volatility (standard deviation) of the simulated model if using jumps, the jump component will lower this argument
#' }
#' @param jumpModel Object of type list with entries \enumerate{
#'    \item modelType 
#'    \item jumpComponent double in (0,1) denoting how much of the total variance of the model that should come from the jump variation.
#'    \item
#'    \item
#'    \item
#'    \item
#' }
#' @export
createHFSimSpec <- function(volatilityModel = list(modelType = "constant", variance = 0.2, burstMultiplier = 3, includeDiurnality = FALSE, 
                                                   diurnalModel = list(C = 0.88929198, A = 0.75, B = 0.25, a = 10, b = 10)),
                            driftModel = list(modelType = "constant", drift = 0),
                            jumpModel = list(modelType = "none", jumpComponent = 1 / 5, jumpTime = c(0.5, 17/32)),
                            noiseModel = list(noiseType = "constant with interday change", signalToNoiseRatio = 2),
                            nSeries = 1, nDays = 1, nObs = 23401, discretize = FALSE, 
                            timeSettings = list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01" , sampling = "equidistant")){
  
  ## Ensuring the models are valid is based on code from Alexios Ghalanos' rugarch.
  
  ######## Volatility model checking starts
  vm <- match(names(volatilityModel), c("modelType", "variance", "burstModel", "includeDiurnality", "diurnalModel"))
  if(any(is.na(vm))){
    idx <- which(is.na(vm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(volatilityModel)[idx[i]])
    warning(paste(c("unidentified option(s) in volatilityModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  # If we don't get the input, we set it to the defaults
  if(is.null(volatilityModel$modelType)) volatilityModel$modelType <- "constant"
  if(is.null(volatilityModel$variance)) volatilityModel$variance <- 0.2
  if(is.null(volatilityModel$includeDiurnality)) volatilityModel$includeDiurnality <- FALSE
  # In case we have not received a diurnal model at all
  if(volatilityModel$includeDiurnality && is.null(volatilityModel$diurnalModel)) volatilityModel$diurnalModel <- list(C = 0.88929198, A = 0.75, B = 0.25, a = 10, b = 10)
  
  # We have received a diurnal model, which we must check
  if(volatilityModel$includeDiurnality && !is.null(volatilityModel$diurnalModel)){
    if(is.null(volatilityModel$diurnalModel$C)) volatilityModel$diurnalModel$C <- 0.88929198
    if(is.null(volatilityModel$diurnalModel$A)) volatilityModel$diurnalModel$A <- 0.75
    if(is.null(volatilityModel$diurnalModel$B)) volatilityModel$diurnalModel$B <- 0.25
    if(is.null(volatilityModel$diurnalModel$a)) volatilityModel$diurnalModel$a <- 10
    if(is.null(volatilityModel$diurnalModel$b)) volatilityModel$diurnalModel$b <- 10
  }
  
  validVolatilityModelTypes <- listAvailableVolatilityModels()[,1]
  if(!is.character(volatilityModel$modelType)){
    stop("volatility model type must be a character.\n", call.=FALSE)
  }
  if(!(volatilityModel$modelType %in% validVolatilityModelTypes)){
    stop("volatility model type specified does not appear in valid model types. See listAvalableVolatilityModels() for valid types.\n", call.=FALSE)
  }
  if(!is.numeric(volatilityModel$variance) | volatilityModel$variance <= 0){
    stop("variance must be a positive numeric")
  }
  
  if(volatilityModel$modelType == "constant burst" && is.null(volatilityModel$burstMultiplier)) volatilityModel$burstMultiplier <- 3
  ######## Volatility model checking done
  
  ######## Drift model checking starts
  dm <- match(names(driftModel), c("modelType", "drift"))
  if(any(is.na(dm))){
    idx <- which(is.na(dm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(driftModel)[idx[i]])
    warning(paste(c("unidentified option(s) in driftModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  # If we don't get the input, we set it to the defaults
  if(is.null(driftModel$modelType)) driftModel$modelType <- "constant"
  if(is.null(driftModel$drift)) driftModel$drift <- 0
  
  if(!is.character(driftModel$modelType)){
    stop("drift model type must be a character.\n", call.=FALSE)
  }
  validDriftModelTypes <- listAvailableDriftModels()[,1]
  if(!(driftModel$modelType %in% validDriftModelTypes)){
    stop("volatility model type specified does not appear in valid model types. See listAvailableDriftModels() for valid types.\n", call.=FALSE)
  }
  
  
  
  if(!is.numeric(driftModel$drift) | (length(driftModel$drift) != 1 & length(driftModel$drift) != nSeries)){
    stop("drift must be a numeric with length equal to 1 or equal to nSeries")
  }
  ######## Drift model checking ends
  
  
  ######## Jump model checking starts
  jm <- match(names(jumpModel), c("modelType", "jumpComponent", "jumpTime", "includeJumps"))
  if(any(is.na(jm))){
    idx <- which(is.na(jm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(jumpModel)[idx[i]])
    warning(paste(c("unidentified option(s) in jumpModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  
  # We set the includeJumps tag to TRUE if we should actually include jumps 
  # And we set it FALSE if we should not.
  if(jumpModel$modelType != "none" && is.null(jumpModel$includeJumps)){
    jumpModel$includeJumps <- TRUE
  } else if(jumpModel$modelType == "none" && is.null(jumpModel$includeJumps)){
    jumpModel$includeJumps <- FALSE
  }
  
  
  if(is.null(jumpModel$modelType)) jumpModel$modelType <- "none"
  if(is.null(jumpModel$jumpComponent)) jumpModel$jumpComponent <- 1/5
  if(is.null(jumpModel$jumpTime)) jumpModel$jumpTime <- c(0.5, 17/32)
  
  # Check whether the jump component makes sense.
  if(jumpModel$includeJumps && jumpModel$jumpComponent >= 1){
    stop("Jump component equal to or greater than 1, this is not allowed. Jump component should be between 0 and 1", call. = FALSE, domain=NULL)
  }
  
  if(jumpModel$includeJumps && jumpModel$jumpComponent <= 0){
    stop("Jump component equal to or less than 0, this is not allowed. Jump component should be between 0 and 1", call. = FALSE, domain=NULL)
  }
  
  if(jumpModel$includeJumps && length(jumpModel$jumpTime) != 2){
    stop("jumpTime must be of length 2", call.=FALSE, domain = NULL)
  }
  
  if(jumpModel$includeJumps && jumpModel$jumpTime[1] > jumpModel$jumpTime[2]){
    stop("First entry of jumpTime must be lower than second entry of jumpTime", call.=FALSE, domain=NULL)
  }
  
  if(jumpModel$includeJumps && (min(jumpModel$jumpTime)<0 | max(jumpModel$jumpTime)>1)){
    stop("jumpTime must be between 0 and 1", call. = FALSE, domain=NULL)
  }
  ######## Jump model checking ends
  
  if(!is.numeric(nSeries) | nSeries <= 0 | nSeries %% 1 != 0){
    stop("nSeries must be a positive integer", call. = FALSE, domain = NULL)
  }
  
  if(!is.numeric(nDays) | nDays <= 0 | nDays %% 1 != 0){
    stop("nDays must be a positive integer", call. = FALSE, domain = NULL)
  }
  
  if(!is.numeric(nObs) | nObs <= 0 | nObs %% 1 != 0){
    stop("nObs must be a positive integer", call. = FALSE, domain = NULL)
  }
  
  if(!is.logical(discretize)){
    stop("discretize must a logical", call. = FALSE, domain = NULL)
  }
  
  ######## time settings checking starts
  
  ts <- match(names(timeSettings), c("tradingStart", "tradingEnd", "origin", "sampling"))
  if(any(is.na(ts))){
    idx <- which(is.na(ts))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(timeSettings)[idx[i]])
    warning(paste(c("unidentified option(s) in timeSettings:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  if(is.null(timeSettings$tradingStart)) timeSettings$tradingStart <- 34200
  if(is.null(timeSettings$tradingEnd)) timeSettings$tradingEnd <- 57600
  if(is.null(timeSettings$origin)) timeSettings$origin <- "1970-01-01"
  if(is.null(timeSettings$sampling)) timeSettings$sampling <- "equidistant"
  
  if(timeSettings$tradingStart < timeSettings$tradingStart){
    stop("Start of trading must be before end of trading")
  }
  
  print("finished checking timeSettings?")
  
  print("Implement checks on noise model")
  
  simSpec <- list(volatilityModel = volatilityModel, driftModel = driftModel, jumpModel = jumpModel, noiseModel = noiseModel, nSeries = nSeries,
                  nDays = nDays, nObs = nObs, discretize = discretize, timeSettings = timeSettings)
  
  # Should probably be an S4 class, but it's bad practice to mix and match S3 and S4 classes AFAIK
  class(simSpec) <- "highfrequencySimSpec"
  
  return(simSpec)
  
}




#' @keywords internal
BrownianMotion <- function(nDays, nObs, nSeries, drift, variance){
  
  prices <- matrix(0, nrow = nDays * nObs, ncol = nSeries)
  # Vector denoting which observations to switch between days on.
  obs.vec <- seq(1, nObs * (nDays + 1), nObs)

  # simulate the drift and variance paths that will serve as the basis of our price simulation
  
  returns <- matrix(rnorm(nObs * nSeries, drift, sqrt(variance)) * sqrt(1/nObs), ncol = nSeries)
  prices[1:nObs, ] <- colCumsum(returns)
  for (i in 2:nDays) {
    returns <- matrix(rnorm((nObs) * nSeries, drift, sqrt(variance)) * sqrt(1/nObs), ncol = nSeries)
    returns[1,] <- prices[obs.vec[i]-1,] # set the initial values of the price on day i
    prices[obs.vec[i]:(obs.vec[(i+1)]-1),] <- colCumsum(returns)
  }
  
  out <- list(prices = prices)
  
  
  return(out)
  
}

#' @keywords internal
BrownianMotionWithJumps <- function(nDays, nObs, nSeries, drift, variance, jumpPath, jumpIndices){
  
  
  
  prices <- matrix(0, nrow = nDays * nObs, ncol = nSeries)
  # Vector denoting which observations to switch between days on.
  obs.vec <- seq(1, nObs * (nDays + 1), nObs)
  
  
  returns <- matrix(rnorm(nObs * nSeries, drift, variance) * sqrt(1/nObs), ncol = nSeries)
  # Here, we insert the jumps
  for (j in 1:nSeries) {
    returns[jumpIndices[1,j], j] <- returns[jumpIndices[1,j], j] + jumpPath[1,j]
  }
  
  prices[1:nObs, ] <- colCumsum(returns)
  for (i in 2:nDays) {
    returns <- matrix(rnorm((nObs) * nSeries, drift, variance) * sqrt(1/nObs), ncol = nSeries)
    returns[1,] <- returns[1, ] + prices[obs.vec[i]-1,] # set the initial values of the price on day i
    # Here, we insert the jumps
    for (j in 1:nSeries) {
      returns[jumpIndices[i,j], j] <- returns[jumpIndices[i,j], j] + jumpPath[i,j]
    }
    
    prices[obs.vec[i]:(obs.vec[(i+1)]-1),] <- colCumsum(returns)
  }
  
  out <- list(prices = prices)
  
  return(out)
}


simulateDrift <- function(model, nDays, nSeries, nObs){
  if(model$driftModel == "constant"){
    out <- model$drift
  }
  
  
  return(out)
  
  
}

# simulateVolatility <- function(model, nDays, nSeries, nObs){
#   browser()
#   
#   if(model$driftModel == "constant"){
#     returns <- matrix(rnorm(nDays * nSeries * nObs, 0, model$variance), ncol = )
#     return(returns)
#   }
#   
#   return(out)
# }

#' @keywords internal
constantVolatilitySim <- function(model, nDays, nSeries, nObs){
  # This is simply a brownian motion
  returns <- matrix(rnorm(nDays * nSeries * nObs, mean = 0, sd = model$volatility) * sqrt(1/nObs), ncol = nSeries, nrow = nDays * nObs)
  return(returns)
}

#' @keywords internal
FoFVolatilitySim <- function(model, nDays, nSeries, nObs){
  
  model$burstModel$burstMultiplier
  model$burstModel$burstInterval
  burstIndices <- round(nObs * model$burstModel$burstInterval)
  volatility <- rep(model$volatility, nObs)
  volatility[burstIndices[1] : burstIndices[2]] <- volatility[burstIndices[1] : burstIndices[2]] * model$burstModel$burstMultiplier
  returns <- matrix(rnorm(nDays * nSeries * nObs, mean = 0, sd = volatility) * sqrt(1/nObs), ncol = nSeries, nrow = nDays * nObs)
  
}




############# Simulate jumps
#' @keywords internal
preAnnouncedJumpSim <- function(model, nDays, nSeries, nObs){
  jumps <- matrix(rnorm(nDays * nSeries, sd = model$jumpVolatility), ncol = nSeries)
  
  jumpIndices <- round(matrix(sample((jumpModel$jumpTime[1] * nObs):(jumpModel$jumpTime[2] * nObs), nSeries * nDays, replace = TRUE), nrow = nDays, ncol = nSeries))
  # make sure the jumps don't happen during trading and not after (i.e. we try to put it in to indices that dont exits)
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmin.int(x, nObs)), ncol = nSeries)
  jumpIndices <- matrix(apply(jumpIndices, 2, FUN = function(x) pmax.int(x, 1)), ncol = nSeries)
  
  
  out <- list("jumps" = jumps, "jumpIndices" = jumpIndices)
  return(out)
}
