
#' create high frequency simulation spec
#' @param model Object of type list with the entries: \enumerate{
#'    \item modelType A string denoting which model type to use, available models are available through the \code{listAvailableVolatilityModels} function
#'    \item driftModel A strig denoting which (if any) drift model type to use, available models are available through the \code{listAvailableDriftModels} function
#'    \item drift
#'    \item volatility the total volatility (standard deviation) of the simulated model if using jumps, the jump component will lower this argument
#' }
#' @param jumpModel Object of type list with entries \enumerate{
#'    \item modelType 
#'    \item jumpComponent double in (0,1) denoting how much of the total sigma of the model that should come from the jump variation.
#'    \item
#'    \item
#'    \item
#'    \item
#' }
#' @author Emil Sjoerup
#' @export
createHFSimSpec <- function(volatilityModel = list(modelType = "constant", sigma = 0.2),
                            driftModel = list(modelType = "constant", drift = 0),
                            jumpModel = list(modelType = "none", jumpComponent = 1 / 5, jumpTime = c(0.5, 17/32)),
                            diurnalModel = list(modelType = "none", C = 0.88929198, A = 0.75, B = 0.25, a = 10, b = 10),
                            burstModel = list(driftModel = list(modelType = "none"), volModel = list(modelType = "none")),
                            noiseModel = list(noiseType = "none", signalToNoiseRatio = 2),
                            nSeries = 1, nDays = 1, nObs = 23401, discretize = FALSE, 
                            timeSettings = list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01" , sampling = "equidistant")){
  
  ## Ensuring the models are valid is based on code from Alexios Ghalanos' rugarch.
  
  ######## Volatility model checking starts
  vm <- match(names(volatilityModel), c("modelType", "sigma"))
  if(any(is.na(vm))){
    idx <- which(is.na(vm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(volatilityModel)[idx[i]])
    warning(paste(c("unidentified option(s) in volatilityModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  # If we don't get the input, we set it to the defaults
  if(is.null(volatilityModel$modelType)) volatilityModel$modelType <- "constant"
  if(is.null(volatilityModel$sigma)) volatilityModel$sigma <- as.matrix(0.2)
  volatilityModel$sigma <- as.matrix(volatilityModel$sigma)
  

  validVolatilityModelTypes <- listAvailableVolatilityModels()[,1]
  if(!is.character(volatilityModel$modelType)){
    stop("volatility model type must be a character.\n", call.=FALSE)
  }
  if(!(volatilityModel$modelType %in% validVolatilityModelTypes)){
    stop("volatility model type specified does not appear in valid model types. See listAvalableVolatilityModels() for valid types.\n", call.=FALSE)
  }
  
  if(!is.numeric(volatilityModel$sigma) | any(diag(volatilityModel$sigma) <= 0) | !isSymmetric(volatilityModel$sigma)){
    stop("sigma must be symmetric matrix with positive diagonal")
  }
  

  
  if(nrow(volatilityModel$sigma) > 1){
    ev <- eigen(volatilityModel$sigma, symmetric = TRUE)
    if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
      warning("sigma is numerically not positive semidefinite")
    }
  }
  
  if(nSeries != nrow(volatilityModel$sigma)){
    warning("nSeries does not match the number of assets mandated in the sigma matrix. Setting nSeries to match nrow(sigma)")
    nSeries <- nrow(volatilityModel$sigma)
  }
  
  
  ######## Volatility model checking done
  
  ######## Drift model checking starts
  dm <- match(names(driftModel), c("modelType", "drift", "meanReversion", "gamma", "driftVol"))
  if(any(is.na(dm))){
    idx <- which(is.na(dm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(driftModel)[idx[i]])
    warning(paste(c("unidentified option(s) in driftModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  # If we don't get the input, we set it to the defaults
  if(is.null(driftModel$modelType)) driftModel$modelType <- "constant"
  if(is.null(driftModel$drift)) driftModel$drift <- 0
  # drift must be same length as sigma has columns
  if(length(driftModel$drift) != ncol(volatilityModel$sigma)) driftModel$drift <- rep(driftModel$drift, ncol(volatilityModel$sigma))[1:ncol(volatilityModel$sigma)]
  
  if(!is.character(driftModel$modelType)){
    stop("drift model type must be a character.\n", call.=FALSE)
  }
  validDriftModelTypes <- listAvailableDriftModels()[,1]
  if(!(driftModel$modelType %in% validDriftModelTypes)){
    stop("drift model type specified does not appear in valid model types. See listAvailableDriftModels() for valid types.\n", call.=FALSE)
  }

  
  if(!is.numeric(driftModel$drift) | (length(driftModel$drift) != 1 & length(driftModel$drift) != nSeries) & length(driftModel$drift) != ncol(volatilityModel$sigma)){
    stop("drift must be a numeric with length equal to 1 or equal to nSeries, or ncol(sigma) ")
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
  } else if(jumpModel$modelType == "none"){
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
  
  
  
  
  ######### Burst model checking starts
  print("Check burst model")
  
  bm <- match(names(burstModel), c("driftModel", "volModel"))
  if(any(is.na(bm))){
    idx <- which(is.na(bm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(burstModel)[idx[i]])
    warning(paste(c("unidentified option(s) in burstModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  if(is.null(burstModel$driftModel$modelType)) burstModel$driftModel$modelType = "none"
  if(is.null(burstModel$volModel$modelType)) burstModel$volModel$modelType = "none"
  
  validBurstModels <- listAvailableBurstModels()[,1]
  if(!(burstModel$driftModel$modelType %in% validBurstModels[c(1,3)])){
    stop("burst model for the drift component's model type specified does not appear in valid model types. See listAvailableBurstModels() for valid types.\n", call.=FALSE)
  }
  if(!(burstModel$volModel$modelType %in% validBurstModels)){
    stop("burst model for the volatility component's model type specified does not appear in valid model types. See listAvailableBurstModels() for valid types.\n", call.=FALSE)
  }
  
  
  
  
  if(burstModel$driftModel$modelType == "singularityBurst" && is.null(burstModel$driftModel)) burstModel$driftModel <- list(alpha = 0.6, a = 0.002,
                                                                                                                 burstInterval = c(15/32, 17/32), modelType = "singularityBurst")
  if(burstModel$driftModel$modelType == "singularityBurst" && !is.null(burstModel$driftModel)){
    if(is.null(burstModel$driftModel$alpha)) burstModel$driftModel$alpha <- 0.6
    if(is.null(burstModel$driftModel$a)) burstModel$driftModel$a <- 0.4
    if(is.null(burstModel$driftModel$burstInterval)) burstModel$driftModel$burstInterval <- c(15/32, 17/32)
  }
  
  
  
  # We have received a burst model, which we must check
  if(burstModel$volModel$modelType == "constantBurst"){
    if(is.null(burstModel$volModel$burstMultiplier)) burstModel$volModel$burstMultiplier <- 3
    if(is.null(burstModel$volModel$burstInterval)) burstModel$volModel$burstInterval <- c(15/32, 17/32)
  }
  
  
  if(burstModel$volModel$modelType == "singularityBurst"){
    if(is.null(burstModel$volModel$b)) burstModel$volModel$b <- 0.2
    if(is.null(burstModel$volModel$beta)) burstModel$volModel$beta <- 0.4
    if(is.null(burstModel$volModel$burstInterval)) burstModel$volModel$burstInterval <- c(15/32, 17/32)
  }
  
  
  if(burstModel$driftModel$modelType == "singularityBurst" && burstModel$volModel$modelType == "singularityBurst"){
    if(burstModel$volModel$beta < 0 | burstModel$volModel$beta > 1/2 | burstModel$driftModel$alpha - burstModel$volModel$beta > 1/2){
      warning("The singularity burst model set up does not ensure absence of arbitrage. The conditions required are: 0 < beta < 1/2 and alpha - beta < 1/2.")
    }
  }
  
  
  if(burstModel$driftModel$modelType == "singularityBurst"){
    if(is.null(burstModel$driftModel$C)) diurnalModel$C <- 0.02
    if(is.null(burstModel$driftModel$A)) diurnalModel$A <- 0.75
  }
  


  
  
  
  ######## diurnal model checking starts
  dm <- match(names(diurnalModel), c("modelType","C", "A", "B", "a", "b"))
  if(any(is.na(dm))){
    idx <- which(is.na(dm))
    enx <- NULL
    for (i in 1:length(idx)) enx <- c(enx, names(diurnalModel)[idx[i]])
    warning(paste(c("unidentified option(s) in diurnalModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
  }
  
  if(!is.null(diurnalModel$modelType)) diurnalModel$modelType = "none"
  
  validDiurnalModels <- listAvailableDiurnalModels()[,1]
  if(!(diurnalModel$modelType %in% validDiurnalModels)){
    stop("diurnal model type specified does not appear in valid model types. See listAvailableDiurnalModels() for valid types.\n", call.=FALSE)
  }
  
  # We have received a diurnal model, which we must check
  if(diurnalModel$modelType == "revJ"){
    if(is.null(diurnalModel$C)) diurnalModel$C <- 0.88929198
    if(is.null(diurnalModel$A)) diurnalModel$A <- 0.75
    if(is.null(diurnalModel$B)) diurnalModel$B <- 0.25
    if(is.null(diurnalModel$a)) diurnalModel$a <- 10
    if(is.null(diurnalModel$b)) diurnalModel$b <- 10
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
  
  simSpec <- list(volatilityModel = volatilityModel, driftModel = driftModel, jumpModel = jumpModel, burstModel = burstModel, diurnalModel = diurnalModel,
                  noiseModel = noiseModel, nSeries = nSeries, nDays = nDays, nObs = nObs, discretize = discretize, timeSettings = timeSettings)
  
  # Should probably be an S4 class, but it's bad practice to mix and match S3 and S4 classes AFAIK
  class(simSpec) <- "highfrequencySimSpec"
  
  return(simSpec)
  
}


#' List the available drift models for simulations
#' @export
#' @author Emil Sjoerup
#' @return This function returns the available drift models in a matrix
listAvailableDriftModels <- function(){
  models <- matrix(
    c("constant", "constant drift",
      "oneFactor", "single factor models"
      
      ), ncol = 2, byrow=TRUE
  )
  
  colnames(models) <- c("Abbreviation", "Description")
  return(models)
}


#' List the available burst models for simulations
#' @export
#' @author Emil Sjoerup
#' @return This function returns the available burst models in a matrix
listAvailableBurstModels <- function(){
  models <- matrix(
    c("none", "no burst",
      "constantBurst", "Piecewise constant volatility with a constant burst of volatility in a pre-defined interval (FoF) (Only available for volatility)",
      "singularityBurst", "Burst that approaches a singularity as in CRO(2018)"), ncol = 2, byrow=TRUE
  )
  
  colnames(models) <- c("Abbreviation", "Description")
  return(models)
}


#' List the available diurnal models for simulations
#' @export
#' @author Emil Sjoerup
#' @return This function returns the available diurnal models in a matrix
listAvailableDiurnalModels <- function(){
  models <- matrix(
    c(
      "none", "no diurnality",
      "revJ", "reverse J shaped diurnality (FORM: C + A * exp(-a * t) + B * exp(-b * (1 - t)) )"
    ), ncol = 2, byrow = TRUE
  )
}

#' List the available noise models for simulations
#' @export
#' @author Emil Sjoerup
#' @return This function returns the available noise models in a matrix
listAvailableNoiseModels <- function(){
  models <- matrix(
    c(
      "none", "no noise",
      "additiveGaussian", "additive Gaussian noise term"
    ), ncol = 2, byrow = TRUE
  )
}

