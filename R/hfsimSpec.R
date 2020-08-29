# #' create high frequency simulation spec
# #' @param volatilityModel Object of type list with the entries: \enumerate{
# #'    \item \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableVolatilityModels}} function
# #'    \item \code{sigma} A symmetric, preferably positive semi-definite variance-covariance matrix. This is not used when modelType is "huangTauchen".
# #'    \item \code{volOfVol} numeric of either length 1, nrow(sigma) or nSeries denoting the volatility of the volatility
# #'    \item \code{meanReversion} numeric of either length 1, nrow or nSeries denoting the mean reversion of the volatlity series
# #'    \item \code{rho} numeric of either length 1, nrow(sigma) or nSeries denoting the correlation between wt and bt in the "Heston" model
# #'    \item \code{alpha1} numeric of either length 1, nrow(sigma) or nSeries denoting the mean reversion of the persistent volatility factor in the "huangTauchen" model
# #'    \item \code{alpha2} numeric of either length 1, nrow(sigma) or nSeries denoting the mean reversion of the transient volatility factor in the "huangTauchen" model
# #'    \item \code{beta0} numeric of either length 1, nrow(sigma) or nSeries used for scaling the volatility in the "huangTauchen" model
# #'    \item \code{beta1} numeric of either length 1, nrow(sigma) or nSeries used for scaling the importance of the persistent volatility factor in the "huangTauchen" model
# #'    \item \code{beta2} numeric of either length 1, nrow(sigma) or nSeries used for scaling the importance of the transient volatility factor in the "huangTauchen" model
# #'    \item \code{phi} numeric of either length 1, nrow(sigma) or nSeries denoting the strenght of the feedback in the transient volatility factor in the "huangTauchen" model
# #'    \item \code{rho1} numeric of either length 1, nrow(sigma) or nSeries denoting the leverage correlations between w1t and w2t in the "huangTauchen" model
# #'    \item \code{rho2} numeric of either length 1, nrow(sigma) or nSeries denoting the leverage correlations between w1t and w2t in the "huangTauchen" model
# #' }
# #' @param driftModel Object of type list with entries \enumerate{
# #'    \item \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableDriftModels}} function
# #'    \item \code{drift} numeric of either length 1, nrow or nSeries denoting the drift in case modelType = "constant", and the long term mean of drift in case that is not true. Default is 0.
# #'    \item \code{meanReversion} numeric of either length 1, nrow or nSeries denoting the mean reversion of drift. Default is 2.
# #'    \item \code{driftVol} numeric of either length 1, nrow or nSeries denoting the volatility of drift in case modelType = "vasicek".
# #' }
# #' @param jumpModel Object of type list with entries \enumerate{
# #'    \item \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableJumpModels}} function
# #'    \item \code{jumpComponent} double between 0 and 1 denoting how much of the total sigma of the model that should come from the jump variation.
# #'    \item \code{}
# #'    \item
# #'    \item
# #'    \item 
# #' }
# #' @param diurnalModel Object of type list with entries \enumerate{
# #'    \item \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableDiurnalModels}} function.
# #'    \item \code{C} double denoting the level of the diurnal pattern. (Usually calibrated to make sure that when integrating over the diurnal factor, it gives 1)
# #'    \item \code{A} double denoting the A value of the diurnal pattern. This is used to control the level of the diurnal factor during the beginning of the day. Default is 0.75
# #'    \item \code{B} double denoting the B value of the diurnal pattern. This is used to control the level of the diurnal factor during the end of the day. Default is 0.25
# #'    \item \code{a} double denoting the a value of the diurnal pattern. This is used to control the steepness of the drop during the beginning of the day. Default is 10
# #'    \item \code{b} double denoting the b value of the diurnal pattern. This is used to control the steepness of the increase during the end of the day. Default is 10
# #' }
# #' @param burstModel Object of type list with entries \enumerate{
# #'    \item driftModel list with entries \enumerate{ 
# #'          \item{ \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableBurstModels}} function}
# #'          \item{ \code{alpha} A numeric of length one denoting the alpha exponent in the drift burst. Default is 0.65 }
# #'          \item{ \code{a} A numeric of length one denoting the a scalar in the drift burst this can be used to control the severity of the drift burst. Default is 1 }
# #'          \item{ \code{burstInterval} A numeric of length two when the drift burst is to take place. Default is \code{c(15/32, 17/32)} }
# #'    }
# #'    \item volModel list with entries \enumerate{
# #'          \item{ \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableBurstModels}} function}
# #'          \item{ \code{beta} (Used only when modelType is "singularityBurst") A numeric of length one denoting the beta exponent in the volatility burst. Default is 0.4 }
# #'          \item{ \code{b} (Used only when modelType is "singularityBurst") A numeric of length one denoting the b scalar in the volatility burst this can be used to control the severity of the volatility burst. Default is 1 }
# #'          \item{ \code{burstMultiplier} (Used only when modelType is "constantBurst") A numeric of length one denoting the severity of the volatility burst. Default is 3 denoting a temporary three-fold increase of volatility}
# #'          \item{ \code{burstInterval} A numeric of length two when the volatility burst is to take place. Default is \code{c(15/32, 17/32)} }
# #'    }
# #' }
# #' @param noiseModel Object of type list with entries \enumerate{
# #'    \item \code{modelType} A string denoting which model type to use, available models and descriptions thereof are available through the \code{\link{listAvailableNoiseModels}} function.
# #'    \item \code{signalToNoise} A numeric of either length 1, nrow(sigma) or nSeries denoting the signal to noise ratio of the noise.
# #'    \item \code{variance} A numeric of either length 1, nrow(sigma) or nSeries denoting the variance of the noise when the variance is constant.
# #' }
# #' @param timeSettings Object of type list with entries \enumerate{
# #'    \item \code{tradingStart} A numeric of length one denoting in seconds after midnight the start of trading. Default is 34200 which corresponds to 09:30
# #'    \item \code{tradingEnd} A numeric of length one denoting in seconds after midnight the start of trading. Default is 57600 which corresponds to 16:00
# #'    \item \code{origin} A character which can be coerced to a date, denoting the first day of the simulated series. Default is "1970-01-01"
# #'    \item \code{sampling} A character denoting which sampling scheme to use. Currently only "equidistant" is implemented.
# #' }
# #' @param nSeries Integer-valued numeric of length one denoting how many series to simulate. This is overwritten in case nrow of the variance covariance matrix is greater than one then, nSeries will be set to match nrow(sigma)
# #' @param nDays Integer-valued numeric of length one denoting how many days to simulate the data over.
# #' @param nObs Integer-valued numeric of length one denoting how many days to simulate the data over.
# #' @param discretize Logical denoting whether to discretize the prices. The prices are discretized using log(round(100 * exp(prices)) / 100). i.e. to nearest cent.
# #' @returns an object of type "highfrequencySimSpec"
# #' @author Emil Sjoerup
# #' @export
# createHFSimSpec <- function(volatilityModel = list(modelType = "constant", sigma = 0.2),
#                             driftModel = list(modelType = "constant", drift = 0),
#                             jumpModel = list(modelType = "none", jumpComponent = 1 / 5, jumpTime = c(0.5, 17/32)),
#                             diurnalModel = list(modelType = "none", C = 0.88929198, A = 0.75, B = 0.25, a = 10, b = 10),
#                             burstModel = list(driftModel = list(modelType = "none"), volModel = list(modelType = "none")),
#                             noiseModel = list(noiseType = "none", signalToNoiseRatio = 2),
#                             timeSettings = list(tradingStart = 34200, tradingEnd = 57600, origin = "1970-01-01", sampling = "equidistant"),
#                             nSeries = 1, nDays = 1, nObs = 23401, discretize = FALSE){
#   
#   ## Ensuring the models are valid is based on code from Alexios Ghalanos' rugarch.
#   
#   ######## Volatility model checking starts
#   vm <- match(names(volatilityModel), c("modelType","meanReversion", "sigma","volOfVol", "rho", "alpha1", "alpha2", "beta0",
#                                         "beta1", "beta2", "phi", "rho1", "rho2"))
#   if(any(is.na(vm))){
#     idx <- which(is.na(vm))
#     enx <- NULL
#     for (i in 1:length(idx)) enx <- c(enx, names(volatilityModel)[idx[i]])
#     warning(paste(c("unidentified option(s) in volatilityModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
#   }
#   
#   # If we don't get the input, we set it to the defaults
#   if(is.null(volatilityModel$modelType)) volatilityModel$modelType <- "constant"
#   if(is.null(volatilityModel$sigma)) volatilityModel$sigma <- diag(0.2, nrow = nSeries, ncol = nSeries)
#   volatilityModel$sigma <- matrix(volatilityModel$sigma, ncol = nSeries)
#   
# 
#   validVolatilityModelTypes <- listAvailableVolatilityModels()[,1]
#   if(!is.character(volatilityModel$modelType)){
#     stop("volatility model type must be a character.\n", call.=FALSE)
#   }
#   if(!(volatilityModel$modelType %in% validVolatilityModelTypes)){
#     stop("volatility model type specified does not appear in valid model types. See listAvalableVolatilityModels() for valid types.\n", call.=FALSE)
#   }
#   
#   if(!is.numeric(volatilityModel$sigma) | any(diag(volatilityModel$sigma) < 0) | !isSymmetric(volatilityModel$sigma)){
#     stop("sigma must be symmetric matrix with non-negative diagonal")
#   }
#   
#   if(nrow(volatilityModel$sigma) > 1){
#     ev <- eigen(volatilityModel$sigma, symmetric = TRUE)
#     if (!all(ev$values >= -sqrt(.Machine$double.eps) * abs(ev$values[1]))) {
#       warning("sigma is numerically not positive semidefinite")
#     }
#   }
#   
#   if(nSeries != nrow(volatilityModel$sigma)){
#     if(nrow(volatilityModel$sigma) != 1){
#       warning("nSeries does not match the number of assets mandated in the sigma matrix. Setting nSeries to match nrow(sigma)")
#       nSeries <- nrow(volatilityModel$sigma)
#     } else { ## We have 1 value for sigma, so we make it into a nSeries by nSeries diagonal matrix
#       volatilityModel$sigma <- diag(as.numeric(volatilityModel$sigma), ncol = nSeries, nrow = nSeries)
#     }
#   }
#   
#   if(volatilityModel$modelType == "heston"){
#     # Defaults on the slides of Bezirgen Veliyev for the 2018 high frequency econometrics course at Aarhus University:
#     if(is.null(volatilityModel$meanReversion)) volatilityModel$meanReversion <- rep(5/250, nSeries)
#     if(is.null(volatilityModel$sigma)) volatilityModel$sigma <- diag(0.2/250, nSeries)
#     if(is.null(volatilityModel$volOfVol)) volatilityModel$volOfVol <- rep(0.5/250, nSeries)
#     if(is.null(volatilityModel$rho)) volatilityModel$rho <- rep(-0.5, nSeries)
#   }
#   
#   
#   if(volatilityModel$modelType == "huangTauchen"){
#     if(is.null(volatilityModel$alpha1)) volatilityModel$alpha1 <- rep(-0.00137, nSeries)
#     if(is.null(volatilityModel$alpha2)) volatilityModel$alpha2 <- rep(-1.386, nSeries)
#     if(is.null(volatilityModel$beta0)) volatilityModel$beta0 <- rep(-1.2, nSeries)
#     if(is.null(volatilityModel$beta1)) volatilityModel$beta1 <- rep(0.04, nSeries)
#     if(is.null(volatilityModel$beta2)) volatilityModel$beta2 <- rep(1.5, nSeries)
#     if(is.null(volatilityModel$phi)) volatilityModel$phi <- rep(0.25, nSeries)
#     if(is.null(volatilityModel$rho1)) volatilityModel$rho1 <- rep(-0.3, nSeries)
#     if(is.null(volatilityModel$rho2)) volatilityModel$rho2 <- rep(-0.3, nSeries)
#   }
#   
#   
#   ######## Volatility model checking done
#   
#   ######## Drift model checking starts
#   dm <- match(names(driftModel), c("modelType", "drift", "meanReversion", "driftVol"))
#   if(any(is.na(dm))){
#     idx <- which(is.na(dm))
#     enx <- NULL
#     for (i in 1:length(idx)) enx <- c(enx, names(driftModel)[idx[i]])
#     warning(paste(c("unidentified option(s) in driftModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
#   }
#   
#   # If we don't get the input, we set it to the defaults
#   if(is.null(driftModel$modelType)) driftModel$modelType <- "constant"
#   if(is.null(driftModel$drift)) driftModel$drift <- 0
#   # drift must be same length as sigma has columns
#   if(length(driftModel$drift) != ncol(volatilityModel$sigma)) driftModel$drift <- rep(driftModel$drift, ncol(volatilityModel$sigma))[1:ncol(volatilityModel$sigma)]
#   if(!is.character(driftModel$modelType)){
#     stop("drift model type must be a character.\n", call.=FALSE)
#   }
#   validDriftModelTypes <- listAvailableDriftModels()[,1]
#   if(!(driftModel$modelType %in% validDriftModelTypes)){
#     stop("drift model type specified does not appear in valid model types. See listAvailableDriftModels() for valid types.\n", call.=FALSE)
#   }
#   
#   ## Check that vasicek model is correctly specified
#   if(driftModel$modelType == "vasicek"){
#     
#     if(is.null(driftModel$drift)) driftModel$drift = rep(0, ncol(volatilityModel$sigma))
#     if(is.null(driftModel$meanReversion)) driftModel$meanReversion = rep(2, ncol(volatilityModel$sigma))
#     if(is.null(driftModel$driftVol)) driftModel$driftVol = rep(0.0391, ncol(volatilityModel$sigma))
#     
#     if(length(driftModel$drift) != ncol(volatilityModel$sigma)) driftModel$drift <- rep(driftModel$drift, ncol(volatilityModel$sigma))[1:ncol(volatilityModel$sigma)]
#     if(length(driftModel$meanReversion) != ncol(volatilityModel$sigma)) driftModel$meanReversion <- rep(driftModel$meanReversion, ncol(volatilityModel$sigma))[1:ncol(volatilityModel$sigma)]
#     if(length(driftModel$driftVol) != ncol(volatilityModel$sigma)) driftModel$driftVol <- rep(driftModel$driftVol, ncol(volatilityModel$sigma))[1:ncol(volatilityModel$sigma)]
#   }
#   
#   if(!is.numeric(driftModel$drift) | (length(driftModel$drift) != 1 & length(driftModel$drift) != nSeries) & length(driftModel$drift) != ncol(volatilityModel$sigma)){
#     stop("drift must be a numeric with length equal to 1 or equal to nSeries, or ncol(sigma) ")
#   }
#   ######## Drift model checking ends
#   
#   
#   ######## Jump model checking starts
#   jm <- match(names(jumpModel), c("modelType", "jumpComponent", "jumpTime"))
#   if(any(is.na(jm))){
#     idx <- which(is.na(jm))
#     enx <- NULL
#     for (i in 1:length(idx)) enx <- c(enx, names(jumpModel)[idx[i]])
#     warning(paste(c("unidentified option(s) in jumpModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
#   }
#   
#   
#   # We set the includeJumps tag to TRUE if we should actually include jumps 
#   # And we set it FALSE if we should not.
#   if(jumpModel$modelType != "none"){
#     jumpModel$includeJumps <- TRUE
#   } else if(jumpModel$modelType == "none"){
#     jumpModel$includeJumps <- FALSE
#   }
#   
#   
#   if(is.null(jumpModel$modelType)) jumpModel$modelType <- "none"
#   if(is.null(jumpModel$jumpComponent)) jumpModel$jumpComponent <- 1/5
#   if(is.null(jumpModel$jumpTime)) jumpModel$jumpTime <- c(15/32, 17/32)
#   
#   # Check whether the jump component makes sense.
#   if(jumpModel$includeJumps && jumpModel$jumpComponent >= 1){
#     stop("Jump component equal to or greater than 1, this is not allowed. Jump component should be between 0 and 1", call. = FALSE, domain=NULL)
#   }
#   
#   if(jumpModel$includeJumps && jumpModel$jumpComponent <= 0){
#     stop("Jump component equal to or less than 0, this is not allowed. Jump component should be between 0 and 1", call. = FALSE, domain=NULL)
#   }
#   
#   if(jumpModel$includeJumps && length(jumpModel$jumpTime) != 2){
#     stop("jumpTime must be of length 2", call.=FALSE, domain = NULL)
#   }
#   
#   if(jumpModel$includeJumps && jumpModel$jumpTime[1] > jumpModel$jumpTime[2]){
#     stop("First entry of jumpTime must be lower than second entry of jumpTime", call.=FALSE, domain=NULL)
#   }
#   
#   if(jumpModel$includeJumps && (min(jumpModel$jumpTime)<0 | max(jumpModel$jumpTime)>1)){
#     stop("jumpTime must be between 0 and 1", call. = FALSE, domain=NULL)
#   }
#   
#   ######## Jump model checking ends
#   
#   
#   
#   ######### Burst model checking starts
#   
#   bm <- match(names(burstModel), c("driftModel", "volModel"))
#   if(any(is.na(bm))){
#     idx <- which(is.na(bm))
#     enx <- NULL
#     for (i in 1:length(idx)) enx <- c(enx, names(burstModel)[idx[i]])
#     warning(paste(c("unidentified option(s) in burstModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
#   }
#   
#   if(is.null(burstModel$driftModel$modelType)) burstModel$driftModel$modelType = "none"
#   if(is.null(burstModel$volModel$modelType)) burstModel$volModel$modelType = "none"
#   
#   validBurstModels <- listAvailableBurstModels()[,1]
#   if(!(burstModel$driftModel$modelType %in% validBurstModels[c(1,3)])){
#     stop("burst model for the drift component's model type specified does not appear in valid model types. See listAvailableBurstModels() for valid types.\n", call.=FALSE)
#   }
#   if(!(burstModel$volModel$modelType %in% validBurstModels)){
#     stop("burst model for the volatility component's model type specified does not appear in valid model types. See listAvailableBurstModels() for valid types.\n", call.=FALSE)
#   }
#   
#   if(burstModel$driftModel$modelType == "singularityBurst" && is.null(burstModel$driftModel)) burstModel$driftModel <- list(alpha = 0.6, a = 0.002,
#                                                                                                                  burstInterval = c(15/32, 17/32), modelType = "singularityBurst")
#   if(burstModel$driftModel$modelType == "singularityBurst" && !is.null(burstModel$driftModel)){
#     if(is.null(burstModel$driftModel$alpha)) burstModel$driftModel$alpha <- 0.6
#     if(length(burstModel$driftModel$alpha) != 1){
#       stop("the alpha scalar in the driftModel of the burstModel must be of length 1", call.=FALSE, domain = NULL)
#     }
#     if(is.null(burstModel$driftModel$a)) burstModel$driftModel$a <- 1
#     if(length(burstModel$driftModel$a) != 1){
#       stop("the a scalar in the driftModel of the burstModel must be of length 1", call.=FALSE, domain = NULL)
#     }
#     if(is.null(burstModel$driftModel$burstInterval)) burstModel$driftModel$burstInterval <- c(15/32, 17/32)
#     if(length(burstModel$driftModel$burstInterval) != 2){
#       stop("burstInterval must be of length 2", call.=FALSE, domain = NULL)
#     }
#   }
#   
#   
#   
#   # We have received a burst model, which we must check
#   if(burstModel$volModel$modelType == "constantBurst"){
#     if(is.null(burstModel$volModel$burstMultiplier)) burstModel$volModel$burstMultiplier <- 3
#     if(length(burstModel$volModel$burstMultiplier) != 1){
#       stop("the burstMultiplier scalar in the volModel of the burstModel must be of length 1", call.=FALSE, domain = NULL)
#     }
#     if(is.null(burstModel$volModel$burstInterval)) burstModel$volModel$burstInterval <- c(15/32, 17/32)
#     if(length(burstModel$volModel$burstInterval) != 2){
#       stop("burstInterval must be of length 2", call.=FALSE, domain = NULL)
#     }
#   }
#   
#   
#   if(burstModel$volModel$modelType == "singularityBurst"){
#     if(is.null(burstModel$volModel$b)) burstModel$volModel$b <- 1
#     if(length(burstModel$volModel$b) != 1){
#       stop("the b scalar in the volModel of the burstModel must be of length 1", call.=FALSE, domain = NULL)
#     }
#     if(is.null(burstModel$volModel$beta)) burstModel$volModel$beta <- 0.4
#     if(length(burstModel$volModel$beta) != 1){
#       stop("the beta scalar in the volModel of the burstModel must be of length 1", call.=FALSE, domain = NULL)
#     }
#     if(is.null(burstModel$volModel$burstInterval)) burstModel$volModel$burstInterval <- c(15/32, 17/32)
#     if(length(burstModel$volModel$burstInterval) != 2){
#       stop("burstInterval must be of length 2", call.=FALSE, domain = NULL)
#     }
#   }
#   
#   
#   if(burstModel$driftModel$modelType == "singularityBurst" && burstModel$volModel$modelType == "singularityBurst"){
#     if(burstModel$volModel$beta < 0 | burstModel$volModel$beta > 1/2 | burstModel$driftModel$alpha - burstModel$volModel$beta > 1/2){
#       warning("The singularity burst model set up does not ensure absence of arbitrage. The conditions required are: 0 < beta < 1/2 and alpha - beta < 1/2.")
#     }
#     if(!all(burstModel$driftModel$burstInterval == burstModel$volModel$burstInterval)){
#       warning("The singularity burst interval of the drift does not match the singularity burst of the volatility.
#                Typically the drift and volatility co-explode.")
#     }
#   }
#   
#   
#   
#   ######## diurnal model checking starts
#   dm <- match(names(diurnalModel), c("modelType","C", "A", "B", "a", "b"))
#   if(any(is.na(dm))){
#     idx <- which(is.na(dm))
#     enx <- NULL
#     for (i in 1:length(idx)) enx <- c(enx, names(diurnalModel)[idx[i]])
#     warning(paste(c("unidentified option(s) in diurnalModel:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
#   }
#   
#   if(is.null(diurnalModel$modelType)) diurnalModel$modelType = "none"
#   
#   validDiurnalModels <- listAvailableDiurnalModels()[,1]
#   if(!(diurnalModel$modelType %in% validDiurnalModels)){
#     stop("diurnal model type specified does not appear in valid model types. See listAvailableDiurnalModels() for valid types.\n", call.=FALSE)
#   }
#   
#   # We have received a diurnal model, which we must check
#   if(diurnalModel$modelType == "revJ"){
#     if(is.null(diurnalModel$C)) diurnalModel$C <- 0.88929198
#     if(is.null(diurnalModel$A)) diurnalModel$A <- 0.75
#     if(is.null(diurnalModel$B)) diurnalModel$B <- 0.25
#     if(is.null(diurnalModel$a)) diurnalModel$a <- 10
#     if(is.null(diurnalModel$b)) diurnalModel$b <- 10
#   }
#   
#   
#   ######## time settings checking starts
#   
#   ts <- match(names(timeSettings), c("tradingStart", "tradingEnd", "origin", "sampling"))
#   if(any(is.na(ts))){
#     idx <- which(is.na(ts))
#     enx <- NULL
#     for (i in 1:length(idx)) enx <- c(enx, names(timeSettings)[idx[i]])
#     warning(paste(c("unidentified option(s) in timeSettings:\n", enx), sep="", collapse= " "), call. = FALSE, domain=NULL)
#   }
#   
#   if(is.null(timeSettings$tradingStart)) timeSettings$tradingStart <- 34200
#   if(length(timeSettings$tradingStart) != 1 | !is.numeric(timeSettings$tradingStart)){
#     stop("the tradingStart argument must be a numeric of length 1", call.=FALSE, domain = NULL)
#   }
#   
#   if(is.null(timeSettings$tradingEnd)) timeSettings$tradingEnd <- 57600
#   if(length(timeSettings$tradingEnd) != 1 | !is.numeric(timeSettings$tradingEnd)){
#     stop("the tradingEnd argument must be a numeric of length 1", call.=FALSE, domain = NULL)
#   }
#   
#   if(is.null(timeSettings$origin)) timeSettings$origin <- "1970-01-01"
#   if(length(timeSettings$origin) != 1){
#     stop("the origin argument must be of length 1", call.=FALSE, domain = NULL)
#   }
#   timeSettings$origin <- try(as.Date(timeSettings$origin), silent = TRUE)
#   if (inherits(timeSettings$origin, "try-error")) {
#     stop("origin must be coercible to a date")
#   }
#   timeSettings$origin <- as.character(timeSettings$origin)
#   
#   
#   if(is.null(timeSettings$sampling)) timeSettings$sampling <- "equidistant"
#   
#   
#   if(timeSettings$tradingEnd < timeSettings$tradingStart){
#     stop("Start of trading must be before end of trading")
#   }
#   
#   if(!is.numeric(nSeries) | nSeries <= 0 | nSeries %% 1 != 0){
#     stop("nSeries must be a positive integer", call. = FALSE, domain = NULL)
#   }
#   
#   if(!is.numeric(nDays) | nDays <= 0 | nDays %% 1 != 0){
#     stop("nDays must be a positive integer", call. = FALSE, domain = NULL)
#   }
#   
#   if(!is.numeric(nObs) | nObs <= 0 | nObs %% 1 != 0){
#     stop("nObs must be a positive integer", call. = FALSE, domain = NULL)
#   }
#   
#   if(!is.logical(discretize)){
#     stop("discretize must a logical", call. = FALSE, domain = NULL)
#   }
#   
#   
#   
#   
#   print("finished checking timeSettings?")
#   
#   print("Implement checks on noise model")
#   
#   simSpec <- list(volatilityModel = volatilityModel, driftModel = driftModel, jumpModel = jumpModel, burstModel = burstModel, diurnalModel = diurnalModel,
#                   noiseModel = noiseModel, nSeries = nSeries, nDays = nDays, nObs = nObs, discretize = discretize, timeSettings = timeSettings)
#   
#   # Should probably be an S4 class, but it's bad practice to mix and match S3 and S4 classes AFAIK
#   class(simSpec) <- "highfrequencySimSpec"
#   
#   return(simSpec)
#   
# }
# 
# 
# #' List the available drift models for simulations
# #' @export
# #' @author Emil Sjoerup
# #' @return This function returns the available drift models in a matrix
# listAvailableDriftModels <- function(){
#   models <- matrix(
#     c("constant", "constant drift",
#       "vasicek", "vasicek model"
#       
#       ), ncol = 2, byrow=TRUE
#   )
#   
#   colnames(models) <- c("Abbreviation", "Description")
#   return(models)
# }
# 
# 
# #' List the available burst models for simulations
# #' @export
# #' @author Emil Sjoerup
# #' @return This function returns the available burst models in a matrix
# listAvailableBurstModels <- function(){
#   models <- matrix(
#     c("none", "no burst",
#       "constantBurst", "Piecewise constant volatility with a constant burst of volatility in a pre-defined interval (FoF) (Only available for volatility)",
#       "singularityBurst", "Burst that approaches a singularity as in CRO(2018)"), ncol = 2, byrow=TRUE
#   )
#   
#   colnames(models) <- c("Abbreviation", "Description")
#   return(models)
# }
# 
# 
# #' List the available diurnal models for simulations
# #' @export
# #' @author Emil Sjoerup
# #' @return This function returns the available diurnal models in a matrix
# listAvailableDiurnalModels <- function(){
#   models <- matrix(
#     c(
#       "none", "no diurnality",
#       "revJ", "reverse J shaped diurnality (FORM: C + A * exp(-a * t) + B * exp(-b * (1 - t)) )"
#     ), ncol = 2, byrow = TRUE
#   )
# }
# 
# #' List the available noise models for simulations
# #' @export
# #' @author Emil Sjoerup
# #' @return This function returns the available noise models in a matrix
# listAvailableNoiseModels <- function(){
#   models <- matrix(
#     c(
#       "none", "no noise",
#       "additiveGaussian", "additive Gaussian noise term",
#       "ratio", "additive Gaussian noise with constant signal to noise ratio"
#     ), ncol = 2, byrow = TRUE
#   )
# }
# 
# 
# #' List the available volatility models for simulations
# #' @export
# #' @author Emil Sjoerup
# #' @return This function returns the available volatility models in a matrix
# listAvailableVolatilityModels <- function(){
#   
#   models <- matrix(
#     c("constant", "constant volatility",
#       "heston", "heston stochastic volatility model",
#       "huangTauchen", "two factor stochastic volatility model of Huang and Tauchen (2005)"), ncol = 2, byrow=TRUE
#   )
#   
#   colnames(models) <- c("Abbreviation", "Description")
#   
#   return(models)
# }
# 
# #' List the available jump models for simulations
# #' @export
# #' @author Emil Sjoerup
# #' @return This function returns the available Jump models in a matrix
# listAvailableJumpModels <- function(){
#   models <- matrix(
#     c("none", "No jumps",
#       "PA", "Single daily pre announced jump"), ncol = 2, byrow=TRUE
#   )
#   
#   colnames(models) <- c("Abbreviation", "Description")
#   return(models)
# }
