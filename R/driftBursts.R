#calculating drift burst test statistic
#' dat <- sampleTData$PRICE
#' storage.mode(dat) <- "numeric"
#' DBH = driftBursts(NULL, log(dat),
#'                   testTimes = seq(34500, 57600, 60), preAverage = 5, ACLag = -1L,
#'                   meanBandwidth = 300L, varianceBandwidth = 900L,
#'                   parallelize = FALSE)
#' 
#' 
#' #plot test statistic
#' plot(DBH)
# #plot both test statistic and price
# plot(DBH, price = y, timestamps = timestamps)
# #Plot the mu series
# plot(DBH, which = "Mu")
# #plot the sigma series
# plot(DBH, which = "Sigma")
# #plot both
# plot(DBH, which = c("Mu", "Sigma"))
#  ################## same example with xts object:
#  library("xts")
#  #Set parameter values of the simulation
#  iT = 66500; dSigma = 0.3; dPhi = 0.98; dMu = -10;
#  #set seed for reproducibility
#  set.seed(123)
#  #Simulate the series
#  y = 500+cumsum(flashCrashSim(iT, dSigma, dPhi, dMu))
#
#  #insert an outlier to illustrate robustness.
#  y[50000] = 500
#
#  #Here, the observations are equidistant, but the code can handle unevenly spaced observations.
#  timestamps = seq(34200 , 57600 , length.out = iT)
#  startTime = strptime("1970-01-01 00:00:00.0000", "\%Y-\%m-\%d \%H:\%M:\%OS", tz = "GMT")
#  testTimes = seq(34260, 57600, 60)
#
#
#  price = xts(vY, startTime + timestamps)
#
#
#  DBHxts = driftBursts(timestamps = NULL,  log(price),
#                       testTimes, preAverage = 5, ACLag = -1L,
#                       meanBandwidth = 300L, varianceBandwidth = 900L,
#                       parallelize = FALSE)
#
#  #check for equality
#  all.equal(as.numeric(getDB(DBH)), as.numeric(getDB(DBHxts)))
#


#' driftBursts
#'   Drift Bursts
#' @description Calculates the Test-Statistic for the Drift Burst Hypothesis
#' 
#' @param timestamps Either: A \code{numeric} of \code{timestamps} for the trades in seconds after midnight.  
#' Or: \code{NULL}, when the time argument is \code{NULL}, the \code{logprices} argument must be an \code{xts} object.
#' @param logPrices A \code{numeric} or \code{xts} object containing the log-prices.
#' @param testTimes A \code{numeric} containing the times at which to calculate the tests. The standard of \code{seq(34260, 57600, 60)} 
#' denotes calculating the test-statistic once per minute, i.e. 390 times for a typical 6.5 hour trading day from 9:31:00 to 16:00:00. See details. Default is seq(34260, 57600, 60.
#' @param preAverage An \code{integer} denoting the length of pre-averaging window for the log-prices. Default is 5
#' @param ACLag A positive \code{integer} greater than 1 denoting how many lags are to be used for the HAC estimator of the variance - the default
#' of \code{-1} denotes using an automatic lag selection algorithm for each iteration. Default is -1L
#' @param meanBandwidth An \code{integer} denoting the bandwidth for the left-sided exponential kernel for the mean. Default is 300L
#' @param varianceBandwidth An \code{integer} denoting the bandwidth for the left-sided exponential kernel for the variance. Default is 900L
#' @param sessionStart A \code{double} to denote the start of the trading session in seconds after midnight. Default is 34200
#' @param sessionEnd A \code{double} to denote the end of the trading session in seconds after midnight. Default is 57600
#' @param parallelize A \code{logical} to determine whether to parallelize the underlying C++ code (Using OpenMP). Default is FALSE
#' @param nCores An \code{integer} denoting the number of cores to use for calculating the code when paralellized. 
#' If this argument is not provided, sequential evaluation will be used even though \code{parallelize} is TRUE. Default is NA
#' @param warnings A \code{logical} denoting whether warnings should be shown. Default is TRUE
#' @details 
#' If the \code{testTimes} vector contains instructions to test before the first trade, or more than 15 minutes after the last trade, these entries will be deleted, as not doing so may cause crashes.
#' The test statistic is unstable before \code{max(meanBandwidth , varianceBandwidth)} seconds has passed.
#' If \code{timestamps} is provided and \code{logPrices} is an \code{xts} object, the indices of logPrices will be used regardless.
#' Note that using an \code{xts} logPrices argument is slower than using a \code{numeric} due to the creation of the timestamps from the index of the input. 
#' When using \code{xts} objects, be careful to use the correct time zones. For example, if I as a dane use the \code{"America/New_York"} time zone for my \code{xts} objects, I have to add 14400 to my testing times.
#' Same correction will have to be made to the \code{startTime} and \code{endTime} arguments in the plotting methods.
#' The lags from the Newey-West algorithm is increased by \code{2 * (preAveage-1)} due to the pre-averaging we know at least this many lags should be corrected for.
#' The maximum of 20 lags is also increased by this factor for the same reason.
#' @return An object of class \code{DBH} and \code{list} containing the series of the drift burst hypothesis test-statistic as well as the estimated spot drift and variance series. 
#' The list also contains some information such as the variance and mean bandwidths along with the pre-averaging setting and the amount of observations. 
#' Additionally, the list will contain information on whether testing happened for all \code{testTimes} entries.
#' 
#' @author Emil Sjoerup
#' @export
driftBursts <- function(timestamps = NULL, logPrices, testTimes = seq(34260, 57600, 60),
                       preAverage = 5, ACLag = -1L, meanBandwidth = 300L, 
                       varianceBandwidth = 900L, sessionStart = 34200, sessionEnd = 57600,
                       parallelize = FALSE, nCores = NA, warnings = TRUE){
  
  #########  Initialization  ############
  k                     <- preAverage 
  vDiff                 <- diff(logPrices)
  iT                    <- length(logPrices)
  xts                   <- FALSE
  pad <- removedFromEnd <- 0
  tt                    <- testTimes #tt is returned in the Info list. 
  #########  init end  ############
  
  ###Checks###
  if (meanBandwidth<0 | meanBandwidth %% 1 != 0) {
    stop("meanBandwidth must be a positive integer")
  }
  if(varianceBandwidth<0 | varianceBandwidth %% 1 != 0){
    stop("varianceBandwidth must be a positive integer")
  }
  if(ACLag !=-1 && ACLag%%1!=0 | -1>ACLag | ACLag == 1){
    stop("ACLag must be a positive integer greater than 1, or -1. \n
         The standard of -1 designates usage of an automated lag selection algorithm.")
    #Specifically Newey-West 1994
  }
  if(preAverage <=0 | preAverage%%1!=0 ){
    stop("preAverage must be a positive integer. No preaveraging is done when preAverage = 1.")
  }
  if(inherits(logPrices, "xts")){
    tz         <- tzone(logPrices)
    timestamps <- index(logPrices)
    timestamps <- as.numeric(timestamps) - (.indexDate(logPrices[1]) * 86400)
    vIndex     <- as.POSIXct((.indexDate(logPrices)[1] * 86400) + testTimes, origin = "1970-01-01", tz = tz)
    logPrices  <- as.numeric(logPrices)
    vDiff      <- as.numeric(vDiff)[-1] ### need to remove first entry because diff() on an xts object produces NA in first entry.
    xts        <- TRUE
  }
  if((anyNA(timestamps) & !is.null(timestamps)) | anyNA(logPrices) | anyNA(testTimes)){
    stop("NA's in timestamps, logPrices or testTimes - might cause crashes and are thus disallowed")
  }
  if((length(timestamps) != length(logPrices) & !is.null(timestamps))){
    stop("timestamps and logPrices input not of same length, to prevent crashing this is not allowed.")
  }
  if((is.na(nCores) | nCores %% 1 != 0) & parallelize){
    if(warnings){
      warning("No iCores argument was provided, or the provided nCores argument is not an integer.\n
             Sequential evaluation is used.")
    }
    parallelize <- FALSE
  }
  if(min(timestamps) + 5 >= min(testTimes)){ ## Safeguard to prevent faulty inputs that causes crashes!
    testTimes <- testTimes[-1]
    pad <- 1
    while(min(timestamps) + 5 >= min(testTimes)){
      testTimes <- testTimes[-1] 
      pad <- pad + 1
    }
    if(warnings){
      warning(paste("\nThe first testing timestamps is before any observations. This causes fatal errors (and crashes if paralellized).",
                    "\nItereatively removing first testing timestamps until this is no longer the case.",
                    "\nRemoved the first", pad, "entries from testTimes and replacing with a 0\n"))
    }
    
    if(length(testTimes) == 0){
      stop("No testing done, the mandated testing times were all too soon after market open. 
           A five second minimum wait is put in as a safe-guard to prevent inputs that may cause crashes.")
    }
    
    
  }
  if(max(testTimes)>max(timestamps) + 900){
    testTimes <- testTimes[-length(testTimes)]
    removedFromEnd <- 1
    while(max(testTimes) >= max(timestamps) + 900){
      testTimes <- testTimes[-length(testTimes)]  
      removedFromEnd <- removedFromEnd + 1
    }
    if(warnings){
      warning(paste("\nThe last testing timestamps is more than 15 minutes after the last trade, this may cause fatal errors or crashes.",
                    "\nIteratively removing the last testing timestamps until this is no longer the case.",
                    "\nRemoved the last", removedFromEnd, "entries from testTimes\n"))
      
    }
    
  }
  if(15 <= max(diff(timestamps))/60){
    stop("There is a period of greater than 15 minutes with no trades.\n
          This causes fatal errors (and crashes if paralellized) and is thus disallowed")
  }
  ###Checks end###
  vpreAveraged <- rep(0, iT - 1)
  vpreAveraged[(k*2 - 1):(iT - 1)] <- cfilter(x = logPrices, c(rep(1,k),rep( -1,k)))[k:(iT - k)]
  
  if(parallelize & !is.na(nCores)){ #Parallel evaluation or not?
    lDriftBursts <- DriftBurstLoopCPAR(vpreAveraged, vDiff, timestamps, testTimes, meanBandwidth, 
                                      varianceBandwidth, preAverage, ACLag, nCores)
  }else{
    lDriftBursts <- DriftBurstLoopC(vpreAveraged, vDiff, timestamps, testTimes, meanBandwidth, 
                                   varianceBandwidth, preAverage, ACLag)  
  }
  
  
  
  if(pad != 0 | removedFromEnd != 0){
    lDriftBursts[["driftBursts"]] <- c(rep(0,pad), lDriftBursts[["driftBursts"]], rep(0,removedFromEnd))
    lDriftBursts[["sigma"]]       <- c(rep(0,pad), lDriftBursts[["sigma"]], rep(0,removedFromEnd))
    lDriftBursts[["mu"]]          <- c(rep(0,pad), lDriftBursts[["mu"]], rep(0,removedFromEnd))
  }
  
  if(xts){
    lDriftBursts[["driftBursts"]] <- xts(lDriftBursts[["driftBursts"]], order.by = vIndex, tz = tz)
    lDriftBursts[["sigma"]]       <- xts(lDriftBursts[["sigma"]], order.by = vIndex, tz = tz)
    lDriftBursts[["mu"]]          <- xts(lDriftBursts[["mu"]], order.by = vIndex, tz = tz)
  }
  
  lInfo = list("varianceBandwidth" = varianceBandwidth, "meanBandwidth" = meanBandwidth,"preAverage" = preAverage,
               "nObs" = iT, "testTimes" = tt, "padding" = c(pad, removedFromEnd), "sessionStart" = sessionStart, "sessionEnd" = sessionEnd)
  lDriftBursts[["info"]] = lInfo
  #replace NANs with 0's
  NANS = is.nan(lDriftBursts[["sigma"]])
  lDriftBursts[["driftBursts"]][NANS] <- 0
  lDriftBursts[["sigma"]][NANS]       <- 0
  
  class(lDriftBursts) = c("DBH", "list")
  return(lDriftBursts)
}


# #
# # #' @export
# plot.DBH = function(x, ...){
#   
#   #### Get extra passed options and data
#   options = list(...)
#   #### List of standard options
#   opt = list(which = "driftbursts", price = NULL, timestamps = NULL, 
#              startTime = ifelse(is.null(x$info[['sessionStart']]), min(x$info[['testTimes']]), x$info[['sessionStart']]), 
#              endTime = ifelse(is.null(x$info[['sessionEnd']]), max(x$info[['testTimes']]), x$info[['sessionEnd']]),
#              leg.x = "topleft", leg.y = NULL,  tz = "GMT", annualize = FALSE, nDays = 252, legend.txt = "")
#   #### Override standard options where user passed new options
#   opt[names(options)] = options
#   
#   #### Extract options (better way to do it?) 
#   which      = tolower(opt$which)
#   startTime  = opt$startTime
#   endTime    = opt$endTime
#   main       = opt$main
#   tz         = opt$tz
#   leg.x      = opt$leg.x
#   leg.y      = opt$leg.y
#   annualize  = opt$annualize
#   nDays      = opt$nDays
#   price      = opt$price
#   timestamps = opt$timestamps
#   tstat      = x$db
#   sigma      = x$sigma
#   mu         = x$mu
#   startpar   = par(no.readonly = TRUE)
#   testTimes  = x$info$testTimes
#   horizLines = seq(round(min(tstat)), round(max(tstat)), 1)
#   ###Setup done
#   if(!all(which %in% c("driftbursts", "mu", "sigma", "db"))){
#     stop("The which argument must be a character vector containing either:\n
#          Sigma, Mu, both of these or DriftBursts. 
#          CasE doesn't matter.")
#   }
#   if(inherits(tstat, "xts")){
#     tstat     = as.numeric(tstat)
#     sigma     = as.numeric(sigma)
#     mu        = as.numeric(mu)
#     if(!is.null(price)){
#       tz          = tzone(price)
#       timestamps  = index(price, tz = tz)
#       timestamps  = as.numeric(timestamps) - (.indexDate(price)[1] * 86400)
#       price       = as.numeric(price)
#     }
#   }
#   if(testTimes[1] == startTime){
#     testTimes = testTimes[-1]
#     sigma = sigma[-1]
#     mu = mu[-1]
#     tstat=tstat[-1]
#   }
#   if(min(testTimes) < startTime | max(testTimes) > endTime){
#     cat('\nTesting was tried before sessionStart or after sessionEnd, thus some of the tests may be cut off from the plot.
#         \nIf the plot looks weird, consider changing sessionStart and sessionEnd. 
#         \nThese should reflect the start of trading and the end of trading respectively')
#   }
#   xtext = as.POSIXct(testTimes, origin = "1970-01-01", tz = tz)
#   xlim = c(startTime, endTime)
#   xlab = "Time"
#   
#   if(all(which %in% c("driftbursts", "db"))){ #use all() because this function should accept which arguments with length longer than 1
#     par(mar = c(4,3.5,2,1.25), mgp = c(2,1,0))
#     if(!is.null(price)) par(mar = c(4,3.5,4,4), mgp = c(2,1,0)) #makes room for values on the right y-axis
#     main = "Drift Bursts test statistic"
#     ylab = "test-statistic"
#     plot(tstat, x = xtext, type = "l", xaxt = 'n', ylab = ylab, main = main, xlab = xlab, xlim = xlim)
#     axis.POSIXct(side  = 1, at = seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 7))
#     abline(h = horizLines, col = "grey" , lty = 3, cex = 0.1)
#     legend.txt = "t-stat"
#     if(!is.null(price)){
#       if(is.null(timestamps)){
#         stop("The timestamps of the price must be passed in the timestamps argument")
#       }
#       par(new = TRUE)
#       plot(price, x = timestamps , type = "l", axes = FALSE, col = "red", xlab = "", ylab = "", lty = 2, xlim = xlim)  
#       axis(4)
#       mtext(side = 4, text = "price", line = 2.5)
#       legend.txt = c(legend.txt, "price")
#       legend(x = leg.x, leg.y, legend = legend.txt, lty = c(1,2), col = c(1,2), bg = rgb(0,0,0,0), box.lwd = 0, 
#              box.col = rgb(0,0,0,0))
#     }
#   }
#   if(all(which == "sigma")){ #use all() because this function should accept which arguments with length longer than 1
#     main = "volatility"
#     ylab = "local volatility"
#     par(mar = c(4,3.5,2,1.25), mgp = c(2,1,0))
#     plot(sigma, x = xtext, type = "l",  xaxt = 'n', ylab = ylab, main = main, xlab = xlab)  
#     axis.POSIXct(side  = 1, at = seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 7))
#   }
#   if(all(which == "mu")){ #use all() because this function should accept which arguments with length longer than 1
#     main = "drift"
#     ylab = "drift"
#     if(annualize){ 
#       ylab = paste("annualized", ylab)
#     }
#     par(mar = c(4,3.5,2,1.25), mgp = c(2,1,0))
#     plot(mu, x = xtext, type = "l",  xaxt = 'n', ylab = ylab, main = main, xlab = xlab)
#     axis.POSIXct(side  = 1, at = seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 7))
#     abline(h = 0, col = "grey" , lty = 3)
#   }
#   if("mu" %in% which & "sigma" %in% which){
#     par(mfrow = c(2,1), omi = c(0,0,0,0), mgp = c(2,1,0), mai = c(0.75,0.75,0.3,0.25))
#     main = "drift"
#     ylab = "drift"
#     if(annualize){ 
#       ylab = paste("annualized", ylab)
#     }
#     plot(mu, x = xtext, type = "l", xlab = "",  xaxt = 'n', ylab = ylab, main = main)
#     axis.POSIXct(side  = 1, at = seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 7))
#     abline(h = 0, col = "grey" , lty = 3)
#     main = "volatility"
#     ylab = "volatility"
#     if(annualize){ 
#       ylab = paste("annualized", ylab)
#     }
#     plot(sigma, x = xtext, type = "l", xlab = "", xaxt = 'n', ylab = ylab, main = main) 
#     axis.POSIXct(side  = 1, at = seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 7))
#   }
#   par(startpar)
# }
# 
# # print.DBH = function(x, ...){
# #   usePolynomialInterpolation = TRUE
# #   options = list(...)
# #   if('criticalValue' %in% names(options)){
# #     usePolynomialInterpolation = FALSE
# #   }
# #   #### List of standard options
# #   opt = list(alpha = 0.95)
# #   #### Override standard options where user passed new options
# #   opt[names(options)] = options
# #   if(usePolynomialInterpolation){
# #     alpha = opt$alpha
# #     criticalValue = getCriticalValues(x, alpha)$quantile  
# #   }else{
# #     criticalValue = opt$criticalValue
# #   }
# #   
# #   varDB = var(x$db)
# #   padding = x$info$padding
# #   #We always remove the first entry, as this is used to denote the start of trading.
# #   whichToInclude = seq(padding[1] + 1, length(x$info$testTimes)- padding[2]) 
# #   cat("\n-------------Drift Burst Hypothesis------------\n")
# #   cat("Tests performed:                     ", length(whichToInclude))
# #   #browser()
# #   if(usePolynomialInterpolation){
# #     cat("\nAny drift bursts (|T| > ",paste0(round(criticalValue,3)),"):    ", ifelse(any(abs(getDB(x))>criticalValue) , 'yes', 'no'))
# #   }else{
# #     cat("\nAny drift bursts (|T| > ",paste0(criticalValue[1]),"):        ", ifelse(any(abs(getDB(x))>criticalValue) , 'yes', 'no'))
# #   }
# #   cat("\nMax absolute value of test statistic:", round(max(abs(getDB(x))), digits=5))
# #   cat("\nMean test statistic:                 ", round(allMeans$meanDB, digits = 5))
# #   cat("\nVariance of test statistic:          ", round(varDB, digits = 5))
# #   cat("\n-----------------------------------------------\n")
# # }
# # 
# # getCriticalValues = function(x, alpha = 0.95){
# #   UseMethod('getCriticalValues', x)
# # }
# # 
# # getCriticalValues.DBH = function(x, alpha = 0.95){
# #   return(DBHCriticalValues(x, alpha))
# # }