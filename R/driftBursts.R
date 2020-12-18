#' driftBursts
#'   Drift Bursts
#' @description Calculates the Test-Statistic for the Drift Burst Hypothesis
#'  
#' @param pData Either a \code{data.table} or an \code{xts} object. If pData is a data.table, columns DT and PRICE must be present, containing timestamps of the trades and the price of the 
#' trades (in levels) respectively. If pData is an \code{xts} object and the number of columns is greater than one, PRICE must be present.
#' @param testTimes A \code{numeric} containing the times at which to calculate the tests. The standard of \code{seq(34260, 57600, 60)} 
#' denotes calculating the test-statistic once per minute, i.e. 390 times for a typical 6.5 hour trading day from 9:31:00 to 16:00:00. See details.
#' Additionally, \code{testTimes} can be set to 'all' where the test statistic will be calculated on each tick more than 5 seconds after opening
#' @param preAverage A positive \code{integer} denoting the length of pre-averaging window for the log-prices. Default is 5
#' @param ACLag A positive \code{integer} greater than 1 denoting how many lags are to be used for the HAC estimator of the variance - the default
#' of \code{-1} denotes using an automatic lag selection algorithm for each iteration. Default is -1L
#' @param meanBandwidth An \code{integer} denoting the bandwidth for the left-sided exponential kernel for the mean. Default is 300L
#' @param varianceBandwidth An \code{integer} denoting the bandwidth for the left-sided exponential kernel for the variance. Default is 900L
#' @param parallelize A \code{logical} to determine whether to parallelize the underlying C++ code (Using OpenMP). Default is FALSE
#' @param nCores An \code{integer} denoting the number of cores to use for calculating the code when parallelized. 
#' If this argument is not provided, sequential evaluation will be used even though \code{parallelize} is TRUE. Default is NA
#' @param warnings A \code{logical} denoting whether warnings should be shown. Default is TRUE
#' 
#' @details 
#' If the \code{testTimes} vector contains instructions to test before the first trade, or more than 15 minutes after the last trade, these entries will be deleted, as not doing so may cause crashes.
#' The test statistic is unstable before \code{max(meanBandwidth , varianceBandwidth)} seconds has passed.
#' The lags from the Newey-West algorithm is increased by \code{2 * (preAveage-1)} due to the pre-averaging we know at least this many lags should be corrected for.
#' The maximum of 20 lags is also increased by this factor for the same reason.
#' @return An object of class \code{DBH} and \code{list} containing the series of the drift burst hypothesis test-statistic as well as the estimated spot drift and variance series. 
#' The list also contains some information such as the variance and mean bandwidths along with the pre-averaging setting and the amount of observations. 
#' Additionally, the list will contain information on whether testing happened for all \code{testTimes} entries.
#' Objects of class \code{DBH} has the methods \code{\link{print.DBH}}, \code{\link{plot.DBH}}, and \code{\link{getCriticalValues.DBH}} which prints, plots, and
#' retrieves critical values for the test described in appendix B 
#' 
#' @examples 
#' # Usage with data.table object
#' dat <- sampleTData[as.Date(DT) == "2018-01-02"]
#' # Testing every 60 seconds after 09:45:00
#' DBH1 <- driftBursts(dat, testTimes = seq(35100, 57600, 60), preAverage = 2, ACLag = -1L,
#'                     meanBandwidth = 300L, varianceBandwidth = 900L)
#' print(DBH1)
#' 
#' plot(DBH1, pData = dat)
#' # Usage with xts object (1 column)
#' library("xts")
#' dat <- xts(sampleTData[as.Date(DT) == "2018-01-03"]$PRICE, 
#'            order.by = sampleTData[as.Date(DT) == "2018-01-03"]$DT)
#' # Testing every 60 seconds after 09:45:00
#' DBH2 <- driftBursts(dat, testTimes = seq(35100, 57600, 60), preAverage = 2, ACLag = -1L,
#'                     meanBandwidth = 300L, varianceBandwidth = 900L)
#' plot(DBH2, pData = dat)
#' 
#' \dontrun{ 
#' # This block takes some time
#' dat <- xts(sampleTDataEurope$PRICE, 
#'            order.by = sampleTDataEurope$DT)
#' # Testing every 60 seconds after 09:00:00
#' system.time({DBH4 <- driftBursts(dat, testTimes = seq(32400 + 900, 63000, 60), preAverage = 2, 
#'              ACLag = -1L, meanBandwidth = 300L, varianceBandwidth = 900L)})
#'
#' system.time({DBH4 <- driftBursts(dat, testTimes = seq(32400 + 900, 63000, 60), preAverage = 2, 
#'                                  ACLag = -1L, meanBandwidth = 300L, varianceBandwidth = 900L,
#'                                  parallelize = TRUE, nCores = 8)})
#' plot(DBH4, pData = dat)
#' 
#' # The print method for DBH objects takes an argument alpha that determines the confidence level
#' # of the test performed
#' print(DBH4, alpha = 0.99)
#' # Additionally, criticalValue can be passed directly
#' print(DBH4, criticalValue = 3)
#' max(abs(DBH4$tStat)) > getCriticalValues(DBH4, 0.99)$quantile
#' }
#' @references 
#' Christensen, K., Oomen, R., and Reno, R. (2018) The drift burst hypothesis. Working paper.
#' 
#' @author Emil Sjoerup
#' @importFrom data.table is.data.table
#' @importFrom xts xts .indexDate
#' @export
driftBursts <- function(pData, testTimes = seq(34260, 57600, 60),
                       preAverage = 5, ACLag = -1L, meanBandwidth = 300L, 
                       varianceBandwidth = 900L, #sessionStart = 34200, sessionEnd = 57600,
                       parallelize = FALSE, nCores = NA, warnings = TRUE){
  PRICE <- DT <- NULL
  ###Checks###
  if (meanBandwidth < 0 | meanBandwidth %% 1 != 0) {
    stop("meanBandwidth must be a positive integer")
  }
  if(varianceBandwidth < 0 | varianceBandwidth %% 1 != 0){
    stop("varianceBandwidth must be a positive integer")
  }
  if(ACLag != -1 && ACLag %% 1 != 0 | -1 > ACLag | ACLag == 1){
    stop("ACLag must be a positive integer greater than 1, or -1. \n
         The standard of -1 designates usage of an automated lag selection algorithm.")
    #Specifically Newey-West 1994
  }
  if(preAverage <= 0 | preAverage %% 1 != 0 ){
    stop("preAverage must be a positive integer. No preaveraging is done when preAverage = 1.")
  }
  
  
  ## Initialization
  wasXTS <- FALSE
  # Data table case
  if(is.data.table(pData)){
    
    if(!all(c("DT", "PRICE") %in% colnames(pData))){
      stop("pData must have columns DT and PRICE")
    }
    
    timeZone <- format(pData$DT[1], format = "%Z")
    if(is.null(timeZone) || timeZone == ""){
      tz <- "UTC"
      pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
    } else {
      tz <- timeZone
    }
    
    timestamps <- as.numeric(pData$DT, tz = tz) - as.numeric(as.POSIXct(as.Date(pData$DT[1], tz = tz), format = "%Y-%m-%d", tz = tz), tz = tz)
    logPrices <- log(pData$PRICE)
    
  } else if (is.xts(pData)){ ##xts case
    tz         <- tzone(pData)
    timestamps <- index(pData)
    timestamps <- as.numeric(timestamps, tz = tz) - as.numeric(as.POSIXct(paste0(as.Date(timestamps[1], tz = tz)), format = "%Y-%m-%d", tz = tz), tz = tz)
    vIndex     <- as.POSIXct((.indexDate(pData[1,]) * 86400) + testTimes, origin = "1970-01-01", tz = tz)
    if(ncol(pData) == 1){
      logPrices <- log(as.numeric(pData))
    } else {
      if(!("PRICE" %in% colnames(pData))){
        stop("pData is xts and has more than one column. Could not find PRICE column")
      }
      logPrices <- log(as.numeric(pData[,"PRICE"]))
    }
    vDiff <- diff(logPrices)
    wasXTS        <- TRUE
  } else {
    stop("pData must be either a data.table with atleast the columns DT and PRICE, or an xts object with 1 column (only price) or several with one called PRICE")
  }
  vDiff                 <- diff(logPrices)
  iT                    <- length(logPrices)
  pad <- removedFromEnd <- 0
  tt                    <- testTimes #tt is returned in the Info list. 
  
  #########  init end  ############

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
  
  if(testTimes[1] == 'all'){
    timestamps[timestamps > timestamps[1] + 5]
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
  vpreAveraged[(preAverage * 2 - 1):(iT - 1)] <- cfilter(x = logPrices, c(rep(1, preAverage),rep(-1, preAverage)))[preAverage:(iT - preAverage)]
  if(parallelize & !is.na(nCores)){ #Parallel evaluation or not?
    lDriftBursts <- DriftBurstLoopCPAR(vpreAveraged, vDiff, timestamps, testTimes, meanBandwidth,
                                       varianceBandwidth, preAverage, ACLag, nCores)
  }else{
    lDriftBursts <- DriftBurstLoopC(vpreAveraged, vDiff, timestamps, testTimes, meanBandwidth, 
                                    varianceBandwidth, preAverage, ACLag)  
  }
  
  
  
  if(pad != 0 | removedFromEnd != 0){
    lDriftBursts[["tStat"]] <- c(rep(0, pad), lDriftBursts[["tStat"]], rep(0,removedFromEnd))
    lDriftBursts[["sigma"]] <- c(rep(0, pad), lDriftBursts[["sigma"]], rep(0,removedFromEnd))
    lDriftBursts[["mu"]]    <- c(rep(0, pad), lDriftBursts[["mu"]], rep(0,removedFromEnd))
  }
  
  if(wasXTS){
    lDriftBursts[["tStat"]] <- xts(lDriftBursts[["tStat"]], order.by = vIndex, tzone = tz)
    lDriftBursts[["sigma"]] <- xts(lDriftBursts[["sigma"]], order.by = vIndex, tzone = tz)
    lDriftBursts[["mu"]]    <- xts(lDriftBursts[["mu"]], order.by = vIndex, tzone = tz)
  }
  
  lInfo = list("varianceBandwidth" = varianceBandwidth, "meanBandwidth" = meanBandwidth,"preAverage" = preAverage,
               "nObs" = iT, "testTimes" = tt, "padding" = c(pad, removedFromEnd))#, "sessionStart" = sessionStart, "sessionEnd" = sessionEnd)
  lDriftBursts[["info"]] = lInfo
  #replace NANs with 0's
  NANS = is.nan(lDriftBursts[["sigma"]])
  lDriftBursts[["tStat"]][NANS] <- 0
  lDriftBursts[["sigma"]][NANS] <- 0
  
  class(lDriftBursts) = c("DBH", "list")
  return(lDriftBursts)
}

#' Plotting method for \code{DBH} objects
#' @param x an object of class \code{DBH}
#' @param ... optional arguments, see details
#' 
#' @details The plotting method has the following optional parameters:
#' \itemize{
#' \item{\code{pData}}{ A \code{data.table} or an \code{xts} object, containing the prices and timestamps of the data used to calculate the test statistic.
#' If specified, and \code{which = "tStat"}, the price will be shown on the right y-axis along with the test statistic}
#' \item{\code{which}}{ A string denoting which of four plots to make. \code{"tStat"} denotes plotting the test statistic. \code{"sigma"} denotes plotting the
#' estimated volatility process. \code{"mu"} denotes plotting the estimated drift process. If \code{which = c("sigma", "mu")} or \code{which = c("mu", "sigma")},
#' both the drift and volatility processes are plotted. CaPiTAlizAtIOn doesn't matter}
#' }
#' @examples
#' # Testing every 60 seconds after 09:15:00
#' DBH <- driftBursts(sampleTDataEurope, testTimes = seq(32400 + 900, 63000, 60), preAverage = 2, 
#'                     ACLag = -1L, meanBandwidth = 300L, varianceBandwidth = 900L)
#' plot(DBH)
#' plot(DBH, pData = sampleTDataEurope)
#' plot(DBH, which = "sigma")
#' plot(DBH, which = "mu")
#' plot(DBH, which = c("sigma", "mu"))
#' @author Emil Sjoerup
#' @importFrom graphics axis axis.POSIXct legend mtext
#' @importFrom grDevices rgb
#' @export
plot.DBH <- function(x, ...){
  prices <- DT <- PRICE <- NULL
  #### Get extra passed options and data
  options <- list(...)
  #### List of standard options
  opt <- list(which = "tStat", pData = NULL, startTime = ifelse(is.null(x$info[['sessionStart']]), min(x$info[['testTimes']]), x$info[['sessionStart']]), 
              endTime = ifelse(is.null(x$info[['sessionEnd']]), max(x$info[['testTimes']]), x$info[['sessionEnd']]), leg.x = "topleft", leg.y = NULL,
              tz = "GMT", annualize = FALSE, nDays = 252, legend.txt = "")
  #### Override standard options where user passed new options
  opt[names(options)] <- options

  #### Extract options (better way to do it?)
  pData      <- opt$pData
  which      <- tolower(opt$which)
  startTime  <- opt$startTime
  endTime    <- opt$endTime
  tz         <- opt$tz
  leg.x      <- opt$leg.x
  leg.y      <- opt$leg.y
  timestamps <- opt$timestamps
  tstat      <- x$tStat
  sigma      <- x$sigma
  mu         <- x$mu
  startpar   <- par(no.readonly = TRUE)
  testTimes  <- x$info$testTimes
  horizLines <- seq(round(min(tstat)), round(max(tstat)), 1)
  
  if(!is.null(pData)){
    if(is.data.table(pData)){
      
      if(!all(c("DT", "PRICE") %in% colnames(pData))){
        stop("pData must have columns DT and PRICE")
      }
      
      timeZone <- format(pData$DT[1], format = "%Z")
      if(is.null(timeZone) || timeZone == ""){
        tz <- "UTC"
        pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
      } else {
        tz <- timeZone
      }
      
      timestamps <- as.numeric(pData$DT, tz = tz) - as.numeric(as.POSIXct(paste0(as.Date(pData$DT[1])), format = "%Y-%m-%d", tz = tz), tz = tz)
      prices <- pData$PRICE
      
    } else if (is.xts(pData)){ ##xts case
      tz         <- tzone(pData)
      timestamps <- index(pData)
      timestamps <- as.numeric(timestamps, tz = tz) - as.numeric(as.POSIXct(paste0(as.Date(timestamps[1])) , format = "%Y-%m-%d", tz = tz), tz = tz)
      if(ncol(pData) == 1){
        prices <- as.numeric(pData)
      } else {
        if(!("PRICE" %in% colnames(pData))){
          stop("pData is xts and has more than one column. Could not find PRICE column")
        }
        prices <- as.numeric(pData[,"PRICE"])
      }
      wasXTS <- TRUE
    } else {
      stop("pData must be either a data.table with atleast the columns DT and PRICE, or an xts object with 1 column (only price) or several with one called PRICE")
    }
  }
  
  
  ###Setup done
  if(!all(which %in% c("tstat", "mu", "sigma"))){
    stop("The which argument must be a character vector containing either:\n
         Sigma, Mu, both of these, or tStat
         CasE doesn't matter.")
  }
  if(inherits(tstat, "xts")){
    tstat <- as.numeric(tstat)
    sigma <- as.numeric(sigma)
    mu    <- as.numeric(mu)
  } 
  xtext <- as.POSIXct(testTimes, tz = tz, origin = as.POSIXct("1970-01-01", tz = tz))
  if(is.null(prices)) {
    xlim  <- c(startTime, endTime)
  } else {
    xlim <- c(min(testTimes, timestamps), max(testTimes, timestamps))
  }
  xlab  <- "Time"
  if(all(which %in% c("tstat"))){
    par(mar = c(4,3.5,2,1.25), mgp = c(2,1,0))
    if(!is.null(prices)) par(mar = c(4,3.5,4,4), mgp = c(2,1,0)) #makes room for values on the right y-axis
    main <- "Drift Bursts test statistic"
    ylab <- "test-statistic"
    plot(tstat, x = testTimes, type = "l", xaxt = 'n', ylab = ylab, main = main, xlab = xlab, xlim = xlim)
    axis(side  = 1, at = testTimes[seq(1, length(testTimes), length.out = 12)], labels = format(seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 12), format = "%H:%M"))
    abline(h = horizLines, col = "grey" , lty = 3, cex = 0.1)
    legend.txt <- "t-stat"
    if(!is.null(prices)){
      if(is.null(timestamps)){
        stop("The timestamps of the price must be passed in the timestamps argument")
      }
      par(new = TRUE)
      plot(prices, x = timestamps , type = "l", axes = FALSE, col = "red", xlab = "", ylab = "", lty = 2, xlim = xlim)
      axis(4)
      mtext(side = 4, text = "price", line = 2.5)
      legend.txt = c(legend.txt, "price")
      legend(x = leg.x, leg.y, legend = legend.txt, lty = c(1,2), col = c(1,2), bg = rgb(0,0,0,0), box.lwd = 0,
             box.col = rgb(0,0,0,0))
    }
  } else if(all(which == "sigma")){ #use all() because this function should accept which arguments with length longer than 1
    main <- "volatility"
    ylab <- "local volatility"
    par(mar = c(4,3.5,2,1.25), mgp = c(2,1,0))
    plot(sigma, x = xtext, type = "l",  xaxt = 'n', ylab = ylab, main = main, xlab = xlab)
    axis(side  = 1, at = testTimes[seq(1, length(testTimes), length.out = 12)], labels = format(seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 12), format = "%H:%M"))
  } else if(all(which == "mu")){ #use all() because this function should accept which arguments with length longer than 1
    main <- "drift"
    ylab <- "drift"
    par(mar = c(4,3.5,2,1.25), mgp = c(2,1,0))
    plot(mu, x = xtext, type = "l",  xaxt = 'n', ylab = ylab, main = main, xlab = xlab)
    axis(side  = 1, at = testTimes[seq(1, length(testTimes), length.out = 12)], labels = format(seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 12), format = "%H:%M"))
    abline(h = 0, col = "grey" , lty = 3)
  } else if("mu" %in% which & "sigma" %in% which){
    par(mfrow = c(2,1), omi = c(0,0,0,0), mgp = c(2,1,0), mai = c(0.75,0.75,0.3,0.25))
    main <- "drift"
    ylab <- "drift"
    plot(mu, x = xtext, type = "l", xlab = "",  xaxt = 'n', ylab = ylab, main = main)
    axis(side  = 1, at = testTimes[seq(1, length(testTimes), length.out = 12)], labels = format(seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 12), format = "%H:%M"))
    abline(h = 0, col = "grey" , lty = 3)
    main <- "volatility"
    ylab <- "volatility"
    plot(sigma, x = xtext, type = "l", xlab = "", xaxt = 'n', ylab = ylab, main = main)
    axis(side  = 1, at = testTimes[seq(1, length(testTimes), length.out = 12)], labels = format(seq.POSIXt(xtext[1], xtext[length(xtext)], length.out = 12), format = "%H:%M"))
  }
  par(startpar)
}

#' Printing method for \code{DBH} objects
#' @param x an object of class \code{DBH}
#' @param ... optional arguments, see details
#' @details 
#' The print method has the following optional parameters:
#' \itemize{
#' \item{\code{criticalValue}}{ A numeric denoting a custom critical value of the test.}
#' \item{\code{alpha}}{ A numeric denoting the confidence level of the test. The alpha value is passed on to \code{\link{getCriticalValues}}.
#' The default value is 0.95}
#' }
#' 
#' @examples
#' \dontrun{
#' DBH <- driftBursts(sampleTDataEurope, testTimes = seq(32400 + 900, 63000, 60), preAverage = 2, 
#'                    ACLag = -1L, meanBandwidth = 300L, varianceBandwidth = 900L)
#' print(DBH)
#' print(DBH, criticalValue = 1) # This value doesn't make sense - don't actually use it!
#' print(DBH, alpha = 0.95) # 5% confidence level - this is the standard
#' print(DBH, alpha = 0.99) # 1% confidence level
#' }
#' @author Emil Sjoerup
#' @export
print.DBH = function(x, ...){
  usePolynomialInterpolation <- TRUE
  options <- list(...)
  if('criticalValue' %in% names(options)){
    usePolynomialInterpolation <- FALSE
  }
  #### List of standard options
  opt <- list(alpha = 0.95)
  #### Override standard options where user passed new options
  opt[names(options)] <- options
  if(usePolynomialInterpolation){
    alpha <- opt$alpha
    criticalValue  <- getCriticalValues(x, alpha)$quantile
  }else{
    criticalValue <- opt$criticalValue
  }

  varDB <- var(x$tStat)
  padding <- x$info$padding

  whichToInclude <- seq(padding[1], length(x$info$testTimes)- padding[2])
  cat("\n-------------Drift Burst Hypothesis------------\n")
  cat("Tests performed:                     ", length(whichToInclude))
  if(usePolynomialInterpolation){
    cat("\nAny drift bursts (|T| > ", formatC(criticalValue, digits = 3, format = "f"), "):    ", ifelse(any(abs(x$tStat) > criticalValue) , 'yes', 'no'))
    # cat("\nAny drift bursts (|T| > ", paste0(round(criticalValue, 3)), "):    ", ifelse(any(abs(x$driftBursts) > criticalValue) , 'yes', 'no'))
  }else{
    #cat("\nAny drift bursts (|T| > ", paste0(criticalValue[1]), "):        ", ifelse(any(abs(x$driftBursts) > criticalValue) , 'yes', 'no'))
    cat("\nAny drift bursts (|T| > ", formatC(criticalValue, digits = 3, format = "f"), "):    ", ifelse(any(abs(x$tStat) > criticalValue) , 'yes', 'no'))
  }
  cat("\nMax absolute value of test statistic:", round(max(abs(x$tStat)), digits=5))
  cat("\nMean test statistic:                 ", round(mean(x$tStat), digits = 5))
  cat("\nVariance of test statistic:          ", round(varDB, digits = 5))
  cat("\n-----------------------------------------------\n")
  
}


#' Get critical value for the drift burst hypothesis t-statistic
#' @description Method for DBH objects to calculate the critical value for the presence of a burst of drift.
#' The critical value is that of the test described in appendix B in Christensen Oomen Reno
#' @param x object of class \code{DBH}
#' @param alpha numeric denoting the confidence level for the critical value. Possible values are \code{c(0.9 0.95 0.99 0.995 0.999 0.9999)}
#' 
#' @author Emil Sjoerup
#' @aliases getCriticalValues.DBH
#' @references 
#' Christensen, K., Oomen, R., and Reno, R. (2018). The drift burst hypothesis. Working paper.
#' @export
getCriticalValues <- function(x, alpha = 0.95){
  UseMethod('getCriticalValues', x)
}
#' @export
getCriticalValues.DBH <- function(x, alpha = 0.95){
  return(DBHCriticalValues(x, alpha))
}
