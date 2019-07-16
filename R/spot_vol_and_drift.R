#' Spot Drift Estimation
#' @description Function used to estimate the spot drift of intra-day (tick) stock prices/returns
#'
#' @param data Can be one of 3 input types, \code{xts}, \code{matrix} or \code{data.table}.
#' If a \code{data.table} is supplied, it will be made into an \code{xts} if possible.
#' If the data is \code{xtsible}, it is assumed that the input is the price in levels.
#' If the data is not \code{xtsible},
#' it will be treated as already prepared return data, i.e. if the desired output is a 5 rolling mean estimation of the spot drift, it should contain returns sampled every 5 minutes.
#' @param method Which method to be used to estimate the spot-drift. Currently 3 methods are available, rolling mean and median as well as the kernel method of Christensen et al. 2018.
#' The kernel is a left hand exponential kernel that will weigh newer observations more heavily than older observations.
#' @param ... Additional arguments for the individual methods. See details
#' @param on What time-frame should the estimator be applied? Accepted inputs are \code{"seconds"} and \code{"secs"} for seconds, \code{"minutes"} and \code{"mins"} for minutes, and \code{"hours"} for hours.
#' Standard is minutes
#' @param k How often should the estimation take place? If \code{k} is 5 the estimation will be done every fifth unit of \code{on}.
#' @param marketopen Opening time of the market, standard is "09:30:00"
#' @param marketclose Closing time of the market, standard is "16:00:00"
#' @param tz Time zone, standard is "GMT"
#'
#' @return An object of class "spotdrift" containing at least the estimated spot drift process. Input on what this class should contain and methods for it is welcome.
#'
#' @details The additional arguments for the mean and median methods are: \code{periods} for the rolling window length which is 5 by standard and
#' \code{align} to allow for control of the alignment, should one wish to do so, the standard is \code{"right"}
#'
#'
#' @references Christensen, Oomen and Reno (2018) <DOI:10.2139/ssrn.2842535>.
#' @author Emil Sjoerup
#' @keywords Drift
#'
#' @examples
#' # Example 1: Rolling mean and median estimators for 2 days
#' dat <- sample_tdata_microseconds[, SYMBOL := NULL]
#' meandrift <- spotDrift(data = dat, k = 1, tz = "EST")
#' mediandrift <- spotDrift(data = dat, method = "driftMedian",k = 1, tz = "EST")
#' plot(meandrift)
#' plot(mediandrift)
#'
#' # Example 2: Kernel based estimator for one day
#' price <- sample_tdata$PRICE
#' storage.mode(price) = "numeric"
#' kerneldrift <- spotDrift(price, method = "driftKernel", on = "minutes", k = 1)
#' plot(kerneldrift)
#'
#' @importFrom stats filter time
#' @importFrom utils tail
#' @importFrom xts xtsible merge.xts
#' @export
spotDrift <- function(data, method = "driftMean", ..., on = "minutes", k = 5,
                     marketopen = "09:30:00", marketclose = "16:00:00",
                     tz = "GMT"){
  if (on == "seconds" | on == "secs")
    delta <- k
  if (on == "minutes" | on == "mins")
    delta <- k * 60
  if (on == "hours")
    delta <- k * 3600

  if(xtsible(data) == TRUE) {
    data <- as.xts(data)
  }
  if (inherits(data, what = "xts")) {
    data <- xts(data, order.by = as.POSIXct(time(data), tz = tz), tzone = tz)
    dates <- unique(format(time(data), "%Y-%m-%d"))
    cDays <- length(dates)
    rdata <- mR <- c()
    intraday <- seq(from = chron::times(marketopen),
                    to = chron::times(marketclose),
                    by = chron::times(delta / (24 * 3600)))
    if (as.character(tail(intraday, 1)) != marketclose)
    intraday <- c(intraday, marketclose)
    intraday <- intraday[2:length(intraday)]
    for (d in 1:cDays) {
      if (method == "driftKernel") {
        break
      }
      datad <- data[as.character(dates[d])]
      if (!all(format(time(datad), format = "%Z") == tz))
        stop(paste("Not all data on ", dates[d], " is in time zone \"", tz,
                   "\". This may be due to daylight saving time. Try using a",
                   " time zone without daylight saving, such as GMT.",
                   sep = ""))

      datad <- aggregatePrice(datad, on = on, k = k , marketopen = marketopen,
                              marketclose = marketclose, tz = tz)
      z <- xts(rep(1, length(intraday)), tzone = tz,
               order.by = as.POSIXct(paste(dates[d], as.character(intraday),
                                           sep = " "), tz = tz))
      datad <- merge.xts(z, datad)$datad
      datad <- na.locf(datad)
      rdatad <- makeReturns(datad)
      rdatad <- rdatad[time(rdatad) > min(time(rdatad))]
      rdata <- rbind(rdata, rdatad)
      mR <- rbind(mR, as.numeric(rdatad))
    }
    if (method != "driftKernel") {
      mR <- t(mR)
    }
  } else if ("matrix" %in% class(data) | "data.table" %in% class(data)) {
    mR <- as.matrix(data)
    rdata <- NULL
  } else {
  stop("Input data has to consist of either of the following:
            1. An xts object or an xtsible data.table containing price data
            2. A matrix or a data.table containing return data")
  }

  options <- list(...)
  out <- switch(method, ### works only for one day at a time! Does the rest of data preparation in the function.
                driftKernel = driftKernel(data = data, delta, intraday, options), 
                driftMean   = driftMean(mR = mR, rdata = rdata, delta, intraday, options),
                driftMedian = driftMedian(mR = mR, rdata = rdata, delta, intraday, options))
  return(out)
}

#' @keywords internal
#' @importFrom xts .indexDate
#' @importFrom zoo index
driftKernel <- function(data, delta, intraday, options) {
  if(ncol(data) > 1){
    stop("driftKernel method currently only accepts single day tick data as it relies on the time-stamps of the trades.")
  }
  if(length(unique(.indexDate(data))) != 1){
    stop("driftKernel method currently only accept single day tick data as it relies on the time-stamps of the trades.")
  }
  op <- list(init = list(), PreAverage = 5, bandwidth = 300)
  op[names(options)] <- options
  data           <- log(data)
  vX             <- c(0,diff(data)[-1])
  k              <- op$PreAverage
  bandwidth      <- op$bandwidth
  iT             <- length(data)
  vPreAveraged   <- rep(0 , iT-1)
  mu             <- numeric(length(intraday))
  mu[1]          <- 0
  vPreAveraged[(k*2-1):(iT-1)] <- filter(x = as.numeric(data) , c(rep(1,k),rep(-1,k)))[k:(iT-k)]
  vPreAveraged <- c(0,vPreAveraged)
  time <- index(data)
  time <- as.numeric(time) - (.indexDate(data)[1] * 86400)
  estimtimes = c(34200, as.numeric(intraday) * 86400)
  for (i in 2:length(estimtimes)) {
    x     <- time - estimtimes[i]
    vWm   <- exp(-abs(x/bandwidth)) * (x<=0)    ##left sided exponential kernel
    idx   <- sum(x <= 0)                        # makes sure we don't include future data!
    mu[i] <- (sum(vWm[1:idx] * vPreAveraged[1:idx])) / bandwidth
  }
  mu = as.matrix(mu * bandwidth, ncol = 1)
  out = list("mu" = mu)
  class(out) <- "spotdrift"
  return(out)
}

#' @keywords internal
#' @importFrom zoo rollmean
driftMean = function(mR, rdata, delta, intraday, options){
  op <- list(init = list(), periods = 5, align = "right")
  op[names(options)] <- options
  periods <- op$periods
  align   <- op$align
  if(dim(mR)[2]>1){
    mu <- apply(mR, 2, rollmean, k = periods, align = align, fill = NA)
    colnames(mu) <- paste0("mu",1:ncol(mR)) #Here we will have a matrix
  }else{
    mu <- rollmean(rdata, k = periods, align = align, fill = NA)
    mu <- as.matrix(mu, ncol = 1) #Here we have a single day
    colnames(mu) <- "mu"
  }
  out <- list("mu" = mu)
  class(out) <- "spotdrift"
  return(out)
}
#' @keywords internal
#' @importFrom zoo rollmedian
driftMedian = function(mR, rdata, delta, intraday, options){
  op <- list(init = list(), periods = 5, align = "right")
  op[names(options)] <- options
  periods <- op$periods
  align   <- op$align
  if(dim(mR)[2]>1){
    mu <- apply(mR, 2, rollmedian, k = periods, align = align, fill = NA)
    colnames(mu) <- paste0("mu",1:ncol(mR))
  }else{
    mu <- rollmedian(rdata, k = periods, align = align, fill = NA)
    mu <- as.matrix(mu, ncol = 1)
    colnames(mu) <- "mu"
  }

  out <- list("mu" = mu)
  class(out) <- "spotdrift"
  return(out)
}

#' @export
plot.spotdrift <- function(x, ...) {
  mu <- x$mu
  if (dim(mu)[2] > 1) {
    #browser()
    ####What should it look like??
   plot(as.numeric(mu), main = "Daily Drift", xlab = "", ylab = "", type = "l")
  } else {
    plot(mu, type = "l", main = "Spot Drift", ylab = "", xlab = "")
  }
}

