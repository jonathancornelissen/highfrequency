
#' Available kernels
#'
#' @description Returns a vector of the available kernels.
#'
#' @return a character vector.
#' 
#' @details The available kernels are: 
#' \itemize{
#' \item Rectangular: \eqn{K(x) = 1}.
#' \item Bartlett: \eqn{K(x) = 1 - x}.
#' \item Second-order: \eqn{K(x) = 1 - 2x - x^2}.
#' \item Epanechnikov: \eqn{K(x) = 1 - x^2}.
#' \item Cubic: \eqn{K(x) = 1 - 3 x^2 + 2 x^3}.
#' \item Fifth: \eqn{K(x) = 1 - 10 x^3 + 15 x^4 - 6 x^5}.
#' \item Sixth: \eqn{K(x) = 1 - 15 x^4 + 24 x^5 - 10 x^6}
#' \item Seventh: \eqn{K(x) = 1 - 21 x^5 + 35 x^6 - 15 x^7}.
#' \item Eighth: \eqn{K(x) = 1 - 28 x^6 + 48 x^7 - 21 x^8}.
#' \item Parzen: \eqn{K(x) = 1- 6 x^2 + 6 x^3} if \eqn{k \leq 0.5} and \eqn{K(x) = 2 (1-x)^3} if \eqn{k > 0.5}.
#' \item TukeyHanning: \eqn{K(x) = 1 + \sin(\pi/2 - \pi \cdot x))/2}.
#' \item ModifiedTukeyHanning: \eqn{K(x) = (1 - \sin(\pi/2 - \pi \ (1 - x)^2 ) / 2}.
#' }
#'
#' @references Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2008). Designing realized kernels to measure the ex post variation of equity prices in the presence of noise. \emph{Econometrica}, 76, 1481-1536.
#'
#' @author Scott Payseur.
#'
#' @examples
#' listAvailableKernels
#' @keywords volatility
#' @export
listAvailableKernels <- function() {
  c("Rectangular",
    "Bartlett",
    "Second",
    "Epanechnikov",
    "Cubic",
    "Fifth",
    "Sixth",
    "Seventh",
    "Eighth",
    "Parzen",
    "TukeyHanning",
    "ModifiedTukeyHanning")
}

#' An estimator of integrated quarticity from applying the median operator on blocks of three returns
#' @description 
#' Calculate the rMedRQ, defined in Andersen et al. (2012). Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}. 
#' Then, the rMedRQ is given by
#' \deqn{
#'   \mbox{rMedRQ}_{t}=\frac{3\pi N}{9\pi +72 - 52\sqrt{3}} \left(\frac{N}{N-2}\right) \sum_{i=2}^{N-1} \mbox{med}(|r_{t,i-1}|, |r_{t,i}|, |r_{t,i+1}|)^4.
#' }
#'   
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"},\code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#'
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#' 
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' rq <- rMedRQ(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'             alignPeriod = 5, makeReturns = TRUE)
#' rq
#' @keywords highfrequency rMedRQ
#' @references 
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#' @importFrom zoo rollmedian
#' @export
rMedRQ <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rMedRQ, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setcolorder(rData, "DT")
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    # dates <- as.character(unique(as.Date(rData$DT)))
    # res <- vector(mode = "list", length = length(dates))
    # names(res) <- dates
    # for (date in dates) {
    #   res[[date]] <- rMedRQ(as.matrix(rData[as.Date(DT) == date][, !"DT"]), makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL) ## aligning is done above.
    # }

    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rMedRQ(dat[starts[i]:ends[i],], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }


    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    colnames(res) <- colnames(rData)
    return(res)

  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }


    q <- abs(as.matrix(rData))
    q <- rollApplyMedianWrapper(q)
    # q <- rollmedian(q, alignPeriod = 3, align = "center")
    N <- nrow(q) + 2
    rMedRQ <- 3 * pi * N / (9 * pi + 72 - 52 * sqrt(3)) * (N / (N-2)) * colSums(q^4)

    return(rMedRQ)
  }
}

#' An estimator of integrated quarticity from applying the minimum operator on blocks of two returns
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup
#' @description 
#' Calculate the rMinRQ, defined in Andersen et al. (2012).
#' Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}.
#' Then, the rMinRQ is given by
#' \deqn{
#'   \mbox{rMinRQ}_{t}=\frac{\pi N}{3 \pi - 8} \left(\frac{N}{N-1}\right) \sum_{i=1}^{N-1} \mbox{min}(|r_{t,i}| ,|r_{t,i+1}|)^4
#' }
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' 
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @examples
#' rq <- rMinRQ(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'             alignPeriod = 5, makeReturns = TRUE)
#' rq
#' @references
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#' @importFrom zoo rollapply
#' @export
rMinRQ <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rMinRQ, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setcolorder(rData, "DT")
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rMinRQ(dat[starts[i]:ends[i], ], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    colnames(res) <- colnames(rData)
    return(res)
  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- abs(as.matrix(rData))
    q <- rollApplyMinWrapper(q)
    N <- nrow(q) + 1
    rMinRQ <- pi * N/(3 * pi - 8)*(N / (N - 1)) * colSums(q^4)

    return(rMinRQ)
  }
}

#' rMinRV
#'
#' @description 
#' Calculate the rMinRV, defined in Andersen et al. (2009).
#' Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.
#' Then, the rMinRV is given by
#' \deqn{
#' \mbox{rMinRV}_{t}=\frac{\pi}{\pi - 2}\left(\frac{M}{M-1}\right) \sum_{i=1}^{M-1} \mbox{min}(|r_{t,i}| ,|r_{t,i+1}|)^2
#' }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @references
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#'
#' @author Jonathan Cornelissen, Kris Boudt, Emil Sjoerup.
#'
#' @examples
#' minrv <- rMinRV(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'                alignPeriod = 5, makeReturns = TRUE)
#' minrv
#'
#' @keywords volatility
#' @export
rMinRV <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...){

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rMinRV, alignBy, alignPeriod, makeReturns)
    return(result)

  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setcolorder(rData, "DT")
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rMinRV(dat[starts[i]:ends[i], ], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    colnames(res) <- colnames(rData)

    if(ncol(rData) == 2){ ## Univariate case
      colnames(res) <- c("DT", "rMinRV")
    }


    return(res)
  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- abs(rData)#as.zoo(abs(as.nueric(rData))) #absolute value
    q <- rollApplyMinWrapper(q)
    # q <- rollapply(q, width = 2, FUN = min, by = 1, align = "left", by.column = TRUE, fill = NULL)
    N <- dim(q)[1] + 1 #number of obs because of fill = NULL
    colnames(q) <- names(rData)
    minrv <- (pi/(pi - 2)) * (N/(N - 1)) * colSums(q^2, na.rm = TRUE)
    return(minrv)
  }
}

#' rMedRV
#'
#' @description
#' Calculate the rMedRV, defined in Andersen et al. (2012). 
#' Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.
#' Then, the rMedRV is given by
#' \deqn{
#'  \mbox{rMedRV}_{t}=\frac{\pi}{6-4\sqrt{3}+\pi}\left(\frac{M}{M-2}\right) \sum_{i=2}^{M-1} \mbox{med}(|r_{t,i-1}|,|r_{t,i}|, |r_{t,i+1}|)^2
#' }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @details
#' The rMedRV belongs to the class of realized volatility measures in this package
#' that use the series of high-frequency returns \eqn{r_{t,i}} of a day \eqn{t}
#' to produce an ex post estimate of the realized volatility of that day \eqn{t}.
#' rMedRV is designed to be robust to price jumps.
#' The difference between RV and rMedRV is an estimate of the realized jump
#' variability. Disentangling the continuous and jump components in RV
#' can lead to more precise volatility forecasts,
#' as shown in Andersen et al. (2012)
#'
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @references
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' medrv <- rMedRV(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'                alignPeriod = 5, makeReturns = TRUE)
#' medrv
#' @importFrom data.table setDT transpose setcolorder
#' @keywords volatility
#' @export
rMedRV <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...){

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rMedRV, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setcolorder(rData, "DT")
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rMedRV(dat[starts[i]:ends[i],], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    if(ncol(res) == 2){
      colnames(res) <- c("DT", "rMedRV")
    } else {
      colnames(res) <- colnames(rData)
    }

    setkey(res, "DT")
    return(res)

  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }


    q <- abs(as.matrix(rData))
    q <- rollApplyMedianWrapper(q)
    # q <- rollmedian(q, k = 3, align = "center")
    N <- nrow(q) + 2
    medrv <- (pi / (6 - 4 * sqrt(3) + pi)) * (N / (N - 2)) * colSums(q^2)
    return(medrv)
  }
}

#' Modulated realized covariance
#'
#' @description Calculate univariate or multivariate pre-averaged estimator, as defined in Hautsch and Podolskij (2013).
#'
#' @param pData a list. Each list-item contains an \code{xts} or \code{data.table} object with the intraday price data of a stock.
#' @param pairwise boolean, should be \code{TRUE} when refresh times are based on pairs of assets. \code{FALSE} by default.
#' @param makePsd boolean, in case it is \code{TRUE}, the positive definite version of rMRCov is returned. \code{FALSE} by default.
#' @param theta a \code{numeric} controlling the preaveragin horizon. Detaults to \code{0.8} as recommended by Hautsch and Podolskij (2013)
#' @param ... used internally, do not change.
#' 
#' @return A \eqn{d \times d} covariance matrix.
#'
#' @details
#'   In practice, market microstructure noise leads to a departure from the pure semimartingale model. We consider the process \eqn{Y} in period \eqn{\tau}:
#'     \deqn{
#'       \mbox{Y}_{\tau} = X_{\tau} + \epsilon_{\tau},
#'     }
#'   where the observed \eqn{d} dimensional log-prices are the sum of underlying Brownian semimartingale process \eqn{X} and a noise term \eqn{\epsilon_{\tau}}.
#'
#'   \eqn{\epsilon_{\tau}} is an \emph{i.i.d.} process with \eqn{X}.
#'
#'   It is intuitive that under mean zero \emph{i.i.d.} microstructure noise some form of smoothing of the observed log-price should tend to diminish the impact of the noise.
#'   Effectively, we are going to approximate a continuous function by an average of observations of \eqn{Y} in a neighborhood, the noise being averaged away.
#'
#'   Assume there is \eqn{N} equispaced returns in period \eqn{\tau} of a list (after refreshing data). 
#'   Let \eqn{r_{\tau_i}} be a return (with \eqn{i=1, \ldots,N}) of an asset in period \eqn{\tau}. Assume there is \eqn{d} assets.
#'
#'   In order to define the univariate pre-averaging estimator, we first define the pre-averaged returns as
#'   \deqn{
#'     \bar{r}_{\tau_j}^{(k)}= \sum_{h=1}^{k_N-1}g\left(\frac{h}{k_N}\right)r_{\tau_{j+h}}^{(k)}
#'   }
#'   where g is a non-zero real-valued function \eqn{g:[0,1]} \eqn{\rightarrow} \eqn{R} given by \eqn{g(x)} = \eqn{\min(x,1-x)}. \eqn{k_N} is a sequence of integers satisfying  \eqn{\mbox{k}_{N} = \lfloor\theta N^{1/2}\rfloor}.
#'   We use \eqn{\theta = 0.8} as recommended in Hautsch and Podolskij (2013). The pre-averaged returns are simply a weighted average over the returns in a local window.
#'   This averaging diminishes the influence of the noise. The order of the window size \eqn{k_n} is chosen to lead to optimal convergence rates.
#'   The pre-averaging estimator is then simply the analogue of the realized variance but based on pre-averaged returns and an additional term to remove bias due to noise
#'   \deqn{
#'     \hat{C}= \frac{N^{-1/2}}{\theta \psi_2}\sum_{i=0}^{N-k_N+1}  (\bar{r}_{\tau_i})^2-\frac{\psi_1^{k_N}N^{-1}}{2\theta^2\psi_2^{k_N}}\sum_{i=0}^{N}r_{\tau_i}^2
#'   }
#'   with
#'   \deqn{
#'     \psi_1^{k_N}= k_N \sum_{j=1}^{k_N}\left(g\left(\frac{j+1}{k_N}\right)-g\left(\frac{j}{k_N}\right)\right)^2,\quad
#'   }
#'   \deqn{
#'     \psi_2^{k_N}= \frac{1}{k_N}\sum_{j=1}^{k_N-1}g^2\left(\frac{j}{k_N}\right).
#'   }
#'   \deqn{
#'     \psi_2= \frac{1}{12}
#'   }
#'   The multivariate counterpart is very similar. The estimator is called the Modulated Realized Covariance (rMRCov) and is defined as
#'   \deqn{
#'     \mbox{MRC}= \frac{N}{N-k_N+2}\frac{1}{\psi_2k_N}\sum_{i=0}^{N-k_N+1}\bar{\boldsymbol{r}}_{\tau_i}\cdot \bar{\boldsymbol{r}}'_{\tau_i} -\frac{\psi_1^{k_N}}{\theta^2\psi_2^{k_N}}\hat{\Psi}
#' }
#' where \eqn{\hat{\Psi}_N = \frac{1}{2N}\sum_{i=1}^N \boldsymbol{r}_{\tau_i}(\boldsymbol{r}_{\tau_i})'}. It is a bias correction to make it consistent.
#' However, due to this correction, the estimator is not ensured PSD.
#' An alternative is to slightly enlarge the bandwidth such that \eqn{\mbox{k}_{N} = \lfloor\theta N^{1/2+\delta}\rfloor}. \eqn{\delta = 0.1} results in a consistent estimate without the bias correction and a PSD estimate, in which case:
#'   \deqn{
#'     \mbox{MRC}^{\delta}= \frac{N}{N-k_N+2}\frac{1}{\psi_2k_N}\sum_{i=0}^{N-k_N+1}\bar{\boldsymbol{r}}_i\cdot \bar{\boldsymbol{r}}'_i
#' }
#'
#' @references Hautsch, N., and Podolskij, M. (2013). Preaveraging-based estimation of quadratic variation in the presence of noise and jumps: theory, implementation, and empirical Evidence. \emph{Journal of Business & Economic Statistics}, 31, 165-183.
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' \dontrun{
#' library("xts")
#' # Note that this ought to be tick-by-tick data and this example is only to show the usage.
#' a <- list(as.xts(sampleOneMinuteData[as.Date(DT) == "2001-08-04", list(DT, MARKET)]),
#'           as.xts(sampleOneMinuteData[as.Date(DT) == "2001-08-04", list(DT, STOCK)]))
#' rMRCov(a, pairwise = TRUE, makePsd = TRUE)
#'
#' }
#' @keywords highfrequency preaveraging
#' @export
rMRCov <- function(pData, pairwise = FALSE, makePsd = FALSE, theta = 0.8, ...) {

  if (!is.list(pData) | is.data.table(pData)) {
    n <- 1
    
  } else {
    n <- length(pData)
  }
  if (n == 1) {
    if (isMultiXts(pData)) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }
    if(is.data.table(pData)){
      DT <- NULL
      date <- as.Date(pData[1, DT])
      pData <- as.matrix(pData[, !"DT"])
      mrc <- data.table(DT = date, MRC = crv(pData))
    } else {
      mrc <- crv(pData)
    }
    
  }

  if (n > 1) {
    if (isMultiXts(pData[[1]])) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }

    if (pairwise) {
      cov <- matrix(rep(0, n * n), ncol = n, dimnames = list(colnames(pData), colnames(pData)))
      diagonal <- numeric(n)
      for (i in 1:n) {
        diagonal[i] <- crv(pData[[i]])
      }
      diag(cov) <- diagonal

      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = preavbi(pData[[i]], pData[[j]])
        }
      }

      mrc <- cov

      if (makePsd) {
        mrc <- makePsd(mrc)
      }

    } else {
      x     <- refreshTime(pData)
      N     <- nrow(x)
      kn    <- floor(theta * sqrt(N))

      ##psi:
      psi2 <- 1 / 12
      if(is.data.table(x)){
        DT <- NULL
        x <- x[, DT := NULL]
        x <- as.matrix(x)
      }

      preavreturn <- as.matrix(hatreturn(x, kn), ncol = ncol(x))
      S <- rCov(preavreturn)

      mrc <- N / (N - kn + 2) * 1/(psi2 * kn) * S

      if (makePsd) {
        mrc <- makePsd(mrc)
      }
    }
  }
  return(mrc)
}

#' DEPRECATED rMRC
#' @description DEPRECATED USE \code{\link{rMRCov}}
#'
#' @param pData DEPRECATED
#' @param pairwise DEPRECATED
#' @param makePsd DEPRECATED
#' @param theta DEPRECATED
#' @param ... DEPRECATED
#' 
rMRC <- function(pData, pairwise = FALSE, makePsd = FALSE, theta = 0.8, ...){
  .Deprecated(new = "rMRC has been renamed to rMRCov to clearly show the covariance part", msg = "rMRC")
  invisible(NULL)
}



#' Realized covariances via subsample averaging
#'
#' @description Calculates realized variances via averaging across partially
#' overlapping grids, first introduced by Zhang et al. (2005).
#' This estimator is basically an average across different \code{\link{rCov}} estimates that start at
#' different points in time, see details below.
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param k numeric denoting which horizon to use for the subsambles. This can be a fraction as long as \eqn{k} is a divisor of \code{alignPeriod} default is \code{1}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return in case the input is and contains data from one day, an \eqn{N} by \eqn{N} matrix is returned. If the data is a univariate \code{xts} object with multiple days, an \code{xts} is returned.
#' If the data is multivariate and contains multiple days (\code{xts} or \code{data.table}), the function returns a list containing \eqn{N} by \eqn{N} matrices. 
#' Each item in the list has a name which corresponds to the date for the matrix.
#' 
#' @details 
#' 
#' Suppose that in period \eqn{t}, there are \eqn{N} equispaced returns \eqn{r_{i,t}} 
#' and let \eqn{\Delta} be equal to \code{alignPeriod}. For \eqn{\ i \geq \Delta}, 
#' we define the subsampled \eqn{\Delta}-period return as
#' \deqn{
#' \tilde r_{t,i} = \sum_{k = 0}^{\Delta - 1} r_{t,i-k}, .
#' }
#' Now define \eqn{N^*(j) = N/\Delta} if \eqn{j = 0} and  \eqn{N^*(j) = N/\Delta - 1} otherwise.
#' The \eqn{j}-th component of the \code{rAVGCov} estimator is given by
#' \deqn{
#' RV_t^j = \sum_{i = 1}^{N^*(j)} \tilde r_{t, j + i \cdot \Delta}^2.
#' }
#' Now taking the average across the different \eqn{RV_t^j, \ j = 0, \dots, \Delta-1,} defines the \code{rAVGCov} estimator.
#' The multivariate version follows analogously.
#'
#' Note that Liu et al. (2015) show that \code{rAVGCov} is not only theoretically but also empirically a more reliable estimator than rCov.
#'
#' @references
#' Liu, L. Y., Patton, A. J., Sheppard, K. (2015). Does anything beat 5-minute RV? A comparison of realized measures across multiple asset classes. \emph{Journal of Econometrics}, 187, 293-311.
#' 
#' Zhang, L., Mykland, P. A. , and Ait-Sahalia, Y. (2005). A tale of two time scales: Determining integrated volatility with noisy high-frequency data. \emph{Journal of the American Statistical Association}, 100, 1394-1411.
#'
#' @author Scott Payseur, Onno Kleen, and Emil Sjoerup.
#'
#' @examples
#' # Average subsampled realized variance/covariance aligned at one minute returns at
#' # 5 sub-grids (5 minutes).
#'
#' # Univariate subsampled realized variance
#' rvAvgSub <- rAVGCov(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'                     makeReturns = TRUE)
#' rvAvgSub
#'
#' # Multivariate subsampled realized variance
#' rvAvgCovSub <- rAVGCov(rData = sampleOneMinuteData[1:391], makeReturns = TRUE)
#' rvAvgCovSub
#' 
#' @importFrom data.table data.table
#' @keywords volatility
#' @export
#'
rAVGCov <- function(rData, cor = FALSE, alignBy = "minutes", alignPeriod = 5, k = 1, makeReturns = FALSE, ...) {
  .SD <- DT <- DT_ROUND <- DT_SUBSAMPLE <- FIRST_DT <- MAXDT <- RETURN <- RETURN1 <- RETURN2 <- NULL

  if (is.xts(rData) && checkMultiDays(rData)) {
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    if (n == 1) {
      result <- apply.daily(rData, rAVGCov, alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = makeReturns)
    }
    if (n > 1) {
      result <- applyGetList(rData, rAVGCov, cor=cor, alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, lapply(.SD, as.numeric), .SDcols = colnames(rData)])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rAVGCov(dat[starts[i]:ends[i],], cor = cor, makeReturns = makeReturns, alignBy = alignBy, alignPeriod = alignPeriod, k = k)
    }

    if(length(res[[1]]) == 1){ ## Univariate case
      res <- data.table(DT = names(res), rAVGCov = as.numeric(res))
    } else if(length(res) == 1){ ## Single day multivariate case
      res <- res[[1]]
    }
    return(res)

  } else {

    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2] - !is.xts(rData)

    }

    if(alignBy == "secs" | alignBy == "seconds"){
      scaleFactor <- alignPeriod
      scaleFactorFast <- k
    }
    if(alignBy == "mins" | alignBy == "minutes"){
      scaleFactor <- alignPeriod * 60
      scaleFactorFast <- k * 60
    }
    if(alignBy == "hours"){
      scaleFactor <- alignPeriod * 60 * 60
      scaleFactorFast <- k * 60 * 60
    }

    # We calculate how many times the fast alignment period divides the slow one and makes sure it is a positive integer.
    scalingFraction <- alignPeriod/k
    if(scalingFraction < 0 | scalingFraction %% 1){
      stop("alignPeriod must be greater than k, and the fraction of these must be an integer value")
    }


    if (n == 1) {
      if(is.xts(rData)){
        rdatabackup <- data.table(DT = as.numeric(index(rData), tz = tzone(rData)), RETURN = as.numeric(rData))
      } else {
        rdatabackup <- data.table(rData)
        colnames(rdatabackup) <- c("DT", "RETURN")
      }
      rData <- rdatabackup
      rData[, FIRST_DT := min(DT)]
      if (makeReturns) {

        rData[, DT_ROUND := ifelse(DT == FIRST_DT,
                                   floor(DT/scaleFactorFast) * scaleFactorFast,
                                   ceiling(DT / scaleFactorFast) * scaleFactorFast)]


        rData[, MAXDT := max(DT), by = "DT_ROUND"]
        rData <- rData[DT == MAXDT]
        rData[, RETURN := log(RETURN) - shift(log(RETURN), n = 1, type = "lag")]
        rData <- rData[!is.na(RETURN)]
        rData <- rData[, c("DT_ROUND", "RETURN")]

      } else {
        rData[, DT_ROUND := ceiling(DT / scaleFactorFast) * scaleFactorFast]

      }

      rvavg <- sum(rData[, DT_SUBSAMPLE := ceiling(DT_ROUND/scaleFactor) * scaleFactor][, list(RETURN = sum(RETURN)), by = list(DT_SUBSAMPLE)]$RETURN^2)



      for (ii in c(1:(scalingFraction - 1))) {
        rdatasub <- rData[-c(1:ii, (dim(rData)[1]-scalingFraction + ii + 1):dim(rData)[1]), ]
        rdatasub[, DT_ROUND := DT_ROUND - ii * scaleFactorFast]
        rvavg <- rvavg + sum(rdatasub[, DT_SUBSAMPLE := ceiling(DT_ROUND/scaleFactor) * scaleFactor][, list(RETURN = sum(RETURN)), by = list(DT_SUBSAMPLE)]$RETURN^2) * (dim(rdatasub)[1]/scalingFraction + 1) / (dim(rdatasub)[1]/scalingFraction)
      }
      return(rvavg / scalingFraction)
    }

    if (n > 1) {
      rdatamatrix <- matrix(0, nrow = n, ncol = n)
      for (ii in 1:n) {
        # calculate variances
        if(is.xts(rData)){
          rdatamatrix[ii, ii] <- rAVGCov(rData[, ii], cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = makeReturns)
        } else {
          rdatamatrix[ii, ii] <- rAVGCov(rData[, c(1, ii + 1)], cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = makeReturns)
        }
        if (ii < n) {
          for (jj in (ii+1):n) {
            if(is.xts(rData)){
              rdatabackup <- data.table(DT = as.numeric(index(rData), tz = tzone(rData)), RETURN1 = as.numeric(rData[, ii]), RETURN2 = as.numeric(rData[,jj]))
            } else {
              rdatabackup <- data.table(DT = rData[,"DT"], RETURN1 = rData[, ii+1], RETURN2 = rData[, jj+1])
            }
            rdatabackup[, FIRST_DT := min(DT)]
            if (makeReturns) {

              rdatabackup[, DT_ROUND := ifelse(DT == FIRST_DT,
                                               floor(DT/scaleFactorFast) * scaleFactorFast,
                                               ceiling(DT/scaleFactorFast) * scaleFactorFast)]
              rdatabackup[, MAXDT := max(DT), by = "DT_ROUND"]
              rdatabackup <- rdatabackup[DT == MAXDT]
              rdatabackup[, RETURN1 := log(RETURN1) - shift(log(RETURN1), n = 1, type = "lag")]
              rdatabackup[, RETURN2 := log(RETURN2) - shift(log(RETURN2), n = 1, type = "lag")]
              rdatabackup <- rdatabackup[!is.na(RETURN1)][!is.na(RETURN2)][, c("DT_ROUND", "RETURN1", "RETURN2")]
            } else {

              rdatabackup[, DT_ROUND := ceiling(DT/scaleFactorFast) * scaleFactorFast]


            }
            returns <- rdatabackup[, DT_SUBSAMPLE := ceiling(DT_ROUND / scaleFactor) * scaleFactor][, list(RETURN1 = sum(RETURN1), RETURN2 = sum(RETURN2)), by = list(DT_SUBSAMPLE)]
            # Calculate off-diagonals
            covavg <- t(returns$RETURN1) %*% returns$RETURN2

            for (kk in c(1:(scalingFraction - 1))) {
              returns <- rdatabackup[, DT_SUBSAMPLE := ceiling(DT_ROUND/scaleFactorFast) * scaleFactorFast][, list(RETURN1 = sum(RETURN1), RETURN2 = sum(RETURN2)), by = list(DT_SUBSAMPLE)]
              rdatasub <- returns[-c(1:kk, (dim(returns)[1]-scalingFraction + kk + 1):dim(returns)[1]), ]
              rdatasub[, DT_SUBSAMPLE := DT_SUBSAMPLE - kk * scaleFactorFast]
              returns <- rdatasub[, DT_SUBSAMPLE := ceiling(DT_SUBSAMPLE/scaleFactor) * scaleFactor][, list(RETURN1 = sum(RETURN1), RETURN2 = sum(RETURN2)), by = list(DT_SUBSAMPLE)]
              covavg <- covavg + t(returns$RETURN1) %*% returns$RETURN2
            }


            rdatamatrix[ii, jj] <- rdatamatrix[jj, ii] <- covavg / scalingFraction

          }
        }
      }
      colnames(rdatamatrix) <- rownames(rdatamatrix) <- colnames(rData)[!colnames(rData) == "DT"] # So we get column names for both xts and data.table input
    }
    return(rdatamatrix)
  }
}


#' Realized beta
#'
#' @description Depending on users' choices of estimator (realized covariance (RCOVestimator) and realized variance (RVestimator)), 
#' the function returns the realized beta, defined as the ratio between both.
#'
#' The realized beta is given by
#' \deqn{
#' \beta_{jm} = \frac {RCOVestimator_{jm}}{RVestimator_{m}}
#' }
#'
#' in which
#'
#' \eqn{RCOVestimator:} Realized covariance of asset j and market index \eqn{m}.
#'
#' \eqn{RVestimator:} Realized variance of market index \eqn{m}.
#'
#' @param rData a \code{xts} object containing all returns in period t for one asset.
#' @param rIndex a \code{xts} object containing return in period t for an index.
#' @param RCOVestimator can be chosen among realized covariance estimators: \code{"rCov"}, \code{"rAVGCov"}, \code{"rBPCov"}, \code{"rHYCov"}, \code{"rKernelCov"}, \code{"rOWCov"}, \code{"rRTSCov"}, \code{"rThresholdCov"} and \code{"rTSCov"} \code{"rCov"} by default.
#' @param RVestimator can be chosen among realized variance estimators: \code{"RV"}, \code{"rMinRV"} and \code{"rMedRV"}. \code{"RV"} by default. 
#' In case of missing \code{RVestimator}, \code{RCOVestimator} function applying for \code{rIndex} will be used.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' 
#' @return numeric
#'
#' @details
#' Suppose there are \eqn{N} equispaced returns on day \eqn{t} for the asset \eqn{j} and the index \eqn{m}. 
#' Denote \eqn{r_{(j)i,t}}, \eqn{r_{(m)i,t}} as the \eqn{i}th return on day \eqn{t} for asset \eqn{j} and index \eqn{m} (with \eqn{i=1, \ldots,N}).
#'
#' By default, the RCov is used and the realized beta coefficient is computed as:
#' \deqn{
#' \hat{\beta}_{(jm)t}= \frac{\sum_{i=1}^{N} r_{(j)i,t} r_{(m)i,t}}{\sum_{i=1}^{N} r_{(m)i,t}^2}.
#' }
#'
#' Note: The function does not support to calculate betas across multiple days.
#'
#' @references
#' Barndorff-Nielsen, O. E. and Shephard, N. (2004). Econometric analysis of realized covariation: high frequency based covariance, regression, and correlation in
#' financial economics. \emph{Econometrica}, 72, 885-925.
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#'
#' @examples
#' \dontrun{
#' library("xts")
#' a <- as.xts(sampleOneMinuteData[as.Date(DT) == "2001-08-04", list(DT, MARKET)])
#' b <-  as.xts(sampleOneMinuteData[as.Date(DT) == "2001-08-04", list(DT, STOCK)])
#' rBeta(a, b, RCOVestimator = "rBPCov", RVestimator = "rMinRV", makeReturns = TRUE)
#' }
#'
#' @keywords highfrequency rBeta
#' @importFrom methods hasArg
#' @importFrom utils data
#' @export
rBeta <- function(rData, rIndex, RCOVestimator = "rCov", RVestimator = "RV", makeReturns = FALSE) {
  if (hasArg(data)) {
    rData <- data
  }

  if (RCOVestimator != "rRTSCov" & RCOVestimator != "rTSCov" &  makeReturns) {
    rData <- makeReturns(rData)
    rIndex <- makeReturns(rIndex)
  }

  if(!makeReturns) {
    if (RCOVestimator == "rRTSCov" || RCOVestimator == "rTSCov"){
      if (min(rData) < 0) {
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rData <- exp(cumsum(rData))
      }
      if( min(rIndex) <0 ){
        print("when using rRTSCov, rTSCov, introduce price data - transformation to price data done")
        rIndex <- exp(cumsum(rIndex))
      }
    }
  }

  if (isMultiXts(rData)) {
    print("No support for multiple days")
  } else {
    rcovfun <- function(rData, rIndex, RCOVestimator) {

      switch(RCOVestimator,
             rCov = rCov(cbind(rData,rIndex) ),
             rAVGCov = rAVGCov(list(rData, rIndex) ),
             rBPCov = rBPCov(cbind(rData, rIndex) ),
             rHYCov = rHYCov(list(rData, rIndex) ),
             rKernelCov = rKernelCov(list(rData, rIndex) ),
             rOWCov = rOWCov(cbind(rData, rIndex) ),
             rRTSCov = rRTSCov(list(rData, rIndex)),
             rThresholdCov = rThresholdCov(cbind(rData, rIndex) ),
             rTSCov = rTSCov(list(rData, rIndex)))
    }

    rcov <- rcovfun(rData,rIndex,RCOVestimator)

    if (RVestimator == RCOVestimator || is.null(RVestimator)) {
      rbeta <- rcov[1,2] / rcov[2,2]
    } else {
      rvfun <- function(rIndex, RVestimator) {

        switch(RVestimator,
               rCov = rCov(rIndex ) ,
               RV = RV(rIndex),
               BV = RBPVar(rIndex),
               rMinRV = rMinRV(rIndex ),
               rMedRV = rMedRV(rIndex ),
               rAVGCov = rAVGCov(rIndex ) ,
               rBPCov = rBPCov(rIndex ) ,
               rHYCov = rHYCov(rIndex ) ,
               rKernelCov = rKernelCov(rIndex ) ,
               rOWCov = rOWCov(rIndex ) ,
               rRTSCov = rRTSCov(rIndex) ,
               rThresholdCov = rThresholdCov(rIndex ) ,
               rTSCov = rTSCov(rIndex))
      }
      rv <- rvfun(rIndex,RVestimator)
      rbeta <- rcov[1,2] / rv
    }
    return(rbeta)
  }
}



#' Realized bipower covariance
#'
#' @description Calculate the Realized BiPower Covariance (rBPCov),
#'  defined in Barndorff-Nielsen and Shephard (2004).
#'
#'  Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M}
#'  the number of intraday returns.
#'
#'  The rBPCov is defined as the process whose value at time \eqn{t}
#'  is the \eqn{N}-dimensional square matrix with \eqn{k,q}-th element equal to
#'  \deqn{
#'    \mbox{rBPCov}[k,q]_t = \frac{\pi}{8} \bigg( \sum_{i=2}^{M}
#'                                               \left|
#'                                                 r_{(k)t,i} + r_{(q)t,i} \right| \ \left| r_{(k)t,i-1} + r_{(q)t,i-1} \right|   \\
#'                                               - \left| r_{(k)t,i}  - r_{(q)t,i} \right| \ \left|
#'                                                 r_{(k)t,i-1} - r_{(q)t,i-1} \right|  \bigg),
#'  }
#'  where \eqn{r_{(k)t,i}} is the
#'  \eqn{k}-th component of the return vector \eqn{r_{i,t}}.
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. E.g. to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param makePsd boolean, in case it is \code{TRUE}, the positive definite version of rBPCov is returned. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return in case the input is and contains data from one day, an \eqn{N} by \eqn{N} matrix is returned. If the data is a univariate \code{xts} object with multiple days, an \code{xts} is returned.
#' If the data is multivariate and contains multiple days (\code{xts} or \code{data.table}), the function returns a list containing \eqn{N} by \eqn{N} matrices. Each item in the list has a name which corresponds to the date for the matrix.
#'
#' @references
#' Barndorff-Nielsen, O. E., and Shephard, N. (2004). Measuring the impact of jumps in multivariate price processes using bipower covariation. Discussion paper, Nuffield College, Oxford University.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' # Realized Bipower Variance/Covariance for a price series aligned
#' # at 5 minutes.
#'
#' # Univariate:
#' rbpv <- rBPCov(rData = sampleTData[, list(DT, PRICE)], alignBy ="minutes",
#'                alignPeriod = 5, makeReturns = TRUE)
#' # Multivariate:
#' rbpc <- rBPCov(rData = sampleOneMinuteData, makeReturns = TRUE, makePsd = TRUE)
#' rbpc
#'
#' @keywords volatility
#' @export
rBPCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, makePsd = FALSE, ...) {
  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    if (n == 1){
      result <- apply.daily(rData, rBPCov, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, makePsd)
    }
    if (n > 1) {
      result <- applyGetList(rData, rBPCov, cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, makePsd)
      names(result) <- unique(as.Date(index(rData)))
    }
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE

    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rBPCov(dat[starts[i]:ends[i], ], cor = cor, makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    if(ncol(rData) == 2){ ## Univariate case
      res <- data.table(DT = names(res), BPV = as.numeric(res))
    } else if(length(res) == 1){ ## Single day multivariate case
      res <- res[[1]]
    }


    return(res)

  } else { #single day code
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }

    if (n == 1) {
      return(RBPVar(rData))
    }

    ## ACTUAL RBPCOV calculation:
    if (n > 1) {

      rData  <- as.matrix(rData)
      n <- dim(rData)[2]
      cov <- matrix(rep(0, n * n), ncol = n, dimnames = list(colnames(rData), colnames(rData)))
      diagonal <- numeric(n)
      for (i in 1:n) {
        diagonal[i] <- RBPVar(rData[, i])
      }
      diag(cov) <- diagonal
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] <- cov[j, i] <- RBPCov_bi(rData[, i], rData[, j])
        }
      }

      if (!cor){
        if (makePsd) {
          cov <- makePsd(cov)
        }
        return(cov)
      } else {
        sdmatrix <- sqrt(diag(diag(cov)))
        rcor <- solve(sdmatrix) %*% cov %*% solve(sdmatrix)
        if (makePsd) {
          rcor <- makePsd(rcor)
        }
        return(rcor)
      }
    }
  }
}

#' Realized covariance
#'
#' @description Function returns the Realized Covariation (rCov).
#' Let \eqn{r_{t,i}} be an intraday \eqn{N \times M} return vector and \eqn{i=1,...,M}
#' the number of intraday returns.
#'
#' Then, the rCov is given by
#' \deqn{
#'  \mbox{rCov}_{t}=\sum_{i=1}^{M}r_{t,i}r'_{t,i}.
#' }
#'
#'
#' @param rData either an \code{xts} or a \code{data.table}. In case it is one of the former \code{rData} should contain returns or prices
#'
#' an \eqn{(M x N)} \code{xts} or data.table object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' In case of a matrix, no multi-day adjustment is possible.
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' @return in case the input is and contains data from one day, an \eqn{N \times N} matrix is returned. If the data is a univariate \code{xts} object with multiple days, an \code{xts} is returned.
#' If the data is multivariate and contains multiple days (\code{xts} or \code{data.table}), the function returns a list containing N by N matrices. Each item in the list has a name which corresponds to the date for the matrix.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' # Realized Variance/Covariance for prices aligned at 5 minutes.
#'
#' # Univariate:
#' rv = rCov(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'           alignPeriod = 5, makeReturns = TRUE)
#' rv
#'
#' # Multivariate:
#' rc = rCov(rData = sampleOneMinuteData, makeReturns = TRUE)
#' rc
#' @keywords volatility
#' @export
rCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...) {

  ## xts case multiday case:
  # Multiday adjustment:
  if (is.xts(rData) && checkMultiDays(rData)) {
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    if (n == 1) {
      result <- apply.daily(rData, rCov, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
    }
    if (n > 1) {
      result <- applyGetList(rData, rCov, cor=cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }



    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rCov(dat[starts[i]:ends[i], ], cor = cor, makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    if(length(res[[1]]) == 1){ ## Univariate case
      res <- data.table(DT = names(res), RV = as.numeric(res))
    } else if(length(res) == 1){ ## Single day multivariate case
      res <- res[[1]]
    }

    return(res)

  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }

    if (n == 1) {
      return(RV(rData))
    } else {
      rData <- as.matrix(rData)
      covariance <- t(rData) %*% rData
      if (!cor) {
        return(covariance)
      }
      if (cor){
        sdmatrix <- sqrt(diag(diag(covariance)));
        rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
        return(rcor)
      }
    }
  }
}

#' Hayashi-Yoshida covariance
#' @description Calculates the Hayashi-Yoshida Covariance estimator
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param period Sampling period
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param makePsd boolean, in case it is \code{TRUE}, the positive definite version of rHYCov is returned. \code{FALSE} by default.
#' @param ... used internally. Do not set.
#' @references Hayashi, T. and Yoshida, N. (2005). On covariance estimation of non-synchronously observed diffusion processes. \emph{Bernoulli}, 11, 359-379.
#'
#' @author Scott Payseur and Emil Sjoerup.
#' @examples
#' library("xts")
#' hy <- rHYCov(rData = as.xts(sampleOneMinuteData)["2001-08-05"],
#'              period = 5, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
#'
#' @keywords volatility
#' @export
rHYCov <- function(rData, cor = FALSE, period = 1, alignBy = "seconds", alignPeriod = 1, makeReturns = FALSE, makePsd = TRUE, ...) {

  opt <- list("INTERNALBOOLEAN" = FALSE)
  op <- list(...)
  opt[names(op)] <- op
  
  if (is.xts(rData) && checkMultiDays(rData)) {
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    if (n == 1) {
      result <- apply.daily(rData, rHYCov, cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, makePsd = makePsd)
    }
    if (n > 1) {
      result <- applyGetList(rData, rHYCov, cor=cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, makePsd = makePsd)
      names(result) <- unique(as.Date(index(rData)))
    }
    return(result)
  } else if (is.data.table(rData) & !opt$INTERNALBOOLEAN) {
    DATE <- .N <- DT <- NULL
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rHYCov(rData[starts[i]:ends[i], ], cor = cor, makeReturns = makeReturns, alignBy = alignBy, alignPeriod = alignPeriod, INTERNALBOOLEAN = TRUE, ...)
    }
    
    if(length(res[[1]]) == 1){ ## Univariate case
      res <- data.table(DT = names(res), rHY = as.numeric(res))
    } else if(length(res) == 1){ ## Single day multivariate case
      res <- res[[1]]
    }
    
    return(res)
    
  } else {
    
    if(is.xts(rData)){
      if(makeReturns){      
        rData <- fastTickAgregation(rData, alignBy = "seconds", alignPeriod = 1)
        aggrdata <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
      } else { 
        rData <- fastTickAgregation_RETURNS(rData, alignBy = "seconds", alignPeriod = 1)
        aggrdata <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
      }
      
    } else if (is.data.table(rData)){
      if(makeReturns){
        rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = "seconds", alignPeriod = 1)
        aggrdata <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
      } else {
        rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = "seconds", alignPeriod = 1)
        aggrdata <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
        
      }
      rData <- as.matrix(rData[, !"DT"])
      aggrdata <- as.matrix(aggrdata[, !"DT"])
    }
  
    if (makeReturns) {
      rData <- makeReturns(rData)
      aggrdata <- makeReturns(aggrdata)
    }
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
  
    if (n == 1) {
      return(rCov(aggrdata))
    }
  
    if (n > 1) {
  
      n <- dim(rData)[2]
      cov <- matrix(rep(0, n * n), ncol = n)
      diagonal <- c()
      for (i in 1:n) {
        diagonal[i] <- rCov(aggrdata[, i])
      }
      diag(cov) <- diagonal
      alignPeriod <- getAlignPeriod(alignPeriod, alignBy)
      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] <-
            sum(pcovcc(
              as.numeric(rData[,i]),
              as.double(rep(0,length(rData[, j])/(period * alignPeriod) + 1)),
              as.numeric(rData[,j]), #b
              as.double(c(1:length(rData[, j]))), #a
              as.double(rep(0, length(rData[, j])/(period * alignPeriod) + 1)), #a
              as.double(c(1:length(rData[, j]))), #b
              as.integer(length(rData[, j])), #na
              as.integer(length(rData[, j])/(period*alignPeriod)),
              as.integer(length(rData[, j])), #na
              as.integer(period * alignPeriod)))
          cov[j, i] <- cov[i, j]
  
        }
      }
  
      if (!cor){
        if (makePsd) {
          cov <- makePsd(cov)
        }
        return(cov)
      } else {
        sdmatrix <- sqrt(diag(diag(cov)))
        rcor <- solve(sdmatrix) %*% cov %*% solve(sdmatrix)
        if (makePsd) {
          rcor <- makePsd(rcor)
        }
        return(rcor)
      }
    }
  }
}

#' Realized kernel estimator
#'
#' @description Realized covariance calculation using a kernel estimator. 
#' The different types of kernels available can be found using \code{\link{listAvailableKernels}}.
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. 
#' \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. 
#' For example, to aggregate based on a 5-minute frequency, set \code{alignPeriod} to 5 and \code{alignBy} to \code{"minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param kernelType Kernel name.
#' @param kernelParam Kernel parameter.
#' @param kernelDOFadj Kernel degree of freedom adjustment.
#' @param ... used internally, do not change.
#' 
#' @details 
#' 
#' Let \eqn{r_{t,i}} be \eqn{N} returns in period \eqn{t}, \eqn{i = 1, \ldots, N}. The returns or prices 
#' do not have to be equidistant. The kernel estimator for \eqn{H = \code{kernelParam}} is given by
#' \deqn{
#' \gamma_0 + 2 \sum_{h = 1}^H k \left(\frac{h-1}{H}\right) \gamma_h,
#' }
#' where \eqn{k(x)} is the chosen kernel function and 
#' \deqn{
#' \gamma_h = \sum_{i = h}^N r_{t,i} \times r_{t,i-h}
#' }
#' is the empirical autocovariance function. The multivariate version employs the cross-covariances instead.
#'
#' @return in case the input is and contains data from one day, an \eqn{N} by \eqn{N} matrix is returned. 
#' If the data is a univariate \code{xts} object with multiple days, an \code{xts} is returned.
#' If the data is multivariate and contains multiple days (\code{xts} or \code{data.table}), the function returns a list containing \eqn{N} by \eqn{N} matrices. 
#' Each item in the list has a name which corresponds to the date for the matrix.
#'
#' @references
#' Barndorff-Nielsen, O. E., Hansen, P. R., Lunde, A., and Shephard, N. (2008). Designing realized kernels to measure the ex post variation of equity prices in the presence of noise. \emph{Econometrica}, 76, 1481-1536.
#'
#' Hansen, P. and Lunde, A. (2006). Realized variance and market microstructure noise. \emph{Journal of Business and Economic Statistics}, 24, 127-218.
#'
#' Zhou., B. (1996). High-frequency data and volatility in foreign-exchange rates. \emph{Journal of Business & Economic Statistics}, 14, 45-52.
#' 
#' @author Scott Payseur, Onno Kleen, and Emil Sjoerup.
#'
#' @examples
#' # Univariate:
#' rvKernel <- rKernelCov(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'                        alignPeriod = 5, makeReturns = TRUE)
#' rvKernel
#'
#' # Multivariate:
#' rcKernel <- rKernelCov(rData = sampleOneMinuteData, makeReturns = TRUE)
#' rcKernel
#' 
#' @keywords volatility
#' @export
rKernelCov <- function(rData, cor = FALSE,  alignBy = NULL, alignPeriod = NULL,
                       makeReturns = FALSE, kernelType = "rectangular", kernelParam = 1,
                       kernelDOFadj = TRUE, ...) {

  if (is.xts(rData) && checkMultiDays(rData)) {
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    if (n == 1){
      result <- apply.daily(rData, rKernelCov, cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns,
                            kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj)
      return(result)
    } else {
      result <- applyGetList(rData, rKernelCov, cor = cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns,
                             kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj)
      names(result) <- unique(as.Date(index(rData)))
      return(result)
    }
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if (!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setcolorder(rData, "DT")
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rKernelCov(dat[starts[i]:ends[i], ], cor = cor, alignBy = NULL, alignPeriod = NULL, makeReturns = makeReturns,
                              kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj)
    }

    if (ncol(rData) == 2) { ## Univariate case
      res <- data.table(DT = names(res), RK = as.numeric(res))
    } else if(length(res) == 1){ ## Single day multivariate case
      res <- res[[1]]
    }

    return(res)

  } else { ## Actual calculations
    # # Aggregate:
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    type <- kernelCharToInt(kernelType)
    if (n == 1) {
      return(kernelEstimator(as.double(rData),
                             as.double(rData),
                             as.integer(length(rData)),
                             as.integer(kernelParam),
                             as.integer(ifelse(kernelDOFadj, 1, 0)),
                             as.integer(type),
                             ab = double(kernelParam + 1),
                             ab2 = double(kernelParam + 1)))
    }

    if (n > 1) {
      cov <- matrix(rep(0, n * n), ncol = n, dimnames = list(colnames(rData), colnames(rData)))
      diagonal <- numeric(n)
      for (i in 1:n) {
        diagonal[i] <- kernelEstimator(as.double(rData[, i]), as.double(rData[, i]), as.integer(length(rData[, i])),
                                       as.integer(kernelParam), as.integer(ifelse(kernelDOFadj, 1, 0)),
                                       as.integer(type), ab = double(kernelParam + 1),
                                       ab2 = double(kernelParam + 1))
      }
      diag(cov) <- diagonal

      for (i in 2:n) {
        for (j in 1:(i - 1)) {
          cov[i, j] = cov[j, i] = kernelEstimator(as.double(rData[, i]), as.double(rData[, j]), as.integer(length(rData[, i])),
                                                  as.integer(kernelParam), as.integer(ifelse(kernelDOFadj, 1, 0)),
                                                  as.integer(type), ab = double(kernelParam + 1),
                                                  ab2 = double(kernelParam + 1))
        }
      }
      if (!cor) {
        return(makePsd(cov))
      }
      if (cor) {
        invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
        if (!inherits(invsdmatrix, "try-error")) {
          rcor <- invsdmatrix %*% cov %*% invsdmatrix
          return(rcor)
        }
      }
    }
  }


}

#' Realized kurtosis of highfrequency return series.
#' 
#' @description
#' Calculate the realized kurtosis as defined in Amaya et al. (2015).
#'
#' Assume there are \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' Then, \code{rKurt} is given by
#' \deqn{
#'   \mbox{rKurt}_{t} = \frac{N \sum_{i=1}^{N}(r_{t,i})^4}{\left( \sum_{i=1}^N r_{t,i}^2 \right)^2}.
#'   }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' 
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#' 
#' @references
#' Amaya, D., Christoffersen, P., Jacobs, K., and Vasquez, A. (2015). Does realized skewness and kurtosis predict the cross-section of equity returns? \emph{Journal of Financial Economics}, 118, 135-167.
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#'
#' @examples
#' rk <- rKurt(sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'             alignPeriod = 5, makeReturns = TRUE)
#' rk
#' @keywords highfrequency rKurt
#' @importFrom xts apply.daily
#' @importFrom data.table setcolorder setkey
#' @export
rKurt <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rKurt, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setcolorder(rData, "DT")
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    setkey(rData, DT)
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rKurt(dat[starts[i]:ends[i], ], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    if(ncol(res) == 2){
      colnames(res) <- c("DT", "rKurt")
    } else {
      colnames(res) <- colnames(rData)
    }

    setkey(res, DT)
    return(res)
  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    q <- as.matrix(rData)
    N <- nrow(q)

    rkurt <- N * colSums(q^4) / (colSums(q^2)^2)

    return(rkurt)

  }
}


#' Realized multipower variation
#'
#' @description Calculate the Realized Multipower Variation rMPV, defined in Andersen et al. (2012).
#'
#' Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}. Then, the rMPV is given by
#'   \deqn{
#'     \mbox{rMPV}_{N}(m,p)= d_{m,p} \frac{N^{p/2}}{N-m+1} \sum_{i=1}^{N-m+1}|r_{t,i}|^{p/m} \ldots |r_{t,i+m-1}|^{p/m}
#'   }
#'
#' in which
#'
#' \eqn{d_{m,p} = \mu_{p/m}^{-m}}:
#'
#' \eqn{m}: the window size of return blocks;
#'
#' \eqn{p}: the power of the variation;
#'
#' and \eqn{m} > \eqn{p/2}.
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param m the window size of return blocks. 2 by default.
#' @param p the power of the variation. 2 by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return numeric
#'
#' @references
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' mpv <- rMPV(sampleTData[, list(DT, PRICE)], m = 2, p = 3, alignBy = "minutes",
#'             alignPeriod = 5, makeReturns = TRUE)
#' mpv
#' @keywords highfrequency rMPV
#' @export
rMPV <- function(rData, m = 2, p = 2, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rMPV, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, m = m, p = p)
    return(result)

  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rMPV(dat[starts[i]:ends[i], ], m = m, p = p, makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    if(ncol(res) == 2){
      colnames(res) <- c("DT", "MPV")
    } else {
      colnames(res) <- colnames(rData)
    }
    return(res)

  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    if (m > p/2) {
      m <- as.numeric(m) ##m> p/2
      p <- as.numeric(p)
      q <- as.matrix(rData)

      q <- abs(rollApplyProdWrapper(q, m))
      #q <- abs(rollapply(q,width=m,FUN=prod,align="left"))
      N <- nrow(rData)

      dmp <- (2^((p/m)/2) * gamma((p/m + 1)/2) / gamma(1/2))^(-m)

      rmpv <- dmp * N^(p/2) / (N - m + 1) * colSums(q^(p/m))
      return(rmpv)
    } else{
      warning("Please supply m>p/2 for the arguments m and p")
    }

  }
}

#' Realized outlyingness weighted covariance
#'
#' @description Calculate the Realized Outlyingness Weighted Covariance (rOWCov), defined in Boudt et al. (2008).
#'
#' Let \eqn{r_{t,i}}, for \eqn{i = 1,..., M} be a sample
#' of \eqn{M} high-frequency \eqn{(N \times 1)} return vectors and \eqn{d_{t,i}}
#' their outlyingness given by the squared Mahalanobis distance between
#' the return vector and zero in terms of the reweighted MCD covariance
#' estimate based on these returns.
#'
#' Then, the rOWCov is given by
#' \deqn{
#' \mbox{rOWCov}_{t}=c_{w}\frac{\sum_{i=1}^{M}w(d_{t,i})r_{t,i}r'_{t,i}}{\frac{1}{M}\sum_{i=1}^{M}w(d_{t,i})},
#' }
#' The weight  \eqn{w_{i,\Delta}} is one if the multivariate jump test statistic for \eqn{r_{i,\Delta}} in Boudt et al. (2008) is less
#' than the 99.9\% percentile of the chi-square distribution with \eqn{N} degrees of freedom and zero otherwise.
#' The scalar \eqn{c_{w}} is a correction factor ensuring consistency of the rOWCov for the Integrated Covariance,
#' under the Brownian Semimartingale with Finite Activity Jumps model.
#'
#' @param rData a \eqn{(M x N)} \code{xts} object containing the \eqn{N}
#' return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. 
#' \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param seasadjR a \eqn{(M x N)} \code{xts} object containing
#' the seasonally adjusted returns. This is an optional argument.
#' @param wFunction determines whether
#' a zero-one weight function (one if no jump is detected based on \eqn{d_{t,i}} and 0 otherwise)
#' or
#' Soft Rejection ("SR") weight function is to be used.
#' By default a zero-one weight function (wFunction = "HR") is used.
#' @param alphaMCD a numeric parameter, controlling the size of
#' the subsets over which the determinant is minimized.
#' Allowed values are between 0.5 and 1 and
#'the default is 0.75. See Boudt et al. (2008) or the \code{covMcd} function in the
#'\pkg{\link[robustbase:covMcd]{robustbase}} package.
#' @param alpha is a parameter between 0 and 0.5,
#'that determines the rejection threshold value
#'(see Boudt et al. (2008) for details).
#' @param ... used internally, do not change.
#' 
#' @return an \eqn{N \times N} matrix
#'
#' @details
#' Advantages of the rOWCov compared to the \code{\link{rBPCov}} include a higher statistical efficiency, positive semi-definiteness and affine equi-variance.
#' However, the rOWCov suffers from a curse of dimensionality.
#' The rOWCov gives a zero weight to a return vector
#' if at least one of the components is affected by a jump.
#' In the case of independent jump occurrences, the average proportion of observations
#' with at least one component being affected by jumps increases fast with the dimension
#' of the series. This means that a potentially large proportion of the returns receives
#' a zero weight, due to which the rOWCov can have a low finite sample efficiency in higher dimensions.
#'
#' @references Boudt, K., Croux, C., and Laurent, S. (2008). Outlyingness weighted covariation. \emph{Journal of Financial Econometrics}, 9, 657–684.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' \dontrun{
#' library("xts")
#' # Realized Outlyingness Weighted Variance/Covariance for prices aligned
#' # at 1 minutes.
#'
#' # Univariate:
#' row <- rOWCov(rData = as.xts(sampleOneMinuteData[as.Date(DT) == "2001-08-04",
#'                                                  list(DT, MARKET)]), makeReturns = TRUE)
#' row
#'
#' # Multivariate:
#' rowc <- rOWCov(rData = as.xts(sampleOneMinuteData[as.Date(DT) == "2001-08-04",]),
#'                makeReturns = TRUE)
#' rowc
#' }
#'
#' @keywords volatility
#' @export
rOWCov <- function (rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, seasadjR = NULL,
                    wFunction = "HR" , alphaMCD = 0.75, alpha = 0.001, ...){

  if (is.null(seasadjR)) {
    seasadjR <- rData
  }
  if (isMultiXts(rData)) {
    stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
  }

  # Aggregate:
  if ((!is.null(alignBy))&&(!is.null(alignPeriod))) {
    rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    seasadjR <- fastTickAgregation(seasadjR, alignBy = alignBy, alignPeriod = alignPeriod)
  }
  if (makeReturns) {
    rData <- makeReturns(rData)
    if (!is.null(seasadjR)) {
      seasadjR <- makeReturns(seasadjR)
    }
  }

  if (is.null(dim(rData))) {
    n <- 1
  } else {
    n <- dim(rData)[2]
  }

  if (n == 1) {
    return(ROWVar(rData, seasadjR = seasadjR, wFunction = wFunction, alphaMCD = alphaMCD, alpha = alpha ))
  }

  if (n > 1) {
    if ((dim(rData)[2] < 2)) {
      stop("Your rData object should have at least 2 columns")
    }

    rData <- as.matrix(rData)
    seasadjR <- as.matrix(seasadjR)
    intraT <- nrow(rData)
    N <- ncol(rData)
    perczeroes <- apply(seasadjR, 2, function(x) sum(1 * (x == 0))) / intraT
    select <- c(1:N)[perczeroes < 0.5]
    seasadjRselect <- seasadjR[, select]
    N <- ncol(seasadjRselect)
    MCDobject <- try(robustbase::covMcd(x = seasadjRselect, alpha = alphaMCD))

    if (length(MCDobject$raw.mah) > 1) {
      betaMCD  <- 1-alphaMCD
      asycor   <- betaMCD / pchisq(qchisq(betaMCD, df = N), df = N+2)
      MCDcov   <- (asycor * t(seasadjRselect[MCDobject$best,]) %*% seasadjRselect[MCDobject$best,]) / length(MCDobject$best)
      invMCDcov <- solve(MCDcov)
      outlyingness <- rep(0,intraT)
      for (i in 1:intraT) {
        outlyingness[i] = matrix(seasadjRselect[i,], ncol = N) %*% invMCDcov %*% matrix(seasadjRselect[i,],nrow=N)
      }
    } else {
      stop("MCD cannot be calculated")
    }
    k <- qchisq(p = 1 - alpha, df = N)
    outlierindic <- outlyingness > k
    weights <- rep(1, intraT)

    if (wFunction == "HR") {
      weights[outlierindic] = 0
      wR <- sqrt(weights) * rData
      covariance <- (conHR(di = N, alpha = alpha) * t(wR) %*% wR) / mean(weights)
      if (!cor) {
        return(covariance)
      } else {
        sdmatrix = sqrt(diag(diag(covariance)))
        inv_matrix <- solve(sdmatrix)
        rcor <- inv_matrix %*% covariance %*% inv_matrix
        return(rcor)
      }
    }
    if (wFunction == "SR") {
      weights[outlierindic] <- k/outlyingness[outlierindic]
      wR <- sqrt(weights) * rData
      covariance <- (conhuber(di = N, alpha = alpha) * t(wR) %*% wR) / mean(weights)
      if (!cor) {
        return(covariance)
      } else {
        sdmatrix <- sqrt(diag(diag(covariance)))
        rcor <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
        return(rcor)
      }
    }
  }
}

#' Realized skewness
#'
#' @description Calculate the realized skewness, defined in Amaya et al. (2015).
#'
#' Assume there are \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}. Then, \code{rSkew} is given by
#'   \deqn{
#'     \mbox{rSkew}_{t}= \frac{\sqrt{N} \sum_{i=1}^{N}(r_{t,i})^3}{\left(\sum r_{i,t}^2\right)^{3/2}}.
#'   }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' 
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @references
#' Amaya, D., Christoffersen, P., Jacobs, K., and Vasquez, A. (2015). Does realized skewness and kurtosis predict the cross-section of equity returns? \emph{Journal of Financial Economics}, 118, 135-167.
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#'
#' @examples
#' rs <- rSkew(sampleTData[, list(DT, PRICE)],alignBy ="minutes", alignPeriod =5,
#'             makeReturns = TRUE)
#' rs
#' @keywords highfrequency rSkew
#' @export
rSkew <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rSkew, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    setcolorder(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rSkew(dat[starts[i]:ends[i],], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    if(ncol(res) == 2){
      colnames(res) <- c("DT", "Skew")
    } else {
      colnames(res) <- colnames(rData)
    }

    setkey(res, "DT")

    return(res)
  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    q <- as.matrix(rData)
    N <- nrow(q)

    #rv <- RV(rData)
    rSkew <- sqrt(N)*colSums(q^3)/(colSums(q^2)^(3/2))

    return(rSkew)
  }
}

#' Realized semivariance of highfrequency return series
#' @description 
#' Calculate the realized semivariances, defined in Barndorff-Nielsen et al. (2008).
#'
#' Function returns two outcomes: 
#' 
#' \enumerate{
#' \item Downside realized semivariance 
#' \item Upside realized semivariance.
#' }
#'
#' Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}.
#'
#' Then, the \code{rSV} is given by
#' \deqn{
#'   \mbox{rSVdownside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2  \ \times \ I [ r_{t,i} < 0]
#' }
#'   \deqn{
#'   \mbox{rSVupside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2 \ \times \ I [ r_{t,i} > 0]
#' }
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example to aggregate.
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. 
#' \code{FALSE} by default.
#' @param ... used internally
#' @return list with two entries, the realized positive and negative semivariances
#' @examples
#' sv <- rSV(sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'           alignPeriod = 5, makeReturns = TRUE)
#' sv
#' @references
#' Barndorff-Nielsen, O. E., Kinnebrock, S., and Shephard N. (2010). \emph{Measuring downside risk: realised semivariance}. In: Volatility and Time Series Econometrics: Essays in Honor of Robert F. Engle,
#' (Edited by Bollerslev, T., Russell, J., and Watson, M.), 117-136. Oxford University Press.
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#' @keywords  highfrequency rSV
#' @export
rSV <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    if(ncol(rData) == 1){
      result <- apply.daily(rData, function(x){
        tmp <- rSV(x, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
        return(cbind(tmp[[1]], tmp[[2]]))
      })
      colnames(result) = c("downside", "upside")
    } else {
      # ... is needed because applyGetList calls functions with ... if this argument is removed from rSV we will get an unused argument error
      result <- applyGetList(rData, rSV, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns, ... = ...) 
      names(result) <- unique(as.Date(index(rData)))
      ## Names
    }
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    setcolorder(rData, "DT")
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rSV(dat[starts[i]:ends[i],], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    # res <- setDT(transpose(res))[, DT := dates]
    # setcolorder(res, "DT")
    # if(ncol(res) == 2){
    #   colnames(res) <- c("DT", "SV")
    # } else {
    #   colnames(res) <- colnames(rData)
    # }
    #
    # setkey(res, "DT")
    # setcolorder(res, "DT")
    return(res)

  } else {

    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    q <- as.matrix(rData)
    select.down <- rData < 0
    select.up <- rData > 0
    rSVd <- colSums((q * select.down)^2)
    rSVu <- colSums((q * select.up)^2)

    out <- list(rSVdownside = rSVd, rSVupside = rSVu)
    return(out)
  }
}

#' Threshold Covariance
#' @description
#' Calculate the threshold covariance matrix proposed in Gobbi and Mancini (2009).
#' Unlike the \code{\link{rOWCov}}, the rThresholdCov uses univariate jump detection rules to truncate the effect of jumps on the covariance
#' estimate. As such, it remains feasible in high dimensions, but it is less robust to small cojumps.
#'
#' Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector of \eqn{N} assets where \eqn{i=1,...,M} and
#' \eqn{M} being the number of intraday returns.
#'
#' Then, the \eqn{k,q}-th element of the threshold covariance matrix is defined as
#'
#' \deqn{
#' \mbox{thresholdcov}[k,q]_{t} = \sum_{i=1}^{M} r_{(k)t,i} 1_{\{r_{(k)t,i}^2 \leq TR_{M}\}}  \ \ r_{(q)t,i} 1_{\{r_{(q)t,i}^2 \leq TR_{M}\}},
#' }
#' with the threshold value \eqn{TR_{M}} set to \eqn{9 \Delta^{-1}} times the daily realized bi-power variation of asset \eqn{k},
#' as suggested in Jacod and Todorov (2009).
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. 
#' \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return in case the input is and contains data from one day, an \eqn{N \times N} matrix is returned. If the data is a univariate \code{xts} object with multiple days, an \code{xts} is returned.
#' If the data is multivariate and contains multiple days (\code{xts} or \code{data.table}), the function returns a list containing \eqn{N \times N} matrices. Each item in the list has a name which corresponds to the date for the matrix.
#'
#' @references
#' Barndorff-Nielsen, O. and Shephard, N. (2004). Measuring the impact of jumps in multivariate price processes using bipower covariation. Discussion paper, Nuffield College, Oxford University.
#'
#' Jacod, J. and Todorov, V. (2009). Testing for common arrival of jumps in discretely-observed multidimensional processes. \emph{Annals of Statistics}, 37, 1792-1838.
#'
#' Mancini, C. and Gobbi, F. (2012). Identifying the Brownian covariation from the co-jumps given discrete observations. \emph{Econometric Theory}, 28, 249-273.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples # Realized threshold  Variance/Covariance:
#' # Multivariate:
#' \dontrun{
#' library("xts")
#' set.seed(123)
#' start <- strptime("1970-01-01", format = "%Y-%m-%d", tz = "UTC")
#' timestamps <- start + seq(34200, 57600, length.out = 23401)
#'
#' dat <- cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401))
#'
#' dat <- exp(cumsum(xts(dat, timestamps)))
#' rcThreshold <- rThresholdCov(dat, alignBy = "minutes", alignPeriod = 1, makeReturns = TRUE)
#' rcThreshold
#' }
#'
#' @keywords volatility
#' @export
rThresholdCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...) {

  # Multiday adjustment:
  if (is.xts(rData) && isMultiXts(rData)) {
    if (is.null(dim(rData))) {
      n <- 1
    } else {
      n <- dim(rData)[2]
    }
    if (n == 1) {
      result <- apply.daily(rData, rThresholdCov, cor = cor, alignBy = alignBy,
                            alignPeriod = alignPeriod, makeReturns = makeReturns)
    }
    if (n > 1) {
      result <- applyGetList(rData, rThresholdCov, cor = cor, alignBy = alignBy,
                              alignPeriod = alignPeriod, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }
    return(result)
  }  else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rThresholdCov(dat[starts[i]:ends[i],], cor = cor, makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    if(length(res[[1]]) == 1){ ## Univariate case
      res <- data.table(DT = names(res), ThresholdCov = as.numeric(res))
    } else if(length(res) == 1){ ## Single day multivariate case
      res <- res[[1]]
    }

    return(res)

  } else { #single day code
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    rData <- as.matrix(rData)
    n <- dim(rData)[1]				                  # number of observations
    delta <- 1 / n
    rbpvars <- apply(rData, 2,FUN = RBPVar)		      # bipower variation per stock
    thresholds <- 3 * sqrt(rbpvars) * (delta^(0.49))	  # treshold per stock
    tresmatrix <- matrix(rep(thresholds, n), ncol = length(thresholds), nrow = n, byrow = TRUE)
    condition <- abs(rData) > tresmatrix
    rData[condition] <- 0
    covariance <- rCov(rData)

    if (!cor) {
      return(covariance)
    }
    if (cor) {
      sdmatrix <- sqrt(diag(diag(covariance)))
      rcor     <- solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
      return(rcor)
    }
  }
}


#' Robust two time scale covariance estimation
#'
#' @description Calculate the robust two time scale covariance matrix proposed in Boudt and Zhang (2010).
#' Unlike the \code{\link{rOWCov}}, but similarly to the \code{\link{rThresholdCov}}, the \code{rRTSCov} uses univariate jump detection rules
#' to truncate the effect of jumps on the covariance
#' estimate. By the use of two time scales, this covariance estimate
#' is not only robust to price jumps, but also to microstructure noise and non-synchronic trading.
#'
#' @param pData a list. Each list-item i contains an \code{xts} object with the intraday price data
#' of stock \eqn{i} for day \eqn{t}.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param startIV vector containing the first step estimates of the integrated variance of the assets, needed in the truncation. Is \code{NULL} by default.
#' @param noisevar vector containing the estimates of the noise variance of the assets, needed in the truncation. Is \code{NULL} by default.
#' @param K positive integer, slow time scale returns are computed on prices that are \code{K} steps apart.
#' @param J positive integer, fast time scale returns are computed on prices that are \code{J} steps apart.
#' @param KCov positive integer, for the extradiagonal covariance elements the slow time scale returns are computed on prices that are \code{K} steps apart.
#' @param JCov positive integer, for the extradiagonal covariance elements the fast time scale returns are computed on prices that are \code{J} steps apart.
#' @param KVar vector of positive integers, for the diagonal variance elements the slow time scale returns are computed on prices that are \code{K} steps apart.
#' @param JVar vector of positive integers, for the diagonal variance elements the fast time scale returns are computed on prices that are \code{J} steps apart.
#' @param makePsd boolean, in case it is \code{TRUE}, the positive definite version of rRTSCov is returned. 
#' \code{FALSE} by default.
#' @param eta positive real number, squared standardized high-frequency returns that exceed eta are detected as jumps.
#' @param ... used internally, do not change.
#' 
#' @return an \eqn{N \times N} matrix
#'
#' @details
#' The rRTSCov requires the tick-by-tick transaction prices. 
#' (Co)variances are then computed using log-returns calculated on a rolling basis
#' on stock prices that are \eqn{K} (slow time scale) and \eqn{J} (fast time scale) steps apart.
#'
#' The diagonal elements of the rRTSCov matrix are the variances, computed for log-price series \eqn{X} with \eqn{n} price observations
#' at times \eqn{  \tau_1,\tau_2,\ldots,\tau_n} as follows:
#' \deqn{
#' (1-\frac{\overline{n}_K}{\overline{n}_J})^{-1}(\{X,X\}_T^{(K)^{*}}-\frac{\overline{n}_K}{\overline{n}_J}\{X,X\}_T^{(J)^{*}}),
#' }
#' where \eqn{\overline{n}_K=(n-K+1)/K},  \eqn{\overline{n}_J=(n-J+1)/J} and
#' \deqn{\{X,X\}_T^{(K)^{*}} =\frac{c_\eta^{*}}{K}\frac{\sum_{i=1}^{n-K+1}(X_{t_{i+K}}-X_{t_i})^2I_X^K(i;\eta)}{\frac{1}{n-K+1}\sum_{i=1}^{n-K+1}I_X^K(i;\eta)}.}
#' The constant  \eqn{c_\eta} adjusts for the bias due to the thresholding  and \eqn{I_{X}^K(i;\eta)} is a jump indicator function
#' that is one if
#' \deqn{ \frac{(X_{t_{i+K}}-X_{t_{i}})^2}{(\int_{t_{i}}^{t_{i+K}} \sigma^2_sds +2\sigma_{\varepsilon_{\mbox{\tiny X}}}^2)}  \ \ \leq  \ \    \eta }
#' and zero otherwise.  The elements in the denominator are the integrated variance (estimated recursively) and noise variance (estimated by the method in Zhang et al, 2005).
#'
#' The extradiagonal elements of the rRTSCov are the covariances.
#' For their calculation, the data is first synchronized by the refresh time method proposed by Harris et al (1995).
#' It uses the function \code{\link{refreshTime}} to collect first the so-called refresh times at which all assets have traded at least once
#' since the last refresh time point. Suppose we have two log-price series:  \eqn{X} and \eqn{Y}. Let \eqn{ \Gamma =\{ \tau_1,\tau_2,\ldots,\tau_{N^{\mbox{\tiny X}}_{\mbox{\tiny T}}}\}} and
#' \eqn{\Theta=\{\theta_1,\theta_2,\ldots,\theta_{N^{\mbox{\tiny Y}}_{\mbox{\tiny T}}}\}}
#' be the set of transaction times of these assets.
#' The first refresh time corresponds to the first time at which both stocks have traded, i.e.
#' \eqn{\phi_1=\max(\tau_1,\theta_1)}. The subsequent refresh time is defined as the first time when both stocks have again traded, i.e.
#' \eqn{\phi_{j+1}=\max(\tau_{N^{\mbox{\tiny{X}}}_{\phi_j}+1},\theta_{N^{\mbox{\tiny{Y}}}_{\phi_j}+1})}. The
#' complete refresh time sample grid is
#' \eqn{\Phi=\{\phi_1,\phi_2,...,\phi_{M_N+1}\}}, where \eqn{M_N} is the total number of paired returns.  The
#' sampling points of asset \eqn{X} and \eqn{Y} are defined to be
#' \eqn{t_i=\max\{\tau\in\Gamma:\tau\leq \phi_i\}} and
#' \eqn{s_i=\max\{\theta\in\Theta:\theta\leq \phi_i\}}.
#'
#' Given these refresh times, the covariance is computed as follows:
#' \deqn{
#' c_{N}( \{X,Y\}^{(K)}_T-\frac{\overline{n}_K}{\overline{n}_J}\{X,Y\}^{(J)}_T ),
#' }
#'
#' where
#' \deqn{\{X,Y\}^{(K)}_T =\frac{1}{K} \frac{\sum_{i=1}^{M_N-K+1}c_i (X_{t_{i+K}}-X_{t_{i}})(Y_{s_{i+K}}-Y_{s_{i}})I_{X}^K(i;\eta)
#' I_{Y}^K(i;\eta)}{\frac{1}{M_N-K+1}\sum_{i=1}^{M_N-K+1}{I_X^K(i;\eta)I_Y^K(i;\eta)}},}
#' with  \eqn{I_{X}^K(i;\eta)} the same jump indicator function as for the variance and \eqn{c_N} a constant to adjust for the bias due to the thresholding.
#'
#' Unfortunately, the rRTSCov is not always positive semidefinite.
#' By setting the argument \code{makePsd = TRUE}, the function  \code{\link{makePsd}} is used to return a positive semidefinite
#' matrix. This function replaces the negative eigenvalues with zeroes.
#'
#' @references
#' Boudt K. and Zhang, J. 2010. Jump robust two time scale covariance estimation and realized volatility budgets. Mimeo.
#'
#' Harris, F., McInish, T., Shoesmith, G., and Wood, R. (1995). Cointegration, error correction, and price discovery on informationally linked security markets. \emph{Journal of Financial and Quantitative Analysis}, 30, 563-581.
#'
#' Zhang, L., Mykland, P. A., and Ait-Sahalia, Y. (2005). A tale of two time scales: Determining integrated volatility with noisy high-frequency data. \emph{Journal of the American Statistical Association}, 100, 1394-1411.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#' @examples
#' \dontrun{
#' library(xts)
#' set.seed(123)
#' start <- strptime("1970-01-01", format = "%Y-%m-%d", tz = "UTC")
#' timestamps <- start + seq(34200, 57600, length.out = 23401)
#'
#' dat <- cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401))
#'
#' dat <- exp(cumsum(xts(dat, timestamps)))
#' price1 <- dat[,1]
#' price2 <- dat[,2]
#' rcRTS <- rRTSCov(pData = list(price1, price2))
#' # Note: List of prices as input
#' rcRTS
#' }
#'
#' @keywords volatility
#' @export
rRTSCov <- function (pData, cor = FALSE, startIV = NULL, noisevar = NULL,
                     K = 300, J = 1,
                     KCov = NULL , JCov = NULL,
                     KVar = NULL , JVar = NULL ,
                     eta = 9, makePsd = FALSE, ...){
  if (!is.list(pData)) {
    n <- 1
  }
  else {
    n = length(pData)
    if (n == 1) {
      pData = pData[[1]]
    }
  }

  if (n == 1) {
    if (nrow(pData) < (10 * K) ) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways.
           Please provide a timeseries of at least 10*K" )
    }
    if (isMultiXts(pData)) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input.")
    }
    return(RTSRV(pData, startIV = startIV, noisevar = noisevar,
                 K = K, J = J, eta = eta))
  }
  if (n > 1) {
    if (nrow(pData[[1]]) < (10*K)) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways.
           Please provide a timeseries of at least 10*K" )
    }

    if (isMultiXts(pData[[1]])) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input.")
    }

    cov <- matrix(rep(0, n * n), ncol = n)
    diagonal <- numeric(n)
    if (is.null(KCov)) {
      KCov <- K
    }
    if (is.null(JCov)) {
      JCov <- J
    }
    if (is.null(KVar)) {
      KVar <- rep(K,n)
    }
    if (is.null(JVar)) {
      JVar <- rep(J,n)
    }
    for (i in 1:n){
      diagonal[i] <- RTSRV(pData[[i]], startIV = startIV[i],
                           noisevar = noisevar[i], K = KVar[i], J = JVar[i],
                           eta = eta)
    }
    diag(cov) <- diagonal
    if (is.null(KCov)) { KCov = K }
    if (is.null(JCov)) { JCov = J }
    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        cov[i, j] = cov[j, i] = RTSCov_bi(pData[[i]],
                                          pData[[j]], startIV1 = diagonal[i], startIV2 = diagonal[j],
                                          noisevar1 = noisevar[i], noisevar2 = noisevar[j],
                                          K = KCov, J = JCov, eta = eta)
      }
    }
    if (!cor) {
      if (makePsd) {
        cov = makePsd(cov)
      }
      return(cov)
    } else {
      invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
      if (!inherits(invsdmatrix, "try-error")) {
        rcor = invsdmatrix %*% cov %*% invsdmatrix
        if (makePsd) {
          rcor = makePsd(rcor)
        }
        return(rcor)
      }
    }
  }
}

#' An estimator of realized variance.
#' @param rData a \code{xts} object containing all returns in period t for one asset.
#' @param ... used internally, do not change.
#' @return numeric
#'
#' @keywords highfrequency RV
#' @export
RV <- function(rData, ...) {
  returns <- as.numeric(rData)
  RV <- sum(returns^2)
  return(RV)
}


#' Realized tri-power quarticity
#'
#' @description Calculate the rTPQuar, defined in Andersen et al. (2012).
#'
#'  Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}. Then, the rTPQuar is given by
#'  \deqn{
#'    \mbox{rTPQuar}_{t}=N\frac{N}{N-2} \left(\frac{\Gamma \left(0.5\right)}{ 2^{2/3}\Gamma \left(7/6\right)} \right)^{3} \sum_{i=3}^{N} \mbox({|r_{t,i}|}^{4/3} {|r_{t,i-1}|}^{4/3} {|r_{t,i-2}|}^{4/3})
#'  }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. 
#' Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}.
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' 
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @references 
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#'
#' @examples
#' tpq <- rTPQuar(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'               alignPeriod = 5, makeReturns = TRUE)
#' tpq
#'
#' @keywords highfrequency rTPQuar
#' @export
rTPQuar <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rTPQuar, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rTPQuar(dat[starts[i]:ends[i], ], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    colnames(res) <- colnames(rData)
    return(res)

  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }

    q <- abs(as.matrix(rData))
    q <- rollApplyProdWrapper(q, 3)
    N <- nrow(q)+2
    rTPVar <- N * (N/(N - 2)) * ((gamma(0.5)/(2^(2/3)*gamma(7/6) ))^3) * colSums(q^(4/3))
    return(rTPVar)
  }
}

#' Realized quad-power variation of intraday returns
#' 
#' @description Calculate the realized quad-power variation, defined in Andersen et al. (2012).
#'
#'  Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}. Then, the rQPVar is given by
#'  \deqn{
#'    \mbox{rQPVar}_{t}=N*\frac{N}{N-3} \left(\frac{\pi^2}{4} \right)^{-4} \mbox({|r_{t,i}|} {|r_{t,i-1}|} {|r_{t,i-2}|} {|r_{t,i-3}|})
#'  }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#'
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @references 
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup
#'
#' @examples
#' qpv <- rQPVar(rData= sampleTData[, list(DT, PRICE)], alignBy= "minutes",
#'               alignPeriod =5, makeReturns= TRUE)
#' qpv
#'
#' @keywords highfrequency rQPVar
#' @export
rQPVar <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE, ...) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rQPVar, alignBy, alignPeriod, makeReturns)
    return(result)
  } else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if (!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rQPVar(dat[starts[i]:ends[i],], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    colnames(res) <- colnames(rData)
    return(res)

  } else {
    ## Do data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- abs(as.matrix(rData))
    q <- rollApplyProdWrapper(q, 4)
    #q <- rollapply(q, width = 4, FUN = prod, align = "left")
    N <- nrow(q) + 3
    rQPVar <- N * (N / (N-3)) * (pi^2 / 4) * colSums(q)
    return(rQPVar)
  }
}

#' Realized quarticity
#' @description  Calculate the realized quarticity (rQuar), defined in Andersen et al. (2012).
#'
#' Assume there are \eqn{N} equispaced returns \eqn{r_{t,i}} in period \eqn{t}, \eqn{i=1, \ldots,N}.
#'
#'  Then, the rQuar is given by
#'  \deqn{
#'    \mbox{rQuar}_{t}=\frac{N}{3} \sum_{i=1}^{N} \mbox(r_{t,i}^4)
#'  }
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#'
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#'
#' @return 
#' \itemize{
#' \item In case the input is an \code{xts} object with data from one day, a numeric of the same length as the number of assets.
#' \item If the input data spans multiple days and is in \code{xts} format, an \code{xts} will be returned.
#' \item If the input data is a \code{data.table} object, the function returns a \code{data.table} with the same column names as the input data, containing the date and the realized measures.
#' }
#'
#' @author Giang Nguyen, Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#' @references 
#' Andersen, T. G., Dobrev, D., and Schaumburg, E. (2012). Jump-robust volatility estimation using nearest neighbor truncation. \emph{Journal of Econometrics}, 169, 75-93.
#' @examples
#' rq <- rQuar(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'             alignPeriod = 5, makeReturns = TRUE)
#' rq
#' @keywords  highfrequency rQuar
#' @export
rQuar <- function(rData, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE) {

  # self-reference for multi-day input
  if (is.xts(rData) && checkMultiDays(rData)) {
    result <- apply.daily(rData, rQuar, alignBy, alignPeriod, makeReturns)
    return(result)
  }  else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rQuar(dat[starts[i]:ends[i], ], makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }
    res <- setDT(transpose(res))[, DT := dates]
    setcolorder(res, "DT")
    colnames(res) <- colnames(rData)
    return(res)

  } else {
    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    q <- as.matrix(rData)
    N <- nrow(q) + 1
    rQuar <- N/3 * colSums(q^4)
    return(rQuar)
  }
}

#' Two time scale covariance estimation
#'
#' @description Calculate the two time scale covariance matrix proposed in Zhang et al. (2005) and Zhang (2010).
#' By the use of two time scales, this covariance estimate
#' is robust to microstructure noise and non-synchronic trading.
#'
#' @param pData a list. Each list-item i contains an \code{xts} object with the intraday price data
#' of stock \eqn{i} for day \eqn{t}.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param K positive integer, slow time scale returns are computed on prices that are \code{K} steps apart.
#' @param J positive integer, fast time scale returns are computed on prices that are \code{J} steps apart.
#' @param KCov positive integer, for the extradiagonal covariance elements the slow time scale returns are computed on prices that are \code{K} steps apart.
#' @param JCov positive integer, for the extradiagonal covariance elements the fast time scale returns are computed on prices that are \code{J} steps apart.
#' @param KVar vector of positive integers, for the diagonal variance elements the slow time scale returns are computed on prices that are \code{K} steps apart.
#' @param JVar vector of positive integers, for the diagonal variance elements the fast time scale returns are computed on prices that are \code{J} steps apart.
#' @param makePsd boolean, in case it is \code{TRUE}, the positive definite version of \code{rTSCov} is returned. \code{FALSE} by default.
#' @param ... used internally, do not change.
#' 
#' @return in case the input is and contains data from one day, an N by N matrix is returned. If the data is a univariate \code{xts} object with multiple days, an \code{xts} is returned.
#' If the data is multivariate and contains multiple days (\code{xts} or \code{data.table}), the function returns a list containing N by N matrices. Each item in the list has a name which corresponds to the date for the matrix.
#'
#' @details The rTSCov requires the tick-by-tick transaction prices. (Co)variances are then computed using log-returns calculated on a rolling basis
#' on stock prices that are \eqn{K} (slow time scale) and \eqn{J} (fast time scale) steps apart.
#'
#' The diagonal elements of the rTSCov matrix are the variances, computed for log-price series \eqn{X} with \eqn{n} price observations
#' at times \eqn{  \tau_1,\tau_2,\ldots,\tau_n} as follows:
#'
#' \deqn{(1-\frac{\overline{n}_K}{\overline{n}_J})^{-1}([X,X]_T^{(K)}-
#'        \frac{\overline{n}_K}{\overline{n}_J}[X,X]_T^{(J))}}
#'
#' where \eqn{\overline{n}_K=(n-K+1)/K},  \eqn{\overline{n}_J=(n-J+1)/J} and
#' \deqn{[X,X]_T^{(K)} =\frac{1}{K}\sum_{i=1}^{n-K+1}(X_{t_{i+K}}-X_{t_i})^2.}
#'
#' The extradiagonal elements of the rTSCov are the covariances.
#' For their calculation, the data is first synchronized by the refresh time method proposed by Harris et al (1995).
#' It uses the function \code{\link{refreshTime}} to collect first the so-called refresh times at which all assets have traded at least once
#' since the last refresh time point. Suppose we have two log-price series:  \eqn{X} and \eqn{Y}. Let \eqn{ \Gamma =\{ \tau_1,\tau_2,\ldots,\tau_{N^{\mbox{\tiny X}}_{\mbox{\tiny T}}}\}} and
#' \eqn{\Theta=\{\theta_1,\theta_2,\ldots,\theta_{N^{\mbox{\tiny Y}}_{\mbox{\tiny T}}}\}}
#' be the set of transaction times of these assets.
#' The first refresh time corresponds to the first time at which both stocks have traded, i.e.
#' \eqn{\phi_1=\max(\tau_1,\theta_1)}. The subsequent refresh time is defined as the first time when both stocks have again traded, i.e.
#' \eqn{\phi_{j+1}=\max(\tau_{N^{\mbox{\tiny{X}}}_{\phi_j}+1},\theta_{N^{\mbox{\tiny{Y}}}_{\phi_j}+1})}. The
#' complete refresh time sample grid is
#' \eqn{\Phi=\{\phi_1,\phi_2,...,\phi_{M_N+1}\}}, where \eqn{M_N} is the total number of paired returns.  The
#' sampling points of asset \eqn{X} and \eqn{Y} are defined to be
#' \eqn{t_i=\max\{\tau\in\Gamma:\tau\leq \phi_i\}} and
#' \eqn{s_i=\max\{\theta\in\Theta:\theta\leq \phi_i\}}.
#'
#' Given these refresh times, the covariance is computed as follows:
#' \deqn{
#' c_{N}( [X,Y]^{(K)}_T-\frac{\overline{n}_K}{\overline{n}_J}[X,Y]^{(J)}_T ),
#' }
#'
#' where
#' \deqn{[X,Y]^{(K)}_T =\frac{1}{K} \sum_{i=1}^{M_N-K+1} (X_{t_{i+K}}-X_{t_{i}})(Y_{s_{i+K}}-Y_{s_{i}}).}
#'
#' Unfortunately, the rTSCov is not always positive semidefinite.
#' By setting the argument makePsd = TRUE, the function \code{\link{makePsd}} is used to return a positive semidefinite
#' matrix. This function replaces the negative eigenvalues with zeroes.
#'
#' @references
#' Harris, F., McInish, T., Shoesmith, G., and Wood, R. (1995). Cointegration, error correction, and price discovery on informationally linked security markets. \emph{Journal of Financial and Quantitative Analysis}, 30, 563-581.
#'
#' Zhang, L., Mykland, P. A., and Ait-Sahalia, Y. (2005). A tale of two time scales: Determining integrated volatility with noisy high-frequency data. \emph{Journal of the American Statistical Association}, 100, 1394-1411.
#'
#' Zhang, L. (2011). Estimating covariation: Epps effect, microstructure noise. \emph{Journal of Econometrics}, 160, 33-47.
#'
#' @author Jonathan Cornelissen, Kris Boudt, and Emil Sjoerup.
#' @examples
#' # Robust Realized two timescales Variance/Covariance
#' # Multivariate:
#' \dontrun{
#' library(xts)
#' set.seed(123)
#' start <- strptime("1970-01-01", format = "%Y-%m-%d", tz = "UTC")
#' timestamps <- start + seq(34200, 57600, length.out = 23401)
#'
#' dat <- cbind(rnorm(23401) * sqrt(1/23401), rnorm(23401) * sqrt(1/23401))
#'
#' dat <- exp(cumsum(xts(dat, timestamps)))
#' price1 <- dat[,1]
#' price2 <- dat[,2]
#' rcovts <- rTSCov(pData = list(price1, price2))
#' # Note: List of prices as input
#' rcovts
#' }
#'
#' @keywords volatility
#' @importFrom data.table is.data.table
#' @importFrom xts as.xts
#' @export
rTSCov <- function (pData, cor = FALSE, K = 300, J = 1, KCov = NULL, JCov = NULL,
                    KVar = NULL, JVar = NULL, makePsd = FALSE, ...) {
  
  if(is.data.table(pData)){
    pData <- as.xts(pData)
  }
  if (!is.list(pData)) {
    n <- 1
  }
  else {
    n <- length(pData)
    if (n == 1) {
      pData <- pData[[1]]
    }
  }
  if (n == 1) {
    if (nrow(pData) < (10 * K)) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways.
           Please provide a timeseries of at least 10 * K." )
    }
    if (isMultiXts(pData)) {
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input");
    }
    return(TSRV(pData, K = K, J = J))
  }
  if (n > 1) {
    if (nrow(pData[[1]]) < (10 * K)) {
      stop("Two time scale estimator uses returns based on prices that are K ticks aways.
           Please provide a timeseries of at least 10*K" )
    }
    if (isMultiXts(pData[[1]])){
      stop("This function does not support having an xts object of multiple days as input. Please provide a timeseries of one day as input")
    }

    cov <- matrix(rep(0, n * n), ncol = n)
    if (is.null(KCov)) {
      KCov <- K
    }
    if (is.null(JCov)) {
      JCov <- J
    }
    if (is.null(KVar)) {
      KVar <- rep(K,n)
    }
    if (is.null(JVar)) {
      JVar <- rep(J,n)
    }

    diagonal <- numeric(n)
    for (i in 1:n) {
      diagonal[i] = TSRV(pData[[i]], K = KVar[i], J = JVar[i])
    }
    diag(cov) <- diagonal

    for (i in 2:n) {
      for (j in 1:(i - 1)) {
        cov[i, j] = cov[j, i] = TSCov_bi(pData[[i]],
                                         pData[[j]], K = KCov, J = JCov)
      }
    }
    if (!cor) {
      if (makePsd) {
        cov <- makePsd(cov)
      }
      return(cov)
    } else {
      invsdmatrix <- try(solve(sqrt(diag(diag(cov)))), silent = F)
      if (!inherits(invsdmatrix, "try-error")) {
        rcor <- invsdmatrix %*% cov %*% invsdmatrix
        if (makePsd) {
          rcor <- makePsd(rcor)
        }
        return(rcor)
      }
    }
  }
}



#' @title CholCov estimator
#' 
#' 
#' @description
#' Positive semi-definite covariance estimation using the CholCov algorithm.
#' The algorithm estimates the integrated covariance matrix by sequentially adding series and using `refreshTime` to synchronize the observations.
#' This is done in order of liquidity, which means that the algorithm uses more data points than most other estimation techniques.
#' @param pData a list. Each list-item i contains an \code{xts} object with the intraday price data
#' of stock \eqn{i} for day \eqn{t}. The order of the data does not matter as it will be sorted according to the criterion specified in the \code{criterion} argument
#' @param IVest integrated variance estimator, default is \code{"rMRCov"}. For a list of implemented estimators, use \code{listCholCovEstimators()}.
#' @param COVest covariance estimator, default is \code{"rMRCov"}. For a list of implemented estimators, use \code{listCholCovEstimators()}.
#' @param criterion criterion to use for sorting the data according to liquidity. 
#' Possible values are \code{"squared duration"}, \code{"duration"}, \code{"count"}, defaults to \code{"squared duration"}.
#' @param ... additional arguments to pass to \code{IVest} and \code{COVest}. See details.
#'
#' @return a list containing the covariance matrix \code{"CholCov"}, and the Cholesky decomposition \code{"L"} and \code{"G"} such that \eqn{\code{L} \times \code{G} \times \code{L}' = \code{CholCov}}.
#'
#' @details
#' Additional arguments for \code{IVest} and \code{COVest} should be passed in the ... argument.
#' For the \code{rMRCov} estimator, which is the default, the \code{theta} and \code{delta} parameters can be set. These default to 1 and 0.1 respectively.
#' 
#' The CholCov estimation algorithm is useful for estimating covariances of \eqn{d} series that are sampled asynchronously and with different liquidities.
#' The CholCov estimation algorithm is as follows:
#' 
#' \itemize{
#'     \item First sort the series in terms of decreasing liquidity according to a liquidity criterion, such that series \eqn{1} is the most liquid, and series \eqn{d} the least.
#'     \item Step 1:
#'     
#'     Apply refresh-time on \eqn{{a} = \{1\}} to obtain the grid \eqn{\tau^{a}}. 
#'     
#'     Estimate \eqn{\hat{g}_{11}} using an IV estimator on  \eqn{f_{\tau^{a}_j}^{(1)}= \hat{u}_{\tau^{a}_j}^{(1)}}.
#'     
#'     \item Step 2:
#'     
#'     Apply refresh-time on \eqn{{b} = \{1,2\}} to obtain the grid \eqn{\tau^{b}}. 
#'     
#'     Estimate \eqn{\hat{h}^{b}_{21}} as the realized beta between \eqn{f_{\tau^{b}_j}^{(1)}} and \eqn{\hat{u}_{\tau^{b}_j}^{(2)}$. Set $\hat{h}_{21}=\hat{h}^{b}_{21}}.
#'     
#'     Estimate \eqn{\hat{g}_{22}} using an IV estimator on  \eqn{f_{\tau^{b}_j}^{(2)}= \hat{u}_{\tau^{b}_j}^{(2)}-\hat{h}_{21}f_{\tau^{b}_j}^{(1)}}. 
#'     
#'     \item  Step 3:
#'     
#'     Apply refresh-time on \eqn{{c} = \{1,3\}} to obtain the grid \eqn{\tau^{c}}. 
#'     
#'     Estimate \eqn{\hat{h}^{c}_{31}} as the realized beta between \eqn{f_{\tau^{c}_j}^{(1)}} and \eqn{\hat{u}_{\tau^{c}_j}^{(3)}}. Set \eqn{\hat{h}_{31}= \hat{h}^{c}_{31}}.
#'     
#'     Apply refresh-time on \eqn{{d} = \{1,2,3\}} to obtain the grid \eqn{\tau^{d}}.
#'     
#'     Re-estimate \eqn{\hat{h}_{21}^{d}} at the new grid, such that the projections \eqn{f_{\tau^{d}_j}^{(1)}} and \eqn{f_{\tau^{d}_j}^{(2)}} are orthogonal.
#'     
#'     Estimate \eqn{\hat{h}^{d}_{32}} as the realized beta between \eqn{f_{\tau^{d}_j}^{(2)}} and \eqn{\hat{u}_{\tau^{d}_j}^{(3)}}. Set \eqn{\hat{h}_{32} = \hat{h}^{d}_{32}}. 
#'     
#'     Estimate \eqn{\hat{g}_{33}} using an IV estimator on \eqn{f_{\tau^{d}_j}^{(3)}= \hat{u}_{\tau^{d}_j}^{(3)}-\hat{h}_{32}f_{\tau^{d}_j}^{(2)} -\hat{h}_{31}f_{\tau^{d}_j}^{(1)}}. 
#'     
#'     \item Step 4 to d:
#'     
#'     Continue in the same fashion by sampling over \eqn{{1,...,k,l}} to estimate \eqn{h_{lk}} using the smallest possible set. 
#'     
#'     Re-estimate the \eqn{h_{nm}} with \eqn{m<n\leq k} at every new grid to obtain orthogonal projections. 
#'     
#'     Estimate the \eqn{g_{kk}} as the IV of projections based on the final estimates, \eqn{\hat{h}}.
#' }
#'
#' @references
#' Boudt, K., Laurent, S., Lunde, A., Quaedvlieg, R., and Sauri, O. (2017). Positive semidefinite integrated covariance estimation, factorizations and asynchronicity. \emph{Journal of Econometrics}, 196, 347-367.
#' @author Emil Sjoerup
#'
#' @importFrom xts xts
#' @importFrom zoo coredata
#' @export
rCholCov <- function(pData, IVest = "rMRCov", COVest = "rMRCov", criterion = "squared duration", ...){

  if (!is.list(pData)) {
    stop("pData must be a list of atleast length one")
  }
  if (!all(as.logical(lapply(pData, is.xts)))) {
    stop("All the series in pData must be xts objects")
  }
  if (criterion == "squared duration") {
    criterion <- function(x) sum(as.numeric(diff(index(x)))^2)
  } else if( criterion == "duration") {
    criterion <- function(x) sum(as.numeric(diff(index(x))))
  } else if( criterion == "count") {
    criterion <- function(x) length(x)
  } else {
    stop("Criterion must be either 'squared duration' or 'duration' or 'count'")
  }

  if (!(IVest %in% listCholCovEstimators() & COVest %in% listCholCovEstimators())) {
    stop("rCholCov IVest or COVest not in the available CholCov estimators. See listCholCovEstimators() for list of implemented estimators.")
  }


  options <- list(...)
  op <- list("delta" = 0.1, "theta" = 1, "alignBy" = "minutes", "alignPeriod" = 5, "kernelType" = "rectangular", "kernelParam" = 1, "kernelDOFadj" = TRUE,
             "startIV" = NULL, "noisevar" = NULL, "K" = 300, "J" = 1, "KCov" = NULL, "JCov" = NULL, "KVar" = NULL, "JVar" = NULL, "eta" = 9, "makePsd" = FALSE, "k" = 1)
  op[names(options)] <- options
  delta <- op[["delta"]]
  theta <- op[["theta"]]
  alignBy <- op[["alignBy"]]
  alignPeriod <- op[["alignPeriod"]]
  kernelType <- op[["kernelType"]]
  kernelParam <- op[["kernelParam"]]
  kernelDOFadj <- op[["kernelDOFadj"]]
  startIV <- op[["startIV"]]
  noisevar <- op[["noisevar"]]
  KCov <- op[["KCov"]]
  JCov <- op[["JCov"]]
  KVar <- op[["KVar"]]
  JVar <- op[["JVar"]]
  eta <- op[["eta"]]
  makePsd <- op[["makePsd"]]
  K <- op[["K"]]
  J <- op[["J"]]
  k <- op[["k"]]

  if (length(delta) != 1 | !is.numeric(delta)) {
    stop("delta must be a numeric of length one")
  }
  if (length(theta) != 1 | !is.numeric(theta)) {
    stop("theta must be a numeric of length one")
  }

  vec <- sort(sapply(pData, criterion), index.return = TRUE)$ix
  nameVec <- names(pData)[vec]

  D <- length(pData)

  G <- matrix(0, D, D)
  Ltemp <- L <- diag(1,D,D)

  #G[1,1] <- rCov(exp(pData[[vec[1]]]), makeReturns = TRUE)

    for (d in 1:D) {

      dat <- refreshTime(lapply(vec[1:d], function(x) pData[[x]]))
      returns <- diff(dat)[-1,]
      f <- matrix(0, nrow(returns), d)
      f[,1] <- returns[,1]
      if(d>1){ # We shouldn't do this on the first pass.
        for (l in 2:d) {

          for (m in 1:(l-1)) {

            COV <- switch(COVest,
                   rMRCov = cholCovrMRCov(as.matrix(coredata(cbind(returns[,l], f[,m]))), delta = delta, theta = theta),
                   rCov = rCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rAVGCov = rAVGCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = TRUE),
                   rBPCov = rBPCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rHYCov = rHYCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rKernelCov = rKernelCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod,
                                           makeReturns = TRUE, kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj),
                   rOWCov = rOWCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rRTSCov = rRTSCov(exp(cumsum(cbind(returns[,l], f[,m]))), cor = FALSE, startIV = startIV, noisevar = noisevar, K = K, J = J,
                                     KCov = KCov, JCov=JCov, KVar=KVar, JVar = JVar, eta = eta, makePsd = makePsd ),
                   rThresholdCov = rThresholdCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                   rSemiCov = rSemiCov(exp(cumsum(cbind(returns[,l], f[,m]))), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE)
                   )


            Ltemp[l,m] <- COV[1,2]/COV[2,2]

          }

          f[,l] <- returns[,l] - f[,1:l] %*% Ltemp[l,1:l]

        }
      }
      L[d ,] <- Ltemp[d,]


      # In this switch, we need to use xts on the data to get the aggregation to work
      G[d,d] <- switch(IVest,
                       rMRCov = cholCovrMRCov(as.matrix(coredata(f[,d])), delta = delta, theta = theta),
                       rCov =          rCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rAVGCov =       rAVGCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, k = k, makeReturns = TRUE),
                       rBPCov =        rBPCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rHYCov =        rHYCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rKernelCov =    rKernelCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod,
                                                  makeReturns = TRUE, kernelType = kernelType, kernelParam = kernelParam, kernelDOFadj = kernelDOFadj),
                       rOWCov =        rOWCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rRTSCov =       rRTSCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), cor = FALSE, startIV = startIV, noisevar = noisevar, K = K, J = J,
                                               KCov = KCov, JCov=JCov, KVar=KVar, JVar = JVar, eta = eta, makePsd = makePsd ),
                       rThresholdCov = rThresholdCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE),
                       rSemiCov = rSemiCov(xts(exp(cumsum(f[,d])), order.by = index(returns)), alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = TRUE)
                       )
    }



  CholCov <- L %*% G %*% t(L)

  colnames(L) <- rownames(L) <- colnames(G) <- rownames(G) <- colnames(CholCov) <- rownames(CholCov) <- nameVec

  out <- list("CholCov" = CholCov, "L" = L, "G" = G)
  return(out)

}



#' Realized semicovariance
#'
#' @description Calculate the Realized Semicovariances (rSemiCov).
#' Let \eqn{ r_{t,i} } be an intraday \eqn{M x N} return matrix and \eqn{i=1,...,M}
#' the number of intraday returns. Then, let \eqn{r_{t,i}^{+} = max(r_{t,i},0)} and \eqn{r_{t,i}^{-} = min(r_{t,i},0)}.
#'
#' Then, the realized semicovariance is given by the following three matrices:
#'
#'\deqn{
#'  \mbox{pos}_t =\sum_{i=1}^{M} r^{+}_{t,i} r^{+'}_{t,i}
#'}
#'\deqn{
#'  \mbox{neg}_t =\sum_{i=1}^{M} r^{-}_{t,i} r^{-'}_{t,i}
#'}
#'\deqn{
#'  \mbox{mixed}_t =\sum_{i=1}^{M} (r^{+}_{t,i} r^{-'}_{t,i} + r^{-}_{t,i} r^{+'}_{t,i})
#'}
#'
#' The mixed covariance matrix will have 0 on the diagonal.
#' From these three matrices, the realized covariance can be constructed as \eqn{pos + neg + mixed}.
#' The concordant semicovariance matrix is \eqn{pos + neg}.
#' The off-diagonals of the concordant matrix is always positive, while for the mixed matrix, it is always negative.
#'
#'
#'
#' @param rData an \code{xts} or \code{data.table} object containing returns or prices, possibly for multiple assets over multiple days.
#' @param cor boolean, in case it is \code{TRUE}, and the input data is multivariate, the correlation is returned instead of the covariance matrix. \code{FALSE} by default.
#' @param alignBy character, indicating the time scale in which \code{alignPeriod} is expressed. Possible values are: \code{"secs"}, \code{"seconds"}, \code{"mins"}, \code{"minutes"}, \code{"hours"}
#' @param alignPeriod positive numeric, indicating the number of periods to aggregate over. For example, to aggregate
#' based on a 5-minute frequency, set \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param makeReturns boolean, should be \code{TRUE} when \code{rData} contains prices instead of returns. \code{FALSE} by default.
#'
#' @return In case the data consists of one day a list of five \eqn{N \times N} matrices are returned. These matrices are named \code{mixed}, \code{positive}, \code{negative}, \code{concordant}, and \code{rCov}.
#' The latter matrix corresponds to the realized covariance estimator and is thus named like the function \code{\link{rCov}}.
#' In case the data spans more than one day, the list for each day will be put into another list named according to the date of the estimates.
#'
#' @details In the case that cor is \code{TRUE}, the mixed matrix will be an \eqn{N \times N} matrix filled with NA as mapping the mixed covariance matrix into correlation space is impossible due to the 0-diagonal.
#'
#' @examples
#' # Realized semi-variance/semi-covariance for prices aligned
#' # at 5 minutes.
#'
#' # Univariate:
#' rSV = rSemiCov(rData = sampleTData[, list(DT, PRICE)], alignBy = "minutes",
#'                    alignPeriod = 5, makeReturns = TRUE)
#' rSV
#' \dontrun{
#' library("xts")
#' # Multivariate multi day:
#' rSC <- rSemiCov(sampleOneMinuteData, makeReturns = TRUE) # rSC is a list of lists
#' # We extract the covariance between stock 1 and stock 2 for all three covariances.
#' mixed <- sapply(rSC, function(x) x[["mixed"]][1,2])
#' neg <- sapply(rSC, function(x) x[["negative"]][1,2])
#' pos <- sapply(rSC, function(x) x[["positive"]][1,2])
#' covariances <- xts(cbind(mixed, neg, pos), as.Date(names(rSC)))
#' colnames(covariances) <- c("mixed", "neg", "pos")
#' # We make a quick plot of the different covariances
#' plot(covariances)
#' addLegend(lty = 1) # Add legend so we can distinguish the series.
#' }
#' @author Emil Sjoerup.
#'
#' @references
#' Bollerslev, T., Li, J., Patton, A. J., and Quaedvlieg, R. (2020). Realized semicovariances. \emph{Econometrica}, 88, 1515-1551.
#' @keywords volatility
#' @importFrom data.table between
#' @export
rSemiCov <- function(rData, cor = FALSE, alignBy = NULL, alignPeriod = NULL, makeReturns = FALSE){

  if (is.xts(rData) && checkMultiDays(rData)) {
    if (ncol(rData) == 1) {
      result <- apply.daily(rData, rSemiCov, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
    } else {
      result <- applyGetList(rData, rSemiCov, cor=cor, alignBy = alignBy, alignPeriod = alignPeriod, makeReturns = makeReturns)
      names(result) <- unique(as.Date(index(rData)))
    }
    return(result)
  }  else if (is.data.table(rData)){
    DATE <- .N <- DT <- NULL
    if(!is.null(alignBy) && !is.null(alignPeriod) && makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    if(!is.null(alignBy) && !is.null(alignPeriod) && !makeReturns) {
      rData <- fastTickAgregation_DATA.TABLE_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }

    setkey(rData, "DT")
    dates <- rData[, list(end = .N), by = list(DATE = as.Date(DT))][, `:=`(end = cumsum(end), DATE = as.character(DATE))][, start := shift(end, fill = 0) + 1]
    res <- vector(mode = "list", length = nrow(dates))
    names(res) <- as.character(dates$DATE)
    starts <- dates$start
    ends <- dates$end
    dates <- dates$DATE
    dat <- as.matrix(rData[, !"DT"])
    for (i in 1:length(dates)) {
      res[[dates[i]]] <- rSemiCov(dat[starts[i]:ends[i], ], cor = cor, makeReturns = makeReturns, alignBy = NULL, alignPeriod = NULL)
    }

    return(res)

  } else {

    ## DO data transformations
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && makeReturns) {
      rData <- fastTickAgregation(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if ((!is.null(alignBy)) && (!is.null(alignPeriod)) && !makeReturns) {
      rData <- fastTickAgregation_RETURNS(rData, alignBy = alignBy, alignPeriod = alignPeriod)
    }
    if (makeReturns) {
      rData <- makeReturns(rData)
    }
    N <- ncol(rData)
    rData <- as.matrix(rData)
    # create p and n
    pos <- pmax(rData, 0)
    neg <- pmin(rData, 0)

    # calculate the mixed covariance (variances will be 0)
    mixCov <- t(pos) %*% neg + t(neg) %*% pos

    # Calculate negative covariance
    negCov <- t(neg) %*% neg
    # Calculate positive covariance
    posCov <- t(pos) %*% pos

    # Construct the concordant covariance
    concordantCov <- negCov + posCov
    # We also return the realized covariance
    rCovEst <- mixCov + concordantCov

    if(cor){

      ## Calculate the correlations from the covariance matrices.
      mixCor <- matrix(NA, N, N)

      sdmatrix <- sqrt(diag(diag(negCov)))
      negCor <- solve(sdmatrix) %*% negCov %*% solve(sdmatrix)

      sdmatrix <- sqrt(diag(diag(posCov)))
      posCor <- solve(sdmatrix) %*% posCov %*% solve(sdmatrix)

      sdmatrix <- sqrt(diag(diag(concordantCov)))
      concordantCor <- solve(sdmatrix) %*% concordantCov %*% solve(sdmatrix)

      sdmatrix <- sqrt(diag(diag(rCovEst)))
      rCorEst <- solve(sdmatrix) %*% rCovEst %*% solve(sdmatrix)

      return(list("mixed" = mixCor, "negative" = negCor,  "positive" = posCor, "concordant" = concordantCor, "rCov" = rCorEst))
    }

    return(list("mixed" = mixCov, "negative" = negCov,  "positive" = posCov, "concordant" = concordantCov, "rCov" = rCovEst ))

  }

}

#' Utility function listing the available estimators for the CholCov estimation
#'
#'
#' @return This function returns a character vector containing the available estimators.
#' @export
listCholCovEstimators <- function(){
  c("rMRCov",
    "rCov",
    "rAVGCov",
    "rBPCov",
    "rHYCov",
    "rKernelCov",
    "rOWCov",
    "rRTSCov",
    "rThresholdCov",
    "rSemiCov")
}

#
#' ReMeDI
#' @description 
#' This function estimates the auto-covariance of market-microstructure noise
#'
#' Let the observed price \eqn{Y_{t}} be given as \eqn{Y_{t} = X_{t} + \varepsilon_{t}}, where \eqn{X_{t}} is the efficient price and \eqn{\varepsilon_t} is the market microstructure noise
#' 
#' The estimator of the \eqn{l}'th lag of the market microstructure is defined as:
#' \deqn{
#'     \hat{R}^{n}_{t,l} = \frac{1}{n_{t}} \sum_{i=2k_{n}}^{n_{t}-k_{n}-l} \left(Y_{i+l}^n - Y_{i+l+k_{n}}^{n} \right) \left(Y_{i}^n - Y_{i- 2k_{n}}^{n} \right),
#' }
#' where \eqn{k_{n}} is a tuning parameter. In the function \code{\link{knChooseReMeDI}}, we provide a function to estimate the optimal \eqn{k_{n}} parameter.
#'
#' @param pData \code{xts} or \code{data.table} containing the log-prices of the asset
#' @param kn numeric of length 1 determining the tuning parameter kn this controls the lengths of the non-overlapping interval in the ReMeDI estimation
#' @param lags numeric containing integer values indicating the lags for which to estimate the (co)variance
#' @param knEqual Use an altered version of the ReMeDI estimator, where we instead use equal kn, instead of kn and 2*kn for the windows. See Figure 1 of paper in reference section.
#' @param makeCorrelation logical indicating whether to transform the autocovariances into autocorrelations. 
#' The estimate of variance is imprecise and thus, constructing the correlation like this may show correlations that fall outside \eqn{(-1,1)}.
#'
#' @references Li, M. and Linton, O. (2019). A ReMeDI for microstructure noise. Cambridge Working Papers in Economics 1908.
#' @keywords microstructure noise autocovariance autocorrelation
#' @note We Thank Merrick Li for contributing his Matlab code for this estimator.
#' @examples
#' remed <- ReMeDI(sampleTData[as.Date(DT) == "2018-01-02", ], kn = 2, lags = 1:8)
#' # We can also use the algorithm for choosing the kn tuning parameter
#' optimalKn <- knChooseReMeDI(sampleTData[as.Date(DT) == "2018-01-02",],
#'                             knMax = 10, tol = 0.05, size = 3,
#'                             lower = 2, upper = 5, plot = TRUE)
#' optimalKn
#' remed <- ReMeDI(sampleTData[as.Date(DT) == "2018-01-02", ], kn = optimalKn, lags = 1:8)
#' @author Emil Sjoerup.
#' @export
ReMeDI <- function(pData, kn = 1, lags = 1, knEqual = FALSE,
                   #correctTime = FALSE, jumpsIndex = NULL,
                   makeCorrelation = FALSE){
  time <- DT <- PRICE <- NULL
  # Check input

  if(!is.logical(knEqual)){
    stop("knEqual must be logical")
  }

  if(is.data.table(pData)){ # We have a data.table

    if(!("PRICE" %in% colnames(pData))){
      stop("ReMeDI with data.table input requires a PRICE column")
    }

    # if(correctTime){
    #   if(!("DT" %in% colnames(pData))){
    #     stop("ReMeDI with correctTime set to TRUE needs a DT (date-time) column when the input is a data.table")
    #   } else {
    #     time <- as.numeric(pData[, DT])
    #   }
    # }

    prices <- as.numeric(pData[, PRICE])

  } else if( is.xts(pData) ) { # We have an xts object
    # if(correctTime){
    #   time <- as.numeric(index(pData))
    # }
    if(ncol(pData) != 1){
      if(!("PRICE" %in% colnames(pData))){
        stop("ReMeDI with data.table input requires a PRICE column")
      }
      prices <- as.numeric(pData[,"PRICE"])
    } else {
      prices <- as.numeric(pData)
    }
  } else {
    stop("Error in ReMeDI: pData must be an xts or a data.table")
  }

  # correctJumps <- FALSE
  #
  # if(is.numeric(jumpsIndex)){
  #   if(!all(jumpsIndex %% 1 == 0)){
  #     stop("Error in ReMeDI: jumpsIndex must be a numeric of integer values")
  #   }
  #   correctJumps <- TRUE
  # }

  if (!all(lags %% 1 == 0 )) {
    stop("lags must be contain integer values")
  }

  res <- numeric(length(lags))
  nObs <- length(prices)

  kn <- c(-kn, 2 * kn)
  resIDX <- 1

  if (makeCorrelation) {
    lags <- c(0, lags) # We make sure we have 0 lag first in the series (we remove it later again)
  }

  foo <- FALSE
  if(knEqual){
    foo <- TRUE
  }

  for (lag in lags) {

    # thisLag <- c(lag, 0)
    #remedi <- (kn[2] + 1):(nObs - lag + kn[1])
    #idx <- 1
    remedi <- 0
    # for(i in 1:(nObs - 3 * (-kn[1]) - lag)){
    #   remedi <- remedi + ((prices[i + 2 * (-kn[1])] - prices[i]) * (prices[i + 3 * (-kn[1]) + lag] - prices[i + 2 * (-kn[1]) + lag]))
    # }
    # browser()

    idx <- seq_len((nObs - (3-foo) * (-kn[1]) - lag))
    remedi <- sum((prices[idx + (2-foo) * (-kn[1])] - prices[idx]) * (prices[idx + (3-foo) * (-kn[1]) + lag] - prices[idx + (2-foo) * (-kn[1]) + lag]))

    # ## Use the time corrections Muzafer provided
    # if(correctTime){
    #
    #  ## We need to make corrections in the code for time adjustment when this is ready.
      # for (i in (kn[2] + 1):(nObs - lag + kn[1])) { # Calculate ReMeDI
      #   remedi[idx] <- prod(prices[i + thisLag] - prices[i + thisLag - kn])
      #   idx <- idx + 1
      # }




    #
    #   timeIDX <- sweep(matrix(rep((kn[2] + 1):(nObs - lag + kn[1]), 4), ncol = 4),2, c(thisLag , thisLag - kn), FUN = "+")
    #
    #   ## Follow up with muzafer whether this is correct
    #   timeCorrection <- (time[timeIDX[,3]] - time[timeIDX[,1]]) * (time[timeIDX[,2]] - time[timeIDX[,4]])
    #
    #   remedi <- remedi * timeCorrection
    #
    # }
    #
    #
    # ## Use the jump correction Muzafer provided
    # if(correctJumps){
    #   jumpIDX <- matrix(0, nrow = length(jumpsIndex), ncol = 3 + (thisLag[1] != 0))
    #
    #   for (i in 1:length(jumpsIndex)) {
    #     if(thisLag[1] != 0){
    #       jumpIDX[i,] <- c(jumpsIndex[i] - sum(abs(kn)) - thisLag[1], jumpsIndex[i] - rep(kn[2], 1 + sum(thisLag != 0)) - thisLag, jumpsIndex[i] )
    #     } else {
    #       jumpIDX[i,] <- c(jumpsIndex[i] - sum(abs(kn)) - thisLag[1], jumpsIndex[i] - kn[2] - thisLag[1], jumpsIndex[i])
    #     }
    #
    #     remedi <- remedi[-as.numeric(jumpIDX)]
    #   }
    #
    #
    # }
    #
    #

    res[resIDX] <- -remedi/(nObs - (3-foo) * (-kn[1]) - lag)

    resIDX <- resIDX +1
  }


  if(makeCorrelation){
    res <- res[-1]/res[1] # We transform the autocovariances into autocorrelations (and remove the 0-lag we added earlier)
  }

  return(res)
}

## For when correcTime is solved:
# #' @examples
# #' optimalKn <- knChooseReMeDI(sampleTData, correctTime = FALSE,
# #'                             jumpsIndex = NULL, knMax = 10, tol = 0.05,
# #'                             size = 3, lower = 2, upper = 5, plot = TRUE)
# #' optimalKn
# #' \dontrun{
# #' # We can also have a much larger search-space
# #' optimalKn <- knChooseReMeDI(sampleTData, correctTime = FALSE,
# #'                             jumpsIndex = NULL, knMax = 50, tol = 0.05,
# #'                             size = 3, lower = 2, upper = 5, plot = TRUE)
# #' optimalKn
# #' }
# #' @param correctTime logical indicating whether to use the time-adjusted ReMeDI measure, default is FALSE
# #' @param jumpsIndex Indices of jump(s) detected



#' ReMeDI tuning parameter
#' @description 
#' Function to choose the tuning parameter, kn in ReMeDI estimation. 
#' The optimal parameter \code{kn} is the smallest value that where the criterion:
#' \deqn{
#'     SqErr(k_{n})^{n}_{t} = \left(\hat{R}^{n,k_{n}}_{t,0} - \hat{R}^{n,k_{n}}_{t,1} - \hat{R}^{n,k_{n}}_{t,2} + \hat{R}^{n,k_{n}}_{t,3} - \hat{R}^{n, k_{n}}_{t,l}\right)^{2}
#' }
#' is perceived to be zero. The tuning parameter \code{tol} can be set to choose the tolerance of the perception of 'close to zero', a higher tolerance will lead to a higher optimal value.
#' @param pData \code{xts} or \code{data.table} containing the log-prices of the asset.
#' @param knEqual Use an altered version of the ReMeDI estimator, where we instead use equal \code{kn}, instead of \code{kn} and \code{2*kn} for the windows. 
#' See Figure 1 of paper in reference section.
#' @param knMax max value of \code{kn} to be considered.
#' @param tol tolerance for the minimizing value. If \code{tol} is high, the algorithm will choose a lower optimal value.
#' @param size size of the local window.
#' @param lower lower boundary for the method if it fails to find an optimal value. If this is the case, the best kn between lower and upper is returned
#' @param upper upper boundary for the method if it fails to find an optimal value. If this is the case, the best kn between lower and upper is returned
#' @param plot logical whether to plot the errors.
#' @details This is the algorithm B.2 in the appendix of the Li and Linton (2019) working paper.
#' @note We Thank Merrick Li for contributing his Matlab code for this estimator.
#' @examples
#' optimalKn <- knChooseReMeDI(sampleTData[as.Date(DT) == "2018-01-02",],
#'                             knMax = 10, tol = 0.05, size = 3,
#'                             lower = 2, upper = 5, plot = TRUE)
#' optimalKn
#' \dontrun{
#' # We can also have a much larger search-space
#' optimalKn <- knChooseReMeDI(sampleTDataEurope,
#'                             knMax = 50, tol = 0.05,
#'                             size = 3, lower = 2, upper = 5, plot = TRUE)
#' optimalKn
#' }
#'
#' @author Emil Sjoerup.
#' @importFrom stats plot.ts
#' @references Li, M. and Linton, O. (2019). A ReMeDI for microstructure noise. Cambridge Working Papers in Economics 1908.
#' @return integer containing the optimal kn
#' @export
knChooseReMeDI <- function(pData, knEqual = FALSE,
                           #correctTime = FALSE, jumpsIndex = NULL,
                           knMax = 10, tol = 0.05, size = 3, lower = 2, upper = 5, plot = FALSE){

  kn <- 1:(knMax + size +1)
  err <- vapply(kn, ReMeDI, FUN.VALUE = numeric(4), pData = pData,
                #correctTime = correctTime, jumpsIndex = jumpsIndex, ## For when correctTime is fixed
                lags = 0:3, knEqual = knEqual)
  err <- (err[1,] - err[2,] - err[3,] + err[4,] - ReMeDI(pData, kn = 1, lags = 0, knEqual = knEqual ))^2
          #                                               , correctTime = correctTime, jumpsIndex = jumpsIndex) ## For when correctTime is fixed


  if(plot){
    plot.ts(err, ylab = "error", xlab = "kn")
  }

  errMax <- max(err[1:(round(knMax/2))])

  kns <- vapply(1:(knMax+1), flat, FUN.VALUE = numeric(1), err = err, errMax = errMax, size = size, tol = tol)
  kns <- kns[!is.na(kns)]
  kn <- kns[1]

  if(is.na(kn)){
    kn <- which(err == min(err[lower:upper]))
  }

  return(as.integer(kn))

}

#' Asymptotic variance of ReMeDI estimator
#' 
#' @description 
#' 
#' Estimates the asymptotic variance of the ReMeDI estimator.
#' 
#' @details 
#' Some notation is needed for the estimator of the asymptotic covariance of the ReMeDI estimator.
#' Let
#' \deqn{
#'     \delta\left(n, i\right) = t_{i}^{n}-t_{t-1}^{n}, i\geq 1,
#' }
#' \deqn{
#'     \hat{\delta}_{t}^{n}=\left(\frac{k_{n}\delta\left(n,i+1+k_{n}\right)-t_{i+2+2k_{n}}^{n}+t_{i+2+k_{n}}^{n}}{\left(t_{i+k_{n}}^{n}-t_{i}^{n}\right)\vee\phi_{n}}\right)^{2},
#' }
#'
#' \deqn{
#'     U\left(1\right)_{t}^{n}=\sum_{i=0}^{n_{t}-\omega\left(1\right)_{n}}\hat{\delta}_{i}^{n},
#' }
#' \deqn{
#'     U\left(2,\boldsymbol{j}\right)_{t}^{n}=\sum_{i=0}^{n_{t}-\omega\left(2\right)_{n}}\hat{\delta}_{i}^{n}\Delta_{\boldsymbol{j}}\left(Y\right)_{i+\omega\left(2\right)_{2}^{n}}^{n},
#' }
#' 
#' \deqn{
#'     U\left(3,\boldsymbol{j},\boldsymbol{j}'\right)_{t}^{n}=\sum_{i=0}^{n_{t}-\omega\left(3\right)_{n}}\hat{\delta}_{i}^{n}\Delta_{\boldsymbol{j}}\left(Y\right)_{i+\omega\left(3\right)_{2}^{n}}^{n}\Delta_{\boldsymbol{j}'}\left(Y\right)_{i+\omega\left(3\right)_{3}^{n}}^{n},
#' }
#' 
#' \deqn{
#'     U\left(4;\boldsymbol{j},\boldsymbol{j}'\right)_{t}^{n}=-\sum_{i=2^{q-1}k_{n}}^{n_{t}-\omega\left(4\right)_{n}}\Delta_{\boldsymbol{j}}\left(Y\right)\Delta_{\boldsymbol{j}^{\prime}}\left(Y\right)_{i+\omega\left(3\right)_{3}^{n}}^{n},
#' }
#' \deqn{
#'     U\left(5,k;\boldsymbol{j},\boldsymbol{j}'\right)_{t}^{n}=\sum_{Q_{q}\in\mathcal{Q}_{q}}\sum_{i=2^{e\left(Q_{q}\right)}k_{n}}^{n_{t}-\omega\left(5\right)_{n}}\Delta_{\boldsymbol{j}_{Q_{q}\oplus\left(\boldsymbol{j}\prime_{Q_{q'}}\left(+k\right)\right)}}\left(Y\right)_{i}^{n}\prod_{\ell:l_{\ell}\in Q_{q}^{c}}\Delta_{\left(j_{l_{\ell}},j\prime_{l_{\ell}}+k\right)\left(Y\right)_{i+\omega\left(5\right)_{\ell+1}^{n}\prime}},
#' }
#' 
#' \eqn{
#'     U\left(6,k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)=\sum_{j_{l}\in\boldsymbol{j},j_{l^{\prime}}^{\prime}\in\boldsymbol{j}^{\prime}}\sum_{i=2k_{n}}^{n_{t}-\omega\left(6\right)n}\Delta_{\left(j_{l},j_{l^{\prime}}^{\prime}+k\right)}\left(Y\right)_{i}^{n}\Delta_{\boldsymbol{j}_{-l}}\left(Y\right)_{i+\omega\left(6\right)_{2}^{n}}^{n}\Delta_{\boldsymbol{j}_{-l^{\prime}}^{\prime}}\left(Y\right)_{i+\omega\left(6\right)_{3}^{n}}^{n} \\
#'     -\sum_{j_{l}\in\boldsymbol{j}}\sum_{i=2^{q}k_{n}}^{n_{t}-\omega^{\prime}\left(6\right)_{n}}\Delta_{\left\{ j_{l}\right\} \oplus\boldsymbol{j}^{\prime}\left(+k\right)}\left(Y\right)_{i}^{n}\Delta_{\boldsymbol{j}-l}\left(Y\right)_{i+\omega^{\prime}\left(6\right)_{2}^{n}}^{n} \\
#'     -\sum_{j_{l^{\prime}\in\boldsymbol{j}^{\prime}}^{\prime}}\sum_{i=2^{q}k_{n}}^{n_{t}-\omega^{\prime\prime}\left(6\right)n}\Delta_{\left\{ j_{l^{\prime}}^{\prime}+k\right\} \oplus\boldsymbol{j}}\left(Y\right)_{i}^{n}\Delta_{\boldsymbol{j}_{-l^{\prime}}^{\prime}}\left(Y\right)_{i+\omega^{\prime\prime}\left(6\right)_{2}^{n}\prime}^{n},
#' }
#'
#' \deqn{
#'     U\left(7,k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=ReMeDI\left(\boldsymbol{j}\oplus\boldsymbol{j}^{\prime}\left(+k\right)\right)_{t}^{n},
#' }
#' \deqn{
#'     U\left(k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=\sum_{\ell=5}^{7}U\left(\ell,k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n},
#' } 
#' \deqn{
#'     U\left(k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=\sum_{\ell=5}^{7}U\left(\ell,k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n},
#' }
#' 
#' Where the indices are given by:
#' \deqn{
#'     \omega\left(1\right)_{n}=2+2k_{n},\ \omega\left(2\right)_{2}^{n}=2+\left(3+2^{q-1}\right)k_{n},\ \omega\left(2\right)_{n}=\omega\left(2\right)_{2}^{n}+j_{1}+k_{n},
#' }
#' 
#' \deqn{
#'     \omega\left(3\right)_{2}^{n}=2+\left(3+2^{q-1}\right)k_{n},\ \omega\left(3\right)_{3}^{n}=2+\left(5+2^{q-1}+2^{q^{\prime}-1}\right)k_{n}+j_{1},
#' }
#' 
#' \deqn{
#'     \omega\left(3\right)_{n}=\omega\left(3\right)_{3}^{n}+j_{1}^{\prime}+k_{n},\ \omega\left(4\right)_{2}^{n}=2k_{n}+q_{n}^{\prime}+j_{1},\ \omega\left(4\right)_{n}=\omega\left(4\right)_{2}^{n}+j_{1}^{\prime}+k_{n},
#' }
#' \deqn{
#'     e\left(Q_{q}\right)=\left(2\left|Q_{q}\right|+q^{\prime}-q-1\right)\vee1,\ \omega\left(5\right)_{\ell+1}^{n}=4\ell k_{n}+\sum_{\ell^{\prime}=1}^{\ell}j_{l_{\ell^{\prime}}}\vee\left(j_{l_{\ell}}^{\prime}+k\right)\textrm{for}\ell\geq 1,
#' }
#' \deqn{
#'     \omega\left(5\right)_{n}=\omega\left(5\right)_{\left|Q_{q}^{c}\right|+1}^{n}+j_{l_{\left|Q_{q}^{c}\right|}}\vee\left(j_{l_{\left|Q_{q}^{c}\right|}}+k\right)+k_{n},
#' }
#' 
#' \deqn{
#'     \omega\left(6\right)_{2}^{n}=\left(2^{q-2}+2\right)k_{n}+j_{\ell}\vee\left(j_{\ell^{\prime}}^{\prime}+k\right),\ \omega\left(6\right)_{3}^{n}=\left(2^{q-2}+2^{q^{\prime}-2}+2\right)k_{n}+j_{1}+j_{\ell}\vee\left(j_{\ell}^{\prime}+k\right),
#' }
#' 
#' \deqn{
#'     \omega^{\prime}\left(6\right)_{2}^{n}=\left(2^{q-2}+2\right)k_{n}+j_{\ell}\vee\left(j_{1}^{\prime}+k\right),\ \omega^{\prime\prime}\left(6\right)_{2}^{n}=\left(2^{q^{\prime}-2}+1\right)k_{n}+\left(j_{\ell^{\prime}}^{\prime}+k\right)\vee j_{1},
#' }
#' \deqn{
#'     \omega\left(6\right)_{n}=\omega\left(6\right)_{3}^{n}+j^{\prime}+k_{n},\ \omega^{\prime}\left(6\right)_{n}=\omega^{\prime}\left(6\right)_{2}^{n}+j_{1}+k_{n},\ \omega^{\prime\prime}\left(6\right)_{n}=\omega^{\prime\prime}\left(6\right)_{2}^{n}j_{1}^{\prime}+k_{n},
#' }
#' 
#' The asymptotic variance estimator is then given by
#' 
#' \deqn{
#'     \hat{\sigma}\left(\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=\frac{1}{n_{t}}\sum_{\ell=1}^{3}\hat{\sigma}_{\ell}\left(\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n},
#' }
#' 
#' where 
#' \deqn{
#'     \hat{\sigma}_{1}\left(\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=U\left(0;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)+\sum_{k=1}^{i_{n}}\left(U\left(k;\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}\right)+\left(2i_{n}+1\right)U\left(4;\boldsymbol{j},\boldsymbol{j}\right)_{t}^{n},
#' }
#' 
#' \deqn{
#'     \hat{\sigma}_{2}\left(\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=U\left(3;\boldsymbol{j},\boldsymbol{j}^{\prime}\right),
#' }
#' \deqn{
#'     \hat{\sigma}_{3}\left(\boldsymbol{j},\boldsymbol{j}^{\prime}\right)_{t}^{n}=\frac{1}{n_{t}^{2}}\textrm{ReMeDI}\left(Y,\boldsymbol{j}\right)_{t}^{n}\textrm{ReMeDI}\left(Y,\boldsymbol{j}^{\prime}\right)_{t}^{n}U\left(1\right)_{t}^{n}\\,
#' }
#' \deqn{
#'     -\frac{1}{n_{t}}\left(\textrm{ReMeDI}\left(Y,\boldsymbol{j}\right)_{t}^{n}U\left(2,\boldsymbol{j}^{\prime}\right)_{t}^{n}+\textrm{ReMeDI}\left(Y,\boldsymbol{j}^{\prime}\right)_{t}^{n}U\left(2,\boldsymbol{j}\right)_{t}^{n}\right),
#' }
#' 
#' 
#' 
#' @param pData \code{xts} or \code{data.table} containing the log-prices of the asset
#' @param kn numerical value determining the tuning parameter kn this controls the lengths of the non-overlapping interval in the ReMeDI estimation
#' @param lags numeric containing integer values indicating the lags for which to estimate the (co)variance
#' @param phi tuning parameter phi
#' @param i tuning parameter i
#' @note We Thank Merrick Li for contributing his Matlab code for this estimator.
#' @return a list with components \code{ReMeDI} and \code{asympVar} containing the ReMeDI estimation and it's asymptotic variance respectively
#' @export
ReMeDIAsymptoticVariance <- function(pData, kn, lags, phi, i){
  PRICE <- DT <- NULL

  if(is.data.table(pData)){ # We have a data.table

    if(!("PRICE" %in% colnames(pData))){
      stop("ReMeDI with data.table input requires a PRICE column")
    }

    # if(correctTime){
    #   if(!("DT" %in% colnames(pData))){
    #     stop("ReMeDI with correctTime set to TRUE needs a DT (date-time) column when the input is a data.table")
    #   } else {
    #     time <- as.numeric(pData[, DT])
    #   }
    # }

    prices <- as.numeric(pData[, PRICE])
    timestamps <- as.numeric(pData[, DT])
  } else if( is.xts(pData) ) { # We have an xts object
    if(ncol(pData) != 1){
      if(!("PRICE" %in% colnames(pData))){
        stop("ReMeDI with data.table input requires a PRICE column")
      }
      prices <- as.numeric(pData[,"PRICE"])
    } else {
      prices <- as.numeric(pData)
    }

    timestamps <- as.numeric(index(pData))
  } else {
    stop("Error in ReMeDI: pData must be an xts or a data.table")
  }

  N <- length(prices)
  timestamps <- timestamps - timestamps[1]
  timestamps <- timestamps/timestamps[N]

  diffTS <- diff(timestamps)
  diffKNTS <- timestamps[(1+kn):N] - timestamps[1:(N-kn)]

  dn1 <- kn * diffTS[(2 + kn):(N-kn-1)] - diffTS[(3 + kn):(N-kn)]
  dn2 <- dn1 / pmax(phi, diffKNTS[1:(N-2-2*kn)])
  dn <- dn2^2
  U1 <- sum(dn)

  diffKNOnce <- prices[1:(N-kn)] - prices[(kn+1):N]
  diffKNTwice <- prices[(2 * kn + 1):N] - prices[1:(N - 2 * kn)]
  diffKNThrice <- prices[(3 * kn + 1):N] - prices[1:(N - 3 * kn)]
  nLags <- length(lags)
  U2 <- S1 <- S2 <- Rj <- numeric(nLags)
  for (l in 1:nLags) {
    Rj[l] <- sum(diffKNOnce[(1 + 2 * kn + lags[l]):(N - kn)] * diffKNTwice[1:(N-lags[l]-3*kn)])

    U2[l] <- sum(diffKNTwice[(3 + 3 * kn):(N - lags[l] - 3 * kn)] * diffKNOnce[(3 + 5 * kn + lags[l]):(N - kn)] * dn[1:(N - 2 - 6 * kn - lags[l])])
    S2[l] <- sum(diffKNOnce[(3 + 5 * kn + lags[l]):(N - lags[l] - 5 * kn)] * diffKNOnce[(3 + 9 * kn + 2 * lags[l]):(N - kn)] *
                diffKNTwice[(3 + 3 * kn):(N - 7 * kn - 2 * lags[l])] * diffKNTwice[(3 + 7 * kn + lags[l]):(N - 3 * kn - lags[l])] * dn[1:(N - 10 * kn - 2 * lags[l] - 2)])

    U4 <- -sum(diffKNOnce[(1 + 2 * kn + lags[l]):(N - lags[l] - 5 * kn)] *  diffKNOnce[(1 + 6 * kn + 2 * lags[l]):(N - kn)] * diffKNTwice[1:(N - 7 * kn - 2 * lags[l])] *  diffKNTwice[(1 + 4 * kn + lags[l]):(N - 3 * kn - lags[l])])

    S1[l] <- sum(diffKNOnce[(6*kn):(N-5*kn-2*lags[l])] * diffKNOnce[(10*kn+2*lags[l]):(N-kn)] * diffKNTwice[(8*kn+2*lags[l]):(N-3*kn)] * diffKNTwice[(8*kn+lags[l]):(N-3*kn-lags[l])]) +
      sum(diffKNOnce[(1+3*kn+lags[l]):(N-kn)] * diffKNOnce[(1+3*kn):(N-kn-lags[l])] * diffKNTwice[(1+kn+lags[l]):(N-3*kn)] * diffKNThrice[1:(N-4*kn-lags[l])]) + U4

    for (k in 1:i) {
      Uk1 <- sum(diffKNOnce[(6*kn+2*k):(N-5*kn-k-2*lags[l])] * diffKNOnce[(10*kn+2*lags[l]+3*k):(N-kn)] * diffKNTwice[(8*kn+2*lags[l]+3*k):(N-3*kn)] * diffKNTwice[(8*kn+lags[l]+2*k):(N-3*kn-k-lags[l])])

      Uk2 <- sum(diffKNOnce[(1+3*kn+lags[l]+k):(N-kn)] * diffKNOnce[(1+3*kn+k):(N-kn-lags[l])] * diffKNTwice[(1+kn+lags[l]):(N-3*kn-k)] * diffKNThrice[1:(N-4*kn-k-lags[l])])

      S1[l] <- S1[l] + 2*(3*Uk1+Uk2+U4)
    }

  }
  Rj <- Rj/(N - 3 * kn - lags)
  S3 <- Rj^3 * U1 - 2 * Rj * U2

  asympVar <- (S1 + S2 + S3)/N
  asympVar <- pmax(-asympVar, asympVar)
  return(list("ReMeDI" = Rj, "asympVar" = asympVar))
}



#### #'
#### #' #' Autocorrelation of noise estimation
#### #' #' @importFrom zoo coredata
#### #' #' @export
#### #' autoCorrelationOfNoise <- function(pData, kn, lags){
#### #'   ## Make general:
#### #'   reticulate::source_python("../pickle_reader.py")
#### #'   dat <- read_pickle_file("/data/data/pickles/AIG_Tr_20140102_cleaned.pickle")
#### #'   pData <- xts(dat$price, anytime::anytime(dat$timestamp))
#### #'   colnames(pData) <- "PRICE"
#### #'   prices <- pData$PRICE
#### #'   kn <- 2
#### #'   lags <- 30
#### #'   averagedPrices <- filter(coredata(prices), rep(1, kn)/kn, sides = 1)[-seq(1, (kn-1))]
#### #'   nObs <- length(averagedPrices)
#### #'   J <- lags
#### #'   R_est <- rep(0, J + 1)
#### #'   r_est <- rep(0, J + 1)
#### #'
#### #'   U_0 <- rep(0, J + 1)
#### #'
#### #'   for (j in 0:J) {
#### #'
#### #'     ind <- seq(1, nObs + 1 - j - 2 * 2 * kn)
#### #'     U_0[(j+1)] <- sum((prices[ind] - averagedPrices[(j + kn + ind)]) * (prices[ j+ ind] - averagedPrices[(j + 3 * kn + ind)]))
#### #'     R_est[(j+1)] <- U_0[j + 1]/length(ind)
#### #'
#### #'   }
#### #'
#### #'
#### #'   UU_2 <- UU_3 <- UU_4 <- UU_1 <- matrix(0 ,nrow= kn + 1, ncol= J + 1)
#### #'
#### #'   for (m in 0:kn) {
#### #'     for (j in 0:J) {
#### #'       mu <- j + m
#### #'       ind <- seq(1, nObs + 1 - mu - 8 * kn)
#### #'
#### #'       UU_1[(m+1), (j+1)] <-
#### #'         sum((prices[ind] - averagedPrices[(mu + kn + ind)]) *
#### #'             (prices[j + ind] - averagedPrices[(mu + 3 * kn + ind)]) *
#### #'             (prices[m + ind] - averagedPrices[(mu + 5 * kn + ind)]) *
#### #'             (prices[j + m + ind] - averagedPrices[(mu + 7 * kn + ind)]))
#### #'
#### #'       UU_2[(m+1),(j+1)] <-
#### #'         sum( (prices[m + ind] - averagedPrices[(mu + kn + ind)]) *
#### #'              (prices[j + m + ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'              (prices[ind] - averagedPrices[(mu + 5 * kn + ind)] ) *
#### #'              (prices[j + ind] - averagedPrices[(mu + 7 * kn + ind)] )  )
#### #'
#### #'       UU_3[(m+1),(j+1)] <-
#### #'         sum( (prices[ind] - averagedPrices[(mu + kn + ind)]) *
#### #'              (prices[j + ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'              (prices[m + ind] - averagedPrices[(mu + 5 * kn + ind)] ) *
#### #'              (prices[m + ind] - averagedPrices[(mu + 7 * kn + ind)] )  )
#### #'
#### #'
#### #'       UU_4[(m+1),(j+1)] <-
#### #'         sum( (prices[m + ind] - averagedPrices[(mu + kn + ind)]) *
#### #'              (prices[j + m + ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'              (prices[ind] - averagedPrices[(mu + 5 * kn + ind)] ) *
#### #'              (prices[ind] - averagedPrices[(mu + 7 * kn + ind)] ))
#### #'     }
#### #'   }
#### #'
#### #'   U_bar_1 <- rep(0, J + 1) # U_bar(0,j,0,j)
#### #'   U_bar_2 <- rep(0, J + 1) # U_bar(0,j,0,0)
#### #'
#### #'
#### #'   for (j in 0:J) {
#### #'     mu <- j
#### #'     mu_pp <- j + j
#### #'
#### #'     ind <- seq(1, nObs + 1 - mu_pp - 9 * kn)
#### #'
#### #'     U_bar_1[(j+1)] <- sum( (prices[ind] - averagedPrices[(mu + kn + ind)]) *
#### #'                            (prices[j+ ind] - averagedPrices[(mu + 3 * kn + ind)] ) *
#### #'                            (prices[mu + 5 * kn + ind] - averagedPrices[(mu_pp + 5 * kn + kn + ind)] ) *
#### #'                            (prices[mu + 5 * kn + j + ind] - averagedPrices[(mu_pp + 5 * kn + 3 * kn + ind)] ) )
#### #'
#### #'     U_bar_2[(j+1)] <- sum( (prices[ind] - averagedPrices[(mu + kn + ind)]) *
#### #'                            (prices[ j+ ind] - averagedPrices[(mu + 3*kn + ind)] ) *
#### #'                            (prices[ mu + 5*kn + ind] - averagedPrices[(mu + 5 * kn + kn + ind)] ) *
#### #'                            (prices[ mu + 5*kn + ind] - averagedPrices[(mu + 5 * kn + 3 * kn + ind)] ) )
#### #'   }
#### #'
#### #'   S <- rep(0, J + 1) # S[0,j,0,j]
#### #'   Bza <- rep(0, J + 1) # Bza[0,j,0,j]
#### #'
#### #'   S_1 <- rep(0, J + 1) # S[0,j,0,0]
#### #'   Bza_1 <- rep(0, J + 1) # Bza[0,j,0,0]
#### #'   sigma_r <- rep(1, J) #
#### #'
#### #'   j<-0
#### #'   S[(j+1)] <- (sum(UU_1[,(j+1)]) + sum(UU_2[,(j+1)]) - UU_1[1,(j+1)]) - (2*kn + 1)*U_bar_1[(j+1)]
#### #'   Bza[(j+1)] <- S[(j+1)]/nObs + U_bar_1[(j+1)]/nObs - R_est[(j+1)]^2
#### #'
#### #'   S_1[(j+1)] <- (sum(UU_3[,(j+1)]) + sum(UU_4[,(j+1)]) - UU_3[1,(j+1)] ) - (2*kn + 1)*U_bar_2[(j+1)]
#### #'   Bza_1[(j+1)] <- S_1[(j+1)]/nObs + U_bar_2[(j+1)]/nObs - R_est[1]*R_est[(j+1)]
#### #'
#### #'
#### #'   for (j in 1:J) {
#### #'     S[(j+1)] <- (sum(UU_1[,(j+1)]) + sum(UU_2[,(j+1)]) - UU_1[1,(j+1)]) - (2*kn + 1)*U_bar_1[(j+1)]
#### #'     Bza[(j+1)] <- S[(j+1)]/nObs + U_bar_1[(j+1)]/nObs - R_est[(j+1)]^2
#### #'
#### #'     S_1[(j+1)] <- (sum(UU_3[,(j+1)]) + sum(UU_4[,(j+1)]) - UU_3[1,(j+1)] ) - (2*kn + 1)*U_bar_2[(j+1)]
#### #'     Bza_1[(j+1)] <- S_1[(j+1)]/nObs + U_bar_2[(j+1)]/nObs - R_est[1]*R_est[(j+1)]
#### #'     sigma_r[j] <- (R_est[1]^2*Bza[(j+1)] + R_est[(j+1)]^2 * Bza[1] - (2 * R_est[1] * R_est[(j+1)] * Bza_1[(j+1)]))/(R_est[1]^4)
#### #'   }
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'   plot.ts(R_est)
#### #'
#### #'   #plot.ts(sigma_r)
#### #'
#### #' }
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'
#### #'











#' rBAC
#' 
#' @description 
#' The proposed Beta Adjusted Covariance (BAC) equals the pre-estimator plus a minimal adjustment matrix such that the covariance-implied stock-ETF beta equals a target beta.
#' 
#' 
#' 
#' @param pData a named list. Each list-item contains an \code{xts} or \code{data.table} object with the intraday price data of an ETF and it's component stocks. \code{xts} objects are turned into \code{data.table}s
#' @param shares a \code{numeric} with length corresponding to the number of component stocks in the ETF. The entries are the stock holdings of the ETF in the corresponding stock.
#' @param outStanding number of shares outstanding of the ETF
#' @param nonEquity aggregated value of the additional components (like cash, money-market funds, bonds, etc.) of the ETF which are not included in the components in \code{pData}.
#' @param ETFNAME a \code{character} denoting which entry in the \code{pData} list is the ETF. Default is \code{"ETF"}
#' @param unrestricted a \code{logical} denoting whether to use the unrestricted estimator, which also affects the diagonal. Default is \code{FALSE}
#' @param targetBeta a \code{character}, one of \code{c("HY", "VAB", "expert")} (default) denoting which target beta to use, only the first entry will be used. A value \code{"HY"} means using the Hayashi-Yoshida estimator to estimate the
#' empirical beta. A value of \code{"VAB"} denotes using the variance adjusted beta. A value of \code{"expert"} denotes using a user-supplied target beta, which can be supplied in the \code{expertBeta} argument.
#' @param expertBeta a \code{numeric} containing the user supplied expert beta used when \code{targetBeta} is \code{"expert"}. The \code{expertBeta} must be of length equal to the number of assets in the ETF. Default is \code{NULL} 
#' @param preEstimator a \code{function} which estimates the integrated covariance matrix. Default is \code{\link{rCov}}
#' @param noiseRobustEstimator a \code{function} which estimates the integrated (co)variance and is robust to microstructure noise (only the diagonal will be estimated).
#'  This function is only used when \code{noiseCorrection} is \code{TRUE}. Default is \code{\link{rTSCov}}
#' @param noiseCorrection a \code{logical} which denotes whether to correct for microstructure noise by using the \code{noiseRobustEstimator} function. Default is \code{FALSE}
#' @param returnL a \code{logical} which denotes whether to return the \code{L} matrix. Default is \code{FALSE}
#' @param ... extra arguments passed to \code{preEstimator} and \code{noiseRobustEstimator}.
#' 
#' @examples
#' \dontrun{
#' # Since we don't have any data in this package that is of the required format we must simulate it.
#' library(xts)
#' library(highfrequency)
#' # Set the seed for replication
#' set.seed(123)
#' iT <- 23400 # Number of observations
#' # Simulate returns
#' rets <- mvtnorm::rmvnorm(iT * 3 + 1, mean = rep(0,4), 
#'                          sigma = matrix(c(0.1, -0.03 , 0.02, 0.04,
#'                                           -0.03, 0.05, -0.03, 0.02,
#'                                           0.02, -0.03, 0.05, -0.03,  
#'                                           0.04, 0.02, -0.03, 0.08), ncol = 4))
#' # We assume that the assets don't trade in a synchronous manner
#' w1 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.5)), 1]
#' w2 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.75)), 2]
#' w3 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.65)), 3]
#' w4 <- rets[sort(sample(1:nrow(rets), size = nrow(rets) * 0.8)), 4]
#' w5 <- rnorm(nrow(rets) * 0.9, mean = 0, sd = 0.005)
#' timestamps1 <- seq(34200, 57600, length.out = length(w1))
#' timestamps2 <- seq(34200, 57600, length.out = length(w2))
#' timestamps3 <- seq(34200, 57600, length.out = length(w3))
#' timestamps4 <- seq(34200, 57600, length.out = length(w4))
#' timestamps4 <- seq(34200, 57600, length.out = length(w4))
#' timestamps5 <- seq(34200, 57600, length.out = length(w5))
#' 
#' w1 <- xts(w1 * c(0,sqrt(diff(timestamps1) / (max(timestamps1) - min(timestamps1)))),
#'           as.POSIXct(timestamps1, origin = "1970-01-01"), tz = "UTC")
#' w2 <- xts(w2 * c(0,sqrt(diff(timestamps2) / (max(timestamps2) - min(timestamps2)))),
#'           as.POSIXct(timestamps2, origin = "1970-01-01"), tz = "UTC")
#' w3 <- xts(w3 * c(0,sqrt(diff(timestamps3) / (max(timestamps3) - min(timestamps3)))),
#'           as.POSIXct(timestamps3, origin = "1970-01-01"), tz = "UTC")
#' w4 <- xts(w4 * c(0,sqrt(diff(timestamps4) / (max(timestamps4) - min(timestamps4)))),
#'           as.POSIXct(timestamps4, origin = "1970-01-01"), tz = "UTC")
#' w5 <- xts(w5 * c(0,sqrt(diff(timestamps5) / (max(timestamps5) - min(timestamps5)))),
#'           as.POSIXct(timestamps5, origin = "1970-01-01"), tz = "UTC")
#' 
#' p1  <- exp(cumsum(w1))
#' p2  <- exp(cumsum(w2))
#' p3  <- exp(cumsum(w3))
#' p4  <- exp(cumsum(w4))
#' 
#' weights <- runif(4) * 1:4
#' weights <- weights / sum(weights)
#' p5 <- xts(rowSums(cbind(w1 * weights[1], w2 * weights[2], w3 * weights[3], w4 * weights[4]),
#'                    na.rm = TRUE),
#'                    index(cbind(p1, p2, p3, p4)))
#' p5 <- xts(cumsum(rowSums(cbind(p5, w5), na.rm = TRUE)), index(cbind(p5, w5)))
#' 
#' p5 <- exp(p5[sort(sample(1:length(p5), size = nrow(rets) * 0.9))])
#' 
#' 
#' BAC_Delta <- rBACov(pData = list(
#'                      "ETF" = p5, "STOCK 1" = p1, "STOCK 2" = p2, "STOCK 3" = p3, "STOCK 4" = p4
#'                    ), shares = 1:4, outStanding = 1, nonEquity = 0, ETFNAME = "ETF", 
#'                    unrestricted = FALSE, preEstimator = rCov, noiseCorrection = FALSE, 
#'                    returnL = FALSE, K = 2, J = 1)
#' }
#' @author Emil Sjoerup, (Kris Boudt and Kirill Dragun for the Python version)
#' @importFrom xts is.xts
#' @importFrom data.table as.data.table merge.data.table data.table setkey setcolorder copy
#' @export
rBACov <- function(pData, shares, outStanding, nonEquity, ETFNAME = "ETF", 
                   unrestricted = TRUE, targetBeta = c("HY", "VAB", "expert"),
                   expertBeta = NULL, preEstimator = rCov, noiseRobustEstimator = rTSCov, noiseCorrection = FALSE, 
                 returnL = FALSE, ...){
  .N <- DT <- .SD <- NULL
  
  if(!is.list(pData) | is.data.table(pData)){
    stop("pData must be a list of data.tables or xts objects")
  }
  if(is.xts(pData[[1]])){
    pData <- lapply(pData,
                    function(x){
                      dimnames(x) <- NULL ## Sometimes needed, don't know why, really
                      x <- as.data.table(x)
                      setnames(x, "index","DT")
                      return(x)
                    })
  }
  targetBeta <- targetBeta[1]
  
  if(noiseCorrection && !is.function(noiseRobustEstimator)){
    stop("noiseRobustEstimator must be a function when noiseCorrection is TRUE")
  }
  
  for (i in 1:length(pData)) {
    setnames(pData[[i]], new= c("DT", names(pData[i])))
  }
  backup <- Reduce(function(x,y) merge.data.table(x, y, all = TRUE, on = "DT"), pData[which(names(pData) != ETFNAME)])
  pData <- Reduce(function(x,y) merge.data.table(x, y, all = TRUE, on = "DT"), pData)
  setkey(pData, "DT")
  setkey(backup, "DT")
  
  timeZone <- format(pData$DT[1], format = "%Z")
  tz <- NULL
  if(is.null(timeZone) || timeZone == ""){
    if(is.null(tz)){
      tz <- "UTC"
    }
    if(!("POSIXct" %in% class(pData$DT))) pData[, DT := as.POSIXct(format(DT, digits = 20, nsmall = 20), tz = tz)]
  } else {
    tz <- timeZone
  }
  
  
  nComps <- dim(pData)[2] - 2 # Number of components in the ETF - 2 because of ETF data and DT
  
  W <- matrix(0, nrow = nComps, ncol = nComps^2)
  Q <- matrix(0, nrow = nComps^2, ncol = nComps^2)
  
  shares <- shares / outStanding
  setcolorder(pData, c("DT", ETFNAME))
  missingPoints <- !is.na(pData)[-1,] # Where the inputs aren't missing
  
  
  RC2 <- matrix(0, ncol = ncol(backup) - 1, nrow = ncol(backup) - 1)
  noiseRobust <- rep(0, ncol(backup) - 1)
  for (i in 1:nComps) {
    for (j in i:nComps) {
      dat <- refreshTimeMatching(as.matrix(backup[, 1 + c(i, j), with = FALSE]), backup$DT)
      
      dat <- data.table(dat$data)[, DT := as.POSIXct(dat$indices, origin = "1970-01-01", tz = tz)]

      setkey(dat, DT)
      setcolorder(dat, "DT")
      RC2[i, j] <- RC2[j, i] <- preEstimator(dat, makeReturns = TRUE)[1,2]
      
    }
  }
  
  
  
  pData <- setnafill(copy(pData), type = "locf", cols = 2:ncol(pData))
  pData <- setnafill(pData, type = "nocb")
  set(pData, j = ETFNAME, value = pData[,ETFNAME , with = FALSE] - nonEquity/outStanding)
  
  returns <- pData[, lapply(.SD, function(x) makeReturns(x)), .SDcols = 3:ncol(pData)][-1,]
  etfReturns <- diff(pData[[2]])
  set(returns, j = "DT", value = pData$DT[-1])
  setcolorder(returns, "DT")
  shares <- c(0,0, shares) / outStanding
  meanWeights <- numeric(ncol(pData))
  meanSquaredWeights <- numeric(ncol(pData))
  assetWeights <- copy(pData)
  for (j in 3:ncol(pData)) {
    set(assetWeights, j = j, value = assetWeights[,j, with = FALSE] * shares[j])
    meanWeights[j] <- sum(assetWeights[c(FALSE, missingPoints[,j]), j, with = FALSE])/sum(missingPoints[,j])
    meanSquaredWeights[j] <- sum(assetWeights[c(FALSE, missingPoints[,j]), j, with = FALSE]^2)/sum(missingPoints[,j])
  }
  
  
  if(noiseCorrection){
    for (i in 1:nComps) {
      noiseRobust[i] <- noiseRobustEstimator(pData[c(TRUE, missingPoints[, i + 2]), c(1,i + 2), with = FALSE], makeReturns = TRUE, ...)
    }
  }
  
  
  meanWeights <- meanWeights[-c(1,2)]
  meanSquaredWeights <- meanSquaredWeights[-c(1,2)]
  shares <- shares[-c(1,2)]
  impliedBeta <- bacImpliedBetaCpp(as.matrix(returns[, -1, with = FALSE]), missingPoints[, -c(1,2)], as.matrix(assetWeights[-1, -c(1,2), with = FALSE]))
  impliedBeta <- as.numeric(impliedBeta)
  # impliedBeta <- colSums(impliedBeta[["beta"]])
  
  if(targetBeta == "HY"){
    targetBeta <- numeric(ncol(returns) - 1)
    weightings <- rep(1, nrow(returns)) ## The bacHY code allows for both weighted and unweighted HY - here we don't weigh and set weights to 1
    
    for (i in 2:ncol(returns)) {
      targetBeta[i-1] <- bacHY(as.matrix(returns[, i, with = FALSE]), as.matrix(etfReturns), missingPoints[,i+1], missingPoints[,2], weightings)
    }
    
  } else if(targetBeta == "VAB"){
    
    targetBeta <- numeric(ncol(returns) - 1)
    weightings <- rep(1, nrow(returns)) ## The bacHY code allows for both weighted and unweighted HY - here we don't weigh and set weights to 1
    
    for (i in 2:ncol(returns)) {
      targetBeta[i-1] <- bacHY(as.matrix(returns[, i, with = FALSE]), as.matrix(etfReturns), missingPoints[,i+1], missingPoints[,2], weightings)
    }
    tempBeta <- targetBeta
    
    sqw <- 0
    aw <- targetBeta <- numeric(ncol(returns) - 1)
    ETFLogReturns <- as.matrix(diff(log(pData[[ETFNAME]])))
    
    logw <- 1/(pData[1:(.N-1),ETFNAME, with =FALSE])^2
    for (i in 2:ncol(returns)) {
      targetBeta[i-1] <- bacHY(as.matrix(returns[, i, with = FALSE]), etfReturns,
                               missingPoints[,i+1], missingPoints[,2], as.matrix(assetWeights[-1, i + 1, with = FALSE] * logw))
      tw <- (logw* assetWeights[-1, i + 1, with = FALSE])[missingPoints[,i+1]]
      aw[i-1] <- mean(tw[[1]])
      sqw <- sqw + mean(tw[[1]]^2)
    }
    noisyETF <- preEstimator(pData[, c(1,2), with = FALSE], makeReturns = TRUE, ...)
    noiseFreeETF <- noiseRobustEstimator(pData[, c(1,2), with = FALSE], makeReturns = TRUE, ...)
    etfNoise <- noisyETF[[length(noisyETF)]] - noiseFreeETF[[length(noiseFreeETF)]]
    
    targetBeta <- tempBeta + aw/sqw * (sum(diff(log(pData[[ETFNAME]]))[missingPoints[,2]]^2) - etfNoise - sum(targetBeta))
    
  } else if(targetBeta == "expert"){
    if(!is.numeric(expertBeta) & length(expertBeta) == (ncol(returns) - 1)){
      targetBeta <- expertBeta
    } else {
      stop("expertBeta is not a numeric with same length as the number of components in the ETF." )
    }
  }
  
  if(noiseCorrection){
    noise <- diag(RC2) - noiseRobust
    impliedBeta <- impliedBeta - noise * meanWeights
    NS <- noise/pmax(1, colSums(missingPoints[, -c(1,2)]))
  } else {
    NS <- 0
    noise <- numeric(nComps)
  }
  for (i in 1:nComps){
    # If noiseCorrection is not TRUE, NS = 0, thus exp(NS * .) = 1 and we can reuse noise and no noise code
    W[i, ((i-1) * nComps + 1):(nComps * i)] <- meanWeights * exp(NS * 0.25) 
  }
    
  for (i in 1:nComps){
    for (j in 1:nComps){
      if(i == j & !unrestricted){ # If unrestricted is true, we change the main diagonal too
        Q[(i-1) * nComps + i, (i-1) * nComps + i] <- 1
      } else if(j != i) {
        Q[(i-1) * nComps + j, (j-1) * nComps + i] <- -0.5
        Q[(i-1) * nComps + j, (i-1) * nComps + j] <- 0.5
      }
    }
  }
  
  L <- (diag(nComps^2) - Q) %*% t(W)
  # If noiseCorrection is not TRUE, NS = 0, thus exp(NS * .) = 1 and we can reuse noise and no noise code
  L <- L %*% (solve(diag(nComps) * sum(meanSquaredWeights * exp(NS * 0.5)) - W %*% Q %*% t(W)))
  
  if(returnL){
    return(list("BAC" = RC2 - (matrix(L %*% (impliedBeta - targetBeta), ncol = nComps) + diag(noise)), "L" = L))
  } else {
    return(RC2 - (matrix(L %*% (impliedBeta - targetBeta), ncol = nComps) + diag(noise)))
    
  }
}