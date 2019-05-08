#' An estimator of integrated quarticity from applying the median operator on blocks of three returns.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @description Function returns the medRQ, defined in Andersen et al. (2012).
#'   
#'   Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'   
#'   Then, the medRQ is given by
#'  \deqn{
#'    \mbox{medRQ}_{t}=\frac{3\pi N}{9\pi +72 - 52\sqrt{3}} \left(\frac{N}{N-2}\right) \sum_{i=2}^{N-1} \mbox{med}(|r_{t,i-1}|, |r_{t,i}|, |r_{t,i+1}|)^4
#'   }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#' @param ... additional arguments.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' medRQ(rdata = sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' medRQ
#' }
#' @keywords highfrequency medRQ
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @importFrom zoo rollmedian
#' @export
medRQ <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  multixts <- .multixts(rdata)
  if (multixts) {
    result <- apply.daily(rdata, medRQ, align.by, align.period, makeReturns) 
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if(makeReturns) {
      rdata <- makeReturns(rdata)
    }
    q <- abs(as.numeric(rdata))
    q <- as.numeric(rollmedian(q, k = 3,align="center"))
    N <- length(q) + 2
    medRQ <- 3 * pi * N / (9 * pi + 72 - 52 * sqrt(3)) * (N / (N-2)) * sum(q^4)
    return(medRQ)
  } 
}

#' An estimator of integrated quarticity from applying the minimum operator on blocks of two returns.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @description Function returns the minRQ, defined in Andersen et al. (2012).
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' Then, the minRQ is given by
#' \deqn{
#'   \mbox{minRQ}_{t}=\frac{\pi N}{3 \pi - 8} \left(\frac{N}{N-1}\right) \sum_{i=1}^{N-1} \mbox{min}(|r_{t,i}| ,|r_{t,i+1}|)^4
#' }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours"
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' minRQ(rdata = sample_tdata$PRICE, align.by = "minutes", align.period = 5, makeReturns = TRUE)
#' minRQ
#' }
#'@references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @importFrom zoo as.zoo
#' @importFrom zoo rollapply
#' @export
minRQ <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) {
    result <- apply.daily(rdata, minRQ, align.by, align.period, makeReturns)
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if(makeReturns) {
      rdata = makeReturns(rdata)
    }
    q     <- as.zoo(abs(as.numeric(rdata)))
    q     <- as.numeric(rollapply(q, width = 2, FUN = min, by = 1, align = "left"))
    N     <- length(q) + 1
    minRQ <- pi * N/(3 * pi - 8)*(N / (N - 1)) * sum(q^4)
    return(minRQ)
  }
}

#' Realized semivariance of highfrequency return series. 
#' @description Function returns realized semivariances, defined in Barndorff-Nielsen et al. (2008).
#' 
#' Function returns two outcomes: 1.Downside realized semivariance and 2.Upside realized semivariance.
#' 
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#' 
#' Then, the rSV is given by
#' \deqn{
#'   \mbox{rSVdownside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2  \ \times \ I [ r_{t,i} <0 ]
#' }
#'   \deqn{
#'   \mbox{rSVupside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2 \ \times \ I [ r_{t,i} >0 ]
#' }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.
#' @return list with to arguments. The realized positive and negative semivariance.
#' @examples 
#' \dontrun{
#' data(sample_tdata)
#' rSV(sample_tdata$PRICE,align.by ="minutes", align.period =5, makeReturns = TRUE)
#' }
#' @references Barndorff-Nielsen, O.E., Kinnebrock, S. and Shephard N. (2008). Measuring downside risk - realized semivariance. CREATES research paper. p. 3-5.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @keywords  highfrequency rSV
#' @export
rSV <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata = data
  }
  
  multixts <-  .multixts(rdata)
  
  if (multixts) {
    result <- apply.daily(rdata, rSV, align.by, align.period, makeReturns)
    return(result)
  }
  
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns)  {
      rdata <- makeReturns(rdata)
    }
    
    q <- as.numeric(rdata)
    select.down <- rdata < 0
    select.up <- rdata > 0
    
    rSVd <- sum(q[select.down]^2)
    rSVu <- sum(q[select.up]^2)
    
    out <- list(rSVdownside = rSVd, rSVupside = rSVu)
    return(out)
  }
}


#' An estimator of realized variance.
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' 
#' @return numeric
#' 
#' @keywords highfrequency RV
#' @export
RV <- function(rdata) {
  if (hasArg(data)) { 
    rdata = data 
  }
  returns <- as.numeric(rdata)
  RV <- sum(returns^2)
  return(RV)
}

#' Realized quarticity of highfrequency return series. 
#' @description  Function returns the rQuar, defined in Andersen et al. (2012).
#'
#' Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'  
#'  Then, the rQuar is given by
#'  \deqn{
#'    \mbox{rQuar}_{t}=\frac{N}{3} \sum_{i=1}^{N} \mbox(r_{t,i}^4)
#'  }
#'  
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours".
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours].
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.
#' 
#' @return numeric
#'
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @references  Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @examples 
#' \dontrun{
#' data(sample_tdata)
#' rQuar(rdata= sample_tdata$PRICE, align.by= "minutes", align.period =5, makeReturns= TRUE)
#' }
#' @keywords  highfrequency rQuar
#' @export
rQuar <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE) {
  if (hasArg(data)) {
    rdata <- data
  }
  multixts = .multixts(rdata)
  if (multixts) {
    result <- apply.daily(rdata, rQuar, align.by, align.period, makeReturns)
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata <- .aggregatets(rdata, on = align.by, k = align.period)
    }
    if (makeReturns == TRUE) {
      rdata <- makeReturns(rdata)
    }
    q     <- as.numeric(rdata)
    N     <- length(q) + 1
    rQuar <- N/3 * sum(q^4)
    return(rQuar)
  }
}


