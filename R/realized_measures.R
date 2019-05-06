#' An estimator of integrated quarticity from applying the median operator on blocks of three returns.
#' @author Giang Nguyen, Jonathan Cornelissen and Kris Boudt
#' @description
#'   
#'   Function returns the medRQ, defined in Andersen et al. (2012).
#'   
#'   Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.
#'   
#'   Then, the medRQ is given by
#'  \deqn{
#'    \mbox{medRQ}_{t}=\frac{3\pi N}{9\pi +72 - 52\sqrt{3}} \left(\frac{N}{N-2}\right) \sum_{i=2}^{N-1} \mbox{med}(|r_{t,i-1}|, |r_{t,i}|, |r_{t,i+1}|)^4
#'   }
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param align.by a string, align the tick data to "seconds"|"minutes"|"hours"
#' @param align.period an integer, align the tick data to this many [seconds|minutes|hours]. 
#' @param makeReturns boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.
#' @param ... additional arguments.
#' 
#' @return numeric
#' 
#' @examples
#' \dontrun{
#' data(sample_tdata)
#' medRQ(rdata= sample_tdata$PRICE, align.by= "minutes", align.period =5, makeReturns= TRUE)
#' medRQ
#'}
#' @keywords highfrequency medRQ
#' @references Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.
#' @export
medRQ <- function(rdata, align.by = NULL, align.period = NULL, makeReturns = FALSE,...)
{
  if (hasArg(data)) {
    rdata = data
  }
  multixts = .multixts(rdata)
  if (multixts) {
    result = apply.daily(rdata, medRQ, align.by, align.period, makeReturns) 
    return(result)
  }
  if (!multixts) {
    if ((!is.null(align.by)) && (!is.null(align.period))) {
      rdata = .aggregatets(rdata, on = align.by, k = align.period)
    }
    if(makeReturns) {
      rdata = makeReturns(rdata)
    }
    q = abs(as.numeric(rdata))
    q = as.numeric(rollmedian(q, k = 3,align="center"))
    N = length(q)+2
    medRQ = 3*pi*N/(9*pi+72-52*sqrt(3))*(N/(N-2))*sum(q^4)
    return(medRQ)
  } 
}

#' An estimator of realized variance.
#' @param rdata a zoo/xts object containing all returns in period t for one asset.
#' @param ... additional arguments.
#' 
#' @return numeric
#' 
#' @keywords highfrequency RV
#' @export
RV <- function(rdata, ...) {
  if (hasArg(data)) { rdata = data }
  returns <- as.numeric(rdata)
  RV <- sum(returns^2)
  return(RV)
}