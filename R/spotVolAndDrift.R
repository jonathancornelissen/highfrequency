#' Spot Drift Estimation
#' @description Function used to estimate the spot drift of intraday (tick) stock prices/returns
#'
#' @param data Can be one of two input types, \code{xts} or \code{data.table}. It is assumed that the input comprises prices in levels.
#' @param method Which method to be used to estimate the spot-drift. Currently, three methods are available, 
#' rolling mean and median as well as the kernel method of Christensen et al. (2018).
#' The kernel is a left hand exponential kernel that will weigh newer observations more heavily than older observations.
#' @param alignBy What time-frame should the estimator be applied? Accepted inputs are \code{"milliseconds"}, \code{"seconds"} and \code{"secs"} for seconds,
#'  \code{"minutes"} and \code{"mins"} for minutes, and \code{"hours"} for hours.
#' Standard is minutes
#' @param alignPeriod How often should the estimation take place? If \code{alignPeriod} is 5 the estimation will be done every fifth unit of \code{alignBy}.
#' @param marketOpen Opening time of the market, standard is "09:30:00".
#' @param marketClose Closing time of the market, standard is "16:00:00".
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. 
#' We attempt to extract the timezone from the \code{DT} column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}.
#' @param ... Additional arguments for the individual methods. See `Details'.
#' @return An object of class \code{"spotDrift"} containing at least the estimated spot drift process. 
#' Input on what this class should contain and methods for it is welcome.
#'
#' @details The additional arguments for the mean and median methods are: 
#' \itemize{
#' \item \code{periods} for the rolling window length which is 5 by default.
#' \item \code{align} controls the alignment. The default is \code{"right"}. 
#' }
#' For the kernel mean estimator, the arguments \code{meanBandwidth} can be used to control the bandwidth of the 
#' drift estimator and the \code{preAverage} argument, which can be used to control the pre-averaging horizon. 
#' These arguments default to 300 and 5 respectively.
#' 
#' The following estimation methods can be specified in \code{method}:
#'
#' \strong{Rolling window mean (\code{"mean"})}
#' 
#' Estimates the spot drift by applying a rolling mean over returns.
#' \deqn{
#'     \hat{\mu_{t}} = \sum_{t = k}^{T} \textrm{mean} \left(r_{t-k : t} \right),
#' }
#' where \eqn{k} is the argument \code{periods}.
#' Parameters:
#' \itemize{
#'    \item{\code{periods}}{ how big the window for the estimation should be. The estimator will have \code{periods} \code{NA}s at the beginning of each trading day.}
#'    \item{\code{align}}{ alignment method for returns. Defaults to \code{"left"}, which includes only past data, but other choices, \code{"center"} and \code{"right"} are available.
#'     Warning: These values includes future data.}
#' }
#' Outputs:
#' \itemize{
#'  \item{\code{mu} a matrix containing the spot drift estimates}
#' }
#'   
#' \strong{Rolling window median (\code{"median"})}
#' 
#' Estimates the spot drift by applying a rolling mean over returns.
#' \deqn{
#'     \hat{\mu_{t}} = \sum_{t = k}^{T} \textrm{median} \left(r_{t-k : t} \right),
#' }
#' where \eqn{k} is the argument \code{periods}.
#' Parameters:
#'  \itemize{
#'    \item{\code{periods}}{ How big the window for the estimation should be. The estimator will have \code{periods} \code{NA}s at the beginning of each trading day.}
#'    \item{\code{align}}{ Alignment method for returns. Defaults to \code{"left"}, which includes only past data, but other choices, \code{"center"} and \code{"right"} are available.
#'     These values includes FUTURE DATA, so beware!}
#' }
#' Outputs:
#' \itemize{
#'  \item{\code{mu} a matrix containing the spot drift estimates}
#' }
#' 
#' \strong{kernel spot drift estimator (\code{"kernel"})}
#' 
# Let the efficient log-price be defined as:
#' \deqn{
#'     dX_{t} = \mu_{t}dt + \sigma_{t}dW_{t} + dJ_{t},
#' }
#' where \eqn{\mu_{t}}, \eqn{\sigma_{t}}, and \eqn{J_{t}} are the spot drift, the spot volatility, and a jump process respectively.
#' However, due to microstructure noise, the observed log-price is 
#' \deqn{
#'     Y_{t} = X_{t} + \varepsilon_{t}
#' }
#' 
#' In order robustify the results to the presence of market microstructure noise, the pre-averaged returns are used:
#' \deqn{
#'     \Delta_{i}^{n}\overline{Y} = \sum_{j=1}^{k_{n}-1}g_{j}^{n}\Delta_{i+j}^{n}Y,
#' }
#'
#' where \eqn{g(\cdot)} is a weighting function, \eqn{min(x, 1-x)}, and \eqn{k_{n}} is the pre-averaging horizon.
#' The spot drift estimator is then:
#' \deqn{
#'     \hat{\bar{\mu}}_{t}^{n} = \sum_{i=1}^{n-k_{n}+2}K\left(\frac{t_{i-1}-t}{h_{n}}\right)\Delta_{i-1}^{n}\overline{Y},
#' }
#' The kernel estimation method has the following parameters:
#'  \itemize{
#'    \item{\code{preAverage}}{ a positive \code{integer} denoting the length of pre-averaging window for the log-prices. Default is 5}
#'    \item{\code{meanBandwidth}}{ an \code{integer} denoting the bandwidth for the left-sided exponential kernel for the mean. Default is \code{300L}}
#' }
#' Outputs:
#' \itemize{
#'  \item{\code{mu} a matrix containing the spot drift estimates}
#' }
#' @references 
#' Christensen, K., Oomen, R., and Reno, R. (2020) The drift burst hypothesis. Journal of Econometrics. Forthcoming.
#' @author 	Emil Sjoerup.
#' @keywords Drift
#'
#' @examples
#' # Example 1: Rolling mean and median estimators for 2 days
#' meandrift <- spotDrift(data = sampleTData, alignPeriod = 1)
#' mediandrift <- spotDrift(data = sampleTData, method = "median", 
#'                          alignBy = "seconds", alignPeriod = 30, tz = "EST")
#' plot(meandrift)
#' plot(mediandrift)
#'\dontrun{
#' # Example 2: Kernel based estimator for one day with data.table format
#' price <- sampleTData[as.Date(DT) == "2018-01-02", list(DT, PRICE)]
#' kerneldrift <- spotDrift(sampleTDataEurope, method = "driftKernel",
#'                          alignBy = "minutes", alignPeriod = 1)
#' plot(kerneldrift)
#'}
#'
#' @importFrom stats filter time
#' @importFrom utils tail
#' @importFrom xts xtsible merge.xts
#' @importFrom data.table setkeyv rbindlist
#' @export
spotDrift <- function(data, method = "mean", alignBy = "minutes", alignPeriod = 5,
                     marketOpen = "09:30:00", marketClose = "16:00:00", tz = NULL, ...) {

  PRICE <- DATE <- RETURN <- NULL

  if (!("PRICE" %in% colnames(data))) {
    if (dim(data)[2] == 1) {
      names(data) <- "PRICE"
    } else {
      stop("data.table or xts needs column named PRICE.")
    }
  }

  dummy_was_xts <- FALSE
  if (!is.data.table(data)) {
    if (is.xts(data)) {
      data <- setnames(as.data.table(data), old = "index", new = "DT")
      data[, PRICE := as.numeric(PRICE)]
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    if (!("DT" %in% colnames(data))) {
      stop("Data.table needs DT column containing the time-stamps of the trades.") # added the timestamp comment for verbosity.
    }
  }
  datad <- aggregatePrice(data[, DATE := ""], alignBy = alignBy, alignPeriod = alignPeriod , marketOpen = marketOpen,
                          marketClose = marketClose, tz = tz, fill = TRUE) # The allocation of date let's aggregatePrice return the date so we don't have to calculate it multiple times.
  setkeyv(datad, "DT")
  datad <- datad[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = "DATE"][!is.na(RETURN)]
  datad <- split(datad, by = "DATE")
  mR <- matrix(unlist(lapply(datad, FUN = function(x) as.numeric(x$RETURN))), ncol = length(datad[[1]]$RETURN), byrow = TRUE)

  if (method != "driftKernel") {
    mR <- t(mR)
    # mR2 <- t(mR2)
  }

  if (method == "driftKernel") {
    intraday <- as.numeric(datad[[1]]$DT)
  }
  options <- list(...)
  out <- switch(method, ### driftKernel works only for one day at a time! Does the rest of data preparation in the function.
                driftKernel = driftKernel(data = data, intraday, options),
                mean   = driftMean(mR = mR, options),
                median = driftMedian(mR = mR, options))
  

  return(out)
}

#' Spot volatility estimation
#' 
#' @description 
#' Estimates a wide variety of spot volatility estimators.
#'
#' @param data Can be one of two input types, \code{xts} or \code{data.table}. 
#' It is assumed that the input comprises prices in levels. Irregularly spaced
#' observations are allowed. They will be aggregated to the level specified by
#' parameters \code{alignBy} and \code{alignPeriod}.
#'
#' @param method specifies which method will be used to estimate the spot
#' volatility. Options include \code{"detPer"} and \code{"stochper"}.
#' See `Details'.
#' @param alignBy string indicating the time scale in which \code{alignPeriod} is expressed.
#' Possible values are: \code{"secs", "seconds", "mins", "minutes", "hours"}.
#' @param alignPeriod positive integer, indicating the number of periods to aggregate
#' over. For example, to aggregate an \code{xts} object to the 5-minute frequency, set
#' \code{alignPeriod = 5} and \code{alignBy = "minutes"}.
#' @param marketOpen the market opening time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketOpen = "09:30:00"}.
#' @param marketClose the market closing time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketClose = "16:00:00"}.
#' @param tz fallback time zone used in case we we are unable to identify the timezone of the data, by default: \code{tz = NULL}. 
#' We attempt to extract the timezone from the DT column (or index) of the data, which may fail. 
#' In case of failure we use \code{tz} if specified, and if it is not specified, we use \code{"UTC"}
#' @param ... method-specific parameters (see `Details' below).
#'
#' @return A \code{spotVol} object, which is a list containing one or more of the
#' following outputs, depending on the method used:
#'
#' \itemize{ 
#' \item \code{spot}
#'
#' An \code{xts} or \code{matrix} object (depending on the input) containing
#' spot volatility estimates \eqn{\sigma_{t,i}}, reported for each interval
#' \eqn{i} between \code{marketOpen} and \code{marketClose} for every day
#' \eqn{t} in \code{data}. The length of the intervals is specified by \code{alignPeriod}
#' and \code{alignBy}. Methods that provide this output: All.
#'
#' \code{daily}
#' An \code{xts} or \code{numeric} object (depending on the input) containing
#' estimates of the daily volatility levels for each day \eqn{t} in \code{data},
#' if the used method decomposed spot volatility into a daily and an intraday
#' component. Methods that provide this output: \code{"detPer"}.
#'
#' \item \code{periodic}
#'
#' An \code{xts} or \code{numeric} object (depending on the input) containing
#' estimates of the intraday periodicity factor for each day interval \eqn{i}
#' between \code{marketOpen} and \code{marketClose}, if the spot volatility was
#' decomposed into a daily and an intraday component. If the output is in
#' \code{xts} format, this periodicity factor will be dated to the first day of
#' the input data, but it is identical for each day in the sample. Methods that
#' provide this output: \code{"detPer"}.
#'
#' \item \code{par}
#'
#' A named list containing parameter estimates, for methods that estimate one
#' or more parameters. Methods that provide this output:
#' \code{"stochper", "kernel"}.
#'
#' \item \code{cp}
#'
#' A vector containing the change points in the volatility, i.e. the observation
#' indices after which the volatility level changed, according to the applied
#' tests. The vector starts with a 0. Methods that provide this output:
#'  \code{"piecewise"}.
#'
#' \item \code{ugarchfit}
#'
#' A \code{ugarchfit} object, as used by the \code{rugarch}
#' package, containing all output from fitting the GARCH model to the data.
#' Methods that provide this output: \code{"garch"}.
#'
#' The \code{spotVol} function offers several methods to estimate spot
#' volatility and its intraday seasonality, using high-frequency data. It
#' returns an object of class \code{spotVol}, which can contain various outputs,
#' depending on the method used. See `Details' for a description of each method.
#' In any case, the output will contain the spot volatility estimates.
#'
#' The input can consist of price data or return data, either tick by tick or
#' sampled at set intervals. The data will be converted to equispaced
#' high-frequency returns \eqn{r_{t,i}} (read: the \eqn{i}-th return on day
#'                                      \eqn{t}).
#' }
#' @details The following estimation methods can be specified in \code{method}:
#'
#' \strong{Deterministic periodicity method (\code{"detPer"})}
#'
#' Parameters:
#' \itemize{
#' \item \code{dailyVol} A string specifying the estimation method for the daily component \eqn{s_t}.
#' Possible values are \code{"bipower", "rv", "medrv"}. \code{"bipower"} by default.
#' \item \code{periodicVol} A string specifying the estimation method for the component of intraday volatility,
#' that depends in a deterministic way on the intraday time at which the return is observed.
#' Possible values are \code{"SD", "WSD", "TML", "OLS"}. See Boudt et al. (2011) for details. Default = \code{"TML"}.
#' \item \code{P1} A positive integer corresponding to the number of cosine terms used in the flexible Fourier
#' specification of the periodicity function, see Andersen et al. (1997) for details. Default = 5.
#' \item \code{P2} Same as \code{P1}, but for the sine terms. Default = 5. 
#' \item \code{dummies} Boolean: in case it is \code{TRUE}, the parametric estimator of periodic standard deviation
#' specifies the periodicity function as the sum of dummy variables corresponding to each intraday period.
#' If it is \code{FALSE}, the parametric estimator uses the flexible Fourier specification. Default is \code{FALSE}. 
#' }
#' 
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item \code{spot}
#' \item \code{daily}
#' \item \code{periodic}
#' }
#' 
#' Let there be \eqn{T} days of \eqn{N} equally-spaced log-returns \eqn{r_{i,t}}, 
#' \eqn{i = 1, \dots, N} and \eqn{i = 1, \dots, T}.
#' In case of \code{method = "detPer"}, the returns are modeled as
#' \deqn{
#' r_{i,t} = f_i s_t u_{i,t}
#' }
#' with independent \eqn{u_{i,t} \sim \mathcal{N}(0,1)}.
#' The spot volatility is decomposed into a deterministic periodic factor
#' \eqn{f_{i}} (identical for every day in the sample) and a daily factor
#' \eqn{s_{t}} (identical for all observations within a day). 
#' Both components are then estimated separately, see Taylor and Xu (1997)
#' and Andersen and Bollerslev (1997). The jump robust versions by Boudt et al.
#' (2011) have also been implemented.
#' 
#' If \code{periodicVol = "SD"}, we have
#' \deqn{
#' \hat f_i^{SD} = \frac{SD_i}{\sqrt{\frac{1}{\lfloor{\lambda / \Delta}\rfloor} \sum_{j = 1}^N SD_j^2}}
#' }
#' with \eqn{\Delta = 1 / N}, cross-daily averages \eqn{SD_i = \sqrt{1/T \sum_{i = t}^T r_{i,t}^2}}, 
#' and \eqn{\lambda} being the length of the intraday time intervals.
#' 
#' If \code{periodicVol = "WSD"}, we have another nonparametric estimator that is robust to jumps in contrast to
#' \code{periodicVol = "SD"}. The definition of this estimator can be found in Boudt et al. (2011, Eqs. 2.9-2.12).
#' 
#' The estimates when \code{periodicVol = "OLS"} and \code{periodicVol = "TML"} are based on the regression equation
#' \deqn{
#' \log \left| 1/T \sum_{t = 1}^T r_{i,t} \right| - c = \log f_i + \varepsilon_i
#' }
#' with \emph{i.i.d.} zero-mean error term \eqn{\varepsilon_i} and \eqn{c = -0.63518}. 
#' \code{periodicVol = "OLS"} employs ordinary-least-squares estimation and 
#' \code{periodicVol = "TML"} truncated maximum-likelihood estimation (see Boudt et al., 2011, Section 2.2, for further details).
#' 
#' \strong{Stochastic periodicity method (\code{"stochPer"})}
#' 
#' Parameters:
#' \itemize{
#' \item{\code{P1}: A positive integer corresponding to the number of cosine terms used in the flexible Fourier
#'  specification of the periodicity function. Default = 5. }
#' \item{\code{P2}: Same as \code{P1}, but for the sine terms. Default = 5.}
#' \item{\code{init}: A named list of initial values to be used in the optimization routine (\code{"BFGS"} in \code{optim}).
#'  Default = \code{list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.005, sigma_k = 0.05,
#'   phi = 0.2, rho = 0.98, mu = c(2, -0.5), delta_c = rep(0, max(1,P1)),
#' delta_s = rep(0, max(1,P2)))}. 
#' The naming of the parameters follows Beltratti and Morana (2001), the corresponding model equations are listed below.
#' \code{init} can contain any number of these parameters.
#' For parameters not specified in \code{init}, the default initial value will be used.}
#' \item{\code{control}: A list of options to be passed down to \code{optim}.}
#' }
#' 
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{par}}
#' }
#' This method by Beltratti and Morana (2001) assumes the periodicity factor to
#' be stochastic. The spot volatility estimation is split into four components:
#' a random walk, an autoregressive process, a stochastic cyclical process and
#' a deterministic cyclical process. The model is estimated using a
#' quasi-maximum likelihood method based on the Kalman Filter. The package
#' \code{FKF} is used to apply the Kalman filter. In addition to
#' the spot volatility estimates, all parameter estimates are returned.
#' 
#' The model for the intraday change in the return series is given by
#' 
#' \deqn{
#' r_{t,n} = \sigma_{t,n} \varepsilon_{t,n}, \ t = 1, \dots, T; \ n = 1, \dots, N,
#' }
#' where \eqn{\sigma_{t,n}} is the conditional standard deviation of the \eqn{n}-th interval
#' of day \eqn{t} and \eqn{\varepsilon_{t,n}} is a \emph{i.i.d.} mean-zero unit-variance process.
#' The conditional standard deviations are modeled as
#' \deqn{
#' \sigma_{t,n} = \sigma \exp \left(\frac{\mu_{t,n} + h_{t,n} + c_{t,n}}{2} \right)
#' }
#' with \eqn{\sigma} being a scaling factor and \eqn{\mu_{t,n}} is the non-stationary volatility
#' component
#' \deqn{
#' \mu_{t,n} = \mu_{t,n-1} + \xi_{t,n}
#' }
#' with independent \eqn{\xi_{t,n} \sim \mathcal{N}(0,\sigma_\xi^2)}. 
#' \eqn{h_{t,n}} is the stochastic stationary acyclical volatility component
#' \deqn{
#' h_{t,n} = \phi h_{t,n-1} + \nu_{t,n} 
#' }
#' with independent \eqn{\eta_{t,n} \sim \mathcal{N}(0,\sigma_\eta^2)} and \eqn{| \phi | \leq 1}.
#' The cyclical component is separated in two components:
#' \deqn{
#' c_{t,n} = c_{1,t,n} + c_{2,t,n}
#' } 
#' The first component is written in state-space form, 
#' \deqn{
#' \left( \begin{array}{r}
#' c_{1,t,n} \\ c_{1,t,n}^*
#' \end{array}\right) =
#' \rho 
#' \left(\begin{array}{rr}
#' \cos \lambda & \sin \lambda \\ -\sin \lambda & \cos \lambda
#' \end{array}\right)
#' \left(\begin{array}{r}
#' c_{1,t,n - 1} \\ c_{1,t,n-1}^*
#' \end{array}\right)
#' +
#' \left(\begin{array}{r}
#' \kappa_{1,t,n} \\ \kappa_{1,t,n}^*
#' \end{array}\right)
#' }
#' with \eqn{0 \leq \rho \leq 1} and \eqn{\kappa_{1,t,n}, \kappa_{1,t,n}^*} are 
#' mutually independent zero-mean normal random variables with variance \eqn{\sigma_\kappa^2}.
#' All other parameters and the process \eqn{c_{1,t,n}^*} in the state-space representation 
#' are only of instrumental use and are not part of
#' the return value which is why we won't introduce them in detail
#' in this vignette; see Beltratti and Morana (2001, pp. 208-209) for more information.
#' 
#' The second component is given by
#' \deqn{
#' c_{2,t,n} = \mu_1 n_1 + \mu_2 n_2 + \sum_{p = 2}^P (\delta_{cp} \cos(p\lambda) + \delta_{sp} \sin (p \lambda n))
#' }
#' with \eqn{n_1 = 2n / (N+1)} and \eqn{n_2 = 6n^2 / (N+1) / (N+2)}. 
#' 
#' \strong{Nonparametric filtering (\code{"kernel"})}
#'
#' Parameters:
#' \itemize{
#' \item{\code{type}}{String specifying the type of kernel to be used. Options
#' include \code{"gaussian", "epanechnikov", "beta"}. Default = \code{"gaussian"}.}
#' \item{\code{h}}{Scalar or vector specifying bandwidth(s) to be used in kernel.
#' If \code{h} is a scalar, it will be assumed equal throughout the sample. If
#' it is a vector, it should contain bandwidths for each day. If left empty,
#' it will be estimated. Default = \code{NULL}.}
#' \item{\code{est}}{String specifying the bandwidth estimation method. Possible
#' values include \code{"cv", "quarticity"}. Method \code{"cv"} equals
#' cross-validation, which chooses the bandwidth that minimizes the Integrated
#' Square Error. \code{"quarticity"} multiplies the simple plug-in estimator
#' by a factor based on the daily quarticity of the returns. \code{est} is
#' obsolete if \code{h} has already been specified by the user.
#' \code{"cv"} by default.}
#' \item{\code{lower}}{Lower bound to be used in bandwidth optimization routine,
#' when using cross-validation method. Default is \eqn{0.1n^{-0.2}}.}
#' \item{\code{upper}}{Upper bound to be used in bandwidth optimization routine,
#' when using cross-validation method. Default is \eqn{n^{-0.2}}.}
#' }
#'
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{par}}
#' }
#'
#' This method by Kristensen (2010) filters the spot volatility in a
#' nonparametric way by applying kernel weights to the standard realized
#' volatility estimator. Different kernels and bandwidths can
#' be used to focus on specific characteristics of the volatility process.
#'
#' Estimation results heavily depend on the bandwidth parameter \eqn{h}, so it
#' is important that this parameter is well chosen. However, it is difficult to
#' come up with a method that determines the optimal bandwidth for any kind of
#' data or kernel that can be used. Although some estimation methods are
#' provided, it is advised that you specify \eqn{h} yourself, or make sure that
#' the estimation results are appropriate.
#'
#' One way to estimate \eqn{h}, is by using cross-validation. For each day in
#' the sample, \eqn{h} is chosen as to minimize the Integrated Square Error,
#' which is a function of \eqn{h}. However, this function often has multiple
#' local minima, or no minima at all (\eqn{h \rightarrow \infty}). To ensure a reasonable
#' optimum is reached, strict boundaries have to be imposed on \eqn{h}. These
#' can be specified by \code{lower} and \code{upper}, which by default are
#' \eqn{0.1n^{-0.2}} and \eqn{n^{-0.2}} respectively, where \eqn{n} is the
#' number of observations in a day.
#'
#' When using the method \code{"kernel"}, in addition to the spot volatility
#' estimates, all used values of the bandwidth \eqn{h} are returned.
#' 
#' A formal definition of the estimator is too extensive for the context of this vignette.
#' Please refer to Kristensen (2010) for more detailed information. Our parameter
#' names are aligned with this reference.
#'
#' \strong{Piecewise constant volatility (\code{"piecewise"})}
#'
#' Parameters:
#' \itemize{
#' \item{\code{type} string specifying the type of test to be used. Options
#' include \code{"MDa", "MDb", "DM"}. See Fried (2012) for details. Default = \code{"MDa"}.}
#' \item{\code{m} number of observations to include in reference window.
#' Default = \code{40}.}
#' \item{\code{n} number of observations to include in test window.
#' Default = \code{20}.}
#' \item{\code{alpha} significance level to be used in tests. Note that the test
#' will be executed many times (roughly equal to the total number of
#' observations), so it is advised to use a small value for \code{alpha}, to
#' avoid a lot of false positives. Default = \code{0.005}.}
#' \item{\code{volEst} string specifying the realized volatility estimator to be
#' used in local windows. Possible values are \code{"bipower", "rv", "medrv"}.
#' Default = \code{"bipower"}.}
#' \item{\code{online} boolean indicating whether estimations at a certain point
#' \eqn{t} should be done online (using only information available at
#' \eqn{t-1}), or ex post (using all observations between two change points).
#' Default = \code{TRUE}.}
#' }
#'
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{cp}}
#' }
#'
#' This nonparametric method by Fried (2012) is a two-step approach and 
#' assumes the volatility to be
#' piecewise constant over local windows. Robust two-sample tests are applied to
#' detect changes in variability between subsequent windows. The spot volatility
#' can then be estimated by evaluating regular realized volatility estimators
#' within each local window.
#' \code{"MDa", "MDb", "DM"} refer to different test statistics, see Section 2.2 in Fried (2012).
#'
#' Along with the spot volatility estimates, this method will return the
#' detected change points in the volatility level. When plotting a
#' \code{spotVol} object containing \code{cp}, these change points will be
#' visualized.
#'
#' \strong{GARCH models with intraday seasonality  (\code{"garch"})}
#'
#' Parameters:
#' \itemize{
#' \item{\code{model} string specifying the type of test to be used. Options
#' include \code{"sGARCH", "eGARCH"}. See \code{ugarchspec} in the
#' \code{rugarch} package. Default = \code{"eGARCH"}.}
#' \item{\code{garchorder} numeric value of length 2, containing the order of
#' the GARCH model to be estimated. Default = \code{c(1,1)}.}
#' \item{\code{dist} string specifying the distribution to be assumed on the
#' innovations. See \code{distribution.model} in \code{ugarchspec} for
#' possible options. Default = \code{"norm"}.}
#' \item{\code{solver.control} list containing solver options.
#' See \code{ugarchfit} for possible values. Default = \code{list()}.}
#' \item{\code{P1} a positive integer corresponding to the number of cosine
#' terms used in the flexible Fourier specification of the periodicity function.
#' Default = 5.}
#' \item{\code{P2} same as \code{P1}, but for the sinus terms. Default = 5.}
#' }
#'
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{ugarchfit}}
#' }
#' 
#' Along with the spot volatility estimates, this method will return the
#' \code{ugarchfit} object used by the \code{rugarch} package.
#' 
#' In this model, daily returns \eqn{r_t} based on intraday observations \eqn{r_{i,t}, i = 1, \dots, N}
#' are modeled as
#' \deqn{
#' r_t = \sum_{i = 1}^N r_{i,t} = \sigma_t \frac{1}{\sqrt{N}} \sum_{i = 1}^N s_i Z_{i,t}.
#' }
#' with \eqn{\sigma_t > 0}, intraday seasonality \eqn{s_i} > 0, and \eqn{Z_{i,t}} being 
#' a zero-mean unit-variance error term.  
#' 
#' The overall approach is as in Appendix B of Andersen and Bollerslev (1997).
#' This method generates the external regressors \eqn{s_i} needed to model the intraday
#' seasonality with a flexible Fourier form (Andersen and Bollerslev, 1997, Eqs. A.1-A.4). 
#' The \code{rugarch} package is then employed to estimate the specified intraday GARCH(1,1) model 
#' on the residuals \eqn{r_{i,t} / s_i}.
#'
#' \strong{Realized Measures (\code{"RM"})}
#' 
#' This estimator takes trailing rolling window observations of intraday returns to estimate the spot volatility.
#' 
#' Parameters:
#' \itemize{
#' \item{\code{RM} string denoting which realized measure to use to estimate the local volatility. 
#' Possible values are: \code{"bipower", "medrv", "minrv", "rv"}.
#' Default = \code{"bipower"}}.
#' \item{\code{lookBackPeriod} positive integer denoting the amount of sub-sampled returns to use 
#' for the estimation of the local volatility. Default is \code{10}.}
#' \item{\code{dontIncludeLast} logical indicating whether to omit the last return in the calculation of the local volatility.
#'  This is done in Lee-Mykland (2008) to produce jump-robust estimates of spot volatility. 
#'  Setting this to \code{TRUE} will then use \code{lookBackPeriod - 1} returns in the construction of the realized measures. Default = \code{FALSE}}.
#' }
#'
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{RM}}
#' \item{\code{lookBackPeriod}}
#' }
#'
#' This method returns the estimates of the spot volatility, a string containing the realized measure used, and the lookBackPeriod.
#'
#' \strong{(Non-overlapping) Pre-Averaged Realized Measures (\code{"PARM"})}
#' 
#' This estimator takes rolling historical window observations of intraday returns to estimate the spot volatility 
#' as in the option \code{"RM"} but adds return pre-averaging of the realized measures. 
#' For a description of return pre-averaging see the details on \code{\link{spotDrift}.}
#' 
#' Parameters:
#' \itemize{
#' \item{\code{RM} String denoting which realized measure to use to estimate the local volatility.
#'  Possible values are: \code{"bipower", "medrv", "minrv", and "rv"}. Default = \code{"bipower"}}.
#' \item{\code{lookBackPeriod} positive integer denoting the amount of sub-sampled returns to use for the estimation of the local volatility. Default = 50.}
#' }
#' 
#' Outputs (see `Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{RM}}
#' \item{\code{lookBackPeriod}}
#' \item{\code{kn}}
#' }
#'
#' @examples
#' \dontrun{
#' init <- list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.007,
#'              sigma_k = 0.06, phi = 0.194, rho = 0.986, mu = c(1.87,-0.42),
#'              delta_c = c(0.25, -0.05, -0.2, 0.13, 0.02),
#'              delta_s = c(-1.2, 0.11, 0.26, -0.03, 0.08))
#'
#' # Next method will take around 370 iterations
#' vol1 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)], method = "stochPer", init = init)
#' plot(vol1$spot[1:780])
#' legend("topright", c("stochPer"), col = c("black"), lty=1)}
#'
#' # Various kernel estimates
#' \dontrun{
#' h1 <- bw.nrd0((1:nrow(sampleOneMinuteData[, list(DT, PRICE = MARKET)]))*60)
#' vol2 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)],
#'                 method = "kernel", h = h1)
#' vol3 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)], 
#'                 method = "kernel", est = "quarticity")
#' vol4 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)],
#'                 method = "kernel", est = "cv")
#' plot(cbind(vol2$spot, vol3$spot, vol4$spot))
#' xts::addLegend("topright", c("h = simple estimate", "h = quarticity corrected",
#'                      "h = crossvalidated"), col = 1:3, lty=1)
#'}
#'
#' # Piecewise constant volatility
#' \dontrun{
#' vol5 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)], 
#'                 method = "piecewise", m = 200, n  = 100, online = FALSE)
#' plot(vol5)}
#'
#' # Compare regular GARCH(1,1) model to eGARCH, both with external regressors
#' \dontrun{
#' vol6 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)], method = "garch", model = "sGARCH")
#' vol7 <- spotVol(sampleOneMinuteData[, list(DT, PRICE = MARKET)], method = "garch", model = "eGARCH")
#' plot(as.numeric(t(vol6$spot)), type = "l")
#' lines(as.numeric(t(vol7$spot)), col = "red")
#' legend("topleft", c("GARCH", "eGARCH"), col = c("black", "red"), lty = 1)
#' }
#' 
#' \dontrun{
#' # Compare realized measure spot vol estimation to pre-averaged version
#' vol8 <- spotVol(sampleTDataEurope[, list(DT, PRICE)], method = "RM", marketOpen = "09:00:00",
#'                 marketClose = "17:30:00", tz = "UTC", alignPeriod = 1, alignBy = "mins",
#'                 lookBackPeriod = 10)
#' vol9 <- spotVol(sampleTDataEurope[, list(DT, PRICE)], method = "PARM", marketOpen = "09:00:00",
#'                 marketClose = "17:30:00", tz = "UTC", lookBackPeriod = 10)
#' plot(zoo::na.locf(cbind(vol8$spot, vol9$spot)))
#' }
#' @author Jonathan Cornelissen, Kris Boudt, Onno Kleen, and Emil Sjoerup.
#'
#' @references 
#' Andersen, T. G. and Bollerslev, T. (1997). Intraday periodicity and volatility persistence in financial markets. \emph{Journal of Empirical Finance}, 4, 115-158.
#'
#' Beltratti, A. and Morana, C. (2001). Deterministic and stochastic methods for estimation of intraday seasonal components with high frequency data. \emph{Economic Notes}, 30, 205-234.
#'
#' Boudt K., Croux C., and Laurent S. (2011). Robust estimation of intraweek periodicity in volatility and jump detection. \emph{Journal of Empirical Finance}, 18, 353-367.
#'
#' Fried, R. (2012). On the online estimation of local constant volatilities. \emph{Computational Statistics and Data Analysis}, 56, 3080-3090.
#'
#' Kristensen, D. (2010). Nonparametric filtering of the realized spot volatility: A kernel-based approach. \emph{Econometric Theory}, 26, 60-93.
#'
#' Taylor, S. J. and Xu, X. (1997). The incremental volatility information in one million foreign exchange quotations. \emph{Journal of Empirical Finance}, 4, 317-340.
#' @export
spotVol <- function(data, method = "detPer", alignBy = "minutes", alignPeriod = 5,
                      marketOpen = "09:30:00", marketClose = "16:00:00",
                      tz = "GMT", ...) {
  
  DT <- PRICE <- DATE <- RETURN <- NULL
  
  validOptions <- c("detPer", "stochPer", "kernel", "piecewise", "garch", "RM", "PARM")
  method <- method[1]
  if(!(method %in% validOptions)){
    stop(c("method not a valid option, valid options are: ", paste(validOptions, collapse = ", ")))
  }
  
  
  if (!("PRICE" %in% colnames(data))) {
    if (dim(data)[2] == 1) {
      names(data) <- "PRICE"
    } else {
      stop("data.table or xts needs column named PRICE.")
    }
  }
  
  dummy_was_xts <- FALSE
  if (!is.data.table(data)) {
    if (is.xts(data)) {
      data <- setnames(as.data.table(data), old = "index", new = "DT")
      data[, PRICE := as.numeric(PRICE)]
      dummy_was_xts <- TRUE
    } else {
      stop("Input has to be data.table or xts.")
    }
  } else {
    data <- copy(data)
    if (!("DT" %in% colnames(data))) {
      stop("Data.table needs DT column containing the time-stamps of the trades.") # added the timestamp comment for verbosity.
    }
  }
  
  if( method != "PARM"){
    if(!is.null(alignPeriod) && !is.null(alignBy)){
      datad <- aggregatePrice(data, alignBy = alignBy, alignPeriod = alignPeriod , marketOpen = marketOpen,
                              marketClose = marketClose, tz = tz, fill = TRUE) # The allocation of date let's aggregatePrice return the date so we don't have to calculate it multiple times.
    } else {
      datad <- copy(data)
    }
    setkeyv(datad, "DT")
    datad <- datad[, RETURN := log(PRICE) - shift(log(PRICE), type = "lag"), by = list(DATE = as.Date(DT))][!is.na(RETURN)]
    rData <- xts(datad$RETURN, order.by = datad$DT)
    datad <- split(datad[, DATE := as.Date(DT)], by = "DATE")
    mR <- matrix(unlist(lapply(datad, FUN = function(x) as.numeric(x$RETURN))), ncol = length(datad[[1]]$RETURN), byrow = TRUE)
    
    if (method == "kernel") {
      if (alignBy == "seconds" | alignBy == "secs")
        delta <- alignPeriod
      if (alignBy == "minutes" | alignBy == "mins")
        delta <- alignPeriod * 60
      if (alignBy == "hours")
        delta <- alignPeriod * 3600
    }
  }
  options <- list(...)
  out <- switch(method,
                detPer = detPer(mR, rData = rData, options = options),
                stochPer = stochPer(mR, rData = rData, options = options),
                kernel = kernelestim(mR, rData = rData, delta, options = options),
                piecewise = piecewise(mR, rData = rData, options = options),
                garch = garch_s(mR, rData = rData, options = options),
                RM = realizedMeasureSpotVol(mR, rData = rData, options = options),
                PARM = preAveragedRealizedMeasureSpotVol(data, options = options)
                )
  
  return(out)
}