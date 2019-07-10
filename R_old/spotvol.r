#' Spot volatility estimation
#' 
#' @param data Either an \code{xts} object, containing price data, or a
#' \code{matrix} containing returns. For price data, irregularly spaced
#' observations are allowed. They will be aggregated to the level specified by
#' parameters \code{on} and \code{k}. For return data, the observations are
#' assumed to be equispaced, with the time between them specified by \code{on}
#' and \code{k}. Return data should be in matrix form, where each row
#' corresponds to a day, and each column to an intraday period. The output
#' will be in the same form as the input (\code{xts} or \code{matrix}/\code{numeric}).
#' 
#' @param method specifies which method will be used to estimate the spot
#' volatility. Options include \code{"detper"} and \code{"stochper"}.
#' See 'Details'.
#' @param on string indicating the time scale in which \code{k} is expressed.
#' Possible values are: \code{"secs", "seconds", "mins", "minutes", "hours"}.
#' @param k positive integer, indicating the number of periods to aggregate
#' over. E.g. to aggregate an \code{xts} object to the 5 minute frequency, set
#' \code{k = 5} and \code{on = "minutes"}.}
#' @param marketopen the market opening time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketopen = "09:30:00"}.
#' @param marketclose the market closing time. This should be in the time zone
#' specified by \code{tz}. By default, \code{marketclose = "16:00:00"}.
#' @param tz string specifying the time zone to which the times in \code{data}
#' and/or \code{marketopen}/ \code{marketclose} belong. Default = \code{"GMT"}.
#' @param ... method-specific parameters (see 'Details').
#' 
#' @return A \code{spotvol} object, which is a list containing one or more of the
#' following outputs, depending on the method used:
#' 
#' \code{spot}
#' 
#' An \code{xts} or \code{matrix} object (depending on the input) containing
#' spot volatility estimates \eqn{\sigma_{t,i}}, reported for each interval
#' \eqn{i} between \code{marketopen} and \code{marketclose} for every day
#' \eqn{t} in \code{data}. The length of the intervals is specifiedby \code{k}
#' and \code{on}. Methods that provide this output: All.
#' 
#' \code{daily}
#' An \code{xts} or \code{numeric} object (depending on the input) containing
#' estimates of the daily volatility levels for each day \eqn{t} in \code{data},
#' if the used method decomposed spot volatility into a daily and an intraday
#' component. Methods that provide this output: \code{"detper"}.
#' 
#' \code{periodic}
#' 
#' An \code{xts} or \code{numeric} object (depending on the input) containing
#' estimates of the intraday periodicity factor for each day interval \eqn{i}
#' between \code{marketopen} and \code{marketclose}, if the spot volatility was
#' decomposed into a daily and an intraday component. If the output is in
#' \code{xts} format, this periodicity factor will be dated to the first day of
#' the input data, but it is identical for each day in the sample. Methods that
#' provide this output: \code{"detper"}.
#' 
#' \code{par}
#' 
#' A named list containing parameter estimates, for methods that estimate one
#' or more parameters. Methods that provide this output:
#' \code{"stochper", "kernel"}.
#' 
#' \code{cp}
#' 
#' A vector containing the change points in the volatility, i.e. the observation
#' indices after which the volatility level changed, according to the applied
#' tests. The vector starts with a 0. Methods that provide this output:
#'  \code{"piecewise"}.
#' 
#' \code{ugarchfit}
#' 
#' A \code{ugarchfit} object, as used by the \code{rugarch}
#' package, containing all output from fitting the GARCH model to the data.
#' Methods that provide this output: \code{"garch"}.
#' 
#' The \code{spotvol} function offers several methods to estimate spot
#' volatility and its intraday seasonality, using high-frequency data. It
#' returns an object of class \code{spotvol}, which can contain various outputs,
#' depending on the method used. See 'Details' for a description of each method.
#' In any case, the output will contain the spot volatility estimates.
#' 
#' The input can consist of price data or return data, either tick by tick or
#' sampled at set intervals. The data will be converted to equispaced
#' high-frequency returns \eqn{r_{t,i}} (read: the \eqn{i}th return on day
#'                                      \eqn{t}).
#' 
#' @details The following estimation methods can be specified in \code{method}:
#' 
#' \strong{Deterministic periodicity method (\code{"detper"})}
#'   
#' Parameters:
#'   \tabular{ll}{
#'     \code{dailyvol} \tab A string specifying the estimation method for the daily
#'     component \eqn{s_t}. Possible values are \code{"bipower", "rv", "medrv"}.
#'     Default = \code{"bipower"}. \cr
#'     \code{periodicvol} \tab A string specifying the estimation method for the
#'     component of intraday volatility, that depends in a deterministic way on the
#'     intraday time at which the return is observed. Possible values are
#'     \code{"TML", "SD", "WSD", "OLS"}. See Boudt et al. (2011) for details.
#'     Default = \code{"TML"}.\cr
#'     \code{P1} \tab A positive integer corresponding to the number of cosinus
#'     terms used in the flexible Fourier specification of the periodicity function,
#'     see Andersen et al. (1997) for details. Default = 5. \cr
#'    \code{P2} \tab Same as \code{P1}, but for the sinus terms. Default = 5.\cr
#'    \code{dummies} \tab Boolean: in case it is \code{TRUE}, the parametric
#'    estimator of periodic standard deviation specifies the periodicity function
#'    as the sum of dummy variables corresponding to each intraday period. If it
#'    is \code{FALSE}, the parametric estimator uses the flexible Fourier
#'    specification. Default = \code{FALSE}.
#' }
#' Outputs (see 'Value' for a full description of each component):
#' \itemize{
#' \item{\code{spot}}
#' \item{\code{daily}}
#' \item{\code{periodic}}
#' }
#'   The spot volatility is decomposed into a deterministic periodic factor
#'   \eqn{f_{i}} (identical for every day in the sample) and a daily factor
#'   \eqn{s_{t}} (identical for all observations within a day). Both components
#'   are then estimated separately. For more details, see Taylor and Xu (1997)
#'   and Andersen and Bollerslev (1997). The jump robust versions by Boudt et al.
#'   (2011) have also been implemented.
#'   
#'   \strong{Stochastic periodicity method (\code{"stochper"})}
#'   
  Parameters:
    \tabular{ll}{
      \code{P1} \tab A positive integer corresponding to the number of cosinus
      terms used in the flexible Fourier specification of the periodicity function.
      Default = 5. \cr
      \code{P2} \tab Same as \code{P1}, but for the sinus terms. Default = 5.\cr
      \code{init} \tab A named list of initial values to be used in the
      optimization routine (\code{"BFGS"} in \code{optim}). Default =
        \code{list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.005, sigma_k = 0.05,
                   phi = 0.2, rho = 0.98, mu = c(2, -0.5), delta_c = rep(0, max(1,P1)),
                   delta_s = rep(0, max(1,P2)))}. See Beltratti & Morana (2001) for a definition
      of each parameter. \code{init} can contain any number of these parameters.
      For parameters not specified in \code{init}, the default initial value will
      be used.\cr
      \code{control} \tab A list of options to be passed down to
      \code{optim}.
    }
  Outputs (see 'Value' for a full description of each component):
    \itemize{
      \item{\code{spot}}
      \item{\code{par}}
    }
  This method by Beltratti and Morana (2001) assumes the periodicity factor to
  be stochastic. The spot volatility estimation is split into four components:
    a random walk, an autoregressive process, a stochastic cyclical process and
  a deterministic cyclical process. The model is estimated using a
  quasi-maximum likelihood method based on the Kalman Filter. The package
  \code{FKF} is used to apply the Kalman filter. In addition to
  the spot volatility estimates, all parameter estimates are returned.
  
  \strong{Nonparametric filtering (\code{"kernel"})}
  
  Parameters:
    \tabular{ll}{
      \code{type} \tab String specifying the type of kernel to be used. Options
      include \code{"gaussian", "epanechnikov", "beta"}. Default =
        \code{"gaussian"}.\cr
      \code{h} \tab Scalar or vector specifying bandwidth(s) to be used in kernel.
      If \code{h} is a scalar, it will be assumed equal throughout the sample. If
      it is a vector, it should contain bandwidths for each day. If left empty,
      it will be estimated. Default = \code{NULL}. \cr
      \code{est} \tab String specifiying the bandwidth estimation method. Possible
      values include \code{"cv", "quarticity"}. Method \code{"cv"} equals
      cross-validation, which chooses the bandwidth that minimizes the Integrated
      Square Error. \code{"quarticity"} multiplies the simple plug-in estimator
      by a factor based on the daily quarticity of the returns. \code{est} is
      obsolete if \code{h} has already been specified by the user. Default =
        \code{"cv"}.\cr
      \code{lower} \tab Lower bound to be used in bandwidth optimization routine,
      when using cross-validation method. Default is \eqn{0.1n^{-0.2}}. \cr
      \code{upper} \tab Upper bound to be used in bandwidth optimization routine,
      when using cross-validation method. Default is \eqn{n^{-0.2}}. \cr
    }
  Outputs (see 'Value' for a full description of each component):
    \itemize{
      \item{\code{spot}}
      \item{\code{par}}
    }
  This method by Kristensen (2010) filters the spot volatility in a
  nonparametric way by applying kernel weights to the standard realized
  volatility estimator. Different kernels and bandwidths can
  be used to focus on specific characteristics of the volatility process.
  
  Estimation results heavily depend on the bandwidth parameter \eqn{h}, so it
  is important that this parameter is well chosen. However, it is difficult to
  come up with a method that determines the optimal bandwidth for any kind of
  data or kernel that can be used. Although some estimation methods are
  provided, it is advised that you specify \eqn{h} yourself, or make sure that
  the estimation results are appropiate.
  
  One way to estimate \eqn{h}, is by using cross-validation. For each day in
  the sample, \eqn{h} is chosen as to minimize the Integrated Square Error,
  which is a function of \eqn{h}. However, this function often has multiple
  local minima, or no minima at all (\eqn{h -> \infty}). To ensure a reasonable
  optimum is reached, strict boundaries have to be imposed on \eqn{h}. These
  can be specified by \code{lower} and \code{upper}, which by default are
  \eqn{0.1n^{-0.2}} and \eqn{n^{-0.2}} respectively, where \eqn{n} is the
  number of observations in a day.
  
  When using the method \code{"kernel"}, in addition to the spot volatility
  estimates, all used values of the bandwidth \eqn{h} are returned.
  
  \strong{Piecewise constant volatility (\code{"piecewise"})}
  
  Parameters:
    \tabular{ll}{
      \code{type} \tab String specifying the type of test to be used. Options
      include \code{"MDa", "MDb", "DM"}. See Fried (2012) for details. Default =
        \code{"MDa"}.\cr
      \code{m} \tab Number of observations to include in reference window.
      Default = \code{40}. \cr
      \code{n} \tab Number of observations to include in test window.
      Default = \code{20}. \cr
      \code{alpha} \tab Significance level to be used in tests. Note that the test
      will be executed many times (roughly equal to the total number of
                                   observations), so it is advised to use a small value for \code{alpha}, to
      avoid a lot of false positives. Default = \code{0.005}. \cr
      \code{volest} \tab String specifying the realized volatility estimator to be
      used in local windows. Possible values are \code{"bipower", "rv", "medrv"}.
      Default = \code{"bipower"}. \cr
      \code{online} \tab Boolean indicating whether estimations at a certain point
      \eqn{t} should be done online (using only information available at
                                     \eqn{t-1}), or ex post (using all observations between two change points).
      Default = \code{TRUE}.  \cr
    }
  Outputs (see 'Value' for a full description of each component):
    \itemize{
      \item{\code{spot}}
      \item{\code{cp}}
    }
  
  This nonparametric method by Fried (2012) assumes the volatility to be
  piecewise constant over local windows. Robust two-sample tests are applied to
  detect changes in variability between subsequent windows. The spot volatility
  can then be estimated by evaluating regular realized volatility estimators
  within each local window.
  
  Along with the spot volatility estimates, this method will return the
  detected change points in the volatility level. When plotting a
  \code{spotvol} object containing \code{cp}, these change points will be
  visualized.
  
  \strong{GARCH models with intraday seasonality  (\code{"garch"})}
  
  Parameters:
    \tabular{ll}{
      \code{model} \tab String specifying the type of test to be used. Options
      include \code{"sGARCH", "eGARCH"}. See \code{ugarchspec} in the
      \code{rugarch} package. Default = \code{"eGARCH"}. \cr
      \code{garchorder} \tab Numeric value of length 2, containing the order of
      the GARCH model to be estimated. Default = \code{c(1,1)}. \cr
      \code{dist} \tab String specifying the distribution to be assumed on the
      innovations. See \code{distribution.model} in \code{ugarchspec} for
      possible options. Default = \code{"norm"}. \cr
      \code{solver.control} \tab List containing solver options.
      See \code{ugarchfit} for possible values. Default = \code{list()}. \cr
      \code{P1} \tab A positive integer corresponding to the number of cosinus
      terms used in the flexible Fourier specification of the periodicity function.
      Default = 5. \cr
      \code{P2} \tab Same as \code{P1}, but for the sinus terms. Default = 5.\cr
    }
  Outputs (see 'Value' for a full description of each component):
    \itemize{
      \item{\code{spot}}
      \item{\code{ugarchfit}}
    }
  This method generates the external regressors needed to model the intraday
  seasonality with a Flexible Fourier form. The \code{rugarch} package
  is then employed to estimate the specified GARCH(1,1) model.
  
  Along with the spot volatility estimates, this method will return the
  \code{ugarchfit} object used by the \code{rugarch} package.
}
\examples{
  # Load sample data
  
  data(sample_real5minprices)
  data(sample_returns_5min)
  
  # Default method, deterministic periodicity
  \dontshow{par.def <- par(no.readonly = TRUE)}
  vol1 <- spotvol(sample_real5minprices)
  plot(vol1)
  \dontshow{par(par.def)}
  # Compare to stochastic periodicity
  \donttest{
    init = list(sigma = 0.03, sigma_mu = 0.005, sigma_h = 0.007,
                sigma_k = 0.06, phi = 0.194, rho = 0.986, mu = c(1.87,-0.42),
                delta_c = c(0.25, -0.05, -0.2, 0.13, 0.02), delta_s = c(-1.2,
                                                                        0.11, 0.26, -0.03, 0.08))
    # next method will take around 110 iterations
    vol2 <- spotvol(sample_real5minprices, method = "stochper", init = init)
    plot(as.numeric(vol1$spot[1:780]), type="l")
    lines(as.numeric(vol2$spot[1:780]), col="red")
    legend("topright", c("detper", "stochper"), col = c("black", "red"), lty=1)}
  
  # Various kernel estimates
  \donttest{
    h1 = bw.nrd0((1:nrow(sample_returns_5min))*(5*60))
    vol3 <- spotvol(sample_returns_5min, method = "kernel", h = h1)
    vol4 <- spotvol(sample_returns_5min, method = "kernel", est = "quarticity")
    vol5 <- spotvol(sample_returns_5min, method = "kernel", est = "cv")
    plot(vol3, length = 2880)
    lines(as.numeric(t(vol4$spot))[1:2880], col="red")
    lines(as.numeric(t(vol5$spot))[1:2880], col="blue")
    legend("topright", c("h = simple estimate", "h = quarticity corrected",
                         "h = crossvalidated"), col = c("black", "red", "blue"), lty=1)}
  \dontshow{par(par.def)}
  # Piecewise constant volatility, using an example from Fried (2012)
  \donttest{
    simdata <- matrix(sqrt(5/3)*rt(3000, df = 5), ncol = 500, byrow = TRUE)
    simdata <- c(1, 1, 1.5, 1.5, 2, 1)*simdata
    # the volatility of the simulated now changes at 1000, 2000 and 2500
    vol6 <- spotvol(simdata, method = "piecewise", m = 200, n  = 100,
                    online = FALSE)
    plot(vol6)}
  
  # Compare regular GARCH(1,1) model to eGARCH, both with external regressors
  \donttest{
    vol7 <- spotvol(sample_returns_5min, method = "garch", model = "sGARCH")
    vol8 <- spotvol(sample_returns_5min, method = "garch", model = "eGARCH")
    plot(as.numeric(t(vol7$spot)), type = "l")
    lines(as.numeric(t(vol8$spot)), col = "red")
    legend("topleft", c("GARCH", "eGARCH"), col = c("black", "red"), lty=1)}
}
#' @references Andersen, T. G. and T. Bollerslev (1997). Intraday periodicity and volatility persistence in financial markets. Journal of Empirical Finance 4, 115-158.
#' 
#' Beltratti, A. and C. Morana (2001). Deterministic and stochastic methods for estimation of intraday seasonal components with high frequency data. Economic Notes 30, 205-234.
#' 
#' Boudt K., Croux C. and Laurent S. (2011). Robust estimation of intraweek periodicity in volatility and jump detection. Journal of Empirical Finance 18, 353-367.
#' 
#' Fried, Roland (2012). On the online estimation of local constant volatilities. Computational Statistics and Data Analysis 56, 3080-3090.
#' 
#' Kristensen, Dennis (2010). Nonparametric filtering of the realized spot volatility: A kernel-based approach. Econometric Theory 26, 60-93.
#' 
#' Taylor, S. J. and X. Xu (1997). The incremental volatility information in one million foreign exchange quotations. Journal of Empirical Finance 4, 317-340.
#' @export
spotvol <- function(data, method = "detper", ..., on = "minutes", k = 5,
                    marketopen = "09:30:00", marketclose = "16:00:00",
                    tz = "GMT") {
  if (on == "seconds" | on == "secs") 
    delta <- k 
  if (on == "minutes" | on == "mins") 
    delta <- k * 60  
  if (on == "hours") 
    delta <- k * 3600 
  
  if (inherits(data, what = "xts")) {
    data <- xts(data, order.by = as.POSIXct(time(data), tz = tz), tzone = tz)
    dates <- unique(format(time(data), "%Y-%m-%d"))
    cDays <- length(dates)
    rdata <- mR <- c()
    intraday <- seq(from = chron::times(marketopen), 
                    to = chron::times(marketclose), 
                    by = chron::times(delta/(24*3600))) 
    if (as.character(tail(intraday, 1)) != marketclose) 
      intraday <- c(intraday, marketclose)
    intraday <- intraday[2:length(intraday)]
    for (d in 1:cDays) {
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
                                    sep=" "), tz = tz))
      datad <- merge.xts(z, datad)$datad
      datad <- na.locf(datad)
      rdatad <- makeReturns(datad)
      rdatad <- rdatad[time(rdatad) > min(time(rdatad))]
      rdata <- rbind(rdata, rdatad)
      mR <- rbind(mR, as.numeric(rdatad))
    }
  } else if (class(data) == "matrix") {
    mR <- data
    rdata <- NULL
  } else stop("Input data has to consist of either of the following: 
            1. An xts object containing price data
            2. A matrix containing return data")

  options <- list(...)
  out <- switch(method, 
           detper = detper(mR, rdata = rdata, options = options), 
           stochper = stochper(mR, rdata = rdata, options = options),
           kernel = kernelestim(mR, rdata = rdata, delta, options = options),
           piecewise = piecewise(mR, rdata = rdata, options = options),
           garch = garch_s(mR, rdata = rdata, options = options))  
  return(out)
}

# Deterministic periodicity model
# 
# Modified spotVol function from highfrequency package
detper <- function(mR, rdata = NULL, options = list()) {
  # default options, replace if user-specified
  op <- list(dailyvol = "bipower", periodicvol = "TML", dummies = FALSE, 
             P1 = 5, P2 = 5)
  op[names(options)] <- options 
  
  cDays <- nrow(mR)
  M <- ncol(mR)
  if (cDays == 1 & is.null(rdata)) { 
    mR <- as.numeric(mR)
    estimdailyvol <- switch(op$dailyvol, 
                           bipower = rBPCov(mR), 
                           medrv = medRV(mR), 
                           rv = rCov(mR))
  } else {
    if (is.null(rdata)) {
      estimdailyvol <- switch(op$dailyvol, 
                             bipower = apply(mR, 1, "rBPCov"),
                             medrv = apply(mR, 1, "medRV"),
                             rv = apply(mR, 1, "rCov"))
    } else {
      estimdailyvol <- switch(op$dailyvol, 
                              bipower = apply.daily(rdata, rBPCov),
                              medrv = apply.daily(rdata, medRV),
                              rv = apply.daily(rdata, rCov))
      dates = time(estimdailyvol)
    }
  }  
  if (cDays <= 50) {
    print("Periodicity estimation requires at least 50 observations. 
          Periodic component set to unity")
    estimperiodicvol = rep(1, M)
  } else {
    mstdR <- mR/sqrt(as.numeric(estimdailyvol) * (1/M))
    selection <- c(1:M)[ (nrow(mR)-apply(mR,2,'countzeroes')) >=20] 
    # preferably no na is between
    selection <- c( min(selection) : max(selection) )
    mstdR <- mstdR[,selection]
    estimperiodicvol_temp <- diurnal(stddata = mstdR, method = op$periodicvol, 
                                     dummies = op$dummies, P1 = op$P1, 
                                     P2 = op$P2)[[1]]
    estimperiodicvol <- rep(1,M)
    estimperiodicvol[selection] <- estimperiodicvol_temp
    mfilteredR <- mR/matrix(rep(estimperiodicvol, cDays), byrow = T, 
                            nrow = cDays)
    estimdailyvol <- switch(op$dailyvol, 
                            bipower = apply(mfilteredR, 1, "rBPCov"),
                            medrv = apply(mfilteredR, 1, "medRV"), 
                            rv = apply(mfilteredR, 1, "rCov"))
    spot <- rep(sqrt(as.numeric(estimdailyvol) * (1/M)), each = M) * 
              rep(estimperiodicvol, cDays)
    if (is.null(rdata)) {
      spot <- matrix(spot, nrow = cDays, ncol = M, byrow = TRUE)
    } else {
      spot <- xts(spot, order.by = time(rdata))
      estimdailyvol <- xts(estimdailyvol, order.by = dates)
      estimperiodicvol <- xts(estimperiodicvol, order.by = time(rdata[1:M]))
    }
    out <- list(spot = spot, daily = estimdailyvol, periodic = estimperiodicvol)
    class(out) <- "spotvol"
    return(out)
  }
}

# Stochastic periodicity model
# 
# This function estimates the spot volatility by using the stochastic periodcity
# model of Beltratti & Morana (2001)
stochper <- function(mR, rdata = NULL, options = list()) 
{
  #require(FKF)
  # default options, replace if user-specified
  op <- list(init = list(), P1 = 5, P2 = 5, control = list(trace=1, maxit=500))
  op[names(options)] <- options 
  
  N <- ncol(mR)
  days <- nrow(mR)
  mR[mR == 0] <- NA
  logr2 <- log(mR^2)
  rvector <- as.vector(t(logr2)) 
  lambda <- (2*pi)/N;
  
  # default starting values of parameters
  sp <- list(sigma = 0.03,
             sigma_mu = 0.005,
             sigma_h = 0.005,
             sigma_k = 0.05,
             phi = 0.2,
             rho = 0.98,
             mu = c(2, -0.5),
             delta_c = rep(0, max(1,op$P1)),
             delta_s = rep(0, max(1,op$P2)))
  
  # replace if user has specified different values
  sp[names(op$init)] <- op$init
  
  # check input
  for (i in c("sigma", "sigma_mu", "sigma_h", "sigma_k", "phi", "rho")) {
    if (sapply(sp, length)[i] != 1) stop(paste(i, " must be a scalar"))  
  }
  if (length(sp$mu) != 2) 
    stop("mu must have length 2")
  if (length(sp$delta_c) != op$P1 & op$P1 > 0) 
    stop("delta_c must have length equal to P1")
  if (length(sp$delta_s) != op$P2 & op$P2 > 0) 
    stop("delta_s must have length equal to P2")
  if (length(sp$delta_c) < 1) 
    stop("delta_c must at least have length 1")
  if (length(sp$delta_s) < 1) 
    stop("delta_s must at least have length 1")
  
  # transform parameters to allow for unrestricted optimization 
  # (domain -Inf to Inf)
  par_t <- c(sigma = log(sp$sigma), sigma_mu = log(sp$sigma_mu), 
             sigma_h = log(sp$sigma_h), sigma_k = log(sp$sigma_k), 
             phi = log(sp$phi/(1-sp$phi)), rho = log(sp$rho/(1-sp$rho)),
             mu = sp$mu, delta_c = sp$delta_c, delta_s = sp$delta_s) 
  
  opt <- optim(par_t, loglikBM, yt = rvector, N = N, days = days, P1 = op$P1, 
               P2 = op$P2, method="BFGS", control = op$control)
  
  # recreate model to obtain volatility estimates
  ss <- ssmodel(opt$par, days, N, P1 = op$P1, P2 = op$P2)
  kf <- FKF::fkf(a0 = ss$a0, P0 = ss$P0, dt = ss$dt, ct = ss$ct, Tt = ss$Tt, 
                 Zt = ss$Zt, HHt = ss$HHt, GGt = ss$GGt, 
                 yt = matrix(rvector, ncol = length(rvector)))
  sigmahat <- as.vector(exp((ss$Zt%*%kf$at[,1:(N*days)] + ss$ct + 1.27)/2))
  
  # transform parameter estimates back
  estimates <- c(exp(opt$par["sigma"]), exp(opt$par["sigma_mu"]), 
                 exp(opt$par["sigma_h"]), exp(opt$par["sigma_k"]),
                 exp(opt$par["phi"])/(1+exp(opt$par["phi"])), 
                 exp(opt$par["rho"])/(1+exp(opt$par["rho"])), opt$par[-(1:6)])

  if (is.null(rdata)) {
    spot <- matrix(sigmahat, nrow = days, ncol = N, byrow = TRUE)
  } else {
    spot <- xts(sigmahat, order.by = time(rdata))
  }
  out <- list(spot = spot, par = estimates)
  class(out) <- "spotvol"
  return(out)
}

# Calculate log likelihood using Kalman Filter
# 
# This function returns the average log likehood value of the stochastic 
# periodicity model, given the input parameters.
loglikBM <- function(par_t, yt, days, N = 288, P1 = 5, P2 = 5) {
  ss <- ssmodel(par_t, days, N, P1 = P1, P2 = P2)
  yt <- matrix(yt, ncol = length(yt))
  kf <- FKF::fkf(a0 = ss$a0, P0 = ss$P0, dt = ss$dt, ct = ss$ct, Tt = ss$Tt, 
                 Zt = ss$Zt, HHt = ss$HHt, GGt = ss$GGt, yt = yt)
  return(-kf$logLik/length(yt))
}

# Generate state space model
# 
# This function creates the state space matrices from the input parameters.
# The output is in the format used by the FKF package.
ssmodel <- function(par_t, days, N = 288, P1 = 5, P2 = 5) {
  par <- c(exp(par_t["sigma"]), exp(par_t["sigma_mu"]), exp(par_t["sigma_h"]), 
           exp(par_t["sigma_k"]), exp(par_t["phi"])/(1+exp(par_t["phi"])), 
           exp(par_t["rho"])/(1+exp(par_t["rho"])), par_t[-(1:6)])
  lambda <- (2*pi)/288
  a0 <- c(0, 0, par["delta_c1"], par["delta_s1"])
  if (P1 == 0) 
    a0[3] <- par["delta_c"]
  if (P2 == 0) 
    a0[4] <- par["delta_s"]   
  m <- length(a0)
  P0 <- Tt <- Ht <- matrix(0, m, m)
  diag(Tt) <- c(1, par["phi"], rep(par["rho"]*cos(lambda), 2))
  Tt[3,4] <- par["rho"]*sin(lambda)
  Tt[4,3] <- par["rho"]*-sin(lambda)
  Zt <- matrix(c(1, 1, 1, 0), ncol = m)
  Gt <- sqrt(0.5*pi^2)
  GGt <- Gt %*% t(Gt)
  diag(Ht) <- c(par["sigma_mu"], par["sigma_h"], rep(par["sigma_k"], 2))
  HHt <- Ht %*% t(Ht)
  dt <- matrix(0, nrow = m)
  ct <- log(par["sigma"]^2) - 1.270363
  
  # calculate deterministic part c2, add to ct
  n <- 1:N
  M1 <- (2*n)/(N+1)
  M2 <- (6*n^2)/((N+1)*(N+2))
  c2 <- par["mu1"]*M1 + par["mu2"]*M2
  if (P1 > 1) {
    for (k in 2:P1) {
      c2 <- c2 + par[paste("delta_c", k, sep="")]*cos(k*lambda*n) 
    }  
  }
  if (P2 > 1) {
    for (p in 2:P2) {
      c2 <- c2 + par[paste("delta_s", p, sep="")]*sin(p*lambda*n)
    }  
  }
  ct <- matrix(ct + c2, ncol = N*days)
  
  return(list(a0 = a0, P0 = P0, Tt = Tt, Zt = Zt, GGt = GGt, HHt = HHt, 
              dt = dt, ct = ct))
}

# Kernel estimation method
# 
# See Kristensen (2010)
kernelestim <- function(mR, rdata = NULL, delta = 300, options = list()) {
  # default options, replace if user-specified
  op <- list(type = "gaussian", h = NULL, est = "cv", lower = NULL, 
             upper = NULL)
  op[names(options)] <- options
  
  D <- nrow(mR)
  N <- ncol(mR)
  if (N < 100 & op$est == "cv") 
    warning("Cross-validation may not return optimal results in small samples.")
  if (op$type == "beta" & op$est == "quarticity" ) {
    warning("No standard estimator available for Beta kernel bandwidth.
                Cross-validation will be used instead.")
    op$est = "cv" 
  }
  t <- (1:N)*delta
  S <- N*delta
  if (is.null(op$h)) { 
    h <- numeric(D) 
  } else {
    h <- rep(op$h, length.out = D)
  }
  sigma2hat <- matrix(NA, nrow = D, ncol = N)
  for(d in 1:D) {
    if (is.null(op$h)) {
      quarticity <- (N/3)*rowSums(mR^4)
      qscale <- quarticity^0.2
      qmult <- qscale/sqrt((1/D)*sum(qscale^2))
      if (op$est == "cv") 
        cat(paste("Estimating optimal bandwidth for day", d, "of", D, "...\n"))
      h[d] <- estbandwidth(mR[d, ], delta = delta, qmult = qmult[d], 
                           type = op$type, est = op$est, lower = op$lower, 
                           upper = op$upper)
    }
    for(n in 1:N) {
      if (op$type == "beta") {
        K <- kernelk(t/S, type = op$type, b = h[d], y = t[n]/S)
      } else {
        K <- kernelk((t-t[n])/h[d], type = op$type)/h[d]
      }
      K <- K/sum(K)
      sigma2hat[d, n] <- K %*% (mR[d, ]^2)
    }
  }
  spot <- as.vector(t(sqrt(sigma2hat)))
  if (is.null(rdata)) {
    spot <- matrix(spot, nrow = D, ncol = N, byrow = TRUE)
  } else {
    spot <- xts(spot, order.by = time(rdata))
  }
  out <- list(spot = spot, par = list(h = h))
  class(out) <- "spotvol"
  return(out)
}

# calculate values of certain kernels
# arguments b and y only needed for type == "beta"
kernelk <- function(x, type = "gaussian", b = 1, y = 1) {
  if (type == "gaussian") 
    return(dnorm(x))  
  if (type == "epanechnikov") {
    z <- (3/4)*(1-x^2)
    z[abs(x) > 1] <- 0
    return(z)
  }
  if (type == "beta") 
    return(dbeta(x, y/b + 1, (1-y)/b + 1))
}

# estimate optimal bandwidth paramater h
# by default, this is done through crossvalidation (cv)
# else the formula for h_opt in Kristensen(2010) is approximated
estbandwidth <- function(x, delta = 300, qmult = 1, type = "gaussian", 
                         est = "cv", lower = NULL, upper = NULL) {
  N <- length(x)
  S <- N*delta
  default <- bw.nrd0((1:N)*delta)
  if (type == "epanechnikov") 
    default <- default*2.34
  if (est == "quarticity")  
    h <- default*qmult 
  if (est == "cv") {
    if (type == "beta") {
      if (is.null(lower)) 
        lower <- 0.0001
      if (is.null(upper)) 
        upper <- 1
    } else {
      if (is.null(lower)) 
        lower <- default/3
      if (is.null(upper)) 
        upper <- default*3
    }
    opt <- optimize(ISE, c(lower, upper), x = x, type = type, delta = delta)
    h <- opt$minimum
  }
  return(h)
}

# calculate Integrated Square Error, given bandwidth h
ISE <- function(h, x, delta = 300, type = "gaussian") {
  N <- length(x)
  t <- (1:N)*delta
  S <- N*delta
  sigma2hat <- rep(NA, N)
  for(n in 1:N) {
    if (type == "beta") {
      K <- kernelk(t/S, type = type, b = h, y = t[n]/S)
    } else {
      K <- kernelk((t - t[n])/h, type = type)/h
    }
    K[n] <- 0
    K <- K/sum(K)
    sigma2hat[n] <- K %*% (x^2) 
  }    
  tl <- 5
  tu <- N-5
  ISE <- sum(((x[tl:tu]^2) - sigma2hat[tl:tu])^2)
  return(ISE)
}

# Piecewise constant volatility method
# See Fried (2012)
piecewise <- function(mR, rdata = NULL, options = list()) {
  # default options, replace if user-specified
  op <- list(type = "MDa", m = 40, n = 20, alpha = 0.005, volest = "bipower",
             online = TRUE)
  op[names(options)] <- options
  
  N <- ncol(mR)
  D <- nrow(mR)
  vR <- as.numeric(t(mR))
  spot <- rep(NA, N*D)
  cp <- changePoints(vR, type = op$type, alpha = op$alpha, m = op$m, n = op$n)  
  for (i in 1:(N*D)) {
    if (op$online) {
      if (i > op$n) {
        lastchange <- max(which(cp + op$n < i))
      } else {
        lastchange = 1
      } 
      lastchange <- cp[lastchange]
      spot[i] = switch(op$volest, 
                       bipower = sqrt((1/(i - lastchange + 1)) * 
                                      (rBPCov(vR[(lastchange + 1):i]))),
                       medrv = sqrt((1/(i - lastchange + 1)) * 
                                   (medRV(vR[(lastchange+1):i]))),
                       rv = sqrt((1/(i - lastchange + 1)) * 
                                (rCov(vR[(lastchange + 1):i]))),
                       sd = sd(vR[(lastchange + 1):i]),
                       tau = robustbase::scaleTau2(vR[(lastchange + 1):i]))
    } else {
      from <- cp[max(which(cp < i))]
      to <- min(c(N*D, cp[which(cp >= i)]))
      len <- to - from
      spot[i] <- switch(op$volest, 
                        bipower = sqrt((1/len)*(rBPCov(vR[from:to]))),
                        medrv = sqrt((1/len)*(medRV(vR[from:to]))),
                        rv = sqrt((1/len)*(rCov(vR[from:to]))),
                        sd = sd(vR[from:to]),
                        tau = robustbase::scaleTau2(vR[from:to]))
    }
  } 
  if (is.null(rdata)) {
    spot <- matrix(spot, nrow = D, ncol = N, byrow = TRUE)
  } else {
    spot <- xts(spot, order.by = time(rdata))
  }
  out <- list(spot = spot, cp = cp)
  class(out) <- "spotvol"
  return(out) 
}

# Detect points on which the volatility level changes
# Input vR should be vector of returns
# Returns vector of indices after which the volatility level in vR changed
changePoints <- function(vR, type = "MDa", alpha = 0.005, m = 40, n = 20)
{
  logR <- log((vR - mean(vR))^2)
  L <- length(logR)
  points <- 0
  np <- length(points)
  N <- n + m
  cat("Detecting change points...\n")
  for (t in 1:L) { 
    if (t - points[np] >= N) {
      reference <- logR[(t - N + 1):(t - n)]
      testperiod <- logR[(t - n + 1):t]  
      if(switch(type,
                MDa = MDtest(reference, testperiod, type = type, alpha = alpha),
                MDb = MDtest(reference, testperiod, type = type, alpha = alpha),
                DM = DMtest(reference, testperiod, alpha = alpha))) {
        points <- c(points, t - n)     
        np <- np + 1
        cat(paste("Change detected at observation", points[np], "...\n"))
      }    
    }
  }
  return(points)
}

# Difference of medians test
# See Fried (2012)
# Returns TRUE if H0 is rejected
DMtest <- function(x, y, alpha = 0.005) {
  m <- length(x)
  n <- length(y)
  xmed <- median(x)
  ymed <- median(y)
  xcor <- x - xmed
  ycor <- y - ymed
  delta1 <- ymed - xmed
  out <- density(c(xcor, ycor), kernel = "epanechnikov")
  fmed <- as.numeric(BMS::quantile.density(out, probs = 0.5))
  fmedvalue <- (out$y[max(which(out$x < fmed))] + 
                  out$y[max(which(out$x < fmed))+1])/2
  test <- sqrt((m*n)/(m + n))*2*fmedvalue*delta1
  return(abs(test) > qnorm(1-alpha/2))
}

# Median difference test
# See Fried (2012)
# Returns TRUE if H0 is rejected
MDtest <- function(x, y, alpha = 0.005, type = "MDa") {
  m <- length(x)
  n <- length(y)
  N <- m + n
  lambda <- m/N
  yrep <- rep(y, each = m)
  delta2 <- median(yrep - x)
  if (type == "MDa") {
    z <- rep(0, N)
    z[1:m] <- x
    z[(m+1):N] <- y
    dif <- rep(z, each = length(z))
    dif <- dif - z
    dif[which(dif == 0)] <- NA
  } else if (type == "MDb") {
    difx <- rep(x, each = length(x))
    difx <- difx - x
    dify <- rep(y, each = length(y))
    dify <- dify - y
    dif <- rep(0, length(difx) + length(dify))
    dif[1:length(difx)] <- difx
    dif[(length(difx) + 1):(length(difx) + length(dify))] <- dify
    dif[which(dif == 0)] <- NA
  } else stop(paste("Type", type, "not found."))
  out <- density(dif, na.rm = TRUE, kernel = "epanechnikov")
  g0 <- (out$y[max(which(out$x < 0))] + out$y[max(which(out$x < 0)) + 1])/2
  test <- sqrt(12*lambda*(1 - lambda)*N)*g0*delta2
  return(abs(test) > qnorm(1 - alpha/2))
}

# GARCH with seasonality (external regressors)
garch_s <- function(mR, rdata = NULL, options = list()) {
  # default options, replace if user-specified
  op <- list(model = "eGARCH", order = c(1,1), dist = "norm", P1 = 5, 
             P2 = 5, solver.control = list())
  op[names(options)] <- options
  
  D <- nrow(mR)
  N <- ncol(mR)
  mR <- mR - mean(mR)
  X <- intraday_regressors(D, N = N, order = 2, almond = FALSE, P1 = op$P1,
                           P2 = op$P2)
  spec <- rugarch::ugarchspec(variance.model = list(model = op$model, 
                                                    external.regressors = X,
                                                    garchOrder = op$order),                    
                              mean.model = list(include.mean = FALSE),
                                                distribution.model = op$dist)
  if (is.null(rdata)) {
    cat(paste("Fitting", op$model, "model..."))
    fit <- tryCatch(rugarch::ugarchfit(spec = spec, data = as.numeric(t(mR)), 
                                       solver = "nloptr", 
                                       solver.control = op$solver.control),
                    error = function(e) e,
                    warning = function(w) w)
    if (inherits(fit, what = c("error", "warning"))) {
      stop(paste("GARCH optimization routine did not converge.\n", 
                 "Message returned by ugarchfit:\n", fit))
    }
    spot <- as.numeric(rugarch::sigma(fit))
  } else {
    cat(paste("Fitting", op$model, "model..."))
    fit <- tryCatch(rugarch::ugarchfit(spec = spec, data = rdata, 
                                       solver = "nloptr",
                                       solver.control = op$solver.control), 
                    error = function(e) e,
                    warning = function(w) w)
    if (inherits(fit, what = c("error", "warning"))) {
      stop(paste("GARCH optimization routine did not converge.\n", 
                 "Message returned by ugarchfit:\n", fit))
    } 
    spot <- rugarch::sigma(fit)
  }
  out <- list(spot = spot, ugarchfit = fit)
  class(out) <- "spotvol"
  return(out)
}
    
plot.spotvol <- function(x, ...) {
  options <- list(...)
  plottable <- c("spot", "periodic", "daily")
  elements <- names(x)
  nplots <- sum(is.element(plottable, elements))
  
  if (nplots == 3) {
    par(mar = c(3, 3, 3, 1))
    layout(matrix(c(1,2,1,3), nrow = 2))
  }
  spot <- as.numeric(t(x$spot))
  
  if(is.element("length", names(options))) {
    length = options$length
  } else {
    length = length(spot)
  }
  
  plot(spot[1:length], type = "l", xlab = "", ylab = "")
  title(main = "Spot volatility")
  if ("cp" %in% elements)
    abline(v = x$cp[-1], lty = 3, col = "gray70")
  if ("periodic" %in% elements) {
    periodic <- as.numeric(t(x$periodic))
    if (inherits(data, what = "xts")) {
      intraday <- time(x$periodic)
      plot(x = intraday, y = periodic, type = "l", xlab = "", ylab = "")
    } else {
      plot(periodic, type = "l", xlab = "", ylab = "") 
    }
    title(main = "Intraday periodicity")
  }
  if ("daily" %in% elements) {
    daily <- as.numeric(t(x$daily))
    if (inherits(data, what = "xts")) {
      dates <- as.Date(time(x$daily))
      plot(x = dates, y = daily, type = "l", xlab = "", ylab = "")
    } else {
      plot(daily, type = "l", xlab = "", ylab = "")
    }
    title(main = "Daily volatility")
  } 
}

intraday_regressors <- function(D, N = 288, order = 1, almond = TRUE, 
                                dummies = FALSE, P1 = 5, P2 = 5) {  
  if (order == 1) {
    vi <- rep(c(1:N), each = D)
  } else {
    vi <- rep(c(1:N), D)
  }
  X <- c()
  if (!dummies) {
    if (P1 > 0) {
      for (j in 1:P1) {
        X <- cbind(X, cos(2 * pi * j * vi/N))
      }
    }
    M1 <- (N + 1)/2
    M2 <- (2 * N^2 + 3 * N + 1)/6
    ADD <- (vi/M1)
    X <- cbind(X, ADD)
    ADD <- (vi^2/M2)
    X <- cbind(X, ADD)
    if (P2 > 0) {
      ADD <- c()
      for (j in 1:P2) {
        ADD <- cbind(ADD, sin(2 * pi * j * vi/N))
      }
    }
    X <- cbind(X, ADD)
    if (almond) {
      opening <- vi - 0
      stdopening <- (vi - 0)/80
      almond1_opening <- (1 - (stdopening)^3)
      almond2_opening <- (1 - (stdopening)^2) * (opening)
      almond3_opening <- (1 - (stdopening)) * (opening^2)
      X <- cbind(X, almond1_opening, almond2_opening, almond3_opening)
      closing <- max(vi) - vi
      stdclosing <- (max(vi) - vi)/max(vi)
      almond1_closing <- (1 - (stdclosing)^3)
      almond2_closing <- (1 - (stdclosing)^2) * (closing)
      almond3_closing <- (1 - (stdclosing)) * (closing^2)
      X <- cbind(X, almond1_closing, almond2_closing, almond3_closing)
    }
  } else {
    for (d in 1:N) {
      dummy <- rep(0, N)
      dummy[d] <- 1
      dummy <- rep(dummy, each = D)
      X <- cbind(X, dummy)
    }
  }
  return(X)
}

### auxiliary internal functions copied from highfrequency package

countzeroes <- function(series) {
  return( sum( 1*(series==0) ) )
}

HRweight <- function(d, k){
  # Hard rejection weight function
  w = 1*(d<=k); return(w)
}

shorthscale <- function(data) {
  sorteddata <- sort(data);
  n <- length(data);
  h <- floor(n/2)+1;
  M <- matrix( rep(0,2*(n-h+1) ) , nrow= 2 );
  for (i in 1:(n-h+1)) {
    M[,i] = c(sorteddata[ i ], sorteddata[ i+h-1 ])
  }
  return(0.7413 * min( M[2,] - M[1,]))
}



diurnal <- function (stddata, method = "TML", dummies = F, P1 = 6, P2 = 4) {
    cDays = dim(stddata)[1]
    intraT = dim(stddata)[2]
    meannozero = function(series) {
      return(mean(series[series != 0]))
    }
    shorthscalenozero = function(series) {
      return(shorthscale(series[series != 0]))
    }
    WSDnozero = function(weights, series) {
      out = sum((weights * series^2)[series != 0])/sum(weights[series != 
                                                                 0])
      return(sqrt(1.081 * out))
    }
    if (method == "SD" | method == "OLS") {
      seas = sqrt(apply(stddata^2, 2, "meannozero"))
    }
    if (method == "WSD" | method == "TML") {
      seas = apply(stddata, 2, "shorthscalenozero")
      shorthseas = seas/sqrt(mean(seas^2))
      shorthseas[shorthseas == 0] = 1
      weights = matrix(HRweight(as.vector(t(stddata^2)/rep(shorthseas, 
                                                           cDays)^2), qchisq(0.99, df = 1)), ncol = dim(stddata)[2], 
                       byrow = T)
      for (c in 1:intraT) {
        seas[c] = WSDnozero(weights[, c], stddata[, c])
      }
    }
    seas = na.locf(seas,na.rm=F) #do not remove leading NA
    seas = na.locf(seas,fromLast=T)
    seas = seas/sqrt(mean(seas^2))
    if (method == "OLS" | method == "TML") {
      c = center()
      vstddata = as.vector(stddata)
      nobs = length(vstddata)
      vi = rep(c(1:intraT), each = cDays)
      if (method == "TML") {
        if( length(vstddata)!= length(seas)*cDays ){ print(length(vstddata)); print(length(seas)); print(cDays)}
        firststepresids = log(abs(vstddata)) - c - log(rep(seas, 
                                                           each = cDays))
      }
      X = intraday_regressors(cDays, N = intraT, dummies = dummies, P1 = P1, P2 = P2)
      selection = c(1:nobs)[vstddata != 0]
      vstddata = vstddata[selection]
      X = X[selection, ]
      if (method == "TML") {
        firststepresids = firststepresids[selection]
      }
      vy = matrix(log(abs(vstddata)), ncol = 1) - c
      if (method == "OLS") {
        Z = try(solve(t(X) %*% X), silent = T)
        if (inherits(Z, "try-error")) {
          print("X'X is not invertible. Switch to TML")
        }
        else {
          theta = solve(t(X) %*% X) %*% t(X) %*% vy
          rm(X)
          rm(vy)
        }
      }
      if (method == "TML") {
        inittheta = rep(0, dim(X)[2])
        l = -2.272
        u = 1.6675
        nonoutliers = c(1:length(vy))[(firststepresids > 
                                         l) & (firststepresids < u)]
        truncvy = vy[nonoutliers]
        rm(vy)
        truncX = X[nonoutliers, ]
        rm(X)
        negtruncLLH = function(theta) {
          res = truncvy - truncX %*% matrix(theta, ncol = 1)
          return(mean(-res - c + exp(2 * (res + c))/2))
        }
        grnegtruncLLH = function(theta) {
          res = truncvy - truncX %*% matrix(theta, ncol = 1)
          dres = -truncX
          return(apply(-dres + as.vector(exp(2 * (res + 
                                                    c))) * dres, 2, "mean"))
        }
        est = optim(par = inittheta, fn = negtruncLLH, gr = grnegtruncLLH, 
                    method = "BFGS")
        theta = est$par
        rm(truncX)
        rm(truncvy)
      }
# disable plot for now      
#       plot(seas, main = "Non-parametric and parametric periodicity estimates", 
#            xlab = "intraday period", type = "l", lty = 3)
#       legend("topright", c("Parametric", "Non-parametric"), cex = 1.1,
#              lty = c(1,3), lwd = 1, bty = "n")
        seas = diurnalfit(theta = theta, P1 = P1, P2 = P2, intraT = intraT, 
                                         dummies = dummies)
#       lines(seas, lty = 1)
      return(list(seas, theta))
    }
    else {
      return(list(seas))
    }
  }

diurnalfit <- function( theta , P1 , P2 , intraT , dummies=F ) {
  vi = c(1:intraT) ;  
  M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;
  
  # Regressors that do not depend on Day of Week:
  X = c()
  if(!dummies){
    if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } 
    
    ADD = (vi/M1 ) ; X = cbind(X,ADD);
    ADD = (vi^2/M2); X = cbind(X,ADD);
    if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 
    
    #openingeffect
    opening = vi-0 ; stdopening = (vi-0)/80 ;
    almond1_opening   = ( 1 - (stdopening)^3 );
    almond2_opening   = ( 1 - (stdopening)^2 )*( opening);
    almond3_opening   = ( 1 - (stdopening)   )*( opening^2);   
    X = cbind(  X, almond1_opening , almond2_opening , almond3_opening   )  ;
    
    #closing effect
    closing = max(vi)-vi ; stdclosing = (max(vi)-vi)/max(vi) ;
    almond1_closing   = ( 1 - (stdclosing)^3 );
    almond2_closing   = ( 1 - (stdclosing)^2 )*( closing);
    almond3_closing   = ( 1 - (stdclosing)   )*( closing^2);   
    X = cbind(  X, almond1_closing , almond2_closing , almond3_closing   )  ;
    
  }else{
    for( d in 1:intraT){
      dummy = rep(0,intraT); dummy[d]=1; 
      X = cbind(X,dummy); 
    }
  }
  # Compute fit
  seas = exp( X%*%matrix(theta,ncol=1) );
  seas = seas/sqrt(mean( seas^2) )    
  return( seas )          
}

center <- function() {
  g <- function(y){ return( sqrt(2/pi)*exp(y-exp(2*y)/2)  )}
  f <- function(y){ return( y*g(y)    )  }
  return( integrate(f,-Inf,Inf)$value )
}

# modified version of 'aggregatePrice' from highfrequency package
aggregatePrice <- function (ts, FUN = "previoustick", on = "minutes", k = 1, marketopen = "09:30:00", marketclose = "16:00:00", tz = "GMT") {
  ts2 = aggregatets(ts, FUN = FUN, on, k)
  date = strsplit(as.character(index(ts)), " ")[[1]][1]
  
  #open
  a = as.POSIXct(paste(date, marketopen), tz = tz)
  b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
  ts3 = c(b, ts2)
  
  #close
  aa = as.POSIXct(paste(date, marketclose), tz = tz)
  condition = index(ts3) < aa
  ts3 = ts3[condition]
  bb = as.xts(matrix(as.numeric(last(ts)),nrow=1), aa)
  ts3 = c(ts3, bb)
  
  return(ts3)
}
