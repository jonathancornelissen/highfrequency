#' @importFrom zoo rollmean
#' @keywords internal
aggRV <- function(RM1, periods, type = "RV") {
  n <- length(RM1);
  nperiods <- length(periods)
  RVmatrix1 <- matrix(nrow = n, ncol = nperiods)
  for (i in 1:nperiods) {
    if (periods[i]==1) {
      RVmatrix1[,i] <- RM1
    } else {
      RVmatrix1[(periods[i]:n),i] = rollmean(x = RM1,k = periods[i], align = "left")
    }
  } #end loop over periods for standard RV estimator
  colnames(RVmatrix1) <- paste(type, periods, sep = "")
  return(RVmatrix1)
}

#' @importFrom zoo rollmean
#' @keywords internal
aggJ <- function(J, periodsJ) {
  n <- length(J);
  nperiods <- length(periodsJ)
  JM <- matrix(nrow = n, ncol = nperiods)
  for (i in c(1:nperiods)) {
    if (periodsJ[i] == 1) {
      JM[,i] <- J
    } else {
      JM[(periodsJ[i]:n),i] <- rollmean(x = J, k = periodsJ[i], align = "left")
    }
  } # End loop over periods for standard RV estimator
  colnames(JM) <- paste("J",periodsJ, sep = "")
  return(JM)
}

#' @importFrom zoo rollmean
#' @keywords internal
aggY <- function(RM1,h,maxp) {
  n <-  length(RM1)
  if (h == 1) {
    y <- RM1[(maxp+1):n]
  }
  if (h != 1) {
    y <- matrix( nrow=length(RM1), ncol=1 )
    colnames(y) <- "y"
    y[(h:n),] <- rollmean(x = RM1,k = h,align = "left")
    y <- matrix(y[((maxp+h):n),], ncol = 1)
    y <- as.data.frame(y)
  }
  return(y)
}

#' @importFrom zoo rollmean
#' @keywords internal
aggRQ <- function(RM3, periods, type = "RQ"){
  n <- length(RM3)
  nperiods <- length(periods)
  RQmatrix <- matrix(nrow = n, ncol = nperiods)
  for(i in 1:nperiods){
    if (periods[i]==1) {
      RQmatrix[,i] <- RM3
    } else {
      RQmatrix[(periods[i]:n),i] <- rollmean(x = RM3, k = periods[i], align = "left")
    }
  } #end loop over periods for standard RV estimator
  colnames(RQmatrix) <- paste(type, periods, sep = "")
  return(RQmatrix)
}

#' @importFrom stats lm formula
#' @keywords internal
estimhar <- function(y, x){ #Potentially add stuff here
  colnames(y) <- "y"
  output <- lm(formula(y ~ .), data = cbind(y, x)) ##Changed this to y~. because I got an error that the data was a list, even though class(data) was data.frame (only happened when using leverage)
}


# Help function to get nicely formatted formula's for print/summary methods.
#' @keywords internal
getHarmodelformula <- function(x) {
  #modelnames = colnames(x$model$x)
  modelnames = colnames(x$model)[-1] ##Changed because the formula is now y~. which makes model a single matrix-like object
  if (is.null(x$transform) == FALSE) {
    modelnames <- paste(x$transform,"(",modelnames,")",sep="")
  } #Added visual tingie for plotting transformed RV

  betas      <- paste("beta", (1:length(modelnames)),"",sep="")
  betas2     <- paste(" + ", betas,"*")
  rightside  <- paste(betas2, modelnames,collapse="")
  h <- x$h
  left <- paste("RV", h, sep = "")
  if (is.null(x$transform) == FALSE) {
    left <- paste(x$transform,"(",left,")",sep="" )
  }
  modeldescription <- paste(left, "= beta0", rightside)
  return(list(modeldescription,betas))
}

#Insanity filter of BPQ, not exported
#' @keywords internal
harInsanityFilter <- function(fittedValues, lower, upper, replacement) {
  replacementIndices <- (fittedValues < lower | fittedValues > upper)
  fittedValues[replacementIndices] <- replacement
  return(fittedValues)
}

#' HAR model estimation (Heterogeneous Autoregressive model for Realized volatility)
#'
#' @description Function returns the estimates for the Heterogeneous Autoregressive model
#' for Realized volatility discussed in Andersen et al. (2007) and Corsi (2009).
#' This model is mainly used to forecast the next days'volatility based on the high-frequency returns of the past. Consult the vignette for more information.
#'
#' @param data  an xts object containing either: intra-day (log-)returns or realized measures already computed from such returns. In case more than one realized measure is needed, the object should have the as many columns as realized measures needed.
#' @param periods a vector of integers indicating over how days the realized measures in the model should be aggregated. By default  periods = c(1,5,22), which corresponds to one day, one week and one month respectively. This default is in line with Andersen et al. (2007).
#' @param periodsJ a vector of integers indicating over what time periods the jump components in the model should be aggregated. By default periodsJ = c(1,5,22), which corresponds to one day, one week and one month respectively.
#' @param periodsQ a vector of integers indicating over what time periods the realized quarticity in the model should be aggregated. By default periodsQ = c(1,5,22), which corresponds to one day, one week and one month respectively.
#' @param leverage a vector of integers indicating over what periods the negative returns should be aggregated.
#' See Corsi and Reno (2012) for more information. By default leverage = NULL and the model assumes the absence of a  leverage effect. Set leverage = c(1,5,22) to mimic the analysis in Corsi and Reno (2012).
#' @param RVest a character vector with one, two, or three elements. The first element always refers to the name of the function to estimate the daily integrated variance (non-jump-robust).
#' The second and third element depends on which type of model is estimated: If type = "HARRVJ", type = "HARRVCJ", type = "HARRVQJ" the second element refers to the name of the function to estimate the continuous component of daily volatility (jump robust). If type = "HARRVQ", the second element refers to the name of the function used to estimate the integrated quarticity.
#' If type = "HARRVQJ" the third element always refers to the name of the function used to estimate integrated quarticity.
#' By default RVest = c("rCov","rBPCov","rQuar"), i.e. using the Realized Volatility, Realized Bi-Power Variance, and Realized Quarticity.
#' @param type a string referring to the type of HAR model you would like to estimate. By default type = "HARRV", the most basic model. Other valid options are type = "HARRVJ", type = "HARRVCJ", type = "HARRVQ", type = "HARRVQJ", type = "CHARRV", or type = "CHARRVQ".
#' @param inputType a string denoting if the input data consists of realized measures or high-frequency returns, default "RM" is the only way to denote realized measures and everything else denotes returns.
#' @param jumptest the function name of a function used to test whether the test statistic which determines whether the jump variability is significant that day. By default jumptest = "ABDJumptest", hence using the test statistic in Equation or Equation (18) of Andersen et al. (2007).
#' @param alpha a real indicating the confidence level used in testing for jumps. By default alpha = 0.05.
#' @param h an integer indicating the number over how many days the dependent variable should be aggregated.
#' By default, h=1, i.e. no aggregation takes place, you just model the daily realized volatility.
#' @param transform optionally a string referring to a function that transforms both the dependent and explanatory variables in the model. By default transform=NULL, so no transformation is done. Typical other choices in this context would be "log" or "sqrt".
#' @param ... extra arguments for jump test.
#'
#' @return The function outputs an object of class \code{HARmodel} and \code{\link{lm}} (so \code{HARmodel} is  a subclass of \code{\link{lm}}).
#'
#' @references Andersen, T. G., T. Bollerslev, and F. Diebold (2007). Roughing it up: including jump components in the measurement, modelling and forecasting of return volatility. The Review of Economics and Statistics 89, 701-720.
#' Corsi, F. (2009). A simple approximate long memory model of realized volatility. Journal of Financial Econometrics 7, 174-196.
#' Corsi, F. and Reno R. (2012). Discrete-time volatility forecasting with persistent leverage effect and the link with continuous-time volatility modeling. Journal of Business and Economic Statistics, forthcoming.
#' Bollerslev, T., Patton, A., Quaedvlieg, R. 2016,  Exploiting the errors: A simple approach for improved volatility forecasting, Journal of Econometrics, vol.192, issue 1, 1-18.
#'
#' @author Jonathan Cornelissen and Kris Boudt
#' @keywords forecasting
#'
#' @examples
#' ##### Example 1: HARRVCJ #####
#' dat <- sample_5minprices_jumps$stock1
#' dat <- makeReturns(dat) #Get the high-frequency return data
#'
#' x <- HARmodel(dat, periods = c(1,5,10), periodsJ = c(1,5,10),
#'              RVest = c("rCov","rBPCov"),
#'              type = "HARRVCJ",transform = "sqrt", inputType = "returns")
#' # Estimate the HAR model of type HARRVCJ
#' class(x)
#' x
#' # plot(x)
#' predict(x)
#'
#'
#' ##### Example 2: HARRV #####
#' # Forecasting daily Realized volatility for the S&P 500 using the basic HARmodel: HARRV
#' library(xts)
#' RV_SP500 <- as.xts(realizedLibrary$rv5, order.by = realizedLibrary$date)
#'
#' x <- HARmodel(data = RV_SP500 , periods = c(1,5,22), RVest = c("rCov"),
#'               type = "HARRV", h = 1, transform = NULL, inputType = "RM")
#' class(x)
#' x
#' summary(x)
#' plot(x)
#' predict(x)
#'
#'
#' ##### Example 3: HARRVQ #####
#' dat <- sample_5minprices_jumps$stock1
#' dat <- makeReturns(dat) #Get the high-frequency return data
#' #
#' x <- HARmodel(dat, periods = c(1,5,10), periodsJ = c(1,5,10),
#'             periodsQ = c(1), RVest = c("rCov", "rQuar"),
#'               type="HARRVQ", inputType = "returns")
#' ## Estimate the HAR model of type HARRVQ
#' class(x)
#' x
#' # plot(x)
#' #predict(x)
#' 
#' ##### Example 4: HARRVQJ with already computed realized measures #####
#' dat <- SP500RM[, c("RV", "BPV", "RQ")]
#' x <- HARmodel(dat, periods = c(1,5,22), periodsJ = c(1),
#'               periodsQ = c(1), type = "HARRVQJ")
#' ## Estimate the HAR model of type HARRVQJ
#' class(x)
#' x
#' # plot(x)
#' predict(x)
#'
#' ##### Example 5: CHARRV with already computed realized measures #####
#' dat <- SP500RM[, c("RV", "BPV")]
#'
#' x <- HARmodel(dat, periods = c(1, 5, 22), type = "CHARRV")
#' # Estimate the HAR model of type CHARRV
#' class(x)
#' x
#' # plot(x)
#' predict(x)
#'
#' ##### Example 6: CHARRVQ with already computed realized measures #####
#' dat <- SP500RM[, c("RV", "BPV", "RQ")]
#' 
#' x <- HARmodel(dat, periods = c(1,5,22), periodsQ = c(1), type = "CHARRVQ")
#' # Estimate the HAR model of type CHARRVQ
#' class(x)
#' x
#' # plot(x)
#' predict(x)
#'
#'#' ##### Example 7: HARRV #####
#' # Forecasting weekly Realized volatility for the S&P 500 using the basic HARmodel: HARRV
#' library(xts)
#' RV_SP500 <- as.xts(realizedLibrary$rv5, order.by = realizedLibrary$date)
#'
#' x <- HARmodel(data = RV_SP500 , periods = c(1,5,22), RVest = c("rCov"),
#'               type = "HARRV", h = 5, transform = NULL, inputType = "RM")
#' class(x)
#' x
#' summary(x)
#' plot(x)
#' predict(x)
#'
#'
#' @import RcppArmadillo
#' @export
HARmodel <- function(data, periods = c(1, 5, 22), periodsJ = c(1, 5, 22), periodsQ = c(1),
                     leverage=NULL, RVest = c("rCov","rBPCov", "rQuar"), type = "HARRV", inputType = "RM",
                     jumptest = "ABDJumptest", alpha = 0.05, h = 1, transform = NULL, ...){

  nperiods <- length(periods) # Number of periods to aggregate over
  nest <- length(RVest)      # Number of RV estimators
  nperiodsQ <- length(periodsQ) #Number of periods to aggregate realized quarticity over
  jumpModels <- c("HARRVJ", "HARRVCJ", "HARRVQJ", "CHARRV", "CHARRVQ")
  quarticityModels <- c("HARRVQ", "HARRVQJ", "CHARRVQ")
  bpvModels <- c("CHARRV", "CHARRVQ")

  if (is.null(transform) == FALSE) {
    Ftransform = match.fun(transform)
  }
  if (!(type %in% c("HARRV", jumpModels, quarticityModels))) {
    warning("Please provide a valid argument for type, see documentation.")
  }

  if (inputType != "RM") { #If it is returns as input
    # Get the daily RMs
    RV1 <- match.fun(RVest[1])
    RM1 <- apply.daily(data, RV1 )
    # save dates:
    alldates = index(RM1)
    if (type %in% jumpModels ) {
      RV2 <- match.fun( RVest[2])
      RM2 <- apply.daily(data, RV2)
    }
    if( type %in% quarticityModels ) {
      if(type %in% c("HARRVQJ","CHARRVQ")){##HARRVQJ and CHARRVQ are the only models that need both BPV and realized quarticity.
        RV2 <- match.fun(RVest[2])
        RM2 <- apply.daily(data, RV2)
        RV3 <- match.fun(RVest[3])
        RM3 <- apply.daily(data, RV3)
      }
      if(type == "HARRVQ"){
        RV3 <- match.fun( RVest[2])
        RM3 <- apply.daily( data, RV3 )
        periodsJ <- periods
      }
    }
  }

  if (inputType == "RM") { #The input is  already realized measures
    dimdata <- dim(data)[2]
    alldates <- index(data)
    RM1 <- data[,1]
    if (type %in% jumpModels) {
      RM2 <- data[,2]
    }
    if (type == "HARRVQ") {
      RM3 <- data[,2]
    }
    if (type %in% c("HARRVQJ", "CHARRVQ")) {
      RM2 <- data[, 2]
      RM3 <- data[, 3]
    }
  }

  # Get the matrix for estimation of linear model:
  maxp <- max(periods,periodsJ,periodsQ) # Max number of aggregation levels

  if (is.null(leverage) == FALSE) {
    maxp <- max(maxp,leverage)
  }
  n <- length(RM1)  #Number of Days

  # Aggregate RV:

  #RVmatrix1 <- aggRV(RM1,periods)
  RVmatrix1 <- har_agg(RM1, periods, nperiods)
  colnames(RVmatrix1) <- paste0("RV", periods)
  # Aggregate and subselect y:
  y <- as.data.frame(har_agg(RM1, c(h), 1L)[(maxp+h):(n), , drop = FALSE])
  colnames(y) <- "y"
  # Only keep useful parts:
  x1 <- RVmatrix1[(maxp:(n-h)), ]
  if (type %in% jumpModels) {
    RVmatrix2 <- har_agg(RM2,periods, nperiods)
    colnames(RVmatrix2) <- paste0("J", periods)
    x2 <- RVmatrix2[(maxp:(n-h)),]
  }  # In case a jumprobust estimator is supplied
  if (type %in% quarticityModels) { #in case realized quarticity estimator is supplied
    RQmatrix  <- as.matrix(har_agg(RM3,periodsQ, nperiodsQ)[(maxp:(n-h)),])
    colnames(RQmatrix) <- paste0("RQ", periodsQ)
    if(nperiodsQ == 1){
      RQmatrix <- as.matrix(sqrt(RQmatrix) - sqrt(mean(RM3)))
    }else{
      RQmatrix <- sqrt(RQmatrix) - sqrt(mean(RM3)) #Demeaned realized quarticity estimator as in BPQ(2016)
    }
  }

  # Jumps:
  if (type %in% jumpModels && !(type %in% bpvModels)) { # If model type is as such that you need jump component, don't spend time on computing jumps for CHAR.. models
    if (any(RM2 == 0) && inputType == "RM") { #The jump contributions were provided
      J <- RM2
    } else { #compute jump contributions
      J <- pmax(RM1 - RM2, 0) # Jump contributions should be positive
    }
    J <- as.data.frame(har_agg(J, periodsJ, length(periodsJ)))
    colnames(J) = paste0("J", periodsJ)
  }

  if (is.null(leverage) == FALSE) {
    if (sum(data < 0) == 0) {
      warning("You cannot use leverage variables in the model in case your input consists of Realized Measures")
    }

    # Get close-to-close returns
    e <- apply.daily(data,sum) #Sum logreturns daily
    # Get the rmins:
    rmintemp <- pmin(e,0)
    # Aggregate everything:
    rmin <- as.data.frame(har_agg(rmintemp, leverage, length(leverage))[(maxp:(n-h)), ,drop = FALSE])
    colnames(rmin) <- paste0("Rmin", leverage)
    # Select:
    #rmin <- rmin[(maxp:(n-h)),]
  } else {
    rmin <- matrix(ncol=0,nrow=dim(x1)[1])
  }

  ###############################
  # Estimate the model parameters, according to type of model :
  # First model type: traditional HAR-RV:
  if (type == "HARRV") {
    if (is.null(transform) == FALSE) {
      y  <- Ftransform(y)
      x1 <- Ftransform(x1)
    }
    x1 <- cbind(x1,rmin)

    model <- estimhar(y = y, x = x1)
    model$type <- "HARRV"
    model$dates <- alldates[(maxp+h):n]
    model$RVest <- RVest[1]

  } #End HAR-RV if cond

  if (type == "HARRVJ") {
    if (!is.null(transform) && transform == "log") {
      J <- J + 1
    }
    J <- J[(maxp:(n-h)),]
    x <- cbind(x1,J)         # bind jumps to RV data
    if (is.null(transform) == FALSE) {
      y <- Ftransform(y)
      x <- Ftransform(x)
    }
    x <- cbind(x, rmin)
    model <- estimhar(y = y, x = x)
    model$type <- "HARRVJ"
    model$dates <- alldates[(maxp+h):n]
    model$RVest <- RVest
  }#End HAR-RV-J if cond

  if (type == "HARRVCJ") {
    # Are the jumps significant? if not set to zero:
    if (jumptest == "ABDJumptest" ) {
      TQ <- apply.daily(data, RTQ)
      J <- J[,1]
      teststats <- ABDJumptest(RV=RM1,BPV=RM2,TQ=TQ )
    } else {
      jtest <- match.fun(jumptest)
      teststats <- jtest(data,...)
    }
    Jindicators  <- teststats > qnorm(1-alpha)
    J[!Jindicators] <- 0

    # Get continuous components if necessary RV measures if necessary:
    Cmatrix <- matrix(nrow = dim(RVmatrix1)[1], ncol = 1)
    Cmatrix[Jindicators,]    <- RVmatrix2[Jindicators, 1]   #Fill with robust one in case of jump
    Cmatrix[(!Jindicators)]  <- RVmatrix1[(!Jindicators), 1] #Fill with non-robust one in case of no-jump
    # Aggregate again:
    Cmatrix <- har_agg(Cmatrix, periods, nperiods)
    colnames(Cmatrix) <- paste0("C", periods)
    Jmatrix <- har_agg(J, periodsJ, nperiods)
    colnames(Jmatrix) <- paste0("J", periodsJ)
    # subset again:
    Cmatrix <- Cmatrix[(maxp:(n-h)), ]
    Jmatrix <- Jmatrix[(maxp:(n-h)), ]
    if (!is.null(transform) && transform=="log") {
      Jmatrix <- Jmatrix + 1
    }

    x <- cbind(Cmatrix, Jmatrix)              # bind jumps to RV data
    if (is.null(transform) == FALSE) {
      y <- Ftransform(y)
      x <- Ftransform(x)
    }
    x <- cbind(x,rmin)
    model <- estimhar(y = y,
                      x = x)
    model$type <- "HARRVCJ"
    model$dates <- alldates[(maxp+h):n]
    model$RVest <- RVest
    model$jumptest <- jumptest
    model$alpha_jumps <- alpha
  }

  if (type == "HARRVQ") {
    if (is.null(transform) == FALSE) {
      y  <- Ftransform(y)
      x1 <- Ftransform(x1)
      warning("The realized quarticity is already transformed with sqrt() thus only realized variance is transformed")
    }

    x1 <- cbind(x1, RQmatrix[,1:nperiodsQ] * x1[,1:nperiodsQ])
    if (is.null(colnames(RQmatrix))) { #special case for 1 aggregation period of realized quarticity. This appends the RQ1 name
      colnames(x1) <- c(colnames(x1[,1:nperiods]),"RQ1")
    }
    x1 <- cbind(x1,rmin)
    model <- estimhar(y=y,x=x1)
    model$fitted.values <- harInsanityFilter(fittedValues = model$fitted.values, lower = min(RM1), upper = max(RM1), replacement = mean(RM1))
    model$type = "HARRVQ"
    model$dates <- alldates[(maxp+h):n]
    model$RVest <- RVest[1]
  }

  if (type == "HARRVQJ") {
    if (!is.null(transform) && transform == "log") {
      J <- J + 1
    }
    J <- J[(maxp:(n-h)),]
    if (is.null(transform) == FALSE) {
      y <- Ftransform(y); x1 = Ftransform(x1)
      warning("The realized quarticity is already transformed with sqrt() thus only realized variance is transformed")
    }
    x1 <- cbind(x1, J, RQmatrix[,1:nperiodsQ] * x1[,1:nperiodsQ])
    if(is.null(colnames(RQmatrix))){ #special case for 1 aggregation period of realized quarticity. This appends the RQ1 name
      colnames(x1) <- c(colnames(x1[,1:(dim(x1)[2]-1)]), "RQ1")
    }
    x1 <- cbind(x1,rmin);
    model <-  estimhar(y = y, x = x1)
    model$fitted.values <- harInsanityFilter(fittedValues = model$fitted.values, lower = min(RM1), upper = max(RM1), replacement = mean(RM1))
    model$type <- "HARRVQJ"
    model$dates <- alldates[(maxp+h):n]
    model$RVest <- RVest[1]
  }

  if (type == "CHARRV") {
    if (is.null(transform) == FALSE) {
      y  <- Ftransform(y)
      x2 <- Ftransform(x2)
    }
    x2 <- cbind(x2,rmin);
    model <- estimhar(y=y,x=x2)
    model$transform <- transform
    model$type <- "CHARRV"
    model$dates < alldates[(maxp+h):n]
    model$RVest < RVest[1]
  } #End CHAR-RV if cond

  if (type == "CHARRVQ") {
    if (is.null(transform) == FALSE) {
      y  <- Ftransform(y)
      x2 <- Ftransform(x2)
      warning("The realized quarticity is already transformed with sqrt() thus only realized variance and bipower variation is transformed")
    }
    x2 <- cbind(x2, RQmatrix[,1:nperiodsQ] * x2[,1:nperiodsQ])

    if (is.null(colnames(RQmatrix))) { #special case for 1 aggregation period of realized quarticity. This appends the RQ1 name
      colnames(x2) <- c(colnames(x2[,1:nperiods]), "RQ1")
    }
    x2 <- cbind(x2,rmin)
    model <- estimhar(y = y, x = x2)
    model$fitted.values <- harInsanityFilter(fittedValues = model$fitted.values, lower = min(RM1), upper = max(RM1), replacement = mean(RM1))
    model$type = "CHARRVQ"
    model$dates < alldates[(maxp+h):n]
    model$RVest <- RVest[1]

  } #End CHAR-RVQ if cond

  model$transform <- transform
  model$inputType <- inputType
  model$h <- h
  model$leverage <- leverage
  class(model) <- c("harModel","lm")
  return (model)
}

#' @importFrom grDevices dev.interactive
#' @importFrom graphics plot panel.smooth points par
#' @importFrom stats residuals
#' @importFrom xts addLegend
#' @export
plot.harModel <- function(x, which = c(1L:3L, 5L), caption = list("Residuals vs Fitted",
                                                                  "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
                                                                  expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))),
                          panel = if (add.smooth) panel.smooth else points, sub.caption = NULL,
                          main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(), legend.loc = "topleft",
                          ..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75,
                          qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"),
                          label.pos = c(4, 2), cex.caption = 1){
  observed <- x$model$y
  fitted   <- x$fitted.values
  dates    <- x$dates
  dates    <- as.POSIXct(dates)
  observed <- xts(observed, order.by=dates)
  fitted   <- xts(fitted, order.by=dates)
  type     <- x$type

  g_range <- range(fitted,observed)
  g_range[1] <- 0.95*g_range[1]
  g_range[2] <- 1.05 * g_range[2]
  #ind = seq(1,length(fitted),length.out=5);
  title <- paste("Observed and forecasted RV based on HAR Model:",type);
  plot(cbind(fitted,observed), col = c('blue', 'red'), main = title, ylab = "Realized Volatility", lty = c(1,2), yaxis.right = FALSE)

  #plot.zoo(observed,col="red",lwd=2,main=title, ylim=g_range,xlab="Time",ylab="Realized Volatility");
  #  axis(1,time(b)[ind], format(time(b)[ind],), las=2, cex.axis=0.8); not used anymore
  #  axis(2);
  #lines(fitted,col="blue",lwd=2);
  #legend("topleft", c("Observed RV","Forecasted RV"), cex=1.1, col=c("red","blue"),lty=1, lwd=2, bty="n");
  addLegend(legend.loc = legend.loc, on = 1,
            legend.names = c("Forecasted RV", "Observed RV"),
            lty = c(1, 2), lwd = c(2, 2),
            col = c("blue", "red"))
}

#' @importFrom stats var
#' @export
predict.harModel <- function(object, newdata = NULL, warnings = TRUE, ...) {
  # If no new data is provided - just forecast on the last day of your estimation sample
  # If new data with colnames as in object$model$x is provided, i.e. right measures for that model, just use that data
  ##### These 4 lines are added to make adding new models (hopefully) easier
  jumpModels <- c("HARRVJ", "HARRVCJ", "HARRVQJ", "CHARRV", "CHARRVQ")
  quarticityModels <- c("HARRVQ", "HARRVQJ", "CHARRVQ")
  bpvModels <- c("CHARRV", "CHARRVQ")
  #### Initialization
  type <- object$type
  inputType <- object$inputType

  if (is.null(newdata) == TRUE) {
    if (is.null(object$transform) == TRUE) {
      return(as.numeric(c(1, xts::last(object$model$x))  %*%  object$coefficients))
    }
    if (object$transform == "log") {
      if (warnings == TRUE) {
        warning("Due to log-transform, forecast of RV is derived under assumption of log-normality.")
      }
      return(as.numeric(exp(c(1, xts::last(object$model$x))  %*%  object$coefficients + 1/2 * var(object$residuals))))
    }
    if (object$transform == "sqrt") {
      if (warnings == TRUE) {
        warning("Forecast for sqrt(RV) due to transform == \"sqrt\".")
      }
      return(as.numeric(c(1, xts::last(object$model$x))  %*%  object$coefficients))
    }
  }

  # Check whether newdata is in right format
  if (sum(colnames(newdata) == colnames(object$model$x)) == length(colnames(object$model$x))) {
    if (is.null(object$transform) == TRUE) {
      return(as.numeric(cbind(1, newdata)  %*%  object$coefficients))
    }
    if (object$transform == "log") {
      warning("Due to log-transform, forecast of RV is derived under assumption of log-normality.")
      return(as.numeric(exp(cbind(1, newdata)  %*%  object$coefficients + 1/2 * var(object$residuals))))
    }
    if (object$transform == "sqrt") {
      warning("Forecast for sqrt(RV) due to transform == \"sqrt\".")
      return(as.numeric(cbind(1, newdata)  %*%  object$coefficients))
    }
  } else {
    # Aggregate price data as in HARmodel function

    # Extract periods from coefficient names
    if (type == "HARRV") {
      # RV component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("RV", names(object$coefficients[-1]))], first = 4))
      periodsJ <- 0
      periodsQ <- 0
      nperiodsQ <- length(periodsQ)
    }
    if (type == "HARRVJ") {
      # RV component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("RV", names(object$coefficients[-1]))], first = 4))
      # Jump components
      periodsJ <- as.numeric(substring(names(object$coefficients[-1])[grep("J", names(object$coefficients[-1]))], first = 3))
      # RQ component
      periodsQ <- 0
      nperiodsJ <- length(periodsJ)
    }
    if (type == "HARRVCJ") {
      # Continuous component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("C", names(object$coefficients[-1]))], first = 3))
      # Jump component
      periodsJ <- as.numeric(substring(names(object$coefficients[-1])[grep("J", names(object$coefficients[-1]))], first = 3))
      # RQ component
      periodsQ <- 0
      nperiodsJ <- length(periodsJ)
    }
    if (type == "HARRVQ") {
      # RV component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("RV", names(object$coefficients[-1]))], first = 4))
      # Jump component
      periodsJ <- 0
      # RQ component
      periodsQ <- as.numeric(substring(names(object$coefficients[-1])[grep("RQ", names(object$coefficients[-1]))], first = 4))
      nperiodsQ <- length(periodsQ)
    }
    if (type == "HARRVQJ") {
      # RV component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("RV", names(object$coefficients[-1]))], first = 4))
      # Jump component
      periodsJ <- as.numeric(substring(names(object$coefficients[-1])[grep("J", names(object$coefficients[-1]))], first = 3))
      # RQ component
      periodsQ <- as.numeric(substring(names(object$coefficients[-1])[grep("RQ", names(object$coefficients[-1]))], first = 4))
      nperiodsQ <- length(periodsQ)
      nperiodsJ <- length(periodsJ)
    }
    if (type == "CHARRV") {
      # Continuous component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("RV", names(object$coefficients[-1]))], first = 4))
      # Jump component
      periodsJ <- 0
      # RQ component
      periodsQ <- 0
      nperiodsQ <- length(periodsQ)
    }
    if (type == "CHARRVQ") {
      # Continuous component
      periods <- as.numeric(substring(names(object$coefficients[-1])[grep("RV", names(object$coefficients[-1]))], first = 4))
      # Jump component
      periodsJ <- 0
      # RQ component
      periodsQ <- as.numeric(substring(names(object$coefficients[-1])[grep("RQ", names(object$coefficients[-1]))], first = 4))
      nperiodsQ <- length(periodsQ)
    }

    RVest <- object$RVest

    nperiods <- length(periods)  # Number of periods to aggregate over
    nest <- length(RVest)        # Number of RV estimators
    if (!is.null(object$transform)) {
      Ftransform <- match.fun(transform)
    }

    if (inputType != "RM") { #If it are returns as input
      # Get the daily RMs
      RV1 <- match.fun(RVest[1])
      RM1 <- apply.daily(newdata, RV1)
      if (type %in% jumpModels) {
        RV2 <- match.fun(RVest[2])
        RM2 <- apply.daily(newdata, RV2)
      }
      if (type %in% quarticityModels ) {
        if(type %in% c("HARRVQJ","CHARRVQ")){##HARRVQJ and CHARRVQ are the only models that need both BPV and realized quarticity.
          RV2 <- match.fun( RVest[2])
          RM2 <- apply.daily(newdata, RV2 )
          RV3 <- match.fun( RVest[3])
          RM3 <- apply.daily(newdata, RV3 )
        }
        if(type == "HARRVQ"){
          RV3 <- match.fun(RVest[2])
          RM3 <- apply.daily(newdata, RV3)
        }
      }
    }
    if (inputType == "RM") { #The input is already realized measures
      stop(paste0(c("If your data is already aggregated, newdata column names should be", colnames(object$model$x),
                    "as harModel-type is", type, "."),
                  collapse = " "))
    }
    leverage <- object$leverage

    maxp <- max(periods,periodsJ,periodsQ); # Max number of aggregation levels
    if (is.null(leverage) == FALSE) {
      maxp <- max(maxp,leverage)
    }
    n <- length(RM1)  #Number of Days

    # Aggregate RV:
    h <- object$h
    RVmatrix1 <- har_agg(RM1, periods, nperiods)
    colnames(RVmatrix1) <- paste0("RV", periods)
    # Aggregate and subselect y:
    y <- as.data.frame(har_agg(RM1, c(h), 1L)[(maxp+h):(n), , drop = FALSE])
    colnames(y) <- "y"

    # Only keep useful parts:
    x1 <- RVmatrix1[(maxp:(n-h)), ]
    if (type %in% jumpModels ){
      RVmatrix2 <- har_agg(RM2, periodsJ, nperiodsJ)
      colnames(RVmatrix2) <- paste0("J", periods)
      x2 <- RVmatrix2[(maxp:(n-h)), ]
    }  # In case a jumprobust estimator is supplied
    if (type %in% quarticityModels) { #in case realized quarticity estimator is supplied
      RQmatrix <- aggRQ(RM3,periodsQ)[(maxp:(n - h)), ]
      RVmatrix1 <- har_agg(RM3, periodsQ, nperiodsQ)
      colnames(RVmatrix1) <- paste0("RV", periods)
      if(nperiodsQ == 1){
        RQmatrix <- as.matrix(sqrt(RQmatrix) - sqrt(mean(RM3)))
      } else {
        RQmatrix <- sqrt(RQmatrix) - sqrt(mean(RM3)) #Demeaned realized quarticity estimator as in BPQ(2016)
      }
    }

    # Jumps:
    if (type %in% jumpModels && !(type %in% bpvModels)) { # If model type is as such that you need jump component, don't spend time on computing jumps for CHAR.. models
      if (any(RM2 == 0) && inputType == "RM") { #The jump contributions were provided
        J <- RM2
      } else { #compute jump contributions
        J <- pmax(RM1 - RM2, 0) # Jump contributions should be positive
      }
      J <- as.data.frame(har_agg(J, periodsJ, length(periodsJ)))
      colnames(J) = paste0("J", periodsJ)
    }

    if (is.null(leverage) == FALSE) {
      if (inputType == "RM") {
        warning("You cannot use leverage variables in the model in case your input consists of Realized Measures")
      }
      # Get close-to-close returns
      e <- apply.daily(newdata, sum) #Sum logreturns daily
      # Get the rmins:
      rmintemp <- pmin(e, 0)
      # Aggregate everything:
      rmin <- as.data.frame(har_agg(rmintemp, leverage, length(leverage))[(maxp:(n-h)), ,drop = FALSE])
      colnames(rmin) <- paste0("Rmin", leverage)
      #rmin <- aggRV(rmintemp, periods = leverage, type = "Rmin")
      # Select:
      #rmin <- rmin[(maxp:n),]
    } else {
      rmin <- matrix(ncol=0,nrow=dim(x1)[1])
    }

    if (type == "HARRV") {
      if (!is.null(object$transform)) {
        x1 <- Ftransform(x1)
      }
      x <- cbind(x1, rmin)
    }

    if (type == "HARRVJ") {

      if (!is.null(object$transform) && transform=="log") {
        J <- J + 1
      }
      J <- J[(maxp:(n)),]
      x <- cbind(x1,J)         # bind jumps to RV data
      if (is.null(transform) == FALSE) {
        x <- Ftransform(x)
      }
      x <- cbind(x,rmin)
    }

    if (type == "HARRVCJ") {

      if (object$jumptest=="ABDJumptest") {
        TQ <- apply.daily(newdata, RTQ)
        J <- J[, 1]
        teststats <- ABDJumptest(RV = RM1, BPV = RM2,TQ = TQ)
      } else {
        jtest <- match.fun(object$jumptest)
        teststats <- jtest(newdata, ...)
      }
      Jindicators  <- teststats > qnorm(1 - object$alpha_jumps)
      J[!Jindicators] <- 0

      # Get continuous components if necessary RV measures if necessary:
      Cmatrix <- matrix(nrow = dim(RVmatrix1)[1], ncol = 1)
      Cmatrix[Jindicators,]    <- RVmatrix2[Jindicators, 1]   #Fill with robust one in case of jump
      Cmatrix[(!Jindicators)]  <- RVmatrix1[(!Jindicators), 1] #Fill with non-robust one in case of no-jump
      # Aggregate again:
      Cmatrix <- har_agg(Cmatrix, periods, nperiods)
      colnames(Cmatrix) <- paste0("C", periods)
      Jmatrix <- har_agg(J, periodsJ, nperiods)
      colnames(Jmatrix) <- paste0("J", periodsJ)
      # subset again:
      Cmatrix <- Cmatrix[(maxp:(n-h)), ]
      Jmatrix <- Jmatrix[(maxp:(n-h)), ]
      if (!is.null(object$transform) && object$transform == "log") {
        Jmatrix <- Jmatrix + 1
      }

      x <- cbind(Cmatrix, Jmatrix)               # bind jumps to RV data
      if (is.null(transform) == FALSE) {
        x <- Ftransform(x)
      }
      x <- cbind(x, rmin)
    }

    if (type == "HARRVQ") {
      if (is.null(transform) == FALSE) {
        y  <- Ftransform(y)
        x1 <- Ftransform(x1)
        warning("The realized quarticity is already transformed with sqrt() thus only realized variance is transformed")
      }
      x1 <- cbind(x1, RQmatrix[,1:nperiodsQ] * x1[,1:nperiodsQ])
      if (is.null(colnames(RQmatrix)) == TRUE) { #special case for 1 aggregation period of realized quarticity. This appends the RQ1 name
        colnames(x1) <- c(colnames(x1[, 1:nperiods]),"RQ1")
      }
      x <- cbind(x1,rmin)
    }

    if (type == "HARRVQJ") {
      if (!is.null(transform) && transform=="log") {
        J <- J + 1
      }
      J <- J[(maxp:(n-h)), ]
      if (is.null(transform) == FALSE) {
        y  <- Ftransform(y)
        x1 <- Ftransform(x1)
        warning("The realized quarticity is already transformed with sqrt() thus only realized variance is transformed")
      }
      x1 <- cbind(x1, J, RQmatrix[,1:nperiodsQ] * x1[,1:nperiodsQ])
      if(is.null(colnames(RQmatrix))){ #special case for 1 aggregation period of realized quarticity. This appends the RQ1 name
        colnames(x1) <- c(colnames(x1[,1:(dim(x1)[2]-1)]), "RQ1")
      }
      x <- cbind(x1,rmin)
    }

    if (type == "CHARRV") {
      if (!is.null(transform)) {
        y <- Ftransform(y)
        x2 <- Ftransform(x2)
      }
      x <- cbind(x2,rmin)
    } #End CHAR-RV if cond

    if (type == "CHARRVQ") {
      if ( !is.null(transform)) {
        y  <- Ftransform(y)
        x2 <- Ftransform(x2)
        warning("The realized quarticity is already transformed with sqrt() thus only realized variance and bipower variation is transformed")
      }
      x2 = cbind(x2, RQmatrix[,1:nperiodsQ] * x2[,1:nperiodsQ])
      if(is.null(colnames(RQmatrix))){ #special case for 1 aggregation period of realized quarticity. This appends the RQ1 name
        colnames(x2) = c(colnames(x2[,1:nperiods]), "RQ1")
      }
      x = cbind(x2,rmin)
    } #End CHAR-RVQ if cond

    if (is.null(object$transform) == TRUE) {
      return(as.numeric(as.matrix(cbind(1, x))  %*%  object$coefficients))
    }
    if (object$transform == "log") {
      if (warnings == TRUE) {
        warning("Due to log-transform, forecast of RV is derived under assumption of log-normality.")
      }
      return(as.numeric(exp(as.matrix(cbind(1, x))  %*%  object$coefficients + 1/2 * var(object$residuals))))
    }
    if (object$transform == "sqrt") {
      if (warnings == TRUE) {
        warning("Forecast for sqrt(RV) due to transform == \"sqrt\".")
      }
      return(as.numeric(as.matrix(cbind(1, x))  %*%  object$coefficients))
    }
  }
}

#' @importFrom stats coef
#' @export
print.harModel <- function(x, digits = max(3, getOption("digits") - 3), ...){
  formula <- getHarmodelformula(x); modeldescription = formula[[1]]; betas = formula[[2]];

  cat("\nModel:\n", paste(modeldescription, sep = "\n", collapse = "\n"),
      "\n\n", sep = "")

  coefs <- coef(x);
  names(coefs) <- c("beta0",betas)

  if (length(coef(x))){
    cat("Coefficients:\n")
    print.default(format(coefs, digits = digits), print.gap = 2,quote = FALSE);
    cat("\n\n");
    Rs <- summary(x)[c("r.squared", "adj.r.squared")]
    zz <- c(Rs$r.squared,Rs$adj.r.squared);
    names(zz) <- c("r.squared","adj.r.squared")
    print.default((format(zz,digits=digits) ),print.gap = 2,quote=FALSE)
  }
  else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}

#' @importFrom stats summary.lm
#' @export
summary.harModel <- function(object, correlation = FALSE, symbolic.cor = FALSE, ...){
  x <- object
  dd <- summary.lm(x)
  formula <- getHarmodelformula(x)
  modeldescription <- formula[[1]]
  betas <- formula[[2]]
  dd$call <- modeldescription
  rownames(dd$coefficients) <- c("beta0", betas)
  return(dd)
}

