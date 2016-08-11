\name{harModel}  
\Rdversion{1.1}  
\alias{harModel} 
\title{HAR model estimation (Heterogeneous Autoregressive model for Realized volatility)} 

\description{ 
Function returns the estimates for the Heterogeneous Autoregressive model 
for Realized volatility discussed in Andersen et al. (2007) and Corsi (2009). 
This model is mainly used to forecast the next days'volatility based on the high-frequency returns of the past. Consult the vignette for more information.} 


\usage{ 
 harModel(data, periods = c(1, 5, 22), periodsJ = c(1,5,22), leverage=NULL, 
  RVest = c("rCov", "rBPCov"), type = "HARRV", jumptest = "ABDJumptest", 
  alpha = 0.05, h = 1, transform = NULL, ...) }

\arguments{
   \item{data}{ an xts-object containing the intraday (log-)returns.}
   \item{periods}{ a vector of integers indicating over how days the realized measures in the model should be aggregated. By default  periods = c(1,5,22), which corresponds to one day, one week and one month respectively. This default is in line with Andersen et al. (2007).}   
   \item{periodsJ}{ a vector of integers indicating over what time periods the jump components in the model should be aggregated. By default periodsJ = c(1,5,22), which corresponds to one day, one week and one month respectively.} 
  \item{leverage}{ a vector of integers indicating over what periods the negative returns should be aggregated.
  See Corsi and Reno (2012) for more information. By default leverage=NULL and the model assumes the absence of a  leverage effect. Set leverage= c(1,5,22) to mimic the analysis in Corsi and Reno (2012).
  }   
  \item{RVest}{ a character vector with one or two elements. 
   The first element refers to the name of the function to estimate the daily integrated variance (non-jump-robust), while the second element refers to the name of the function to estimate the continuous component of daily volatility (jump-robust).  By default RVest = c("rCov","rBPCov"), i.e. using the Realized Volatility and Realized Bi-Power Variance.}
   \item{type}{ a string referring to the type of HAR model you would like to estimate. By default type = "HARRV", the most basic model. Other valid options are type = "HARRVJ" or type = "HARRVCJ".}
   \item{jumptest}{ the function name of a function used to test whether the test statistic which determines whether the jump variability is significant that day. By default jumptest = "ABDJumptest", hence using the test statistic in Equation or Equation (18) of Andersen et al. (2007).}
   \item{alpha}{ a real indicating the confidence level used in testing for jumps. By default alpha = 0.05.}
   \item{h}{ an integer indicating the number over how many days the dependent variable should be aggregated. 
   By default, h=1, i.e. no aggregation takes place, you just model the daily realized volatility.}
   \item{transform}{ optionally a string referring to a function that transforms both the dependent and explanatory variables in the model. By default transform=NULL, so no transformation is done. Typical other choices in this context would be "log" or "sqrt".}
   \item{...}{ extra arguments}
}

\section{Details}{ 
See vignette.
}

\value{ 
The function outputs an object of class \code{harModel} and \code{\link{lm}} (so \code{harModel} is  a subclass of \code{\link{lm}}). So far I only added a print method as you can see in the examples.  Input here is welcome, what should a plot of an "harmodel" object look like? What other methods are useful? 
}


\references{
Andersen, T. G., T. Bollerslev, and F. Diebold (2007). Roughing it up: includ-
ing jump components in the measurement, modelling and forecasting of return
volatility. The Review of Economics and Statistics 89, 701-720.

Corsi, F. (2009). A simple approximate long memory model of realized volatility.
Journal of Financial Econometrics 7, 174-196.

Corsi, F. and Reno R. (2012). Discrete-time volatility forecasting with persistent leverage effect and the link with continuous-time volatility modeling. Journal of Business and Economic Statistics, forthcoming.
}


\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{forecasting}

\examples{ 
 ##### Example 1: HARRVCJ ##### 
 data("sample_5minprices_jumps"); 
 data = sample_5minprices_jumps[,1];
 data = makeReturns(data); #Get the high-frequency return data
 
 x = harModel(data, periods = c(1,5,10), periodsJ=c(1,5,10), RVest = c("rCov","rBPCov"), 
       type="HARRVCJ",transform="sqrt"); 
 # Estimate the HAR model of type HARRVCJ  
 class(x);
 x 

 ##### Example 2:  ##### 
 # Forecasting daily Realized volatility for DJI 2008 using the basic harModel: HARRV
 data(realized_library); #Get sample daily Realized Volatility data
 DJI_RV = realized_library$Dow.Jones.Industrials.Realized.Variance; #Select DJI
 DJI_RV = DJI_RV[!is.na(DJI_RV)]; #Remove NA's
 DJI_RV = DJI_RV['2008'];

 x = harModel(data=DJI_RV , periods = c(1,5,22), RVest = c("rCov"), 
    type="HARRV",h=1,transform=NULL);
 class(x); 
 x;
 summary(x);
 plot(x);
}
