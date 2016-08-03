\name{BNSjumptest}
\alias{BNSjumptest}
\title{
Barndorff- Nielsen and Shephard (2006) tests for the presence of jumps in the price series.
}
\description{
 
This test examines the presence of jumps in highfrequency price series. It is based on theory of Barndorff- Nielsen and Shephard (BNS). The null hypothesis is no jumps. 
Depending on users' choices of estimator (integrated variance (IVestimator), integrated quarticity (IQestimator)), mechanism (linear, ratio) and adjustment (logarith), the function returns the result.
Function returns three outcomes: 1.z-test value 2.critical value(with confidence level of 95\%) and 3.pvalue of the test. 

Assume there is \eqn{N} equispaced returns in period \eqn{t}. 

Assume the Realized variance (RV), IVestimator and IQestimator are based on \eqn{N} equispaced returns. 

Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}. 

Then the BNSjumptest is given by: 
\deqn{
\mbox{BNSjumptest}= \frac{RV - IVestimator}{\sqrt{(\theta-2)\frac{1}{N} {IQestimator}}}
}
in which, \eqn{IVestimator} can be: bipower variance (BV), minRV, medRV. 
\eqn{IQestimator} can be: tripower quarticity (TPQ), quadpower quarticity (QPQ), minRQ, medRQ.

\eqn{\theta}: depends on IVestimator. 
(Huang and Tauchen (2005))
}
\usage{
BNSjumptest(rdata, IVestimator= "BV", IQestimator= "TPQ", type= "linear", logtransform= FALSE, 
            max= FALSE, align.by= NULL, align.period= NULL, makeReturns = FALSE, ...)
}

\arguments{
  \item{rdata}{a zoo/xts object containing all returns in period t for one asset.}
  \item{IVestimator}{can be chosen among jump robust integrated variance estimators: BV, minRV, medRV and corrected threshold bipower variation (CTBV). If CTBV is chosen, an argument of \eqn{startV}, start point of auxiliary estimators in threshold estimation (Corsi et al. (2010) can be included. BV by default.}
  \item{IQestimator}{can be chosen among jump robust integrated quarticity estimators: TPQ, QPQ, minRQ and medRQ. TPQ by default.}
  \item{type}{a method of BNS testing: can be linear or ratio. Linear by default.}
  \item{logtransform}{boolean, should be TRUE when QVestimator and IVestimator are in logarith form. FALSE by default.}
  \item{max}{boolean, should be TRUE when max adjustment in SE. FALSE by default.}  
  \item{align.by}{a string, align the tick data to "seconds"|"minutes"|"hours" }
  \item{align.period}{an integer, align the tick data to this many [seconds|minutes|hours].}  
  \item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.} 
  \item{...}{additional arguments.}
}

\value{
list
}

\details{
The theoretical framework underlying jump test is that the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
\deqn{
\mbox{X}_{t}=  \int_{0}^{t} a_udu + \int_{0}^{t}\sigma_{u}dW_{u} + Z_t
}
where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot volatility process, \eqn{W} is a standard Brownian motion and \eqn{Z} is a jump process defined by:
\deqn{
\mbox{Z}_{t}=  \sum_{j=1}^{N_t}k_j
}
where \eqn{k_j} are nonzero random variables. The counting process can be either finite or infinite for finite or infinite activity jumps.

Since the realized volatility converges to the sum of integrated variance and jump variation, while the robust IVestimator converges to the integrated variance,  it follows that the difference between \eqn{RV_{t,N}} and the IVestimator captures the jump part only, and this observation underlines the BNS test for jumps. (Theodosiou& Zikes(2009))
}

\references{
Barndorff-Nielsen, O. E., & Shephard, N. (2006). Econometrics of testing for jumps in financial economics using bipower variation. Journal of financial Econometrics, 4(1), 1-30. 

Corsi, F., Pirino, D., & Reno, R. (2010). Threshold bipower variation and the impact of jumps on volatility forecasting. Journal of Econometrics, 159(2), 276-288.

Huang, X., & Tauchen, G. (2005). The relative contribution of jumps to total price variance. Journal of financial econometrics, 3(4), 456-499.

Theodosiou, M., & Zikes, F. (2009). A comprehensive comparison of alternative tests for jumps in asset prices. Unpublished manuscript, Graduate School of Business, Imperial College London.
}
\author{
Giang Nguyen, Jonathan Cornelissen and Kris Boudt
}

\examples{
data(sample_tdata)
BNSjumptest(sample_tdata$PRICE, QVestimator= "RV", IVestimator= "minRV", 
            IQestimator = "medRQ", type= "linear", makeReturns = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ highfrequency }
\keyword{ BNSjumptest }