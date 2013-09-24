\name{ivInference}
\alias{ivInference}
\title{
Function returns the value, the standard error and the confidence band of the integrated variance (IV) estimator. 
}
\description{
 
This function supplies information about standard error and confidence band of integrated variance (IV) estimators under Brownian semimartingales model such as: bipower variation, minRV, medRV.  
Depending on users' choices of estimator (integrated variance (IVestimator), integrated quarticity (IQestimator)) and confidence level, the function returns the result.(Barndorff (2002))
Function returns three outcomes: 1.value of IV estimator 2.standard error of IV estimator and 3.confidence band of IV estimator. 

Assume there is \eqn{N} equispaced returns in period \eqn{t}.

Then the ivInference is given by: 
\deqn{
\mbox{standard error}= \frac{1}{\sqrt{N}} *sd
}
\deqn{
\mbox{confidence band}= \hat{IV} \pm cv*se
}
in which,
\deqn{
\mbox{sd}= \sqrt{\theta \times \hat{IQ}} 
}

\eqn{cv:} critical value. 

\eqn{se:} standard error.

\eqn{\theta:} depending on IQestimator, \eqn{\theta} can take different value (Andersen et al. (2012)). 

\eqn{\hat{IQ}} integrated quarticity estimator.
}

\usage{
ivInference (rdata, IVestimator="RV", IQestimator="rQuar", confidence=0.95, 
            align.by= NULL, align.period = NULL, makeReturns = FALSE, ...)
}

\arguments{
  \item{rdata}{a zoo/xts object containing all returns in period t for one asset.}
  \item{IVestimator}{can be chosen among integrated variance estimators: RV, BV, minRV or medRV. RV by default.}
  \item{IQestimator}{can be chosen among integrated quarticity estimators: rQuar, TP, QP, minRQ or medRQ. rQuar by default.}
  \item{confidence}{confidence level set by users. 0.95 by default. }
  \item{align.by}{a string, align the tick data to "seconds"|"minutes"|"hours"}
  \item{align.period}{an integer, align the tick data to this many [seconds|minutes|hours].}  
  \item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by  default.} 
  \item{...}{additional arguments.}
}

\value{
list
}

\details{
The theoretical framework is the logarithmic price process \eqn{X_t} belongs to the class of Brownian semimartingales, which can be written as:
\deqn{
\mbox{X}_{t}=  \int_{0}^{t} a_udu + \int_{0}^{t}\sigma_{u}dW_{u}
}
where \eqn{a} is the drift term, \eqn{\sigma} denotes the spot volatility process, \eqn{W} is a standard Brownian motion (assume that there are no jumps). 

}

\references{
Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.

Barndorff-Nielsen, O. E. (2002). Econometric analysis of realized volatility and its use in estimating stochastic volatility models. Journal of the Royal Statistical Society: Series B (Statistical Methodology), 64(2), 253-280.
}

\author{
Giang Nguyen, Jonathan Cornelissen and Kris Boudt
}

\examples{
data(sample_tdata)
ivInference(sample_tdata$PRICE, IVestimator= "minRV", IQestimator= "medRQ", 
            confidence=0.95, makeReturns = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ highfrequency }
\keyword{ ivInference }
