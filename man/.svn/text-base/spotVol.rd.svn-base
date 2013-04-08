\name{spotVol}
\Rdversion{1.1}
\alias{spotVol}
\title{
Spot volatility estimation}

\description{
Function returns an estimate of the standard deviation \eqn{\sigma_{t,i}} of equispaced high-frequency returns \eqn{r_{t,i}} (read: the \eqn{i}th return
on day \eqn{t}). The underlying assumption is that, in the absence of price jumps, high-frequency returns
are normally distributed with mean zero and standard deviation \eqn{\sigma_{t,i}}, where 
the standard deviation is the product between a deterministic periodic factor \eqn{f_{i}} (identical for every day in the sample)
and a daily factor \eqn{s_{t}} (identical for all observations within a day). 

For the estimation of \eqn{s_{t}} one can choose between the realized volatility, the bipower variation of Barndorff-Nielsen and Shephard (2004)
or the MedRV of Andersen et al. (2009). The latter two have the advantage of being robust to price jumps. 

The function takes as input the tick-by-tick price series. From these prices, equispaced returns are computed as the change in the log price of previous
tick interpolated prices sampled every k minutes.

The estimation of \eqn{f_{i}} is either based on scale or regression estimators. The scale estimator can be the standard deviation or its jump robust version
called the weighted standard deviation. For regression, choose OLS for the classical estimation and TML (truncated maximum likelihood) for jump 
robust regression. The regression specification consists either of one dummy for each intraday period (dummies=TRUE) or the flexible fourrier
form with P1 cosinus and P2 sinus terms. For more details on the classical methods, see Taylor and Xu (1997) and Andersen et al. (1997). 
For the jump robust versions, see Boudt et al. (2010).  
}

\usage{
spotVol(pdata, dailyvol = "bipower", periodicvol = "TML", 
    on = "minutes", k = 5, dummies = FALSE, P1 = 4, P2 = 2,  
    marketopen = "09:30:00", marketclose = "16:00:00")}

\arguments{
  \item{pdata}{xts object, containing the price series.}
  \item{dailyvol}{determines the estimation method for the component of intraday volatility that is constant over the day, but changes 
  from day to day. Possible values are "bipower","rv", "medrv".}
   \item{periodicvol}{determines the estimation method for the component of intraday volatility that depends in a deterministic way on the intraday time
   at which the return is observed. Possible values are "TML","sd", "wsd", "OLS".}
   \item{on}{ character, indicating the time scale in which "k" is expressed. Possible values are: "secs", "seconds", "mins", "minutes","hours".}
   \item{k}{ positive integer, indicating the number of periods to aggregate over. E.g. to aggregate a 
   xts object to the 5 minute frequency set k=5 and on="minutes".}
   \item{dummies}{ boolean, in case it is TRUE, the parametric estimator of periodic standard deviation  specifies the periodicity function as the sum of dummy variables corresponding to each intraday period. 
   If it false, the parametric estimator uses the Flexible Fourrier specification. FALSE by default.}
   \item{P1}{ is a positive integer valued parameter that corresponds to the number of cosinus terms used in the flexible fourrier specification for the periodicity function, see Andersen et al. (1997) for details.}
   \item{P2}{ is a positive integer valued parameter that corresponds to the number of sinus terms used in the flexible fourrier specification for the periodicity function, see Andersen et al. (1997) for details.}
   \item{marketopen}{the market opening time, by default: marketopen = "09:30:00".}
   \item{marketclose}{the market closing time, by default: marketclose = "16:00:00".} 
}

\details{
Returns an xts object with first column equal to the high-frequency return series, second column is the estimated standard deviation, third column is the daily standard deviation factor and, finally, the fourth column is the periodic component. 
}

\references{
Andersen, T. G. and T. Bollerslev (1997). Intraday periodicity and volatility persistence in financial markets. 
Journal of Empirical Finance 4, 115-158.

Andersen, T. G., D. Dobrev, and E. Schaumburg (2009). Jump-robust volatility 
estimation using nearest neighbor truncation. NBER Working Paper No.
15533.

Barndorff-Nielsen, O. and N. Shephard (2004). Power and bipower variation with 
stochastic volatility and jumps. Journal of Financial Econometrics 2 (1), 1-37.

Boudt K., Croux C. and Laurent S. (2011).  Robust estimation of intraweek periodicity 
in volatility and jump detection. Journal of Empirical Finance 18, 353-367.

Taylor, S. J. and X. Xu (1997). The incremental volatility information in one million foreign exchange quotations. 
Journal of Empirical Finance 4, 317-340.
}

\examples{
data("sample_real5minprices");

# Compute and plot intraday periodicity:
out = spotVol(sample_real5minprices,P1=6,P2=4,periodicvol="TML",k=5, dummies=FALSE);
head(out);
}

\keyword{volatility}

\author{Jonathan Cornelissen and Kris Boudt}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd
