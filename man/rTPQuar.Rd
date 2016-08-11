\name{rTPQuar}
\alias{rTPQuar}
\title{
Realized tri-power quarticity of highfrequency return series.
}
\description{
 
Function returns the realized tri-power quarticity, defined in Huang and Tauchen (2005) and Andersen et al. (2012).

Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.

Then, the rTPQuar is given by
\deqn{
\mbox{rTPQuar}_{t}=\frac{N^2}{N-2} \left( 2^{2/3} \frac{\Gamma \left(7/6\right)}{ \Gamma \left(1/2\right)} \right)^{-3} \sum_{i=3}^{N} \mbox({|r_{t,i}|}^{4/3} {|r_{t,i-1}|}^{4/3} {|r_{t,i-2}|}^{4/3})
}
}
\usage{
rTPQuar (rdata, align.by=NULL, align.period=NULL, makeReturns=FALSE,...)
}

\arguments{
  \item{rdata}{a zoo/xts object containing all returns in period t for one asset.}
  \item{align.by}{a string, align the tick data to "seconds"|"minutes"|"hours"}
  \item{align.period}{an integer, align the tick data to this many [seconds|minutes|hours].}  
  \item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.} 
  \item{...}{additional arguments.}
}

\value{
numeric
}
\references{
Andersen, T. G., D. Dobrev, and E. Schaumburg (2012). Jump-robust volatility estimation using nearest neighbor truncation. Journal of Econometrics, 169(1), 75- 93.

Huang, X. and Tauchen, G. (2005). The relative contribution of jumps to total price variance. Journal of Financial Econometrics, 3(4), 456-499.
}
\author{
Giang Nguyen, Jonathan Cornelissen and Kris Boudt
}

\examples{
data(sample_tdata)
rTPQuar(rdata= sample_tdata$PRICE, align.by= "minutes", align.period =5, makeReturns= TRUE)
rTPQuar
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ highfrequency }
\keyword{ rTPQuar}
