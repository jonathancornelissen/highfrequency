\name{rSV}
\alias{rSV}
\title{
Realized semivariance of highfrequency return series. 
}
\description{
 
Function returns Realized semivariance, defined in Barndorff-Nielsen et al. (2008).

Function returns two outcomes: 1.Downside realized semivariance and 2.Upside realized semivariance.

Assume there is \eqn{N} equispaced returns in period \eqn{t}. Let \eqn{r_{t,i}} be a return (with \eqn{i=1, \ldots,N}) in period \eqn{t}.

Then, the rSV is given by
\deqn{
\mbox{rSVdownside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2  \ \times \ I [ r_{t,i} <0 ]
}
\deqn{
\mbox{rSVupside}_{t}= \sum_{i=1}^{N} (r_{t,i})^2 \ \times \ I [ r_{t,i} >0 ]
}
}
\usage{
rSV (rdata,align.by=NULL,align.period=NULL,makeReturns=FALSE,...)
}

\arguments{
  \item{rdata}{a zoo/xts object containing all returns in period t for one asset.}
  \item{align.by}{a string, align the tick data to "seconds"|"minutes"|"hours"}
  \item{align.period}{an integer, align the tick data to this many [seconds|minutes|hours].}  
  \item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by   default.} 
  \item{...}{additional arguments.}
}

\value{
list
}
\references{
Barndorff- Nielsen, O.E., Kinnebrock, S. and Shephard N. (2008). Measuring downside risk- realized semivariance. CREATES research paper. p. 3-5
}
\author{
Giang Nguyen, Jonathan Cornelissen and Kris Boudt
}

\examples{
data(sample_tdata)
rSV(sample_tdata$PRICE,align.by ="minutes", align.period =5, makeReturns = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ highfrequency }
\keyword{ rSV }
