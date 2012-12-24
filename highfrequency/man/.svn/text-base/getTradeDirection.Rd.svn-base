\name{getTradeDirection}
\Rdversion{1.1}
\alias{getTradeDirection}
\title{
Get trade direction
}

\description{
Function returns a vector with the inferred trade direction which is 
determined using the Lee and Ready algorithym (Lee and Ready, 1991). 
}

\usage{
getTradeDirection(tqdata,...);
}

\arguments{
  \item{tqdata}{ 
xts object, containing joined trades and quotes (e.g. using \code{\link{matchTradesQuotes}})
}
\item{...}{additional arguments.}
}

\section{Details}{
NOTE: The value of the first (and second) observation of the output should be ignored if price==midpoint
for the first (second) observation.
}

\value{
A vector which has values 1 or (-1) if the inferred trade direction
is buy or sell respectively.
}

\references{
Lee, C. M. C. and M. J. Ready (1991). 
Inferring trade direction from intraday
data. Journal of Finance 46, 733-746.
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{liquidity}
