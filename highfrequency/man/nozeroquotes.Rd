\name{noZeroQuotes}
\Rdversion{1.1}
\alias{noZeroQuotes}
\title{
Delete the observations where the bid or ask is zero
}
\description{
Function deletes the observations where the bid or ask is zero.
}

\usage{
noZeroQuotes(qdata)
}

\arguments{
  \item{qdata}{an xts object at least containing the columns "BID" and "OFR".} 
}

\value{
xts object
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ cleaning }
