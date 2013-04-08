\name{noZeroPrices}
\Rdversion{1.1}
\alias{noZeroPrices}
\title{
Delete the observations where the price is zero
}
\description{
Function deletes the observations where the price is zero.
}

\usage{
noZeroPrices(tdata)
}

\arguments{
  \item{tdata}{an xts object at least containing a column "PRICE".} 
}

\value{
xts object
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ cleaning }
