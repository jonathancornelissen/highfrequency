\name{rmNegativeSpread}
\Rdversion{1.1}
\alias{rmNegativeSpread}
\title{
Delete entries for which the spread is negative
}
\description{
Function deletes entries for which the spread is negative.
}

\usage{
rmNegativeSpread(qdata)
}

\arguments{
  \item{qdata}{an xts object at least containing the columns "BID" and "OFR".} 
}


\value{
xts object
}

\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{ cleaning }