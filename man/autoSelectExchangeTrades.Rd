\name{autoSelectExchangeTrades}
\Rdversion{1.1}
\alias{autoSelectExchangeTrades}
\title{
Retain only data from the stock exchange with the highest trading volume
}
\description{
Function returns an xts object containing only observations of the 
exchange with the highest value for the variable "SIZE", 
i.e. the highest trade volume.
}

\usage{
autoSelectExchangeTrades(tdata)
}

\arguments{
  \item{tdata}{ 
an xts object with at least a column "EX", 
indicating the exchange symbol and "SIZE", 
indicating the trade volume. The chosen exchange is printed on the console.
The possible exchanges are:
\itemize{
\item A: AMEX
\item N: NYSE
\item B: Boston
\item P: Arca
\item C: NSX
\item T/Q: NASDAQ
\item D: NASD ADF and TRF
\item X: Philadelphia
\item I: ISE
\item M: Chicago
\item W: CBOE
\item Z: BATS
}
}
}

\value{
xts object
}


\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ cleaning }