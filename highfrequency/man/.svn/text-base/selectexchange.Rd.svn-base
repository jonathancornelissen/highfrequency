\name{selectExchange}
\Rdversion{1.1}
\alias{selectExchange}
\title{
Retain only data from a single stock exchange
}
\description{
Function returns an xts object containing the data of only 1 stock exchange.
}

\usage{
selectExchange(data,exch="N");
}

\arguments{
  \item{data}{ an xts object containing the time series data. 
The object should have a column "EX", indicating the exchange by its symbol.}
  \item{exch}{ The symbol of the stock exchange that should be selected. 
By default the NYSE is chosen (exch="N"). Other exchange symbols are:
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