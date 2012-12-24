\name{quotesCleanup}
\Rdversion{1.1}
\alias{quotesCleanup}
\title{
Cleans quote data
}

\description{
This is a wrapper function for cleaning the quote data of all stocks in "ticker" over the interval [from,to]. 
The result is saved in the folder datadestination. 

In case you supply the argument "rawqdata", the on-disk functionality is ignored
 and the function returns a list with the cleaned quotes as xts object (see examples).

The following cleaning functions are performed sequentially:
\code{\link{noZeroQuotes}}, \code{\link{selectExchange}}, \code{\link{rmLargeSpread}},
\code{\link{mergeQuotesSameTimestamp}}, \code{\link{rmOutliers}}.
}

\usage{
quotesCleanup(from,to,datasource,datadestination,ticker,exchanges,
              qdataraw,report,selection,maxi,window,type,
              rmoutliersmaxi,...)
}

\arguments{
\item{from}{character indicating first date to clean, e.g. "2008-01-30".}
\item{to}{character indicating last date to clean, e.g. "2008-01-31".}
\item{datasource}{character indicating the folder in which the original data is stored.}
\item{datadestination}{character indicating the folder in which the cleaned data is stored.}
\item{ticker}{vector of tickers for which the data should be cleaned, e.g. ticker = c("AAPL","AIG")}.
\item{exchanges}{vector of stock exchange symbols for all tickers in vector "ticker". It thus should have the same length as the vector ticker.
Only data from one exchanges will be retained for each stock respectively, e.g. exchanges = c("T","N").
The possible exchange symbols are:
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
\item{qdataraw}{xts object containing (ONE day and for ONE stock only) raw quote data. This argument is NULL by default. Enabling it means the arguments
from, to, datasource and datadestination will be ignored. (only advisable for small chunks of data)}
\item{report}{boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many quotes remained
after each cleaning step.}
\item{selection}{argument to be passed on to the cleaning routine \code{\link{mergeQuotesSameTimestamp}}. The default is "median".}
\item{maxi}{argument to be passed on to the cleaning routine \code{\link{rmLargeSpread}}. }
\item{window}{argument to be passed on to the cleaning routine \code{\link{rmOutliers}}. }
\item{type}{argument to be passed on to the cleaning routine \code{\link{rmOutliers}}.}
\item{rmoutliersmaxi}{argument to be passed on to the cleaning routine \code{\link{rmOutliers}}. }                                                                                              
\item{...}{additional arguments.}
}


\value{For each day an xts object is saved into the folder of that date, containing the cleaned data.
This procedure is performed for each stock in "ticker". 
The function returns a vector indicating how many quotes remained after each cleaning step.

In case you supply the argument "rawqdata", the on-disk functionality is ignored
 and the function returns a list with the cleaned quotes as xts object (see examples).
}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized
kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.

Brownlees, C.T. and Gallo, G.M. (2006). 
Financial econometric analysis at ultra-high frequency: Data handling concerns.
 Computational Statistics & Data Analysis, 51, pages 2232-2245.

Falkenberry, T.N. (2002). High frequency data filtering. Unpublished technical report.
}

\author{Jonathan Cornelissen and Kris Boudt}

\examples{
#Consider you have raw quote data for 1 stock for 1 day 
data("sample_qdataraw");
head(sample_qdataraw);
dim(sample_qdataraw);
qdata_aftercleaning = quotesCleanup(qdataraw=sample_qdataraw,exchanges="N");
qdata_aftercleaning$report; 
barplot(qdata_aftercleaning$report);
dim(qdata_aftercleaning$qdata);

#In case you have more data it is advised to use the on-disk functionality
#via "from","to","datasource",etc. arguments
}

\keyword{ cleaning }