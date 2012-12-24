\name{tradesCleanup}
\Rdversion{1.1}
\alias{tradesCleanup}
\title{
Cleans trade data
}
\description{
This is a wrapper function for cleaning the trade data of all stocks in "ticker" over the interval [from,to]. 
The result is saved in the folder datadestination. The function returns a vector
indicating how many trades were removed at each cleaning step.

In case you supply the argument "rawtdata", the on-disk functionality is ignored
 and the function returns a list with the cleaned trades as xts object (see examples).

The following cleaning functions are performed sequentially:
\code{\link{noZeroPrices}}, \code{\link{selectExchange}}, \code{\link{salesCondition}},
\code{\link{mergeTradesSameTimestamp}}.

Since the function \code{\link{rmTradeOutliers}}
also requires cleaned quote data as input, it is not incorporated here and
there is a seperate wrapper called \code{\link{tradesCleanupFinal}}.
}

\usage{
tradesCleanup(from,to,datasource,datadestination,ticker,exchanges,
                tdataraw,report,selection,...)}

\arguments{
\item{from}{character indicating first date to clean, e.g. "2008-01-30".}
\item{to}{character indicating last date to clean, e.g. "2008-01-31".}
\item{datasource}{character indicating the folder in which the original data is stored.}
\item{datadestination}{character indicating the folder in which the cleaned data is stored.}
\item{ticker}{vector of tickers for which the data should be cleaned, e.g. ticker = c("AAPL","AIG")}
\item{exchanges}{vector of stock exchange symbols for all tickers in vector "ticker". It thus should have the same length as the vector ticker.
Only data from one exchanges will be retained for each stock respectively, e.g. exchanges = c("Q","N")
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
\item{tdataraw}{xts object containing (ONE day and for ONE stock only) raw trade data. This argument is NULL by default. Enabling it means the arguments
from, to, datasource and datadestination will be ignored. (only advisable for small chunks of data)}
\item{report}{boolean and TRUE by default. In case it is true the function returns (also) a vector indicating how many trades remained
after each cleaning step.}
\item{selection}{argument to be passed on to the cleaning routine \code{\link{mergeTradesSameTimestamp}}. The default is "median".}
\item{...}{additional arguments.}
}

\value{For each day an xts object is saved into the folder of that date, containing the cleaned data.
This procedure is performed for each stock in "ticker".
The function returns a vector indicating how many trades remained after each cleaning step.

In case you supply the argument "rawtdata", the on-disk functionality is ignored
 and the function returns a list with the cleaned trades as xts object (see examples).
}

\examples{
#Consider you have raw trade data for 1 stock for 1 day 
data("sample_tdataraw");
head(sample_tdataraw);
dim(sample_tdataraw);
tdata_afterfirstcleaning = tradesCleanup(tdataraw=sample_tdataraw,exchanges="N");
tdata_afterfirstcleaning$report; 
barplot(tdata_afterfirstcleaning$report);
dim(tdata_afterfirstcleaning$tdata);

#In case you have more data it is advised to use the on-disk functionality
#via "from","to","datasource",etc. arguments
}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized
kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.

Brownlees, C.T. and Gallo, G.M. (2006). 
Financial econometric analysis at ultra-high frequency: Data handling concerns.
 Computational Statistics & Data Analysis, 51, pages 2232-2245.

Falkenberry, T.N. (2002). High frequency data filtering. Unpublished technical report.
}

\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{ cleaning }