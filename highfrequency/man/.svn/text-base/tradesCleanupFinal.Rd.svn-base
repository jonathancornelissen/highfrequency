\name{tradesCleanupFinal}
\Rdversion{1.1}
\alias{tradesCleanupFinal}
\title{
Perform a final cleaning procedure on trade data}

\description{
Function performs cleaning procedure \code{\link{rmTradeOutliers}} 
for the trades of all stocks in "ticker" over the interval 
[from,to] and saves the result in "datadestination". 
Note that preferably the input data for this function 
is trade and quote data cleaned by respectively e.g. \code{\link{tradesCleanup}}
and \code{\link{quotesCleanup}}.
}

\usage{
tradesCleanupFinal(from,to,datasource,datadestination,ticker,
                    tdata,qdata,...)
}

\arguments{
\item{from}{character indicating first date to clean, e.g. "2008-01-30".}
\item{to}{character indicating last date to clean, e.g. "2008-01-31".}
\item{datasource}{character indicating the folder in which the original data is stored.}
\item{datadestination}{character indicating the folder in which the cleaned data is stored.}
\item{ticker}{vector of tickers for which the data should be cleaned.}
\item{tdata}{xts object containing (ONE day and for ONE stock only) trade data cleaned by \code{\link{tradesCleanup}}. This argument is NULL by default. Enabling it, means the arguments
from, to, datasource and datadestination will be ignored. (only advisable for small chunks of data)}
\item{qdata}{xts object containing (ONE day and for ONE stock only) cleaned quote data. This argument is NULL by default. Enabling it means the arguments
from, to, datasource, datadestination will be ignored. (only advisable for small chunks of data)}
\item{...}{additional arguments.}
}

\value{For each day an xts object is saved into the folder of that date, containing the cleaned data.
This procedure is performed for each stock in "ticker".

In case you supply the arguments "tdata" and "qdata", the on-disk functionality is ignored
 and the function returns a list with the cleaned trades as xts object (see examples).
}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized
kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.

Brownlees, C.T. and Gallo, G.M. (2006). 
Financial econometric analysis at ultra-high frequency: Data handling concerns.
 Computational Statistics & Data Analysis, 51, pages 2232-2245.
}


\author{ Jonathan Cornelissen and Kris Boudt}

\examples{
#Consider you have raw trade data for 1 stock for 1 day 
data("sample_qdata");    #load cleaned quote data
data("sample_tdataraw"); #load raw trade data
tdata_afterfirstcleaning = tradesCleanup(tdataraw=sample_tdataraw,
exchange="N",report=FALSE);
dim(tdata_afterfirstcleaning);
tdata_afterfinalcleaning = tradesCleanupFinal(qdata=sample_qdata,
tdata=tdata_afterfirstcleaning);
dim(tdata_afterfinalcleaning);
#In case you have more data it is advised to use the on-disk functionality
#via "from","to","datasource",etc. arguments
}
\keyword{ cleaning }