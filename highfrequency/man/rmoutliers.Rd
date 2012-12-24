\name{rmOutliers}
\Rdversion{1.1}
\alias{rmOutliers}
\title{
Delete entries for which the mid-quote is outlying with respect to surrounding entries
}
\description{
If type="standard":  Function deletes entries for which the mid-quote deviated by more than "maxi"
median absolute deviations from a rolling centered median (excluding
the observation under consideration) of "window" observations.

If type="advanced":  Function deletes entries for which the mid-quote deviates by more than "maxi"
median absolute deviations from the value closest to the mid-quote of
these three options:
\enumerate{
\item Rolling centered median (excluding the observation under consideration)
\item Rolling median of the following "window" observations
\item Rolling median of the previous "window" observations
}

The advantage of this procedure compared to the "standard" proposed
by Barndorff-Nielsen et al. (2010) is that it will not incorrectly remove
large price jumps. Therefore this procedure has been set as the default
for removing outliers. 

Note that the median absolute deviation is taken over the entire
sample. In case it is zero (which can happen if mid-quotes don't change much), 
the median absolute deviation is taken over a subsample without constant mid-quotes.
}

\usage{
rmOutliers(qdata,maxi=10,window=50,type="advanced")}

\arguments{
  \item{qdata}{an xts object at least containing the columns "BID" and "OFR".} 
  \item{maxi}{an integer, indicating the maximum number of median absolute deviations allowed.} 
  \item{window}{an integer, indicating the time window for which the "outlyingness" is considered.}
  \item{type}{should be "standard" or "advanced" (see description).}
}

\section{Details}{
NOTE: This function works only correct if supplied input data consists of 1 day.}

\value{xts object}

\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde, and N. Shephard (2009). Realized
kernels in practice: Trades and quotes. Econometrics Journal 12, C1-C32.

Brownlees, C.T. and Gallo, G.M. (2006). 
Financial econometric analysis at ultra-high frequency: Data handling concerns.
 Computational Statistics & Data Analysis, 51, pages 2232-2245.
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ cleaning }