\name{sample_5minprices_jumps}
\docType{data}
\alias{sample_5minprices_jumps}
\title{Ten artificial time series (including jumps) for the NYSE trading days during January 2010}
\description{
Ten simulated price series for the 19 trading days in January 2010:

Ten hypothetical price series were simulated 
according to the factor diffusion process discussed in Barndorff-Nielsen et al.
On top of this process we added a jump process, 
with jump occurrences governed by the Poisson process with 1 expected jump per day and
jump magnitude modelled as in Boudt et al. (2008). We assume that prices are only observed when a transaction takes place. 
The intensity of transactions follows a Poisson process and consequently,
the inter transaction times are exponentially distributed. 
Therefore, we generated the inter transaction times of the price series 
by an independent exponential distributions with lambda = 0.1,
which we keep constant over time. This means we expect one transaction every ten seconds.
In a final step, the time series were aggregated to the 5-minute frequency by previous tick aggregation.
}
\usage{data("sample_5minprices_jumps")}
\format{xts object}
\references{
Barndorff-Nielsen, O. E., P. R. Hansen, A. Lunde and N. Shephard (2009). 
Multivariate realised kernels: consistent positive semi-definite
estimators of the covariation of equity prices with noise and non-synchronous
trading. Journal of Econometrics, forthcoming.

Boudt, K., C. Croux, and S. Laurent (2008). Outlyingness weighted 
covariation. Mimeo.
}
\keyword{datasets}
