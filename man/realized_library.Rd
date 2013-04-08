 \name{realized_library} 
 \docType{data} 
 \alias{realized_library} 
 \title{The realized library from the Oxford-Man Institute of Quantitative Finance} 
 \description{ 
 An xts object containing the daily returns, daily Realized Variance and daily Realized Kernels  ranging from  1996-01-03 up to 2009-03-01 for several indices and exchange rates. 
Use \code{colnames(realized_library)} to see which assets are included. 
The full library of the Oxford-Man Institute of Quantitative Finance can be found on their  website: http://realized.oxford-man.ox.ac.uk.} 
\usage{data("realized_library")} 

\references{ 
Gerd Heber, Asger Lunde, Neil Shephard, and Kevin Sheppard (2009)
  "Oxford-Man Institute's realized library, version 0.1", Oxford-Man
  Institute, University of Oxford. 

Shephard, N. and K. Sheppard (2010). Realising the future: forecasting with high 
frequency based volatility (heavy) models. Journal of Applied Econometrics 25, 
197-231.} 
\format{xts object} 
\keyword{datasets}  