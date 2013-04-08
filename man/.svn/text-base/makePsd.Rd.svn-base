\name{makePsd}
\Rdversion{1.1}
\alias{makePsd}
\title{Returns the positive semidinite projection of a symmetric matrix using the eigenvalue method}


\description{
Function returns the positive semidinite projection of a symmetric matrix using the eigenvalue method.
}

\usage{
makePsd(S,method="covariance")
}

\arguments{
\item{S}{ matrix.}
\item{method}{ character, indicating whether the negative eigenvalues of the correlation or covariance should be replaced by zero. Possible values are "covariance" and "correlation".}
}

\section{Details}{
We use the eigenvalue method to transform \eqn{S} into a positive
semidefinite covariance matrix (see e.g. Barndorff-Nielsen and Shephard, 2004, and Rousseeuw and Molenberghs, 1993).  Let \eqn{\Gamma} be the
orthogonal matrix consisting of the \eqn{p} eigenvectors of \eqn{S}. Denote
\eqn{\lambda_1^+,\ldots,\lambda_p^+} its \eqn{p} eigenvalues, whereby the negative eigenvalues have been replaced by zeroes.  
Under this approach, the positive semi-definite
 projection of \eqn{S} is \eqn{ S^+ = \Gamma' \mbox{diag}(\lambda_1^+,\ldots,\lambda_p^+) \Gamma}. 
 
If method="correlation", the eigenvalues of the correlation matrix corresponding to the matrix \eqn{S} are 
transformed. See Fan et al (2010).  
 
}

\value{
An xts object containing the aggregated trade data.
}

\references{
Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of
jumps in multivariate price processes using bipower covariation. Discussion
paper, Nuffield College, Oxford University.

Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection. Working paper.

Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
}

\author{ Jonathan Cornelissen and Kris Boudt}
\keyword{data manipulation}


