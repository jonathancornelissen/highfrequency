#' @useDynLib highfrequency
#' @importFrom Rcpp sourceCpp
NULL

#' @useDynLib highfrequency heavy_likelihoodR_
heavy_likelihoodR <- function(parameters, data, T1, K1, doublemeans, p, q, pMax1, qMax1, 
                              doublebackcast, doubleLB, doubleUB, compconst1, doubleh, doublells, doublellRM, doublell) {
  .C(heavy_likelihoodR_, parameters, data, T1, K1, doublemeans, p, q, pMax1, qMax1, 
     doublebackcast, doubleLB, doubleUB, compconst1, doubleh, doublells, doublellRM, doublell)
}

#' @useDynLib highfrequency heavy_parameter_transformR_
heavy_parameter_transformR <- function(pparameters, K, p, q, O, A, B, pMax1, qMax1) {
  .C(heavy_parameter_transformR_, parameters, K, p, q, O, A, B, pMax1, qMax1)
}

#' @useDynLib highfrequency heavy_likelihoodR_
heavy_parameter_transform_RetrackR <- function(parameters, K,  p,  q,   means, O, A, B, pMax1, qMax1) {
  .C(heavy_parameter_transform_RetrackR_, parameters, K,  p,  q,   means, O, A, B, pMax1, qMax1)
}
