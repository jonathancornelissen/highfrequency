
#' @description The highfrequency package provides numerous tools for analyzing financial (highfrequency) data. It has five main goals:
#'
#' \itemize{ 
#' \item Provide functionality to manage, clean and match highfrequency trades and quotes data, 
#' \item calculate various liquidity measures, 
#' \item estimate and forecast volatility, 
#' \item detect price jumps and 
#' \item investigate microstructure noise and intraday periodicity.
#' }
#'
#' To learn more about highfrequency, start with the vignettes:
#' `browseVignettes(package = "highfrequency")`
#' 
#' @author Kris Boudt, Jonathan Cornelissen, Scott Payseur
#' 
#' Maintainer: Kris Boudt <Kris.Boudt@econ.kuleuven.be>
#' 
#' Contributors: Giang Nguyen, Onno Kleen
#' 
#' Thanks: We would like to thank Brian Peterson, Chris Blakely, Eric Zivot and Maarten Schermer. We are also grateful to Dirk Eddelbuettel for his extraordinary support as a mentor during the Google Summmer of Code 2019. Moreover, we thank Emil Sjørup for implementing additional options in the harModel function.
#'
#' @useDynLib highfrequency, .registration = TRUE
"_PACKAGE"