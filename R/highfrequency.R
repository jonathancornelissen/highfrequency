
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
#' Contributors: Giang Nguyen, Maarten Schermers.
#' 
#' Thanks: A special thanks for additional contributions from Chris Blakely, Brian Peterson, Eric Zivot, Dirk Eddelbuettel and GSoC.
#'
#' @useDynLib highfrequency, .registration = TRUE
"_PACKAGE"