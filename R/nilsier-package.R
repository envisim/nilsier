#' @docType package
#' @name nilsier
#' @importFrom Rcpp evalCpp
#' @useDynLib nilsier
#'
#' @description
#' The general design for the National Inventories of Landscapes in Sweden (NILS) environmental
#' monitoring program can be described as follows:
#' A large collection of PSUs (tracts) is sampled from the area frame.
#' From this collection, a smaller collection is drawn, and from that, an even smaller collection
#' may be drawn, et cetera.
#'
#' For each PSU level, categorizations are introduced.
#' The largest PSU level categorizes some plots in the tract, or markes the plots as unclassified.
#' For every subsequent PSU level, additional categorizations may be introduced amongst the
#' unclassified plots, until all plots have been categorized (or classified as totally
#' uninteresting).
#'
#' Estimates are produced per category and tract, and subsequently combined. Variance estimation
#' takes into account some of the dependencies that exists between categories. If the PSU
#' collections were drawn using  some spatially balanced design, the variance estimator assuming OSU
#' sampling of PSUs will overestimate the variance. An alternative variance, such as the local mean
#' variance estimator, can be used in order to reduce this overestimation somewhat
#' (Grafström & Schelin, 2014).
#'
#' This package provides estimators and variance estimators tailored to the NILS hierarchical design.
#' For further details on the design (in Swedish), see Grafström et al. (2023).
#'
#' @author Wilmer Prentius \email{wilmer.prentius@slu.se}.
#'
#' @references
#' Grafström, A., Randlund, Å., & Adler, S. (2023).
#' Skattningar baserade på hierarkiska urval. Arbetsrapport 558.
#' Institutionen för skoglig resurshushållning, Sveriges lantbruksuniversitet, SLU.
#'
#' Grafström, A., & Schelin, L. (2014).
#' How to select representative samples.
#' Scandinavian Journal of Statistics, 41(2), 277-290.
#'
#'
"_PACKAGE"
NULL
