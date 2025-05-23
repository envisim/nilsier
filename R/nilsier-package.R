#' @docType package
#' @name nilsier
#' @importFrom Rcpp evalCpp
#' @useDynLib nilsier
#'
#' @description
#' The general design for the National Inventories of Landscapes in Sweden (NILS) environmental
#' monitoring program can be described as follows:
#' A (largest) PSU of tracts is sampled from the area frame. From this PSU, a smaller PSU is drawn,
#' from which an even smaller PSU might be drawn, and so on.
#'
#' On each PSU, categorizations are introduced. The largest PSU categorizes some plots in the tract
#' to be "uninteresting" (certain to not contain any variable of interest), while other plots are
#' given a categorization, or left uncategorized. Next, the smaller PSU introduces categorization on
#' the sofar uncategorized plots, until all potential plots have been categorized.
#'
#' Estimates are then produced per category and tract, and combined. Variance estimation takes into
#' account some of the dependencies that exists between categories. If the PSUs were drawn using
#' some spatially balanced design, the variance estimator assuming OSU sampling of PSUs will
#' overestimate the variance. An alternative variance, such as the local mean variance estimator,
#' can be used in order to reduce this overestimation somewhat (Grafström & Schelin, 2014).
#'
#' This package provides esimators and variance estimators for the NILS hierarchical design. The
#' design is fruther described in Swedish by Grafström et al. (2003).
#'
#' @author Wilmer Prentius \email{wilmer.prentius@slu.se}.
#'
#' @references
#' Grafström, A., Randlund, Å., & Adler, S. (2003).
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
