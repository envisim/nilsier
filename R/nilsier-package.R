#' @docType package
#' @importFrom Rcpp evalCpp
#' @useDynLib nilsier
#'
#' @details
#' Estimates are produced per category and tract, and subsequently combined.
#' Variance estimation takes into account some of the dependencies that exists between categories.
#'
#' If the PSU collections were drawn using  some spatially balanced design, the variance estimator
#' assuming OSU sampling of PSUs will overestimate the variance.
#' An alternative variance, such as the local mean variance estimator, can be used in order to
#' reduce this overestimation somewhat (Grafström & Schelin, 2014).
#'
#' ## Conseptual overview of NILS hierachical design
#' The primary sampling unit is a tract, and in the first step, a large number of tracts are
#' selected (PSU1).
#' From the collection PSU1, a smaller sample of tracts is drawn to form PSU2, and this process
#' continues to subsequent levels.
#'
#' Each tract consists of a grid of 14 x 14 circular plots, each with a 10 m radius.
#'
#' Amongst PSU1, some categories (strata) are identified.
#' The plots in the tracts are assigned to one of these categories or marked as unclassified
#' (considered uninteresting at that stage).
#' Within each tract, a sample of plots is then drawn from each category.
#'
#' At lower PSU levels, additional categories are introduced, and previously unclassified plots
#' are assigned to one of these new categories.
#'
#' For further details on the design (in Swedish), see e.g. Adler et al. (2020) and
#' Grafström et al. (2023).
#'
#' @author Wilmer Prentius \email{wilmer.prentius@slu.se}.
#'
#' @references
#' Adler, S., Christensen, P., Gardfjell, H., Grafström, A., Hagner, Å., Hedenås, H., & Ranlund, Å. (2020).
#' Ny design för riktade naturtypsinventeringar inom NILS och THUF. Arbetsrapport 513.
#' Institutionen för skoglig resurshushållning, Sveriges lantbruksuniversitet, SLU.
#'
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
