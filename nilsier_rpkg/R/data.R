#' @title Sample data on cover of lawns from the NILS, 2024.
#' @name data
#' @description Small sample data set from a subset of the NILS inventory.
#'
#' @details
#' The datasets includes information collected in the southern boreal region of Sweden (i.e. the
#' boreal region South of the Dal River).
#'
#' This sample is a reduced version of the original data, containing only the two most sparse
#' subsamples and the two strata (selection classes/categories) sampled within them.
#'
#' @source <https://www.slu.se/om-slu/organisation/institutioner/skoglig-resurshushallning/miljoanalys/nils/>
NULL

#' @rdname data
#' @format ## `tracts`
#' A list containing tract information for 300 tracts:
#' \describe{
#'   \item{tract}{A vector of tract IDs.}
#'   \item{psu}{A vector of PSU IDs.}
#'   \item{auxiliaries}{A matrix of auxiliary information (see details).}
#' }
#'
#' @section Auxiliaries:
#' The tracts have the following auxiliary information attached:
#' \describe{
#'   \item{x}{longitude of the tract centre}
#'   \item{y}{latitude of the tract centre}
#'   \item{elev_max}{the tract's maximum elevation}
#'   \item{elev_range}{the tract's elevation range}
#'   \item{nmd_artificial}{the area of the tract covered by buildings}
#'   \item{nmd_open_land}{the area of the tract covered by open land}
#'   \item{nmd_forest}{the area of the tract covered by forest}
#'   \item{nmd_mountain_forest}{the area of the tract covered by low-growth mountain forest}
#'   \item{wetness_mean}{the tract's mean wetness}
#'   \item{decidiuous_sum}{the area of the tract covered by deciduous forest}
#'   \item{arable}{the area of the tract covered by arable land}
#'   \item{pasture}{the area of the tract covered by pasture}
#' }
"tracts"

#' @rdname data
#' @format ## `plots`
#' A data.frame with 16 rows of plot level data for area covered by grassland:
#' \describe{
#'   \item{tract}{the tract ID of the plot}
#'   \item{category}{the category ID of the plot}
#'   \item{dw}{the conditional design weight of the plot (inverse inclusion probability)}
#'   \item{y}{the variable of interest -- the area of the (r=10) plot covered by grassland}
#' }
"plots"

#' @rdname data
#' @format ## `psus`
#' A vector of the (two) PSU IDs, in order from the largest to the smallest; each PSU represents a
#' subset of the previous PSU.
"psus"

#' @rdname data
#' @format ## `category_psu_map`
#' A data.frame with 2 rows (categories), mapping the largest PSU where the category can appear.
#' \describe{
#'   \item{category}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSu in which the category can appear (integer).}
#' }
"categories"
