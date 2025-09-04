#' Tract sample data
#'
#' These data sets contain sample information on the cover of lawns, gathered by the National
#' Inventories of Landscapes in Sweden (NILS) in 2024. The sample covers the southern boreal region of
#' Sweden (i.e. the boreal region South of the Dal River).
#'
#' The sample is reduced from the original, containing the two most sparse subsamples, and the
#' two strata (selection classes/categories) sampled only there.
#'
#' @section Conceptual overview of the NILS sampling design:
#' In NILS, a hierarchical sample of tracts is drawn (primary sampling unit, PSU), where each PSU is
#' a subset of the previous. A tract consists of 14 x 14 circular plots of 10m radius in a grid
#' pattern.
#'
#' In the highest order sample (i.e. largest PSU), some categories (or strata) are identified. All
#' plots within the tracts of the PSU classifies the plots as belonging to a specific stratum, or
#' being otherwise deemed unclassified. For each tract and category, a sample of plots are drawn and
#' surveyed. The process is then repeated for the second largest PSU, and so on.
#'
#' @format ## `tracts`
#' A matrix with 300 rows (tracts)
#' \describe{
#'   \item{tid}{the tract id number}
#'   \item{psu}{the id number of the primary sampling unit (PSU) of the tract}
#' }
#'
#' @source <https://www.slu.se/om-slu/organisation/institutioner/skoglig-resurshushallning/miljoanalys/nils/>
#'
"tracts"

#' @rdname tracts
#' @format ## `tract_auxilliaries`
#' A matrix with 300 rows (auxilliary information about the tracts)
#' \describe{
#'   \item{x}{longitude of the tract center}
#'   \item{y}{latitude of the tract center}
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
"tract_auxilliaries"

#' @rdname tracts
#' @format ## `plots`
#' A matrix with 16 rows (plots)
#' \describe{
#'   \item{tid}{the tract id number of the tract containing this plot}
#'   \item{cat}{the category (stratum) of the plot}
#'   \item{dw}{the conditional design weight of the plot (inverse inclusion probability)}
#'   \item{y}{the variable of interest -- the area of the (r=10) plot covered by grassland}
#' }
"plots"

#' @rdname tracts
#' @format ## `psus`
#' A length 2 ordered vector of PSU id numbers, ranging from the largest PSU to the smallest PSU
"psus"

#' @rdname tracts
#' @format ## `category_psu_map`
#' A matrix with 2 rows (categories), containing the mapping of the largest PSU that the category
#' map to.
#' \describe{
#'   \item{category}{the category (stratum) id number}
#'   \item{psu}{the id number of the largest PSU that the category can be observed in}
#' }
"category_psu_map"
