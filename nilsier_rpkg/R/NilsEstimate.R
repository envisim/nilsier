#' Estimate totals using the NILS hierarchical design
#'
#' @description
#' Estimates the total of some variable surveyed under the NILS hierarchical sampling framework.
#'
#' @param plot_data A data frame with information about observations at the plot level.
#' Must contain (in order):
#'   1. The tract ID (integer) of the parent tract.
#'   2. The category ID (integer) recorded for the plot.
#'   3. The design weight (double) for the plot, conditional on the tract.
#'   4. The observed value of the target variable (double).
#'
#' @param tract_data A matrix with information about all sampled tracts,
#' including those where no relevant categories were found.
#' Must contain (in order):
#'   1. The tract ID (integer) of each sampled tract.
#'   2. The PSU collection ID (integer) of the smallest PSU that contains the tract.
#'
#' @param psus An ordered vector of PSU levels, from largest to smallest.
#'
#' @param category_psu_map A matrix describing the categories used in the design.
#' Must contain (in order):
#'   1. The category ID (integer), as used in `plot_data`.
#'   2. The PSU collection ID (integer) of the smallest PSU in which the category is sampled.
#'
#' @param area The size of the area frame. Typically larger than the actual area of interest.
#'
#' @param tract_area The area of a tract, expressed in the same units as the target variable.
#'
#' @details
#' The function combines plot-level observations (`plot_data`), tract-level information
#' (`tract_data`), PSU hierarchy (`psus`), and category assignments (`category_psu_map`) to estimate
#' totals under the NILS sampling design.
#'
#' @returns A `NilsEstimate` object, essentially a data frame with one row per category and the
#' following columns:
#' \describe{
#'   \item{Cat. ID}{The category ID number.}
#'   \item{Est. total}{The estimated total of the target variable within the category.}
#'   \item{Est. variance}{The estimated variance of the total estimator within the category.}
#'   \item{Positive tracts}{The number of tracts with at least one positive value of the
#'   target variable in the category.}
#' }
#'
#' @examples
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#'
#' @export
NilsEstimate = function(
  plot_data,
  tract_data,
  psus,
  category_psu_map,
  area = 46519242.1175867,
  tract_area = 196 * 100.0 * pi
) {
  category_psu_map = .PrepareCategoryPsuMap(category_psu_map);
  tract_data = .PrepareTractData(tract_data);
  plot_data = .PreparePlotData(plot_data);

  area = .PrepareArea(area, "area");
  tract_area = .PrepareArea(tract_area, "tract_area");

  psus = .PreparePsus(psus, tract_data);

  obj = .NilsEstimate(
    psus,
    category_psu_map,
    tract_data,
    plot_data,
    area,
    tract_area
  );

  return(.ConstructNilsEstimate(
    obj,
    psus = psus,
    category_psu_map = category_psu_map,
    area = area,
    tract_area = tract_area,
    balanced = FALSE
  ));
}

.ConstructNilsEstimate = function(obj, ...) {
  params = list(...);

  cat_ids = params$category_psu_map[, 1];
  cat_names = rownames(params$category_psu_map);

  ne = data.frame(
    cat_id = cat_ids,
    est = obj$cat_estimates,
    var = diag(obj$cat_covmat),
    pos = obj$positive_tracts_per_cat
  );

  colnames(ne) = c("Cat. ID", "Est. total", "Est. variance", "Positive tracts");
  rownames(ne) = cat_names;

  class(ne) = c("NilsEstimate", class(ne));

  for (p in names(params)) {
    attr(ne, p) = params[[p]];
  }

  attr(ne, "estimate") = obj$estimate;
  attr(ne, "variance") = obj$variance;
  attr(ne, "filtered") = FALSE;

  covmat = obj$cat_covmat;
  rownames(covmat) = cat_ids;
  colnames(covmat) = cat_ids;
  attr(ne, "covmat") = covmat;

  attr(ne, "nonnil_tracts") = obj$nonnil_tracts;

  return(ne);
}
