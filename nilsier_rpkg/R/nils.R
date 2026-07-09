#' Estimate totals using the NILS hierarchical design
#'
#' @description
#' Estimates the total of some variable surveyed under the NILS hierarchical sampling framework.
#'
#' @inheritParams prepare_psus
#' @inheritParams prepare_categories
#' @inheritParams prepare_tracts
#' @inheritParams prepare_data
#'
#' @param frame_area The size of the area frame. Typically larger than the actual area of interest.
#'
#' @param tract_area The area of a tract, expressed in the same units as the target variable.
#'
#' @param variance_strategy The strategy for estimating variance. One of `"srs"` or
#' `"nearest_neighbour"`.
#'
#' @details
#' The function combines plot-level observations (`plot_data`), tract-level information
#' (`tract_data`), PSU hierarchy (`psus`), and category assignments (`category_psu_map`) to estimate
#' totals under the NILS sampling design.
#'
#' ## Variance estimation strategy
#' If the `psus` were selected using a spatially balanced sampling design, the `"srs"` variance
#' strategy often overestimates the variance.
#'
#' When using the `"nearest_neighbours"` strategy, a local neighbourhood variance is used.
#' The neighbourhood size is either manually supplied or calculated from the psu sizes (see
#' [prepare_psus()]). When calculated from the sizes, the PSUs scales linearly with the nearest
#' neighbourhood size of the smallest PSU, given by `neighbourhood_size`.
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
#' obj = nils(psus, category_psu_map, tracts, plots);
#'
#' @export
nils = function(
  psus,
  categories,
  tracts,
  data,
  frame_area = 46519242.1175867,
  tract_area = 196 * 100.0 * pi,
  neighbourhood_size = 4L,
  variance_strategy = "srs"
) {
  psus       = prepare_psus(psus, neighbourhood_size);
  categories = prepare_categories(categories);
  tracts     = prepare_tracts(tracts);
  data       = prepare_data(data);

  frame_area = prepare_area(frame_area);
  tract_area = prepare_area(tract_area);

  if (!(variance_strategy %in% c("srs", "nearest_neighbours"))) {
    warning("unsupported variance strategy ... defaulting to 'srs'");
    variance_strategy = "srs";
  }
  if (variance_strategy == "nearest_neighbours" && !("auxiliaries" %in% names(tracts))) {
    warning(
      "'nearest_neighbours' variance strategy requires auxiliaries to be provided",
      " ... defaulting to 'srs'"
    );
    variance_strategy = "srs";
  }

  # Run algo
  obj = rust_nils_estimate(
    psus,
    categories,
    tracts,
    data,
    frame_area,
    tract_area,
    variance_strategy
  );

  # Construct NilsEstimate object
  cat_ids = categories$category;

  ne = data.frame(
    cat_id = cat_ids,
    est = obj$category_estimates,
    var = diag(obj$category_covariances),
    pos = obj$positive_tracts_per_category
  );

  colnames(ne) = c("Cat. ID", "Est. total", "Est. variance", "Positive tracts");

  class(ne) = c("NilsEstimate2", class(ne));

  attr(ne, "psus")              = psus;
  attr(ne, "categories")        = categories;
  attr(ne, "frame_area")        = frame_area;
  attr(ne, "tract_area")        = tract_area;
  attr(ne, "variance_strategy") = variance_strategy;

  attr(ne, "estimate")          = obj$estimate;
  attr(ne, "variance")          = obj$variance;
  attr(ne, "positive_tracts")   = obj$positive_tracts;
  attr(ne, "filtered")          = FALSE;

  covmat = obj$category_covariances;
  rownames(covmat) = cat_ids;
  colnames(covmat) = cat_ids;
  attr(ne, "covmat") = covmat;

  ne
}

#' @rdname nils
#' @export
NilsEstimate = function(
  plot_data,
  tract_data,
  psus,
  category_psu_map,
  area = 46519242.1175867,
  tract_area = 196 * 100 * pi
) {
  warning("NilsEstimate() was deprecated in 0.2.0, in favor of nils()");
  nils(psus, category_psu_map, tract_data, plot_data, area, tract_area, "srs")
}

#' @rdname nils
#' @export
NilsEstimateBalanced = \(
  plot_data,
  tract_data,
  auxiliaries,
  psus,
  category_psu_map,
  area = 46519242.1175867,
  tract_area = 196 * 100 * pi,
  size_of_neighbourhood = 4L
) {
  warning("NilsEstimateBalanced() was deprecated in 0.2.0, in favor of nils()");
  tracts = as.matrix(tract_data);
  tracts = list(
    tract       = tracts[, 1],
    psu         = tracts[, 2],
    auxiliaries = auxiliaries
  );
  nils(psus, category_psu_map, tracts, plot_data, area, tract_area, "nearest_neighbour");
}
