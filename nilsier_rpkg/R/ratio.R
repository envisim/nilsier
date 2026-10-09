#' Estimate a ratio
#'
#' @description
#' Estimates the ratio of some two variables `data / data_z` surveyed under the NILS hierarchical
#' sampling framework.
#'
#' @inheritParams nilsier
#'
#' @param data_z The denominator variable of the ratio.
#'
#' @details
#' The function combines plot-level observations (`data`, `data_z`), tract-level information
#' (`tracts`), PSU hierarchy (`psus`), and category assignments (`categories`) to estimate totals
#' under the NILS sampling design.
#'
#' ## Variance estimation strategy
#' See [`nilsier`].
#'
#' @returns A `nilsier` object, essentially a data frame with one row per category and the
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
#' # obj = nilsier_ratio(psus, categories, tracts, plots);
#'
#' @export
nilsier_ratio = function(
  psus,
  categories,
  tracts,
  data,
  data_z,
  frame_area = 46519242.1175867,
  tract_area = 196 * 100.0 * pi,
  neighbourhood_size = 4L,
  variance_strategy = "srs"
) {
  categories = prepare_categories(categories);
  tracts     = prepare_tracts(tracts);
  psus       = prepare_psus(psus, tracts, neighbourhood_size);
  data_y     = prepare_data(data);
  data_z     = prepare_data(data_z);

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
  obj = rust_nils_estimate_ratio(
    psus,
    categories,
    tracts,
    data_y,
    data_z,
    frame_area,
    tract_area,
    variance_strategy
  );

  # Adjust variance
  totals   = obj$total_estimates;
  covmat   = obj$category_covariances / totals[2]^2;
  variance = obj$variance / totals[2]^2;


  # Construct NilsEstimate object
  cat_ids = categories$category;

  ne = data.frame(
    cat_id = cat_ids,
    est = obj$category_estimates,
    var = diag(covmat),
    pos = obj$positive_tracts_per_category
  );

  class(ne) = c("nilsier", class(ne));

  attr(ne, "psus")              = psus;
  attr(ne, "categories")        = categories;
  attr(ne, "frame_area")        = frame_area;
  attr(ne, "tract_area")        = tract_area;
  attr(ne, "variance_strategy") = variance_strategy;

  attr(ne, "estimate")          = obj$estimate;
  attr(ne, "totals")            = totals;
  attr(ne, "variance")          = variance;
  attr(ne, "positive_tracts")   = obj$positive_tracts;
  attr(ne, "filtered")          = FALSE;

  rownames(covmat) = cat_ids;
  colnames(covmat) = cat_ids;
  attr(ne, "covmat") = covmat;

  ne
}

#' Estimate a total by a ratio estimator
#'
#' @description
#' Estimates the total of a variables `data` using a ratio estimator and auxiliary information
#' `data_z` (i.e. full cover data).
#'
#' @inheritParams nilsier
#'
#' @param data_z A data frame (or list) containing the survey data:
#' \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{category}{the category identifier (integer).}
#'   \item{value}{the recorded value (float).}
#' }
#' If the columns (or items) are named, the default names can be overidden by the corresponding
#' argument.
#' If the columns (or items) are unnamed, the order of the columns (or items) is assumed to be
#' according to the order given by the parameter list.
#'
#' @param data_z_total The population total of the target variable.
#'
#' @details
#' The function combines plot-level observations (`data`, `data_z`), tract-level information
#' (`tracts`), PSU hierarchy (`psus`), and category assignments (`categories`) to estimate totals
#' under the NILS sampling design.
#'
#' ## Variance estimation strategy
#' See [`nilsier`].
#'
#' @returns A `nilsier` object, essentially a data frame with one row per category and the
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
#' # obj = nilsier_ratio_estimator(psus, categories, tracts, plots);
#'
#' @export
nilsier_ratio_estimator = function(
  psus,
  categories,
  tracts,
  data,
  data_z,
  data_z_total,
  frame_area = 46519242.1175867,
  tract_area = 196 * 100.0 * pi,
  neighbourhood_size = 4L,
  variance_strategy = "srs"
) {
  data_z = .to_named_df(data, c("tract", "category", "value"));
  data_z$dw = 1.0;

  ne = nilsier_ratio(
    psus,
    categories,
    tracts,
    data,
    data_z,
    frame_area,
    tract_area,
    neighbourhood_size,
    variance_strategy
  );

  ne$est = ne$est * data_z_total;
  ne$var = ne$var * data_z_total^2;
  attr(ne, "estimate") = attr(ne, "estimate") * data_z_total;
  attr(ne, "variance") = attr(ne, "variance") * data_z_total^2;
  attr(ne, "covmat") = attr(ne, "covmat") * data_z_total^2;

  ne
}
