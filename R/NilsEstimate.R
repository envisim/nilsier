#' Estimate totals using the NILS hierachical design
#'
#' @description
#' Estimates the total of some variable surveyed under the NILS hiearchical sampling framework.
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

#' Estimate totals using the NILS hierachical design, assuming a spatially balanced design
#'
#' @param auxiliaries A numeric matrix of auxiliary variables used for balancing. Must have the same
#' dimensions and order as `tract_data`.
#'
#' @param size_of_neighbourhood An optional numeric vector specifying the neighbourhood size for
#' each PSU level.
#'
#' @details
#' ## NilsEstimateBalanced
#' In the balanced variant, variance is estimated using a local neighbourhood deviance measure.
#' The neighbourhood size defaults to 4 for the smallest PSU level and increases linearly with PSU
#' level size.
#'
#' Covariance between categories belonging to different PSU levels are measured over their
#' intersection, i.e. on the smaller PSU collections.
#' Consequently, the smaller PSU collection also determines the local neighbourhood size.
#'
#' It is possible to provide a matrix as `psus` instead of a vector. This matrix should contain:
#' 1. PSU IDs, ordered from largest to smallest (by PSU size).
#' 2. The neighbourhood size for each PSU.
#'
#' If `psus` is provided as a vector, the neighbourhood size of PSU \eqn{k} defaults to
#' \deqn{4 \frac{n_{k}}{n_{(0)}} ,}
#' where \eqn{n_{k}} is the size of PSU collection \eqn{k}, and \eqn{n_{(0)}} is the size of the
#' smallest PSU collection.
#'
#' @examples
#' obj = NilsEstimateBalanced(
#'   plots,
#'   tracts,
#'   tract_auxilliaries,
#'   psus,
#'   category_psu_map
#' );
#'
#' @rdname NilsEstimate
#' @export
NilsEstimateBalanced = function(
  plot_data,
  tract_data,
  auxiliaries,
  psus,
  category_psu_map,
  area = 46519242.1175867,
  tract_area = 196 * 100 * pi,
  size_of_neighbourhood = NULL
) {
  category_psu_map = .PrepareCategoryPsuMap(category_psu_map);
  tract_data = .PrepareTractData(tract_data);
  plot_data = .PreparePlotData(plot_data);

  area = .PrepareArea(area, "area");
  tract_area = .PrepareArea(tract_area, "tract_area");

  auxiliaries_names = colnames(auxiliaries);
  auxiliaries = .PrepareAuxiliaries(auxiliaries, nrow(tract_data));

  psus = .PreparePsus(psus, tract_data);
  psus = .PrepareNeighbourhood(psus, size_of_neighbourhood);

  obj = .NilsBalancedEstimate(
    psus,
    category_psu_map,
    tract_data,
    plot_data,
    area,
    tract_area,
    auxiliaries
  );

  return(.ConstructNilsEstimate(
    obj,
    psus = psus,
    category_psu_map = category_psu_map,
    area = area,
    tract_area = tract_area,
    balanced = TRUE,
    auxiliaries = auxiliaries_names
  ));

  return(obj);
}

#' Prepare plot-level data
#'
#' @description
#' Reorders and extracts columns from a data frame to create a valid `plot_data` object.
#' Column names are supplied as formulas for convenience.
#'
#' @param data A data.frame containing plot-level information.
#' @param tid A formula specifying the column containing tract IDs.
#' @param cat A formula specifying the column containing category IDs.
#' @param dw A formula specifying the column containing design weights.
#' @param y A formula specifying the column containing the variable of interest.
#'
#' @returns
#' A data frame with the required column order for use as `plot_data`:
#' 1. Tract ID (integer).
#' 2. Category ID (integer).
#' 3. Design weight (double).
#' 4. Value of the variable of interest (double).
#'
#' @examples
#' wide_df = data.frame(
#'   variable_y = runif(16),
#'   variable_x = runif(16),
#'   variable_z = runif(16),
#'   grassland_cover = plots[, 4],
#'   design_weight = plots[, 3],
#'   category = plots[, 2],
#'   tract_id = plots[, 1]
#' );
#'
#' plot_data = PreparePlotData(
#'   wide_df,
#'   ~tract_id,
#'   ~category,
#'   ~design_weight,
#'   ~grassland_cover
#' );
#'
#' @export
PreparePlotData = function(data, tid, cat, dw, y) {
  tid = all.vars(tid);
  if (length(tid) < 1) {
    stop("tid needs to be defined");
  }

  cat = all.vars(cat);
  if (length(cat) < 1) {
    stop("cat needs to be defined");
  }

  dw = all.vars(dw);
  if (length(dw) < 1) {
    stop("dw needs to be defined");
  }

  y = all.vars(y);
  if (length(dw) < 1) {
    stop("y needs to be defined");
  }

  data[, c(tid[1], cat[1], dw[1], y[1])]
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

