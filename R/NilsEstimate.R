#' Estimate the total
#'
#' @description
#' Estimates the total of some variable, surveyed using the NILS hiearchical design framework.
#'
#' @param plot_data A data frame containing information about observations.
#' @param tract_data A matrix containing tract ids and the psus they belong to.
#' @param psus An ordered vector of PSUs, ranging from largest to smallest (in terms of PSU sizes).
#' @param category_psu_map A matrix containing categories and the psus they map to.
#' @param area The size of the area frame. The area frame is most likely larger than the area of
#' interest.
#' @param tract_area The area of a tract, in the same unit as the value of the target variable.
#'
#' @details
#' `category_psu_map` contains information about the categories used in the design.
#' The object should be a matrix with the following columns (in order):
#'
#' 1. The category id number (integer), as used in `plot_data`.
#' 2. The PSU id number (integer) of that the category belongs to; the smallest PSU (id) in which the
#' category is sampled.
#'
#' `tract_data` contains information about all sampled tracts, even those where no interesting
#' category was found.
#' The object should be a matrix with the following columns (in order):
#' 1. The tract id numbers (integer) of all sampled tracts.
#' 2. The PSU id number (integer) of the PSU that last sampled the tract; the smallest PSU which
#' contains the tract.
#'
#' `plot_data` contains information about observed values on plot level.
#' The object can contain multiple records per plot.
#' The object should be a data frame with the following columns (in order):
#' 1. the tract id number (integer) of the parent tract.
#' 2. the category id number (integer) of the category recorded for the plot.
#' 3. the design weight (double) for the plot, conditioned on the tract.
#' 4. the value of the target variable (double).
#'
#' @returns A `NilsEstimate` object, essentially a data frame with the estimates per category. The
#' data frame has the following columns
#' \describe{
#'   \item{Cat. ID}{Category id number}
#'   \item{Est. total}{The estimated total within the category}
#'   \item{Est. variance}{The estimated variance of the estimator of total within the category}
#'   \item{Positive tracts}{The number of tracts on which there was a recorded positive value of the
#'   variable of interest, within the category}
#' }
#'
#' @examples
#' \dontrun{
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' }
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

#' Estimate the total from a sample drawn using a spatially balanced design
#' @param auxiliaries A (double) matrix of auxiliary information used in the balancing.
#' Must match the size (and order) of tract_data.
#' @param size_of_neighbourhood An optional vector of sizes of the neighbourhood for each PSU.
#'
#' @details
#' ## NilsEstimateBalanced
#' For the balanced variant, the variance is calculated by using a local neighbourhood deviance
#' measure.
#' The size of this local neighbourhood defaults to 4 for the smallest PSUs, and increases linearly
#' by size.
#'
#' As the covariance between two categories belonging to different PSUs are measured on the
#' intersect between these categories, e.g. on the smaller of the PSUs, the smaller PSU also decides
#' the size of the local neighbourhood.
#'
#' It is possible to provide a matrix as `psus` instead of the vector. The matrix should have the
#' following columns:
#' 1. The PSU IDs, ranging from largest to smallest (in terms of PSU sizes).
#' 2. The size of the neighbourhoods to use for each PSU.
#'
#' If a matrix is not provided as `psus`, the neighbourhood size of PSU $k$ will default to
#' \deqn{4 \frac{n_{k}}{n_{(0)}} ,}
#' where \eqn{n_{k}} is the size of PSU \eqn{k}, and \eqn{n_{(0)}} is the size of the smallest PSU.
#'
#' @examples
#' \dontrun{
#' obj = NilsEstimateBalanced(plots, tracts, tract_auxilliaries, psus, category_psu_map);
#' }
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

#' Prepare plot data
#'
#' @description
#' Takes the column names as formulas, and returns a correctly ordered data frame for plot data.
#'
#' @param data A data.frame containing the plot information
#' @param tid The name of the column containing tract id numbers
#' @param cat The name of the column containing category id numbers
#' @param dw The name of the column containing design weights
#' @param y The name of the column containing the variable of interest
#'
#' @examples
#' \dontrun{
#' wide_df = data.frame(
#'   variable_y = runif(16),
#'   variable_x = runif(16),
#'   variable_z = runif(16),
#'   grassland_cover = plots[, 4],
#'   design_weight = plots[, 3],
#'   category = plots[, 2],
#'   tract_id = plots[, 1]
#' );
#' plot_data = PreparePlotData(wide_df, ~tract_id, ~category, ~design_weight, ~grassland_cover);
#' }
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

