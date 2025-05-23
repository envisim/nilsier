#' Estimate the total
#'
#' @description
#' Estimates the total of some variable, surveyed using the NILS hiearchical design framework.
#'
#' @param plot_data A data frame containing information about observations.
#' @param tract_data A matrix containing tract ids and the psus they belong to.
#' @param psus An ordered vector of psus, ranging from largest to smallest.
#' @param category_psu_map A matrix containing categories and the psus they map to.
#' @param area The size of the area frame.
#' @param tract_area The area of a tract.
#'
#' @details
#' `category_psu_map` contains information about the categories used in the
#' design.
#' The object should be a matrix with the following columns (in order):
#' * the category id (integer), as used in `plot_data`;
#' * the psu id (integer) of the largest psu that the category begins to be
#'   sampled in.
#'
#' `tract_data` contains information about all sampled tracts, even those where
#' no interesting category was found.
#' The object should be a matrix with the following columns (in order):
#' * the tract id (integer) of the sampled tracts;
#' * the psu id (integer) of the psu that the tract were sampled in.
#'
#' `plot_data` contains information about observed values on plot level. The
#' object can contain multiple records per plot.
#' The object should be a data frame with the following columns (in order):
#' * the tract id (integer) of the parent tract;
#' * the category id (integer) of the category recorded for the plot;
#' * the design weight (double) for the plot, conditioned on the tract;
#' * the value of the target variable (double).
#'
#' The functions returns a NilsEstimate object (list) with the following names:
#' * `estimate`: the estimate of the total;
#' * `variance`: the variance estimate of the total estimate;
#' * `cat_estimates`: the estimates per category;
#' * `cat_covmat`: the estimated covariance matrix of the per category estimates.
#'
#' @returns A NilsEstimate object (list).
#'
#' @export
NilsEstimate = function(
  plot_data,
  tract_data,
  psus,
  category_psu_map,
  area = 46390172.0,
  tract_area = 196 * 100 * pi
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
#'
#' @details
#' ## NilsEstimateBalanced
#' For the balanced variant, the variance is calculated by using a local
#' neighbourhood deviance measure.
#' The size of this local neighbourhood defaults to 4 for the smallest PSUs, and
#' increases linearly by size.
#'
#' As the covariance between two categories belonging to different psus are
#' measured on the intersect between these categories, e.g. on the smaller of
#' the psus, the smaller psu also decides the size of the local neighbourhood.
#'
#' It is possible to provide custom neighbourhood sizes by adding a third column
#' to `psu_sizes`:
#' * size of neighbourhood
#'
#' @rdname NilsEstimate
#' @export
NilsEstimateBalanced = function(
  plot_data,
  tract_data,
  auxiliaries,
  psus,
  category_psu_map,
  area = 46390172.0,
  tract_area = 196 * 100 * pi,
  size_of_neighbourhood = NULL
) {
  category_psu_map = .PrepareCategoryPsuMap(category_psu_map);
  tract_data = .PrepareTractData(tract_data);
  plot_data = .PreparePlotData(plot_data);

  area = .PrepareArea(area, "area");
  tract_area = .PrepareArea(tract_area, "tract_area");

  auxiliaries_names = colnames(auxiliaries);
  auxiliaries = .PrepareAuxiliaries(auxiliaries);

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
#' Takes the column names as formulas, and returns a correctly ordered data frame.
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

