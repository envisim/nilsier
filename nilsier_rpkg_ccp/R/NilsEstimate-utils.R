#' Estimates of a NILS estimate
#'
#' @description
#' Accesses the estimate and variance estimate of a [NilsEstimate] object.
#'
#' @param object A [NilsEstimate] object.
#' @param ... Additional arguments (currently unused).
#'
#' @returns A named vector with the following elements:
#' \describe{
#'   \item{estimate}{Estimated total of the target variable.}
#'   \item{variance}{Estimated variance of the total estimator.}
#' }
#'
#' @examples
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' coef(obj);
#'
#' @method coef NilsEstimate
#' @export
coef.NilsEstimate = function(object, ...) {
  sne = summary(object);

  return(c(
    estimate = sne$estimate,
    variance = sne$variance
  ));
}

#' Covariance matrix for NILS category estimates
#'
#' @description
#' Accesses the covariance matrix of a [NilsEstimate] object.
#'
#' @param object A [NilsEstimate] object.
#' @param complete Logical. If `FALSE`, excludes apparent zero-tracts.
#' @param ... Additional arguments (currently unused).
#'
#' @returns the covariance matrix of the [NilsEstimate] object.
#'
#' @examples
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' vcov(obj);
#'
#' @method vcov NilsEstimate
#' @export
vcov.NilsEstimate = function(object, complete = TRUE, ...) {
  mat = attr(object, "covmat");

  if (complete) {
    return(mat);
  }

  non_nil = object[, 4] > 0;
  return(mat[non_nil, non_nil]);
}
