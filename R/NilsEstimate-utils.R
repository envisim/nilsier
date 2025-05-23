#' Estimates of the NilsEstimate
#'
#' @description
#' Returns a vector of the estimate and variance estimate of the NilsEstimate object.
#'
#' @param object An object of class `NilsEstimate`
#' @param ... additional unused arguments
#'
#' @returns A vector containing the estimate and the variance estimate
#' \describe{
#'   \item{estimate}{the estimate of the total}
#'   \item{variance}{the estimated variance of the estimator of the total}
#' }
#'
#' @examples
#' \dontrun{
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' coef(obj);
#' }
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

#' Covariance matrix for categories
#'
#' @description
#' Returns the covariance matrix of the NilsEstimate object
#'
#' @param complete If `FALSE`, excludes apparent 0-tracts
#' @param ... additional unused arguments
#'
#' @examples
#' \dontrun{
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' vcov(obj);
#' }
#'
#' @rdname coef.NilsEstimate
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
