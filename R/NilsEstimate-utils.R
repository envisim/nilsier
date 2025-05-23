#' Estimates of the NilsEstimate
#'
#' @description
#' Returns a vector of the estimate and variance estimate of the NilsEstimate object.
#'
#' @param obj An object of class `NilsEstimate`
#'
#' @export
coef.NilsEstimate = function(obj) {
  sne = summary(obj);

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
#'
#' @rdname coef.NilsEstimate
#' @export
vcov.NilsEstimate = function(obj, complete = TRUE) {
  mat = attr(obj, "covmat");

  if (complete) {
    return(mat);
  }

  non_nil = obj[, 4] > 0;
  return(mat[non_nil, non_nil]);
}
