#' Summary of the NilsEstimate
#'
#' @description
#' Returns a list of summary statistics of the NilsEstimate object
#'
#' @param object,x An object of class `NilsEstimate`
#' @param ... additional unused arguments
#'
#' @details
#' The returned list has the following names:
#' \describe{
#'   \item{estimate}{the estimate of the total}
#'   \item{variance}{the estimated variance of the estimator of the total}
#'   \item{rel_se}{the estimated relative standard error of the estimator of the total}
#'   \item{nonnil_tracts}{the number of non-nil tracts (not shown if filter has been applied)}
#' }
#'
#' @examples
#' \dontrun{
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' summary(obj);
#' }
#'
#' @method summary NilsEstimate
#' @export
summary.NilsEstimate = function(object, ...) {
  sne = list(
    estimate = attr(object, "estimate"),
    variance = attr(object, "variance"),
    rel_se = sqrt(attr(object, "variance")) / attr(object, "estimate")
  );
  class(sne) = c("summary.NilsEstimate", "NilsEstimate");

  attr(sne, "balanced") = attr(object, "balanced");
  attr(sne, "filtered") = attr(object, "filtered");

  if (!attr(object, "filtered")) {
    sne$nonnil_tracts = attr(object, "nonnil_tracts");
  }

  return(sne);
}

#' @rdname summary.NilsEstimate
#' @method print summary.NilsEstimate
#' @export
print.summary.NilsEstimate = function(x, ...) {
  cat(
    "Estimated total: ", x$estimate, "\n",
    "Estimated SE: ", sqrt(x$variance), "\n",
    "Relative SE: ", x$rel_se, "\n",
    sep = ""
  );

  additional = "";

  if (attr(x, "filtered")) {
    additional = paste0(additional, "Filtered: TRUE\n");
  } else {
    cat(
      "Nonnil tracts: ", x$nonnil_tracts, "\n",
      sep = ""
    )
  }

  if (attr(x, "balanced")) {
    additional = paste0(additional, "Balanced: TRUE\n");
  }

  if (additional != "") {
    cat(
      "---\n",
      additional,
      sep = ""
    )
  }
}
