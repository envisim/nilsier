#' Summarize a NILS estimate
#'
#' @description
#' Produces summary statistics for a [NilsEstimate] object.
#'
#' @param object A [NilsEstimate] object.
#' @param ... Additional arguments (currently unused).
#'
#' @returns
#' The returned list has the following components:
#' \describe{
#'   \item{estimate}{Estimated total of the target variable.}
#'   \item{variance}{Estimated variance of the total estimator.}
#'   \item{rel_se}{Estimated relative standard error of the total estimator.}
#'   \item{nonnil_tracts}{Number of non-nil tracts. Not shown if filtering has been applied.}
#' }
#'
#' @examples
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' summary(obj);
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
#'
#' @param x A [summary.NilsEstimate].
#'
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

  invisible(x)
}
