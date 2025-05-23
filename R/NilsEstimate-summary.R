#' Summary of the NilsEstimate
#'
#' @description
#' Returns a list of summary statistics of the NilsEstimate object
#'
#' @param obj An object of class `NilsEstimate`
#'
#' @details
#' The returned list has the following names:
#' - `estimate` for the estimate of the total,
#' - `variance` for the variance estimate of the total,
#' - `rel_se` for the relative standard error of the total.
#'
#' If the data has not been filtered, it also contains:
#' - `nonnil_tracts` for the number of non-nil tracts
#'
#' @export
summary.NilsEstimate = function(obj) {
  sne = list(
    estimate = attr(obj, "estimate"),
    variance = attr(obj, "variance"),
    rel_se = sqrt(attr(obj, "variance")) / attr(obj, "estimate")
  );
  class(sne) = c("summary.NilsEstimate", "NilsEstimate");

  attr(sne, "balanced") = attr(obj, "balanced");
  attr(sne, "filtered") = attr(obj, "filtered");

  if (!attr(obj, "filtered")) {
    sne$nonnil_tracts = attr(obj, "nonnil_tracts");
  }

  return(sne);
}

#' @rdname summary.NilsEstimate
#' @export
print.summary.NilsEstimate = function(obj) {
  cat(
    "Estimated total: ", obj$estimate, "\n",
    "Estimated SE: ", sqrt(obj$variance), "\n",
    "Relative SE: ", obj$rel_se, "\n",
    sep = ""
  );

  additional = "";

  if (attr(obj, "filtered")) {
    additional = paste0(additional, "Filtered: TRUE\n");
  } else {
    cat(
      "Nonnil tracts: ", obj$nonnil_tracts, "\n",
      sep = ""
    )
  }

  if (attr(obj, "balanced")) {
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
