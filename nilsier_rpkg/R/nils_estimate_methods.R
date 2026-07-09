#' Print a NILS estimate
#'
#' @description
#' Prints a summary of a [NilsEstimate] object.
#'
#' @param x A [NilsEstimate] object.
#' @param complete Logical. If `FALSE` (default), excludes apparent zero-tracts from the printed
#' output.
#' @param ... Additional arguments (currently unused)
#'
#' @returns
#' Invisibly returns the input [NilsEstimate] object.
#'
#' @method print NilsEstimate2
#' @export
print.NilsEstimate2 = function(x, complete = TRUE, ...) {
  complete_rows = complete | x[, 4] > 0;

  cat("Per category:\n")
  print.data.frame(x[complete_rows, ]);
  cat("---\n");
  print(summary(x));

  invisible(x)
}
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
#' @method summary NilsEstimate2
#' @export
summary.NilsEstimate2 = function(object, ...) {
  sne = list(
    estimate = attr(object, "estimate"),
    variance = attr(object, "variance"),
    rel_se = sqrt(attr(object, "variance")) / attr(object, "estimate")
  );
  class(sne) = c("summary.NilsEstimate2", "NilsEstimate2");

  attr(sne, "variance_strategy") = attr(object, "variance_strategy");
  attr(sne, "filtered") = attr(object, "filtered");

  if (!attr(object, "filtered")) {
    sne$positive_tracts = attr(object, "positive_tracts");
  }

  return(sne);
}

#' @rdname summary.NilsEstimate2
#' @method print summary.NilsEstimate2
#'
#' @param x A [summary.NilsEstimate2].
#'
#' @export
print.summary.NilsEstimate2 = function(x, ...) {
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
      "Positive tracts: ", x$positive_tracts, "\n",
      sep = ""
    )
  }

  additional = paste0(additional, "Variance strategy: '", attr(x, "variance_strategy"), "'\n");

  if (additional != "") {
    cat(
      "---\n",
      additional,
      sep = ""
    )
  }

  invisible(x)
}
