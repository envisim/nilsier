#' Print a NILS estimate
#'
#' @description
#' Prints a summary of a [nilsier] object.
#'
#' @param x A [nilsier] object.
#' @param complete Logical. If `FALSE` (default), excludes apparent zero-tracts from the printed
#' output.
#' @param ... Additional arguments (currently unused)
#'
#' @returns
#' Invisibly returns the input [nilsier] object.
#'
#' @method print nilsier
#' @export
print.nilsier = function(x, complete = TRUE, ...) {
  complete_rows = complete | x$pos > 0;

  cat("Per category:\n")
  print.data.frame(x[complete_rows, ]);
  cat("---\n");
  print(summary(x));

  invisible(x)
}
#' Summarize a NILS estimate
#'
#' @description
#' Produces summary statistics for a [nilsier] object.
#'
#' @param object A [nilsier] object.
#' @param ... Additional arguments (currently unused).
#'
#' @returns
#' The returned list has the following components:
#' \describe{
#'   \item{estimate}{Estimated total of the target variable.}
#'   \item{variance}{Estimated variance of the total estimator.}
#'   \item{rel_se}{Estimated relative standard error of the total estimator.}
#'   \item{positive_tracts}{Number of non-nil tracts. Not shown if filtering has been applied.}
#' }
#'
#' @examples
#' obj = nilsier(plots, tracts, psus, category_psu_map);
#' summary(obj);
#'
#' @method summary nilsier
#' @export
summary.nilsier = function(object, ...) {
  sne = list(
    estimate = attr(object, "estimate"),
    variance = attr(object, "variance"),
    rel_se = sqrt(attr(object, "variance")) / attr(object, "estimate")
  );
  class(sne) = c("summary.nilsier", "nilsier");

  attr(sne, "variance_strategy") = attr(object, "variance_strategy");
  attr(sne, "filtered") = attr(object, "filtered");

  if (!attr(object, "filtered")) {
    sne$positive_tracts = attr(object, "positive_tracts");
  }

  return(sne);
}

#' @rdname summary.nilsier
#' @method print summary.nilsier
#'
#' @param x A [summary.nilsier].
#'
#' @export
print.summary.nilsier = function(x, ...) {
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

#' @rdname efilter.nilsier
#' @export
efilter = function(obj, ...) {
  UseMethod("efilter");
}

#' Filter NILS estimates
#'
#' @description
#' Filters a [nilsier] object by PSU level or category.
#'
#' @param obj A [nilsier] object.
#' @param psus An optional vector of PSU IDs to keep.
#' @param categories An optional vector of the category IDs to keep.
#' @param ... Additional arguments (currently unused)
#'
#' @returns a filtered [nilsier] object.
#'
#' @examples
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#'
#' # Keep only category with ID 1
#' efilter(obj, categories = 1)
#'
#' # Keep PSU 5
#' efilter(obj, psus = 5)
#'
#'
#' @method efilter nilsier
#' @export
efilter.nilsier = function(obj, psus = NULL, categories = NULL, ...) {
  filter = rep(TRUE, nrow(obj));

  if (!is.null(psus)) {
    filter = filter & vapply(
      seq_along(obj),
      \(i) {
        psu = attr(obj, "categories")$psu[attr(obj, "categories")$category == obj$cat_id[i]];
        psu %in% psus
      },
      TRUE
    );
  }

  if (!is.null(categories)) {
    filter = filter & (obj$cat_id %in% categories);
  }

  obj = obj[filter, ];
  attr(obj, "covmat") = attr(obj, "covmat")[filter, filter];

  attr(obj, "estimate") = sum(obj$est);
  attr(obj, "variance") = sum(attr(obj, "covmat"));
  attr(obj, "filtered") = TRUE;
  return(obj);
}
#' Estimates of a NILS estimate
#'
#' @description
#' Accesses the estimate and variance estimate of a [nilsier] object.
#'
#' @param object A [nilsier] object.
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
#' @method coef nilsier
#' @export
coef.nilsier = function(obj, ...) {
  sne = summary(obj);

  return(c(
    estimate = sne$estimate,
    variance = sne$variance
  ));
}

#' Covariance matrix for NILS category estimates
#'
#' @description
#' Accesses the covariance matrix of a [nilsier] object.
#'
#' @param object A [nilsier] object.
#' @param complete Logical. If `FALSE`, excludes apparent zero-tracts.
#' @param ... Additional arguments (currently unused).
#'
#' @returns the covariance matrix of the [nilsier] object.
#'
#' @examples
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' vcov(obj);
#'
#' @method vcov nilsier
#' @export
vcov.nilsier = function(obj, complete = TRUE, ...) {
  mat = attr(obj, "covmat");

  if (complete) {
    return(mat);
  }

  pos = obj$pos > 0;
  return(mat[pos, pos]);
}
