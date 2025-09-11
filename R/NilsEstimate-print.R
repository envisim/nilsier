#' Print a NILS estimate
#'
#' @description
#' Prints a summary of a `NilsEstimate` object.
#'
#' @param x A `NilsEstimate` object.
#' @param complete Logical. If `FALSE` (default), excludes apparent zero-tracts from the printed
#' output.
#' @param ... Additional arguments (currently unusued)
#'
#' @returns
#' Invisibly returns the input `NilsEstimate` object.
#'
#' @method print NilsEstimate
#' @export
print.NilsEstimate = function(x, complete = TRUE, ...) {
  complete_rows = complete | x[, 4] > 0;

  cat("Per category:\n")
  print.data.frame(x[complete_rows, ]);
  cat("---\n");
  print(summary(x));

  invisible(x)
}
