#' Print
#'
#' @param x the NilsEstimate object
#' @param complete If `FALSE`, excludes apparent 0-tracts
#' @param ... additional unused arguments
#'
#' @method print NilsEstimate
#' @export
print.NilsEstimate = function(x, complete = TRUE, ...) {
  complete_rows = complete | x[, 4] > 0;

  cat("Per category:\n")
  print.data.frame(x[complete_rows, ]);
  cat("---\n");
  print(summary(x));
}
