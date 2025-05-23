#' Print
#' @param complete If `FALSE`, excludes apparent 0-tracts
#' @export
print.NilsEstimate = function(obj, complete = TRUE) {
  complete_rows = complete | obj[, 4] > 0;

  cat("Per category:\n")
  print.data.frame(obj[complete_rows, ]);
  cat("---\n");
  print(summary(obj));
}
