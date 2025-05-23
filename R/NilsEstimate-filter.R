efilter = function(obj, ...) {
  UseMethod("efilter");
}

#' Filter
#'
#' @description
#' Filters the results per PSU or categories.
#'
#' @param obj the NilsEstimate object
#' @param psus a vector of the psu id numbers to keep
#' @param categories a vector of the category id numbers to keep
#' @param ... additional unused arguments
#'
#' @examples
#' \dontrun{
#' obj = NilsEstimate(plots, tracts, psus, category_psu_map);
#' # Exclude all categories except the categories with id number 1
#' efilter(obj, categories = c(1));
#' }
#'
#' @method efilter NilsEstimate
#' @export
efilter.NilsEstimate = function(obj, psus = NULL, categories = NULL, ...) {
  if (!is.null(psus)) {
    cats = attr(obj, "category_psu_map");
    cats = cats[cats[, 2] %in% psus, 1];
    obj_filter = obj[, 1] %in% cats;
    obj = obj[obj_filter, ];
    attr(obj, "covmat") = attr(obj, "covmat")[obj_filter, obj_filter];
  }

  if (!is.null(categories)) {
    obj_filter = obj[, 1] %in% categories;
    obj = obj[obj_filter, ];
    attr(obj, "covmat") = attr(obj, "covmat")[obj_filter, obj_filter];
  }

  attr(obj, "estimate") = sum(obj[, 2]);
  attr(obj, "variance") = sum(attr(obj, "covmat"));
  attr(obj, "filtered") = TRUE;
  return(obj);
}

