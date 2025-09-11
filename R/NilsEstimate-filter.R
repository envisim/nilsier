#' @rdname efilter.NilsEstimate
#' @export
efilter = function(obj, ...) {
  UseMethod("efilter");
}

#' Filter NILS estimates
#'
#' @description
#' Filters a `NilsEstimate` object by PSU level or category.
#'
#' @param obj A `NilsEstimate` object.
#' @param psus An optional vector of PSU IDs to keep.
#' @param categories An optional vector of the category IDs to keep.
#' @param ... Additional arguments (currently unused)
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

