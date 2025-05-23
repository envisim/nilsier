efilter = function(obj, ...) {
  UseMethod("efilter");
}

#' Filter
#'
#' @export
efilter.NilsEstimate = function(obj, ...) {
  params = list(...);

  if ("psu" %in% names(params)) {
    values = params$psu;
    cats = attr(obj, "category_psu_map");
    cats = cats[cats[, 2] %in% values, 1];
    obj_filter = obj[, 1] %in% cats;
    obj = obj[obj_filter, ];
    attr(obj, "covmat") = attr(obj, "covmat")[obj_filter, obj_filter];
  }

  if ("categories" %in% names(params)) {
    values = params$categories;
    obj_filter = obj[, 1] %in% values;
    obj = obj[obj_filter, ];
    attr(obj, "covmat") = attr(obj, "covmat")[obj_filter, obj_filter];
  }

  attr(obj, "estimate") = sum(obj[, 2]);
  attr(obj, "variance") = sum(attr(obj, "covmat"));
  attr(obj, "filtered") = TRUE;
  return(obj);
}

