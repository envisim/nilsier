#' Converts `obj` to a data.frame, or stops.
.to_named_df = function(obj, columns, optional_columns = NULL, name = "input") {
  if (is.data.frame(obj)) {
    # Assert that data frame contains all columns
    for (col in columns) .assert_contains_item(obj, col, name);

    return(obj);
  } else if (is.list(obj)) {
    if (is.null(names(col))) {
      # Unnamed list
      # Check length is at least minimum size
      len = length(obj);
      if (len < length(columns)) stop(name, " contains too few items");

      # Early return if no optional columns
      if (is.null(optional_columns)) {
        return(as.data.frame(obj, col.names = columns));
      }

      # Shorten column name vector to correct size
      cns = c(columns, optional_columns);
      if (len < length(cns)) cns = cns[seq_len(len)];

      return(as.data.frame(obj, col.names = cns));
    } else {
      # Named list
      for (col in columns) .assert_contains_item(obj, col, name);

      return(as.data.frame(obj));
    }
  } else if (is.matrix(obj)) {
    lobj = lapply(seq_len(ncol(obj)), \(i) obj[, i]);
    return(.to_named_df(lobj, columns, optional_columns, name));
  }

  stop(name, " cannot be converted to a data.frame");
}

#' Converts `obj` to a list, or stops.
.to_list = function(obj, name = "input") {
  if (is.list(obj)) {
    return(obj);
  } else if (is.matrix(obj)) {
    return(lapply(seq_len(ncol(obj)), \(i) obj[, i]));
  }
  stop(name, " is not a list (or data.frame)");
}

#' Asserts that `obj` contains a named object `item`.
.assert_contains_item = function(obj, item, name = "input") {
  if (!(item %in% names(obj))) stop(name, " must contain an item (or column) named '", item, "'");
}

#' Asserts that `vec` has correct length.
.assert_len = function(vec, len, name = "input") {
  if (len != length(vec)) stop(name, " have incorrect length (should be ", len, ")");
}

#' Asserts that `vec` contains no NA values.
.assert_contains_no_na = function(vec, name = "input") {
  if (any(is.na(vec))) stop(name, " contains NA values");
}

#' Asserts that `vec` is numeric.
.assert_numeric = function(vec, name = "input") {
  if (storage.mode(vec) != "double" && storage.mode(vec) != "integer") {
    stop(name, " is not numeric");
  }
}

#' Asserts that `vec` is double.
.assert_double = function(vec, name = "input") {
  if (storage.mode(vec) != "double") stop(name, " is not double");
}

#' Asserts that `vec` is integer.
.assert_integer = function(vec, name = "input") {
  if (storage.mode(vec) != "integer") stop(name, " is not integer");
}

#' Assert that `vec` is numeric and contains no NA values
.assert_numeric_contains_no_na = function(vec, name = "input") {
  .assert_numeric(vec, name);
  .assert_contains_no_na(vec, name);
}

#' Assert that `vec` is double and contains no NA values
.assert_double_contains_no_na = function(vec, name = "input") {
  .assert_double(vec, name);
  .assert_contains_no_na(vec, name);
}

#' Assert that `vec` is integer and contains no NA values
.assert_integer_contains_no_na = function(vec, name = "input") {
  .assert_integer(vec, name);
  .assert_contains_no_na(vec, name);
}

#' Prepare a PSU object
#'
#' @description
#' Prepare a PSU object, which can be either a data frame or a list.
#'
#' @details
#' If the items (or columns) are unnamed, the order of the items (or columns) are assumed to be
#' according to the order given by the parameter list.
#' If a matrix is supplied, the order is also assumed to be in the parameter list order.
#'
#' @param psus A data frame, list, or a matrix containing the PSUs:
#' \describe{
#'   \item{psu}{the PSU identifier (integer).}
#'   \item{size}{the number of tracts in the PSU (integer).}
#'   \item{nn_size}{(optional) the neighbourhood size to use (integer > `1L`). If not given, it can
#'                  be derived from `neighbourhood_size`.}
#' }
#'
#' @param neighbourhood_size The default neighbourhood size to use for the smallest PSU
#' (integer > `1L`). The neighbourhood size for larger PSUs scales linearly.
#'
#' @param tracts A list of tracts and their corresponding PSUs, defined according to the return
#' value of [`prepare_tracts`] or [`tracts`].
#'
#' @returns A data frame
#' \describe{
#'   \item{psu}{the PSU identifier (integer).}
#'   \item{size}{the number of tracts in the PSU (integer).}
#'   \item{nn_size}{the neighbourhood size to use (integer).}
#' }
#'
#' @example
#' prepped_psus = prepare_psus(psus, neighbourhood_size = 4L);
#'
#' @family prepare
#' @export
prepare_psus = function(psus, neighbourhood_size = 4L) {
  psus = .to_named_df(psus, c("psu", "size"), c("nn_size"));

  .assert_numeric_contains_no_na(psus$psu, "'psu'");
  .assert_numeric_contains_no_na(psus$size, "'size'");
  storage.mode(psus$psu) = "integer";
  storage.mode(psus$size) = "integer";

  if ("nn_size" %in% names(psus)) {
    .assert_numeric(psus$nn_size, "'nn_size'");
    if (!all(2L <= psus$nn_size)) # using all to catch NAs and NaNs
      stop("'nn_size' must be an integer > 1L");
    storage.mode(psus$nn_size) = "integer";
  } else {
    ns = as.integer(neighbourhood_size);
    if (is.null(ns)) {
      warning("'neighbourhood_size' is NULL ... setting to 4L");
      ns = 4L;
    } else if (ns <= 1L) {
      warning("'neighbourhood_size' is <= 2L ... setting to 4L");
      ns = 4L;
    }

    # derive from neighbourhood_size
    psus$nn_size = ns;
    psu_seq = seq_along(psus)[order(psus$nn_size)];
    min_size = psus$nn_size[psu_seq[1L]];

    for (i in psu_seq[2L:nrow(psus)]) {
      size = psus$nn_size[i];
      nn_sizes[i] = as.integer(round(ns * size / min_size));
    }
  }

  psus
}

#' Prepare a category object
#'
#' @description
#' Prepare a category object, which can be either a data frame or a list.
#'
#' @details
#' If the items (or columns) are unnamed, the order of the items (or columns) are assumed to be
#' according to the order given by the parameter list.
#' If a matrix is supplied, the order is also assumed to be in the parameter list order.
#'
#' @param categories A data frame, list, or a matrix containing the categories:
#' \describe{
#'   \item{category}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSu in which the category can appear (integer).}
#' }
#'
#' @returns A data frame
#' \describe{
#'   \item{category}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSu in which the category can appear (integer).}
#' }
#'
#' @example
#' prepped_cats = prepare_categories(category_psu_map);
#'
#' @family prepare
#' @export
prepare_categories = function(categories) {
  categories = .to_named_df(categories, c("category", "psu"));

  .assert_numeric_contains_no_na(categories$category, "'category'");
  .assert_numeric_contains_no_na(categories$psu, "'psu'");
  storage.mode(categories$category) = "integer";
  storage.mode(categories$psu) = "integer";

  categories
}

#' Prepare a tract object
#'
#' @description
#' Prepare a category object, which can be either a list or a data frame.
#'
#' @details
#' An unnamed list will assume the same order as given by the parameter list.
#' A data frame, unnamed or not, will assume the same order as given by the parameter list,
#' discarding any non-floaty columns.
#'
#' @param tracts A list, or a data frame containing the tracts:
#' \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSU containing the tract (integer).}
#'   \item{auxiliaries}{the auxiliary information of the tracts (float matrix).}
#' }
#'
#' @returns A list
#' \describe{
#'   \item{tract}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSU containing the tract (integer).}
#'   \item{auxiliaries}{the auxiliary information of the tracts (float matrix).}
#' }
#'
#' @example
#' prepped_cats = prepare_tracts(tracts);
#'
#' @family prepare
#' @export
prepare_tracts = function(tracts) {
  if (length(tracts) < 2L) stop("'tracts' contains too few items");
  lns = c("tract", "psu");
  lns_optional = c("auxiliaries")

  if (is.list(tracts)) {
    # If list is unnamed, take optional if possible
    if (is.null(names(tracts))) {
      if (length(tracts) == 2L) {
        names(tracts) = lns;
      } else {
        names(tracts) = c(lns, lns_optional);
      }
    } else {
      # If list is named, assert that mandatory items exist
      for (item in lns) .assert_contains_item(tracts, item, "'tracts'");
    }
  } else if (is.data.frame(tracts)) {
    if (length(tracts) == 2L) {
      # If data frame have few cols, we know the layout
      tracts = list(
        tract = tracts[, 1],
        psu = tracts[, 2]
      );
    } else {
      # If data frame have many cols, we need to identify floats
      aux_bool = rep(FALSE, ncol(tracts));
      for (col in 3L:ncol(tracts)) { # ncol guaranteed > 2L b/c check in beginning of fun
        if (storage.mode(tracts[, col]) == "double") aux_bool[col] = TRUE;
      }
      tracts = list(
        tract = tracts[, 1],
        psu = tracts[, 2],
        auxiliaries = as.matrix(tracts[, aux_bool])
      );
    }
  }

  # Do mandatory checks
  len = length(tracts$tract);
  .assert_numeric_contains_no_na(tracts$tract, "'tract'");
  .assert_numeric_contains_no_na(tracts$psu, "'psu'");
  .assert_len(tracts$psu, len, "'psu'");
  storage.mode(tracts$tract) = "integer";
  storage.mode(tracts$psu) = "integer";

  if ("auxiliaries" %in% names(tracts)) {
    .assert_double_contains_no_na(tracts$auxiliaries, "'auxiliaries'");
    .assert_len(tracts$auxiliaries, len, "'psu'");
  }

  tracts
}

#' Prepare a tract data object
#'
#' @description
#' Prepare a tract data object (or plot data), which can be either a list or a data frame.
#'
#' @details
#' If the items (or columns) are unnamed, the order of the items (or columns) are assumed to be
#' according to the order given by the parameter list.
#' If a matrix is supplied, the order is also assumed to be in the parameter list order.
#'
#' @param data A list, or a data frame containing the survey data:
#' \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{category}{the category identifier (integer).}
#'   \item{dw}{the design weight of the data (float).}
#'   \item{value}{the recorded value (float).}
#' }
#'
#' @returns A data frame
#' \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{category}{the category identifier (integer).}
#'   \item{dw}{the design weight of the data (float).}
#'   \item{value}{the recorded value (float).}
#' }
#'
#' @example
#' prepped_data = prepare_data(plots);
#'
#' @family prepare
#' @export
prepare_data = function(data) {
  data = .to_named_df(data, c("tract", "category", "dw", "value"));

  .assert_numeric_contains_no_na(data$tract, "'tract'");
  .assert_numeric_contains_no_na(data$category, "'category'");
  .assert_numeric_contains_no_na(data$dw, "'dw'");
  .assert_numeric_contains_no_na(data$value, "'value'");
  storage.mode(data$tract) = "integer";
  storage.mode(data$category) = "integer";
  storage.mode(data$dw) = "double";
  storage.mode(data$value) = "double";

  data
}

#' Prepare an area
#'
#' @description
#' Prepare an area value, or rejects it if non-positive
#'
#' @param area An area value
#'
#' @returns The value
#'
#' @example
#' prepped_area = prepare_area(2.0);
#'
#' @family prepare
#' @export
prepare_area = function(area) {
  .assert_numeric_contains_no_na(area, "area");
  storage.mode(area) = "double";

  if (area <= 0.0) stop("area must be positive");

  area
}
