#' Converts `obj` to a data.frame, or stops.
#' @keywords internal
#' @noRd
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
    lobj = lapply(seq_len(ncol(obj)), \(i) obj[, i, drop = TRUE]);
    return(.to_named_df(lobj, columns, optional_columns, name));
  }

  stop(name, " cannot be converted to a data.frame");
}

#' Converts `obj` to a list, or stops.
#' @keywords internal
#' @noRd
.to_list = function(obj, name = "input") {
  if (is.list(obj)) {
    return(obj);
  } else if (is.matrix(obj)) {
    return(lapply(seq_len(ncol(obj)), \(i) obj[, i, drop = TRUE]));
  }
  stop(name, " is not a list (or data.frame)");
}

#' Asserts that `obj` contains a named object `item`.
#' @keywords internal
#' @noRd
.assert_contains_item = function(obj, item, name = "input") {
  if (!(item %in% names(obj))) stop(name, " must contain an item (or column) named '", item, "'");
}

#' Asserts that `vec` has correct length.
#' @keywords internal
#' @noRd
.assert_len = function(vec, len, name = "input") {
  if (len != length(vec)) stop(name, " have incorrect length (should be ", len, ")");
}

#' Asserts that `vec` contains no NA values.
#' @keywords internal
#' @noRd
.assert_contains_no_na = function(vec, name = "input") {
  if (any(is.na(vec))) stop(name, " contains NA values");
}

#' Asserts that `vec` is numeric.
#' @keywords internal
#' @noRd
.assert_numeric = function(vec, name = "input") {
  if (storage.mode(vec) != "double" && storage.mode(vec) != "integer") {
    stop(name, " is not numeric");
  }
}

#' Asserts that `vec` is double.
#' @keywords internal
#' @noRd
.assert_double = function(vec, name = "input") {
  if (storage.mode(vec) != "double") stop(name, " is not double");
}

#' Asserts that `vec` is integer.
#' @keywords internal
#' @noRd
.assert_integer = function(vec, name = "input") {
  if (storage.mode(vec) != "integer") stop(name, " is not integer");
}

#' Assert that `vec` is numeric and contains no NA values
#' @keywords internal
#' @noRd
.assert_numeric_contains_no_na = function(vec, name = "input") {
  .assert_numeric(vec, name);
  .assert_contains_no_na(vec, name);
}

#' Assert that `vec` is double and contains no NA values
#' @keywords internal
#' @noRd
.assert_double_contains_no_na = function(vec, name = "input") {
  .assert_double(vec, name);
  .assert_contains_no_na(vec, name);
}

#' Assert that `vec` is integer and contains no NA values
#' @keywords internal
#' @noRd
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
#' If `nn_size` is provided through `psus`, `tracts` and `neighbourhood_size` can be ignored.
#' Otherwise, the neighbourhood size is calculated from the PSU sizes.
#' The PSU size is derived from the number of `tracts` with a PSU id, and neighbourhood sizes scales
#' linearly with these sizes, starting from `neighbourhood_size`.
#'
#' @param psus A vector of PSU identifiers (integer) in order from the largest to the smallest, or;
#' a data frame, list, matrix, or vector containing the PSUs:
#' \describe{
#'   \item{psu}{the PSU identifiers (integer) in order from the largest to the smallest.}
#'   \item{nn_size}{(optional) the neighbourhood size to use (integer > `1L`). Will be derived from
#'                  `neighbourhood_size` if not given.}
#' }
#' If the columns (or items) are unnamed, or if a matrix is supplied, the order of the columns
#' (or items) are assumed to follow the order given by the list above.
#' If a matrix is supplied, the order is also assumed to be in the parameter list order.
#'
#' @param tracts A list or data.frame of tracts with the following items (columns):
#' \describe{
#'   \item{psu}{the PSU ID of the tract.}
#' }
#'
#' @param neighbourhood_size The default neighbourhood size to use for the smallest PSU
#' (integer > `1L`). The neighbourhood size for larger PSUs scales linearly.
#'
#' @returns A data frame
#' \describe{
#'   \item{psu}{the PSU identifier (integer).}
#'   \item{size}{the number of tracts in the PSU (integer).}
#'   \item{nn_size}{the neighbourhood size to use (integer).}
#' }
#'
#' @examples
#' prepped_psus = prepare_psus(psus, tracts, 4L);
#'
#' @family prepare
#' @export
prepare_psus = function(psus, tracts = NULL, neighbourhood_size = 4L) {
  if (is.vector(psus)) {
    psus = list(psu = psus);
  } else {
    psus = .to_named_df(psus, c("psu"), c("nn_size"));
  }

  .assert_numeric_contains_no_na(psus$psu, "'psu'");
  storage.mode(psus$psu) = "integer";

  .assert_contains_item(tracts, "psu", "'tracts'");

  # Count the number of tracts matching each PSU. Since PSUs are large->small, we need to reverse
  # the order before the cumsum, and reverse it back after.
  psus$size = rev(cumsum(rev(vapply(psus$psu, \(x) sum(tracts$psu == x), 0L))));
  storage.mode(psus$size) = "integer";


  if (!("nn_size" %in% names(psus))) {
    ns = as.integer(neighbourhood_size);
    if (is.null(ns)) {
      warning("'neighbourhood_size' is NULL ... setting to 4L");
      ns = 4L;
    } else if (ns <= 1L) {
      warning("'neighbourhood_size' is <= 2L ... setting to 4L");
      ns = 4L;
    }

    min_size = psus$size[length(psus$size)];
    psus$nn_size = as.integer(round(ns * psus$size / min_size));
  }

  .assert_numeric(psus$nn_size, "'nn_size'");
  if (!all(2L <= psus$nn_size)) # using all to catch NAs and NaNs
    stop("'nn_size' must be an integer > 1L");
  storage.mode(psus$nn_size) = "integer";

  psus
}

#' Prepare a category object
#'
#' @description
#' Prepare a category object, which can be either a data frame or a list.
#'
#' @param categories A data frame, list, or a matrix containing the categories:
#' \describe{
#'   \item{category}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSu in which the category can appear (integer).}
#' }
#' If the columns (or items) are unnamed, the order of the columns (or items) is assumed to be
#' according to the order given by the parameter list.
#' If a matrix is supplied, the order is also assumed to be in the parameter list order.
#'
#' @returns A data frame
#' \describe{
#'   \item{category}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSU in which the category can appear (integer).}
#' }
#'
#' @examples
#' prepped_cats = prepare_categories(categories);
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
#' @param tracts Any of the following:
#' - A named list with items: \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSU containing the tract (integer).}
#'   \item{auxiliaries}{the auxiliary information of the tracts (float matrix).}
#' }
#' - An unnamed list with items following the item order of the named list.
#' - A data frame with items following the item order of the named list. Any columns after the first
#' two will assumed to be auxiliaries, however discarded if they are non-floaty.
#' - A matrix with two columns containing the `tract`s and the `psu`s, in that order.
#'
#' @returns A list:
#' \describe{
#'   \item{tract}{the category identifier (integer).}
#'   \item{psu}{the PSU identifier of the smallest PSU containing the tract (integer).}
#'   \item{auxiliaries}{the auxiliary information of the tracts (float matrix).}
#' }
#'
#' @examples
#' prepped_tracts = prepare_tracts(tracts);
#'
#' @family prepare
#' @export
prepare_tracts = function(tracts) {
  if (length(tracts) < 2L) stop("'tracts' contains too few items");
  lns = c("tract", "psu");
  lns_optional = c("auxiliaries")

  if (is.data.frame(tracts)) {
    if (length(tracts) == 2L) {
      # If data frame have few cols, we know the layout
      tracts = list(
        tract = tracts[[1]],
        psu = tracts[[2]]
      );
    } else {
      # If data frame have many cols, we need to identify floats
      aux_bool = rep(FALSE, ncol(tracts));
      for (col in 3L:ncol(tracts)) { # ncol guaranteed > 2L b/c check in beginning of fun
        if (storage.mode(tracts[[col]]) == "double") {
          aux_bool[col] = TRUE;
        } else if (storage.mode(tracts[[col]]) == "integer") {
          storage.mode(tracts[[col]]) = "double";
          aux_bool[col] = TRUE;
        } else {
          warning("auxiliaries on column ", col, " discarded as non-floaty");
        }
      }

      tracts = list(
        tract = tracts[[1]],
        psu = tracts[[2]],
        auxiliaries = as.matrix(tracts[, aux_bool])
      );
    }
  } else if (is.list(tracts)) {
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
  } else if (is.matrix(tracts) && ncol(tracts) == 2L) {
    tracts = list(
      tract = tracts[, 1, drop = TRUE],
      psu = tracts[, 2, drop = TRUE]
    );
  } else {
    stop("'tracts' cannot be converted to a list");
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
    if (len != nrow(tracts$auxiliaries)) {
      stop("'auxiliaries' have incorrect length (should be ", len, ")");
    }
  }

  tracts
}

#' Prepare a tract data object
#'
#' @description
#' Prepare a tract data object (or plot data), which can be either a list or a data frame.
#'
#' @param data A data frame (or list) containing the survey data:
#' \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{category}{the category identifier (integer).}
#'   \item{dw}{the design weight of the data (float).}
#'   \item{value}{the recorded value (float).}
#' }
#' If the columns (or items) are named, the default names can be overidden by the corresponding
#' argument.
#' If the columns (or items) are unnamed, the order of the columns (or items) is assumed to be
#' according to the order given by the parameter list.
#'
#' @param tract A formula specifying the column (or item) containing tract IDs.
#' @param category A formula specifying the column (or item) containing category IDs.
#' @param dw A formula specifying the column (or item) containing design weights.
#' @param value A formula specifying the column (or item) containing the variable of interest.
#'
#' @returns A data frame
#' \describe{
#'   \item{tract}{the tract identifier (integer).}
#'   \item{category}{the category identifier (integer).}
#'   \item{dw}{the design weight of the data (float).}
#'   \item{value}{the recorded value (float).}
#' }
#'
#' @examples
#' prepped_data = prepare_data(plots);
#'
#' @family prepare
#' @export
prepare_data = function(data, tract = ~tract, category = ~category, dw = ~dw, value = ~value) {
  tract = all.vars(tract)[1];
  category = all.vars(category)[1];
  dw = all.vars(dw)[1];
  value = all.vars(value)[1];

  data = .to_named_df(data, c(tract, category, dw, value));

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

#' @rdname prepare_data
#' @export
PreparePlotData = function(data, tid, cat, dw, y) {
  warning("PreparePlotData() was deprecated in 0.2.0. Use prepare_data instead.")
  prepare_data(data, tid, cat, dw, y)
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
#' @examples
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
