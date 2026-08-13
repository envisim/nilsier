.StopIfNa = function(vec, name = "input") {
  if (any(is.na(vec))) {
    stop(paste0("NA values in ", name));
  }
}

.StopIfNotMatrix = function(mat, name = "input") {
  if (!is.matrix(mat)) {
    stop(paste0(name, " is not a matrix"));
  }
}

.StopIfNaN = function(vec, name = "input") {
  if (storage.mode(vec) != "double" && storage.mode(vec) != "integer") {
    stop(paste0(name, " is not containing numbers"));
  }
}

.TrueIfIntegerStopIfNaN = function(vec, name = "input") {
  .StopIfNaN(vec, name);
  .StopIfNa(vec, name);
  return(storage.mode(vec) == "integer");
}

.TrueIfDoubleStopIfNaN = function(vec, name = "input") {
  .StopIfNaN(vec, name);
  .StopIfNa(vec, name);
  return(storage.mode(vec) == "double");
}

.PreparePsus = function(psus, tract_data) {
  if (.TrueIfDoubleStopIfNaN(psus, "psus")) {
    storage.mode(psus) = "integer";
  }

  psus = cbind(psus, 0L);

  if (nrow(psus) == 0) {
    stop("psus needs to be a non empty vector");
  }

  psus[, 2] = rev(cumsum(rev(vapply(psus[, 1], function(x) sum(tract_data[, 2] == x), 0L))));

  if (any(psus[, 2] < 2)) {
    stop("some psus are smaller than 2");
  }

  return(psus);
}

.PrepareNeighbourhood = function(psus, size_of_neighbourhood) {
  # Automagically fix neighbours, if not set
  psus = cbind(psus, 0L);

  if (is.null(size_of_neighbourhood)) {
    psus[, 3] = as.integer(round(4 * psus[, 2] / min(psus[, 2]), ))
    return(psus);
  }

  .TrueIfIntegerStopIfNaN(size_of_neighbourhood, "size_of_neighbourhood");

  if (length(size_of_neighbourhood) != nrow(psus)) {
    stop("psus and size_of_neighbourhood does not match");
  }

  if (any(size_of_neighbourhood < 2)) {
    stop("psu_sizes neighbours need to be at least 2 for each psu");
  }

  psus[, 3] = as.integer(size_of_neighbourhood);

  return(psus);
}

.PrepareCategoryPsuMap = function(category_psu_map) {
  category_psu_map = as.matrix(category_psu_map);

  if (.TrueIfDoubleStopIfNaN(category_psu_map, "category_psu_map")) {
    storage.mode(category_psu_map) = "integer";
  }

  if (nrow(category_psu_map) == 0 || ncol(category_psu_map) < 2) {
    stop("category_psu_map needs to be a non empty matrix with two columns");
  }

  return(category_psu_map);
}

.PrepareTractData = function(tract_data) {
  tract_data = as.matrix(tract_data);

  if (.TrueIfDoubleStopIfNaN(tract_data, "tract_data")) {
    storage.mode(tract_data) = "integer";
  }

  if (nrow(tract_data) == 0 || ncol(tract_data) < 2) {
    stop("tract_data needs to be a non empty matrix with two columns");
  }

  return(tract_data);
}

.PreparePlotData = function(plot_data) {
  plot_data = as.data.frame(plot_data);

  if (nrow(plot_data) == 0 || ncol(plot_data) < 4) {
    stop("plot_data needs to be a non empty data frame of at least 4 columns");
  }

  if (.TrueIfDoubleStopIfNaN(plot_data[, 1], "plot_data, tract ids")) {
    storage.mode(plot_data[, 1]) = "integer";
  }

  if (.TrueIfDoubleStopIfNaN(plot_data[, 2], "plot_data, plot categories")) {
    storage.mode(plot_data[, 2]) = "integer";
  }

  if (.TrueIfIntegerStopIfNaN(plot_data[, 3], "plot_data, design weights")) {
    storage.mode(plot_data[, 3]) = "double";
  }

  if (.TrueIfIntegerStopIfNaN(plot_data[, 4], "plot_data, values")) {
    storage.mode(plot_data[, 4]) = "double";
  }

  return(plot_data);
}

.PrepareArea = function(area, name = "area") {
  if (.TrueIfIntegerStopIfNaN(area, name)) {
    storage.mode(area) = "double";
  }

  if (area <= 0.0) {
    stop(paste0(name, " must be positive"));
  }

  return(area);
}

.PrepareAuxiliaries = function(auxiliaries, nobs) {
  auxiliaries = t(as.matrix(auxiliaries));

  if (.TrueIfIntegerStopIfNaN(auxiliaries, "auxiliaries")) {
    storage.mode(auxiliaries) = "double";
  }

  if (ncol(auxiliaries) != nobs || nrow(auxiliaries) == 0) {
    stop("auxiliaries needs to be a non empty matrix with the same size as tract_data");
  }

  return(auxiliaries);
}



