#include <cmath>
#include <stddef.h>
#include <stdexcept>
#include <vector>

#include <Rcpp.h>

#include "KeyValueMap.h"
#include "TractStore.h"

KeyValueMap CreatePsuKeyValueMap(const Rcpp::IntegerMatrix &mat) {
  size_t n = mat.nrow();
  int *rptr = INTEGER(mat);
  KeyValueMap map(rptr, n);

  if (mat.nrow() == 0) {
    throw std::range_error("(CreatePsuKeyValueMap) nrow = 0");
  }
  if (mat.ncol() < 2) {
    throw std::range_error("(CreatePsuKeyValueMap) ncol < 2");
  }

  rptr += n; // Second column

  if (rptr[n-1] <= 0) {
    throw std::range_error(
      std::string("PSUs must have strictly postive size:")
      + std::string(" PSU ") + std::to_string(n-1) + std::string(" is ") + std::to_string(rptr[n-1])
      );
  }


  for (size_t i = 0; i < n; i++) {
    int external_value = rptr[i];

    // PSU must be strictly decreasing in size
    if (i > 0 && external_value >= rptr[i-1]) {
      throw std::range_error(
        std::string("PSUs must be strictly decreasing in size:")
        + std::string(" PSU ") + std::to_string(rptr[i-1-n]) + std::string(" is ") + std::to_string(rptr[i-1])
        + std::string(" PSU ") + std::to_string(rptr[i-n]) + std::string(" is ") + std::to_string(external_value)
        );
    }

    map.values_.push_back((size_t)external_value);
  }

  return map;
}

KeyValueMap CreateNeighboursKeyValueMap(
  const Rcpp::IntegerMatrix &mat,
  const KeyValueMap &psus
) {
  size_t n = psus.Size();

  if ((size_t)mat.nrow() != n) {
    throw std::range_error("(CreateNeighboursKeyValueMap) nrow != psu.size");
  }

  if (mat.ncol() < 3) {
    throw std::range_error("(CreateNeighboursKeyValueMap) ncol < 3");
  }

  int *rptr = INTEGER(mat) + n * 2;
  KeyValueMap map(psus.keys_, n);

  for (size_t i = 0; i < n; i++) {
    int external_value = rptr[i];

    if (external_value <= 1) {
      throw std::range_error("");
    }

    map.values_.push_back((size_t)external_value);
  }

  return map;
}

KeyValueMap CreateTranslatedKeyValueMap(
  const Rcpp::IntegerMatrix &mat,
  const KeyValueMap &translation_map
) {
  size_t n = mat.nrow();
  int *rptr = INTEGER(mat);
  KeyValueMap map(rptr, n);

  if (mat.nrow() == 0) {
    throw std::range_error("(CreateTranslatedKeyValueMap) nrow = 0");
  }
  if (mat.ncol() < 2) {
    throw std::range_error("(CreateTranslatedKeyValueMap) ncol < 2");
  }

  rptr += n; // Second column
  for (size_t i = 0; i < n; i++) {
    int external_value = rptr[i];
    map.values_.push_back(translation_map.GetInternalKey(external_value));
  }

  return map;
}

double Sum(const std::vector<double> &vec) {
  double tot = 0.0;
  for (size_t k = vec.size(); k --> 0;) {
    if (!std::isnan(vec[k])) {
      tot += vec[k];
    }
  }
  return tot;
}

// [[Rcpp::export(.NilsEstimate)]]
Rcpp::List NilsEstimate(
  const Rcpp::IntegerMatrix &r_ordered_psu_size, // PSU, SIZE
  const Rcpp::IntegerMatrix &r_cat_psu, // CAT, PSU
  const Rcpp::IntegerMatrix &r_tracts, // ID, PSU
  const Rcpp::DataFrame &r_plot_data, // TractID, CAT, WEIGHT, VAL
  const double area,
  const double tract_area // 196*100*pi
) {
  // Prepare maps
  KeyValueMap psus = CreatePsuKeyValueMap(r_ordered_psu_size);
  KeyValueMap categories = CreateTranslatedKeyValueMap(r_cat_psu, psus);

  // Fill TractStore with values from plots
  int *tract_arr = INTEGER(r_tracts);
  size_t n_tracts = r_tracts.nrow();
  TractStore tract_store(tract_arr, tract_arr + n_tracts, n_tracts, psus, categories.Size());
  tract_store.Fill(r_plot_data, categories, tract_area);

  // Calcualte estimate and variance estimate
  std::vector<double> estimates = tract_store.CatEstimates(psus, categories, area);
  std::vector<double> covmat = tract_store.Variance(psus, categories, area);
  double estimate = Sum(estimates);
  double variance = Sum(covmat);

  Rcpp::List ret = Rcpp::List::create(
    Rcpp::Named("estimate") = estimate,
    Rcpp::Named("variance") = variance,
    Rcpp::Named("cat_estimates") = Rcpp::wrap(estimates),
    Rcpp::Named("cat_covmat") = Rcpp::NumericMatrix(categories.Size(), categories.Size(), covmat.begin()),
    Rcpp::Named("nonnil_tracts") = tract_store.NonNilTracts(),
    Rcpp::Named("positive_tracts_per_cat") = Rcpp::wrap(tract_store.PositiveTractsPerCat())
  );

  return ret;
}

// [[Rcpp::export(.NilsBalancedEstimate)]]
Rcpp::List NilsBalancedEstimate(
  const Rcpp::IntegerMatrix &r_ordered_psu_size, // PSU, SIZE, NEIGHBOURS
  const Rcpp::IntegerMatrix &r_cat_psu, // CAT, PSU
  const Rcpp::IntegerMatrix &r_tracts, // ID, PSU
  const Rcpp::DataFrame &r_plot_data, // TractID, CAT, WEIGHT, VAL
  const double area,
  const double tract_area, // 196*100*pi
  Rcpp::NumericMatrix &r_xbalance
) {
  // Prepare maps
  KeyValueMap psus = CreatePsuKeyValueMap(r_ordered_psu_size);
  KeyValueMap neighbours = CreateNeighboursKeyValueMap(r_ordered_psu_size, psus);
  KeyValueMap categories = CreateTranslatedKeyValueMap(r_cat_psu, psus);

  // Fill TractStore with values from plots
  int *tract_arr = INTEGER(r_tracts);
  size_t n_tracts = r_tracts.nrow();
  TractStore tract_store(tract_arr, tract_arr + n_tracts, n_tracts, psus, categories.Size());
  tract_store.Fill(r_plot_data, categories, tract_area);

  // Calcualte estimate and variance estimate
  std::vector<double> estimates = tract_store.CatEstimates(psus, categories, area);
  std::vector<double> covmat = tract_store.VarianceBalanced(
    psus,
    categories,
    area,
    REAL(r_xbalance),
    r_xbalance.nrow(),
    neighbours
  );
  double estimate = Sum(estimates);
  double variance = Sum(covmat);

  Rcpp::List ret = Rcpp::List::create(
    Rcpp::Named("estimate") = estimate,
    Rcpp::Named("variance") = variance,
    Rcpp::Named("cat_estimates") = Rcpp::wrap(estimates),
    Rcpp::Named("cat_covmat") = Rcpp::NumericMatrix(categories.Size(), categories.Size(), covmat.begin()),
    Rcpp::Named("nonnil_tracts") = tract_store.NonNilTracts(),
    Rcpp::Named("positive_tracts_per_cat") = Rcpp::wrap(tract_store.PositiveTractsPerCat())
  );

  return ret;
}
