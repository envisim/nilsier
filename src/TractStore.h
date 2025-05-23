#ifndef TRACTSTORE_HEADER
#define TRACTSTORE_HEADER

#include <stddef.h>
#include <unordered_map>
#include <vector>

#include <Rcpp.h>

#include "KeyValueMap.h"

class Tract {
public:
  std::vector<double> values_;
  int external_id_;
  size_t internal_psu_;
  bool recorded_ = false;
  bool nonnil_ = false;

  Tract(const size_t, const int, const size_t);
  void Add(const size_t, const double);
  double Get(const size_t) const;
  double Sum() const;

  size_t GetInternalPsu() const;
};

using TractInternalId = std::unordered_map<int, size_t>;

class TractStore {
public:
  std::vector<Tract> tract_map_;
  TractInternalId internal_id_map_; // Maps external -> internal ids
  size_t n_cats_;

  TractStore(const int*, const int*, const size_t, const KeyValueMap&, const size_t);

  Tract* FindInternal(const size_t);
  Tract* FindExternal(const int);
  size_t Size() const;

  void Fill(const Rcpp::DataFrame&, const KeyValueMap&, const double);

  int NonNilTracts();
  std::vector<int> PositiveTractsPerCat();
  std::vector<double> CatEstimates(const KeyValueMap&, const KeyValueMap&, const double);

  std::vector<double> Variance(const KeyValueMap&, const KeyValueMap&, const double);

  std::vector<double> VarianceBalanced(
    const KeyValueMap&,
    const KeyValueMap&,
    const double,
    double*,
    const size_t,
    const KeyValueMap&
  );
};

#endif

