#include <algorithm>
#include <limits>
#include <vector>
#include <stddef.h>
#include <stdexcept>

#include <Rcpp.h>

#include "KDStoreClass.h"
#include "KDTreeClass.h"
#include "KeyValueMap.h"
#include "TractStore.h"

Tract::Tract(const size_t n_cats, const int id, const size_t psu) {
  values_ = std::vector<double>(n_cats, 0.0);
  external_id_ = id;
  internal_psu_ = psu;
  return;
}

void Tract::Add(const size_t cat, const double value) {
  values_[cat] += value;

  if (!recorded_) {
    recorded_ = true;
  }

  if (!nonnil_ && value != 0.0) {
    nonnil_ = true;
  }
}

double Tract::Get(const size_t cat) const {
  return values_[cat];
}

double Tract::Sum() const {
  if (!nonnil_) {
    return 0.0;
  }

  double sum = 0.0;
  for (size_t i = values_.size(); i --> 0; ) {
    sum += values_[i];
  }
  return sum;
}

size_t Tract::GetInternalPsu() const {
  return internal_psu_;
}

/*
 * Create a TractMap from an array of indices
 */
TractStore::TractStore(
  const int *tract_ids,
  const int *tract_external_psus,
  const size_t n_tracts,
  const KeyValueMap &psus,
  const size_t n_cats
) {
  n_cats_ = n_cats;

  if (n_tracts != psus.GetValue(0)) {
    throw std::range_error("(TractStore::TractStore) n_tracts != largest psu");
  }

  tract_map_.reserve(n_tracts);
  internal_id_map_.reserve(n_tracts);

  for (size_t i = 0; i < n_tracts; i++) {
    TractInternalId::const_iterator it = internal_id_map_.find(tract_ids[i]);
    if (it != internal_id_map_.end()) {
      throw std::range_error("(TractStore::TractStore) duplicate tract_id provided");
    }

    int external_id = tract_ids[i];
    int external_psu = tract_external_psus[i];

    tract_map_.push_back(Tract(n_cats, external_id, psus.GetInternalKey(external_psu)));
    internal_id_map_.emplace(external_id, i);
  }

  return;
}

Tract* TractStore::FindInternal(const size_t internal_id) {
  return &tract_map_[internal_id];
}

Tract* TractStore::FindExternal(const int external_id) {
  TractInternalId::const_iterator it = internal_id_map_.find(external_id);

  if (it == internal_id_map_.end()) {
    return nullptr;
  }

  return FindInternal(it->second);
}

size_t TractStore::Size() const {
  return tract_map_.size();
}

/*
 * Fill the TractMap with values from a data frame w/ a certain design:
 * Columns (in order): tract id, category, weight, value
 */
void TractStore::Fill(
  const Rcpp::DataFrame &data,
  const KeyValueMap &categories,
  const double tract_area
) {
  Rcpp::IntegerVector dt_tract_ids = data[0];
  Rcpp::IntegerVector dt_cats = data[1];
  Rcpp::NumericVector dt_weights = data[2];
  Rcpp::NumericVector dt_values = data[3];

  size_t n_dt = data.nrows();

  // TractMap::iterator it;

  for (size_t i = 0; i < n_dt; i++) {
    int id = dt_tract_ids[i];
    int external_cat = dt_cats[i];
    double weight = dt_weights[i];
    double value = dt_values[i];

    if (weight == 0.0 || value == 0.0) {
      continue;
    }

    Tract *tract = FindExternal(id);

    if (tract == nullptr) {
      // ERROR -- User input error if tract IDs doesnt exist
      // Might be OK to input larger data set than needed.
      Rcpp::warning(
        std::string("Tract of plot ") + std::to_string(i+1)
        + std::string(" (") + std::to_string(id)
        + std::string(") does not exist; plot is ignored")
        );
      continue;
    }

    size_t internal_cat = categories.GetInternalKey(external_cat);
    size_t plot_psu = categories.GetValue(internal_cat);

    if (tract->GetInternalPsu() < plot_psu) {
      Rcpp::warning(
        std::string("Category of plot ") + std::to_string(i+1)
        + std::string(" does not match PSU of tract ") + std::to_string(id)
        + std::string("; plot is ignored")
        );
      continue;
    }

    tract->Add(internal_cat, weight * value / tract_area);
  }

  return;

}

int TractStore::NonNilTracts() {
  int ntracts = 0;

  for (size_t i = tract_map_.size(); i --> 0;) {
    if (tract_map_[i].nonnil_) {
      ntracts += 1;
    }
  }

  return ntracts;
}

std::vector<int> TractStore::PositiveTractsPerCat() {
  std::vector<int> ntracts(n_cats_, 0);

  for (size_t i = tract_map_.size(); i --> 0;) {
    for (size_t j = 0; j < n_cats_; j++) {
      if (tract_map_[i].Get(j) > 0.0) {
        ntracts[j] += 1;
      }
    }
  }

  return ntracts;
}

/*
 * Sum up the tracts, i.e. return the sum of the tract means per area unit
 */
std::vector<double> TractStore::CatEstimates(
  const KeyValueMap &psus,
  const KeyValueMap &categories,
  const double area
) {
  std::vector<double> sums(n_cats_, 0.0);

  for (size_t i = tract_map_.size(); i --> 0;) {
    Tract *tract = &tract_map_[i];
    if (!tract->nonnil_) {
      continue;
    }

    for (size_t k = 0; k < n_cats_; k++) {
      sums[k] += tract->Get(k);
    }
  }

  for (size_t k = 0; k < n_cats_; k++) {
    size_t psu_n = psus.GetValue(categories.GetValue(k));
    if (psu_n > 0) {
      sums[k] *= area / (double)psu_n;
    } else {
      sums[k] = std::numeric_limits<double>::quiet_NaN();
    }
  }

  return sums;
}

/*
 * Calculate covariances for the categories. Returns an upper triangular
 * covariance matrix.
 */
std::vector<double> TractStore::Variance(
  const KeyValueMap &psus,
  const KeyValueMap &categories,
  const double area
) {
  std::vector<double> sums(n_cats_, 0.0);
  std::vector<bool> all_nils(n_cats_, true);
  std::vector<double> covs(n_cats_ * n_cats_, 0.0);

  std::vector<size_t> sorted_cats; sorted_cats.reserve(n_cats_);
  std::vector<size_t> ids; ids.reserve(Size());
  for (size_t cat = 0; cat < n_cats_; cat++) sorted_cats.push_back(cat);

  std::sort(
    sorted_cats.begin(),
    sorted_cats.end(),
    [&categories](size_t a, size_t b) { return categories.GetValue(a) > categories.GetValue(b); }
    );
  size_t first_cat = 0;

  // We will loop through all psu's, and create a list of tracts belonging to
  // each psu. If the loop goes from smallest to largest, we only need to append
  // units to the list as we increase the psu.
  //
  // For each psu, we will check all pairs of categories to see if any pair
  // matches the condition of (l, m) being a pair where either l is part of the
  // current psu, and m is part of a larger psu, or vice verse.
  // (However, the order is reversed; a small psu-value indicates a larger sample)
  //
  // If a pair matches this criterion, we will calculate the covariance.

  // Go smallest -> largest psu
  for (size_t psu = psus.Size(); psu --> 0;) {
    // Prepare ids
    for (size_t i = Size(); i --> 0;) {
      Tract *tract = FindInternal(i);
      // Any other tract w/ smaller psu have already been added
      if (tract->GetInternalPsu() != psu) {
        continue;
      }

      ids.push_back(i);

      if (!tract->nonnil_) {
        continue;
      }

      for (size_t cat = 0; cat < n_cats_; cat++) {
        if (tract->Get(cat) == 0.0) {
          continue;
        }

        sums[cat] += tract->Get(cat);

        if (all_nils[cat]) {
          all_nils[cat] = false;
        }
      }
    }

    double psu_size = (double)psus.GetValue(psu);

    if (psu_size <= 1.0) {
      for (size_t cat_ki = first_cat; cat_ki < n_cats_; cat_ki++) {
        size_t cat_k = sorted_cats[cat_ki];

        if (categories.GetValue(cat_k) < psu) {
          first_cat = cat_ki;
          break;
        }

        covs[cat_k * (n_cats_ + 1)] = std::numeric_limits<double>::quiet_NaN();

        for (size_t cat_li = cat_ki + 1; cat_li < n_cats_; cat_li++) {
          size_t cat_l = sorted_cats[cat_li];
          covs[cat_k * n_cats_ + cat_l] = std::numeric_limits<double>::quiet_NaN();
          covs[cat_l * n_cats_ + cat_k] = std::numeric_limits<double>::quiet_NaN();
        }
      }

      continue;
    }

    // Each pair of categories from at least this, or larger, PSU, should be
    // calcualted
    // Outer loop of smaller (current PSU)
    // Inner loop of larger
    for (size_t cat_ki = first_cat; cat_ki < n_cats_; cat_ki++) {
      size_t cat_k = sorted_cats[cat_ki];

      // We don't need to go further than this
      if (categories.GetValue(cat_k) < psu) {
        first_cat = cat_ki;
        break;
      }

      // If all current units are 0, the covariance of any category l is 0
      if (all_nils[cat_k]) {
        continue;
      }

      double mean_k = sums[cat_k] / psu_size;

      for (size_t cat_li = cat_ki; cat_li < n_cats_; cat_li++) {
        size_t cat_l = sorted_cats[cat_li];

        // If all current units are 0, the covariance of any category l is 0
        if (all_nils[cat_l]) {
          continue;
        }

        // Upper triangular id
        size_t covs_index = cat_k * n_cats_ + cat_l;
        double mean_l = sums[cat_l] / psu_size;

        for (size_t i = ids.size(); i --> 0;) {
          Tract *tract = FindInternal(ids[i]);
          covs[covs_index] += (tract->Get(cat_k) - mean_k)
            * (tract->Get(cat_l) - mean_l);
        }

        size_t psu_larger = categories.GetValue(cat_l);
        double psu_size_larger = (double)psus.GetValue(psu_larger);
        covs[covs_index] *= (area / psu_size) * (area / psu_size_larger)
          * (psu_size / (psu_size - 1.0));

        if (cat_k != cat_l) {
          covs[cat_l * n_cats_ + cat_k] = covs[covs_index];
        }

      }
    }
  }

  return covs;
}

std::vector<double> TractStore::VarianceBalanced(
  const KeyValueMap &psus,
  const KeyValueMap &categories,
  const double area,
  double *xbalance,
  const size_t p_xbalance,
  const KeyValueMap &neighbours
) {
  std::vector<double> means(n_cats_, 0.0);
  std::vector<bool> all_nils(n_cats_, true);
  std::vector<double> covs(n_cats_ * n_cats_, 0.0);

  std::vector<size_t> sorted_cats; sorted_cats.reserve(n_cats_);
  std::vector<size_t> ids; ids.reserve(Size());
  for (size_t cat = 0; cat < n_cats_; cat++) sorted_cats.push_back(cat);

  std::sort(
    sorted_cats.begin(),
    sorted_cats.end(),
    [&categories](size_t a, size_t b) { return categories.GetValue(a) > categories.GetValue(b); }
    );
  size_t first_cat = 0;
  size_t last_cat = 0;

  KDStore *store = new KDStore(Size(), neighbours.GetValue(0));

  // Go smallest -> largest psu
  for (size_t psu = psus.Size(); psu --> 0;) {
    if (psus.Size() > 0) {
      // Prepare ids
      for (size_t i = Size(); i --> 0;) {
        Tract *tract = FindInternal(i);
        // Any other tract w/ smaller psu have already been added
        if (tract->GetInternalPsu() != psu) {
          continue;
        }

        ids.push_back(i);

        if (!tract->nonnil_) {
          continue;
        }

        for (size_t cat = 0; cat < n_cats_; cat++) {
          if (all_nils[cat] && tract->Get(cat) != 0.0) {
            all_nils[cat] = false;
          }
        }
      }
    }

    double psu_size = (double)psus.GetValue(psu);

    first_cat = last_cat;
    for (; last_cat < n_cats_; last_cat++) {
        size_t cat = sorted_cats[last_cat];
        if (categories.GetValue(cat) < psu) {
          break;
        }
    }

    if (psu_size <= 1.0) {
      for (size_t cat_ki = first_cat; cat_ki < last_cat; cat_ki++) {
        size_t cat_k = sorted_cats[cat_ki];

        covs[cat_k * (n_cats_ + 1)] = std::numeric_limits<double>::quiet_NaN();

        for (size_t cat_li = cat_ki + 1; cat_li < n_cats_; cat_li++) {
          size_t cat_l = sorted_cats[cat_li];
          covs[cat_k * n_cats_ + cat_l] = std::numeric_limits<double>::quiet_NaN();
          covs[cat_l * n_cats_ + cat_k] = std::numeric_limits<double>::quiet_NaN();
        }
      }

      continue;
    }

    // Prepare trees and stores
    store->maxSize = neighbours.GetValue(psu);
    double neighbour_size_dbl = (double)neighbours.GetValue(psu);
    KDTree *tree = new KDTree(
      xbalance,
      Size(),
      p_xbalance,
      (size_t)30,
      KDTreeSplitMethod::midpointSlide,
      ids.data(),
      ids.size()
      );


    for (size_t i = ids.size(); i --> 0;) {
      size_t internal_id = ids[i];
      double *tract_balancing_data = xbalance + internal_id * p_xbalance;
      std::fill(means.begin(), means.end(), 0.0);

      tree->FindNeighbours(store, tract_balancing_data);
      Tract *tract;

      // Not accounting for equals
      for (size_t j = store->GetSize(); j --> 0;) {
        tract = FindInternal(store->neighbours[j]);
        for (size_t cat_i = first_cat; cat_i < n_cats_; cat_i++) {
          size_t cat = sorted_cats[cat_i];
          means[cat] += tract->Get(cat);
        }
      }

      double mean_size = (double)store->GetSize();
      for (size_t cat_i = first_cat; cat_i < n_cats_; cat_i++) {
        size_t cat = sorted_cats[cat_i];
        means[cat] /= mean_size;
      }

      // Set self tract
      tract = FindInternal(internal_id);

      for (size_t cat_ki = first_cat; cat_ki < last_cat; cat_ki++) {
        size_t cat_k = sorted_cats[cat_ki];

        // If all current units are 0, the covariance of any category l is 0
        if (all_nils[cat_k]) {
          continue;
        }

        for (size_t cat_li = cat_ki; cat_li < n_cats_; cat_li++) {
          size_t cat_l = sorted_cats[cat_li];

          // If all current units are 0, the covariance of any category l is 0
          if (all_nils[cat_l]) {
            continue;
          }

          // Upper triangular id
          size_t covs_index = cat_k * n_cats_ + cat_l;
          covs[covs_index] +=
            (tract->Get(cat_k) - means[cat_k]) *
            (tract->Get(cat_l) - means[cat_l]);
        }
      }
    }


    for (size_t cat_ki = first_cat; cat_ki < last_cat; cat_ki++) {
      size_t cat_k = sorted_cats[cat_ki];
      for (size_t cat_li = cat_ki; cat_li < n_cats_; cat_li++) {
        size_t cat_l = sorted_cats[cat_li];
        size_t psu_larger = categories.GetValue(cat_l);
        double psu_size_larger = (double)psus.GetValue(psu_larger);
        // Upper triangular id
        size_t covs_index = cat_k * n_cats_ + cat_l;
        covs[covs_index] *=
          (area / psu_size) *
          (area / psu_size_larger) *
          (neighbour_size_dbl / (neighbour_size_dbl - 1.0));

        if (cat_l != cat_k) {
          covs[cat_l * n_cats_ + cat_k] = covs[covs_index];
        }
      }
    }

    delete tree;
  }

  delete store;
  return covs;
}
