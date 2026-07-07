// Copyright (C) 2026 Wilmer Prentius.
//
// This program is free software: you can redistribute it and/or modify it under the terms of the
// GNU Affero General Public License as published by the Free Software Foundation, version 3.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License along with this
// program. If not, see <https://www.gnu.org/licenses/>.

//! Design-based estimators and variance estimators for National Inventories of Landscapes in
//! Sweden (NILS)

pub mod category;
pub mod psu;
pub mod tract;

use std::fmt::Display;
use std::hash::Hash;
use std::num::{
    NonZeroU32,
    NonZeroUsize,
};
use std::ops::Range;

use category::{
    CatError,
    CategoryValue,
};
use envisim_utils::kd_tree::Tree;
use envisim_utils::kd_tree::searcher::KNearestNeighbourSearcher;
use envisim_utils::matrix::Matrix;
use envisim_utils::sampling_options::SpreadingOptions;
use envisim_utils::utils::PointSet;
use num_traits::ToPrimitive;
use psu::PsuHeader;
use rustc_hash::{
    FxBuildHasher,
    FxHashMap,
};
use thiserror::Error;
use tract::TractError;

use crate::category::{
    CatIdPair,
    CategoryStore,
};
use crate::psu::{
    PsuData,
    PsuError,
    PsuStore,
};
use crate::tract::{
    Area,
    Tract,
    TractHeaderEntry,
    TractStore,
    TractValueEntry,
};

#[non_exhaustive]
#[derive(Error, Debug, Clone)]
pub enum NilsError {
    #[error("no categories have been added")]
    NoCategoriesAdded,
    #[error(transparent)]
    Psu(#[from] PsuError),
    #[error(transparent)]
    Category(#[from] CatError),
    #[error(transparent)]
    Tract(#[from] TractError),
}
/// Shorthand for `Result` with [`NilsError`] error type.
type NilsResult<T> = Result<T, NilsError>;

/// Strategy used in the estimation of the covariance
#[non_exhaustive]
#[must_use]
#[derive(Debug, Copy, Clone)]
pub enum CovarianceStrategy {
    /// Assumes the sample(s) were drawn using SRS.
    SimpleRandomSample,
    /// Uses the nearest neighbour variance estimation.
    NearestNeighbour,
}
/// Contains information of covariance between category id-pairs
#[must_use]
#[derive(Debug, Clone)]
pub struct CovarianceMatrix<CID> {
    /// Store of category-pairs and their respective covariance.
    cov_mat: FxHashMap<CatIdPair<CID>, f64>,
    /// Number of categories. `cov_mat` should be of length `n_cats * (n_cats + 1) / 2`.
    n_cats: NonZeroUsize,
    /// Covariance estimation strategy
    estimation_strategy: CovarianceStrategy,
}
impl<CID> CovarianceMatrix<CID> {
    /// Constructs a new covariance storage from `psu_store`
    /// # Errors
    /// Returns an error if the `psu_store` doesn't contain any categories.
    #[inline]
    fn from_psustore<PID>(psu_store: &PsuStore<PID, CID>) -> NilsResult<Self>
    where
        CID: Copy + Hash + Ord,
    {
        let n_cats =
            NonZeroUsize::new(psu_store.iter().map(PsuData::categories_len).sum::<usize>())
                .ok_or(NilsError::NoCategoriesAdded)?;
        #[expect(clippy::integer_division, reason = "have exact solution")]
        let mut cov_mat = FxHashMap::with_capacity_and_hasher(
            n_cats.get() * (n_cats.get() + 1) / 2,
            FxBuildHasher,
        );
        let mut iter = psu_store.category_iter();
        while let Some((cats_small, _)) = iter.next() {
            for (cats_large, _) in iter.clone() {
                cov_mat.insert((cats_small, cats_large).into(), 0.0);
            }
        }

        Ok(Self {
            cov_mat,
            n_cats,
            estimation_strategy: CovarianceStrategy::SimpleRandomSample,
        })
    }
    /// Returns the estimation strategy
    #[inline]
    pub fn estimation_strategy(&self) -> CovarianceStrategy { self.estimation_strategy }
    /// Sets the estimation strategy
    #[inline]
    fn set_estimation_strategy(&mut self, strategy: CovarianceStrategy) {
        self.estimation_strategy = strategy;
    }
    /// Returns the covariance between a `cat_pair`.
    #[must_use]
    #[inline]
    pub fn get<PAIR>(&self, cat_pair: PAIR) -> Option<f64>
    where
        CID: Hash + Eq,
        PAIR: Into<CatIdPair<CID>>,
    {
        self.cov_mat.get(&cat_pair.into()).copied()
    }
    /// Returns a mutable reference to the covariance between a `cat_pair`.
    #[must_use]
    #[inline]
    pub fn get_mut<PAIR>(&mut self, cat_pair: PAIR) -> Option<&mut f64>
    where
        CID: Hash + Eq,
        PAIR: Into<CatIdPair<CID>>,
    {
        self.cov_mat.get_mut(&cat_pair.into())
    }
    /// Returns the variance, i.e. the sum of the covariance matrix
    #[must_use]
    #[inline]
    pub fn variance(&self) -> f64 { self.cov_mat.values().sum() }
    /// Returns a tuple containing the covariance matrix, and the categories in their order of
    /// appearance in the matrix.
    #[expect(clippy::integer_division, reason = "have exact solution")]
    #[expect(clippy::missing_panics_doc, reason = "panic implies bug")]
    #[inline]
    pub fn to_matrix(&self) -> (Matrix<f64>, Vec<CID>)
    where
        CID: Copy + Hash + Ord,
    {
        let mut cats: Vec<CID> = self.cov_mat.keys().map(CatIdPair::get_a).copied().collect();
        cats.sort();
        cats.dedup();
        assert_eq!(
            cats.len(),
            self.n_cats.get() * (self.n_cats.get() + 1) / 2,
            "store does not contain correct amount of categories"
        );

        let mut cm = Matrix::from_value(0.0, (self.n_cats, self.n_cats));

        for i in 0..self.n_cats.get() {
            cm[(i, i)] = self.get((cats[i], cats[i])).expect("cat to exist");

            for j in 1..i {
                cm[(i, j)] = self.get((cats[i], cats[j])).expect("cat to exist");
                cm[(j, i)] = cm[(i, j)];
            }
        }

        (cm, cats)
    }
}
impl<CID> From<CovarianceMatrix<CID>> for Matrix<f64>
where
    CID: Copy + Hash + Ord,
{
    #[inline]
    fn from(value: CovarianceMatrix<CID>) -> Matrix<f64> { value.to_matrix().0 }
}

/// The return value of an estimate.
#[must_use]
#[derive(Debug, Clone)]
pub struct EstimatePerCategory<CID> {
    /// Estimate per category.
    pub estimate_per_category: CategoryStore<CID, f64>,
    /// Number of tracts per category that have positive values.
    pub positive_tracts_per_category: CategoryStore<CID, u64>,
    /// Number of tracts that have positive values in any category.
    pub positive_tracts: u64,
}
impl<CID> EstimatePerCategory<CID> {
    /// Returns the estimate
    #[must_use]
    #[inline]
    pub fn estimate(&self) -> f64 {
        self.estimate_per_category
            .iter()
            .map(CategoryValue::get)
            .sum()
    }
}

/// A store of Nils PSUs and tracts
#[must_use]
#[derive(Debug, Clone)]
pub struct Nils<PID, CID, TID> {
    /// PSUs
    psus: PsuStore<PID, CID>,
    /// Tracts
    tracts: TractStore<TID, PID, CID>,
}
impl<PID, CID, TID> Nils<PID, CID, TID> {
    /// Initialise with psus and sizes. `capacity` determines the number of tracts to reserve space
    /// for.
    /// # Errors
    /// Returns an error if the iterators does not match in size.
    #[inline]
    pub fn new<I, J>(psus: I, nn_sizes: Option<J>, capacity: usize) -> NilsResult<Self>
    where
        PID: Ord,
        I: ExactSizeIterator<Item = PsuHeader<PID>>,
        J: ExactSizeIterator<Item = NonZeroU32>,
    {
        let psus = PsuStore::new(psus, nn_sizes)?;
        let tracts = TractStore::with_capactity(capacity);
        Ok(Self { psus, tracts })
    }
    /// Add a category to a psu
    /// # Errors
    /// Returns an error if any category duplicates are found.
    #[inline]
    pub fn add_category_to_psu(&mut self, psu_id: &PID, category: CID) -> NilsResult<()>
    where
        PID: Display + Eq,
        CID: Display + Ord,
    {
        self.psus.insert_category(psu_id, category)?;
        Ok(())
    }
    /// Initialise categories-psu maps
    /// # Errors
    /// Returns an error if any category duplicates are found.
    #[inline]
    pub fn add_categories_to_psu<I>(&mut self, psu_id: &PID, categories: I) -> NilsResult<()>
    where
        PID: Display + Eq,
        CID: Display + Ord,
        I: ExactSizeIterator<Item = CID>,
    {
        for cat_id in categories {
            self.psus.insert_category(psu_id, cat_id)?;
        }
        Ok(())
    }
    /// Initialise tract-psu maps
    /// # Errors
    /// Returns an error if the `tract_area` cannot be constructed, if a PSU ID is not found, or if a
    /// tract ID duplicate is found.
    #[inline]
    pub fn add_tracts<I, A>(&mut self, tracts: I, tract_area: A) -> NilsResult<()>
    where
        PID: Display + Eq,
        TID: Copy + Display + Eq + Hash,
        I: Iterator<Item = TractHeaderEntry<TID, PID>>,
        A: TryInto<Area>,
        A::Error: Into<TractError>,
    {
        let area = tract_area.try_into().map_err(Into::into)?;
        for tract in tracts {
            // Assert that the psu is valid
            if self.psus.get_psu(tract.psu_id()).is_none() {
                return Err(PsuError::PsuIdNotFound(tract.psu_id().to_string()).into());
            }

            // Insert and replace
            self.tracts.insert(Tract::new(tract, area))?;
        }
        Ok(())
    }
    /// Add tract value entry. Does not check if category exists.
    /// # Errors
    /// Returns an error if a tract ID cannot be found.
    #[inline]
    pub fn add_value_entry(&mut self, tract_entry: TractValueEntry<TID, CID>) -> NilsResult<()>
    where
        CID: Copy + Ord,
        TID: Copy + Display + Eq + Hash,
    {
        self.tracts.add_value_from_entry(tract_entry)?;
        Ok(())
    }
    /// Add tract value entries. Does not check if category exists.
    /// # Errors
    /// Returns an error if a tract ID cannot be found.
    #[inline]
    pub fn add_value_entries<I>(&mut self, tract_entries: I) -> NilsResult<()>
    where
        CID: Copy + Ord,
        TID: Copy + Display + Eq + Hash,
        I: IntoIterator<Item = TractValueEntry<TID, CID>>,
    {
        for tract in tract_entries {
            self.tracts.add_value_from_entry(tract)?;
        }

        Ok(())
    }
    /// Estimates totals per category.
    /// Returns a tuple:
    /// 1. Totals per category.
    /// 2. Number of tracts with positive values per category.
    /// 3. Number of tracts with positive values in a category.
    #[expect(clippy::missing_panics_doc, reason = "panic implies bug")]
    #[inline]
    pub fn estimate_per_category(&self, frame_area: Area) -> EstimatePerCategory<CID>
    where
        CID: Copy + Ord,
    {
        let mut epc = EstimatePerCategory {
            estimate_per_category: self
                .psus
                .category_iter()
                .map(|(cat_id, _)| cat_id)
                .copied()
                .collect(),
            positive_tracts_per_category: self
                .psus
                .category_iter()
                .map(|(cat_id, _)| cat_id)
                .copied()
                .collect(),
            positive_tracts: 0,
        };

        for (_, tract) in self.tracts.iter() {
            let mut pos_tract = false;
            tract.totals().iter().for_each(|&ct| {
                if 0.0 < *ct.get() {
                    epc.estimate_per_category.add_value(ct);
                    epc.positive_tracts_per_category
                        .add_value((*ct.cat_id(), 1));
                    pos_tract = true;
                }
            });
            epc.positive_tracts += u64::from(pos_tract);
        }

        for psu in self.psus.iter() {
            let size = f64::from(psu.size().get());
            let area_frac = frame_area.get() / size;

            for cat_id in psu.categories_iter() {
                *epc.estimate_per_category
                    .get_mut(cat_id)
                    .expect("cat id to exist") *= area_frac;
            }
        }

        epc
    }
    /// Sorts the `tract_ids` and constructs tract ranges over the psus
    #[must_use]
    #[inline]
    fn tract_ranges(&self, tract_ids: &mut [TID]) -> Vec<(PID, Range<usize>)>
    where
        PID: Copy + Eq,
        TID: Eq + Hash,
    {
        // Sort tract ids by psu size
        tract_ids.sort_unstable_by(|a, b| {
            let idx_a = self
                .psus
                .order_of_psu(self.tracts.get(a).expect("tract id to exist").psu_id())
                .expect("psu id to exist");
            let idx_b = self
                .psus
                .order_of_psu(self.tracts.get(b).expect("tract id to exist").psu_id())
                .expect("psu id to exist");
            idx_a.cmp(&idx_b)
        });

        // Create a loop order for the psus, going from smallest to largest psu. For each psu,
        // attach the range of tracts that is contained within the psu
        let mut ranges = Vec::with_capacity(self.psus.len());
        let mut rest = 0;

        for psu in self.psus.iter() {
            let psu_id = psu.psu_id();

            if rest < tract_ids.len()
                && self
                    .tracts
                    .get(&tract_ids[rest])
                    .expect("tract id to exist")
                    .psu_id()
                    == psu_id
            {
                let end = rest
                    + tract_ids[rest..].partition_point(|tid| {
                        self.tracts.get(tid).expect("tract id to exist").psu_id() == psu_id
                    });
                ranges.push((*psu_id, rest..end));
                rest = end;
            } else {
                ranges.push((*psu_id, rest..rest));
            }
        }

        ranges
    }
    /// Returns an estimated covariance matrix.
    /// # Errors
    /// Returns an error if the psus doesn't contain any categories, or if the `frame_area` is
    /// invalid.
    #[expect(clippy::missing_panics_doc, reason = "panic implies bug")]
    #[inline]
    pub fn covariance_estimate<A>(&self, frame_area: A) -> NilsResult<CovarianceMatrix<CID>>
    where
        PID: Copy + Eq,
        CID: Copy + Hash + Ord,
        TID: Copy + Eq + Display + Hash,
        A: TryInto<Area>,
        A::Error: Into<TractError>,
    {
        let frame_area = frame_area.try_into().map_err(Into::into)?;
        // Prepare covariance matrix structure
        let mut cov_mat = CovarianceMatrix::from_psustore(&self.psus)?;
        cov_mat.set_estimation_strategy(CovarianceStrategy::SimpleRandomSample);
        // Prepare category sum container
        let mut sums: CategoryStore<CID, f64> = self
            .psus
            .category_iter()
            .map(|(cat_id, _)| *cat_id)
            .collect();
        // Get vector of tract ids
        let mut tract_ids: Vec<TID> = self.tracts.iter().map(|(tid, _)| *tid).collect();
        let tract_ranges = self.tract_ranges(&mut tract_ids);

        // loop from smallest to largest psu
        // We should calculate the covariance of category A and B by including the tracts that
        // are part of A and B (intersect). Hence, if A < B, then tracts in A should be counted.
        // We can do this by having an inner loop from the smallest category, where we identify
        // the tracts of this cat, and then comparing to all larger categories
        for (psu_id, tract_range) in tract_ranges {
            // Successively add to the totals.
            for tract_id in &tract_ids[tract_range.clone()] {
                let tract = self.tracts.get(tract_id).expect("tract id to exist");
                tract.totals().iter().for_each(|&ct| {
                    sums.add_value(ct);
                });
            }

            let psu = self.psus.get_psu(&psu_id).expect("psu id to exist");
            let psu_size = psu.size().get().to_f64().expect("u32 -> f64");
            let area_frac = frame_area.get() / psu_size;

            // Outer loop of cats from current psu
            for curr_cat in psu.categories_iter() {
                // Values are non-negative -- a zero sum implies that all tracts are 0.0
                let curr_mean = match sums.get(curr_cat).copied() {
                    Some(sum) if sum > 0.0 => sum / psu_size,
                    _ => continue,
                };

                // Inner loop of cats from larger psus
                for large_psu in self.psus.superset_psu(&psu_id).expect("psu to exist") {
                    let large_psu_size = f64::from(large_psu.size().get());
                    let large_area_frac = frame_area.get() / large_psu_size;

                    for large_cat in large_psu.categories_iter() {
                        // We're looking for mean in intersect set, hence psu_size is the mean size
                        let large_mean = match sums.get(large_cat).copied() {
                            Some(sum) if sum > 0.0 => sum / psu_size,
                            _ => continue,
                        };

                        // Reference to correct cov mat entry
                        let cov_mat_entry = cov_mat
                            .get_mut((curr_cat, large_cat))
                            .expect("cats to exist");

                        for tract_id in &tract_ids[..tract_range.end] {
                            let curr_deviance = self
                                .tracts
                                .get_category_value(tract_id, curr_cat)
                                .unwrap_or(0.0)
                                - curr_mean;
                            let large_deviance = self
                                .tracts
                                .get_category_value(tract_id, large_cat)
                                .unwrap_or(0.0)
                                - large_mean;

                            *cov_mat_entry += curr_deviance * large_deviance;
                        }

                        *cov_mat_entry *=
                            area_frac * large_area_frac * (psu_size / (psu_size - 1.0));
                    }
                }
            }
        }

        Ok(cov_mat)
    }
    /// Returns an estimated covariance matrix using the nearest neighbour variance estimator.
    ///
    /// Spreading matrix assumed to be in same order as insertion order of tracts
    /// # Errors
    /// Returns an error if the psus doesn't contain any categories, or if the `frame_area` is
    /// invalid.
    /// # Panics
    /// Panics if `u32` cannot be converted into `usize`.
    #[inline]
    pub fn covariance_estimate_nn<A, P>(
        &self,
        frame_area: A,
        spreading: &SpreadingOptions<P>,
    ) -> NilsResult<CovarianceMatrix<CID>>
    where
        PID: Copy + Eq,
        CID: Copy + Hash + Ord,
        TID: Copy + Eq + Display + Hash,
        A: TryInto<Area>,
        A::Error: Into<TractError>,
        P: PointSet<Id = TID>,
    {
        let frame_area = frame_area.try_into().map_err(Into::into)?;
        // Prepare covariance matrix structure
        let mut cov_mat = CovarianceMatrix::from_psustore(&self.psus)?;
        cov_mat.set_estimation_strategy(CovarianceStrategy::NearestNeighbour);
        // Get vector of tract ids,
        let mut tract_ids: Vec<TID> = self.tracts.iter().map(|(&tid, _)| tid).collect();
        // Create tree
        let mut tree = Tree::new(spreading, &mut tract_ids);
        let mut searcher = KNearestNeighbourSearcher::new(
            (*self.psus.get_max_psu().expect("there to be psus").nn_size())
                .try_into()
                .expect("u32 -> usize"),
            tree.data(),
        );
        let tract_ranges = self.tract_ranges(&mut tract_ids);

        // Remove all tract_ids from the tree (except the first)
        // We will add tract_ids to the tree again, as we loop through the psus
        let (first_psu, first_range) = tract_ranges[0].clone();
        for tract_id in tract_ids.iter().skip(first_range.end) {
            tree.remove_unit(*tract_id);
        }

        // loop from smallest to largest psu
        // We should calculate the covariance of category A and B by including the tracts that
        // are part of A and B (intersect). Hence, if A < B, then tracts in A should be counted.
        // We can do this by having an inner loop from the smallest category, where we identify
        // the tracts of this cat, and then comparing to all larger categories
        for (psu_id, tract_range) in tract_ranges {
            let range_end = tract_range.end;
            // Anytime we are not in the first psu, we need to re-add the units to the tree
            if psu_id != first_psu {
                for tract_id in &tract_ids[tract_range] {
                    tree.insert_unit(*tract_id);
                }
            }

            // Smaller psu info
            let psu = self.psus.get_psu(&psu_id).expect("psu id to exist");
            let psu_size = psu.size().get().to_f64().expect("u32 -> f64");
            let psu_nn = psu.nn_size().get().to_f64().expect("u32 -> f64");
            let area_frac = frame_area.get() / psu_size;

            // In normal variance est, we loop through psus->cats->tracts.
            // This we can do since fetching tracts is not too expensive, so looping
            // cats->tracts vs tracts->cats doesnt really matter.
            // Here, tracts is expensive, since we need to find neighbours.
            // Thus, instead of psu_outer->cat_outer->psu_inner->cat_inner->tracts, we go
            // psu_outer->tracts->cat_outer->psu_inner->cat_inner.

            // Set searcher size
            searcher.set_nominal_size((*psu.nn_size()).try_into().expect("u32 -> usize"));

            for tract_id in &tract_ids[..range_end] {
                searcher
                    .reset_from_point(tree.data().coords(*tract_id))
                    .expect("size to match")
                    .search(&tree)
                    .expect("neighbours to be found");

                for curr_cat in psu.categories_iter() {
                    let curr_mean = searcher
                        .neighbours()
                        .iter()
                        .map(|n| {
                            self.tracts
                                .get_category_value(&n.id(), curr_cat)
                                .expect("curr_cat to exist in tract")
                        })
                        .sum::<f64>()
                        / psu_nn;
                    let curr_deviance = self
                        .tracts
                        .get_category_value(tract_id, curr_cat)
                        .expect("curr_cat to exist in tract_id")
                        - curr_mean;

                    for large_psu in self.psus.superset_psu(&psu_id).expect("psu to exist") {
                        for large_cat in large_psu.categories_iter() {
                            let large_mean = searcher
                                .neighbours()
                                .iter()
                                .map(|n| {
                                    self.tracts
                                        .get_category_value(&n.id(), large_cat)
                                        .expect("large_cat to exist in tract")
                                })
                                .sum::<f64>()
                                / psu_nn;
                            let large_deviance = self
                                .tracts
                                .get_category_value(tract_id, large_cat)
                                .expect("large_cat to exist in tract_id")
                                - large_mean;

                            // Reference to correct cov mat entry
                            *cov_mat
                                .get_mut((curr_cat, large_cat))
                                .expect("cats to exist") += curr_deviance * large_deviance;
                        }
                    }
                }
            }

            for curr_cat in psu.categories_iter() {
                // Smaller psu info
                for large_psu in self.psus.superset_psu(&psu_id).expect("psu to exist") {
                    // Larger psu info
                    let large_psu_size = f64::from(large_psu.size().get());
                    let large_area_frac = frame_area.get() / large_psu_size;
                    for large_cat in large_psu.categories_iter() {
                        *cov_mat
                            .get_mut((curr_cat, large_cat))
                            .expect("cats to exist") *=
                            area_frac * large_area_frac * (psu_nn / (psu_nn - 1.0));
                    }
                }
            }
        }

        Ok(cov_mat)
    }
}
