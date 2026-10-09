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
//! Sweden (NILS).

pub mod category;
pub mod psu;
pub mod tract;
mod utils;

use std::fmt::Display;
use std::hash::Hash;
use std::iter::once;
use std::num::NonZeroUsize;
use std::ops::{
    Index,
    IndexMut,
    Range,
};

use envisim_utils::kd_tree::Tree;
use envisim_utils::kd_tree::searcher::{
    KNearestNeighbourSearcher,
    Neighbour,
    NeighbourView,
};
use envisim_utils::matrix::Matrix;
use envisim_utils::sampling_options::SpreadingOptions;
use envisim_utils::utils::PointSet;
use num_traits::ToPrimitive;
use rustc_hash::{
    FxBuildHasher,
    FxHashMap,
};
pub use utils::{
    Area,
    NilsError,
};

use crate::category::{
    CatIdPair,
    CategoryStore,
};
use crate::psu::{
    PsuData,
    PsuStore,
};
use crate::tract::{
    Tract,
    TractStore,
};

/// Strategy used in the estimation of the covariance.
#[non_exhaustive]
#[must_use]
#[derive(Debug, Copy, Clone)]
pub enum CovarianceStrategy {
    /// Assumes the sample(s) were drawn using SRS.
    SimpleRandomSample,
    /// Uses the nearest neighbours variance estimation.
    NearestNeighbours,
}
/// Contains information of covariance between category id-pairs.
#[must_use]
#[derive(Debug, Clone)]
pub struct CovarianceMatrix<CID> {
    /// Store of category-pairs and their respective covariance.
    cov_mat: FxHashMap<CatIdPair<CID>, f64>,
    /// Number of categories. `cov_mat` should be of length `n_cats * (n_cats + 1) / 2`.
    n_cats: NonZeroUsize,
    /// Covariance estimation strategy.
    estimation_strategy: CovarianceStrategy,
}
impl<CID> CovarianceMatrix<CID> {
    /// Constructs a new covariance storage from `psu_store`.
    #[inline]
    fn from_psustore<PID>(psu_store: &PsuStore<PID, CID>) -> Self
    where
        CID: Copy + Hash + Ord,
    {
        let n_cats =
            NonZeroUsize::new(psu_store.iter().map(PsuData::categories_len).sum::<usize>())
                .expect("categories to have been added");
        #[expect(clippy::integer_division, reason = "have exact solution")]
        let mut cov_mat = FxHashMap::with_capacity_and_hasher(
            n_cats.get() * (n_cats.get() + 1) / 2,
            FxBuildHasher,
        );
        let mut iter = psu_store.category_iter();
        while let Some((cats_small, _)) = iter.next() {
            cov_mat.insert((cats_small, cats_small).into(), 0.0);
            for (cats_large, _) in iter.clone() {
                cov_mat.insert((cats_small, cats_large).into(), 0.0);
            }
        }

        Self {
            cov_mat,
            n_cats,
            estimation_strategy: CovarianceStrategy::SimpleRandomSample,
        }
    }
    /// Returns the estimation strategy.
    #[inline]
    pub fn estimation_strategy(&self) -> CovarianceStrategy { self.estimation_strategy }
    /// Sets the estimation strategy.
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
    /// Sets the covaraince of a `cat_pair`.
    /// # Panics
    /// Panics if `cat_pair` does not exist.
    #[inline]
    fn set<PAIR>(&mut self, cat_pair: PAIR, cov: f64)
    where
        CID: Hash + Eq,
        PAIR: Into<CatIdPair<CID>>,
    {
        *self
            .cov_mat
            .get_mut(&cat_pair.into())
            .expect("cat_pair to exist") = cov;
    }
    /// Adds to a covariance element.
    /// # Panics
    /// Panics if `cat_pair` does not exist.
    #[inline]
    fn add<PAIR>(&mut self, cat_pair: PAIR, cov: f64)
    where
        CID: Hash + Eq,
        PAIR: Into<CatIdPair<CID>>,
    {
        *self
            .cov_mat
            .get_mut(&cat_pair.into())
            .expect("cat_pair to exist") += cov;
    }
    /// Returns the variance, i.e. the sum of the covariance matrix.
    #[must_use]
    #[inline]
    pub fn variance(&self) -> f64
    where
        CID: Eq,
    {
        self.cov_mat
            .iter()
            .map(|(pair, &val)| if pair.is_same() { val } else { val * 2.0 })
            .sum()
    }
    /// Returns a tuple containing the covariance matrix, and the categories in their order of
    /// appearance in the matrix.
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
            self.n_cats.get(),
            "store does not contain correct amount of categories"
        );

        let mut cm = Matrix::from_value(0.0, (self.n_cats, self.n_cats));

        for i in 0..self.n_cats.get() {
            cm[(i, i)] = self.get((cats[i], cats[i])).expect("cat to exist");

            for j in (i + 1)..self.n_cats.get() {
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
impl<CID, PAIR> Index<PAIR> for CovarianceMatrix<CID>
where
    CID: Eq + Hash,
    PAIR: Into<CatIdPair<CID>>,
{
    type Output = f64;
    #[inline]
    fn index(&self, index: PAIR) -> &Self::Output {
        let index = index.into();
        &self.cov_mat[&index]
    }
}
impl<CID, PAIR> IndexMut<PAIR> for CovarianceMatrix<CID>
where
    CID: Eq + Hash,
    PAIR: Into<CatIdPair<CID>>,
{
    #[inline]
    fn index_mut(&mut self, index: PAIR) -> &mut Self::Output {
        self.get_mut(index).expect("cat_pair to exist")
    }
}

#[must_use]
#[derive(Clone, Copy, Default, Debug)]
/// Contains an estimate for a category.
pub struct CategorySum {
    /// Sum.
    pub sum: f64,
    /// Number of tracts which have positive measurements for this category.
    pub positive_tracts: u64,
    /// Number of tracts which have non zero measurements for this category.
    pub non_zero_tracts: u64,
}
impl CategorySum {
    /// Constructs a new `CategorySum`.
    #[inline]
    pub fn new(sum: f64) -> Self {
        Self {
            sum,
            ..Default::default()
        }
    }
}

/// The return value of an estimate.
#[must_use]
#[derive(Debug, Clone)]
pub struct CategorySums<CID> {
    /// Sums per category.
    pub categories: CategoryStore<CID, CategorySum>,
    /// Number of tracts that have positive values in any category.
    pub positive_tracts: u64,
    /// Number of tracts that have non-zero values in any category.
    pub non_zero_tracts: u64,
}
impl<CID> CategorySums<CID> {
    /// Constructs a new, empty, epc-store.
    #[inline]
    pub fn new<PID>(psus: &PsuStore<PID, CID>) -> Self
    where
        CID: Copy + Ord,
    {
        Self {
            categories: psus
                .category_iter()
                .map(|(cat_id, _)| cat_id)
                .copied()
                .collect(),
            positive_tracts: 0,
            non_zero_tracts: 0,
        }
    }
    /// Constructs a new, empty, epc-store, with some capacity.
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            categories: CategoryStore::with_capacity(capacity),
            positive_tracts: 0,
            non_zero_tracts: 0,
        }
    }
    /// Get the value of a category, if it exists and has non-zero entries.
    #[must_use]
    #[inline]
    pub fn get_sum_nz(&self, cat_id: &CID) -> Option<f64>
    where
        CID: Ord,
    {
        let cs = self.categories.get(cat_id)?;
        (cs.non_zero_tracts > 0).then_some(cs.sum)
    }
    /// Returns the estimate.
    #[must_use]
    #[inline]
    pub fn total(&self) -> f64 { self.categories.iter().map(|ct| ct.get().sum).sum() }
    /// Running sum tracts.
    #[inline]
    fn sum_tracts_running<'btract, PID, TID, I>(&mut self, tracts: I)
    where
        PID: 'btract,
        CID: 'btract + Ord,
        TID: 'btract,
        I: Iterator<Item = &'btract Tract<TID, PID, CID>>,
        CID: Ord,
    {
        for tract in tracts {
            for ct in tract.totals().iter() {
                if *ct.get() == 0.0 {
                    continue;
                }

                let Some(sum_cat) = self.categories.get_mut(ct.cat_id()) else {
                    continue;
                };
                sum_cat.sum += ct.get();
                sum_cat.non_zero_tracts += 1;

                if 0.0 < *ct.get() {
                    sum_cat.positive_tracts += 1;
                }
            }
        }
    }
    /// Sum tracts.
    #[inline]
    fn sum_tracts<'btract, PID, TID, I>(&mut self, tracts: I)
    where
        PID: 'btract,
        CID: 'btract + Ord,
        TID: 'btract,
        I: Iterator<Item = &'btract Tract<TID, PID, CID>>,
        CID: Ord,
    {
        for tract in tracts {
            let mut pos_tract = false;
            let mut nz_tract = false;
            for ct in tract.totals().iter() {
                if *ct.get() == 0.0 {
                    continue;
                }

                let Some(sum_cat) = self.categories.get_mut(ct.cat_id()) else {
                    continue;
                };
                sum_cat.sum += ct.get();
                sum_cat.non_zero_tracts += 1;
                nz_tract = true;

                if 0.0 < *ct.get() {
                    sum_cat.positive_tracts += 1;
                    pos_tract = true;
                }
            }

            if nz_tract {
                self.non_zero_tracts += 1;
                if pos_tract {
                    self.positive_tracts += 1;
                }
            }
        }
    }
}
impl<CID> FromIterator<CID> for CategorySums<CID>
where
    CID: Ord,
{
    #[inline]
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = CID>,
    {
        Self {
            categories: iter.into_iter().collect(),
            positive_tracts: 0,
            non_zero_tracts: 0,
        }
    }
}

/// A store of Nils PSUs and tracts.
#[must_use]
#[derive(Debug, Clone)]
pub struct Nils<PID, CID, TID> {
    /// PSUs.
    psus: PsuStore<PID, CID>,
    /// Tracts.
    tracts: TractStore<TID, PID, CID>,
    /// Frame area.
    frame_area: Area,
    /// Estimates.
    estimates: CategorySums<CID>,
}
/// Deconstructioning tuple of `Nils`.
pub type NilsParts<PID, CID, TID> = (
    PsuStore<PID, CID>,
    TractStore<TID, PID, CID>,
    Area,
    CategorySums<CID>,
);

impl<PID, CID, TID> Nils<PID, CID, TID> {
    /// Initialise a `Nils` from with `psus` and `tracts` stores, and calculate estimates.
    /// # Errors
    /// Returns an error if the psu sizes does not match the tract sizes.
    #[inline]
    pub fn new(
        psus: PsuStore<PID, CID>,
        tracts: TractStore<TID, PID, CID>,
        frame_area: Area,
    ) -> Result<Self, NilsError>
    where
        PID: Copy + Eq,
        CID: Copy + Ord,
    {
        // Store PID, size of tracts that starts appearing here and actual number of tracts
        let mut psu_sizes: Vec<(PID, u32, u32)> = Vec::with_capacity(psus.len());
        // Count number of cats
        let mut number_of_categories = 0;

        // Check tract sizes and categories
        for psu in psus.iter() {
            number_of_categories += psu.categories_len();
            psu_sizes.push((*psu.psu_id(), psu.size().get(), 0));
        }

        // Check so some categories have been added
        if number_of_categories == 0 {
            return Err(NilsError::NoCategoriesAdded);
        }

        // De-cumulative the sizes
        for i in 1..psu_sizes.len() {
            psu_sizes[i].1 -= psu_sizes[i - 1].1;
        }

        // Find the corresponding tracts
        for tract in tracts.iter() {
            for psu_size in &mut psu_sizes {
                if *tract.psu_id() == psu_size.0 {
                    psu_size.2 += 1;
                    break;
                }
            }
        }

        // Check sizes
        for psu_size in psu_sizes {
            if psu_size.1 != psu_size.2 {
                return Err(NilsError::IncorrectNumberOfTracts(psu_size.2, psu_size.1));
            }
        }

        // Calc estimates
        // Sum estimates per categories
        let mut epc = CategorySums::new(&psus);
        epc.sum_tracts(tracts.iter());

        for psu in psus.iter() {
            let size = f64::from(psu.size().get());
            let area_frac = frame_area.get() / size;

            #[expect(clippy::missing_panics_doc, reason = "bug")]
            for cat_id in psu.categories_iter() {
                epc.categories.get_mut(cat_id).expect("CID to exist").sum *= area_frac;
            }
        }

        Ok(Self {
            psus,
            tracts,
            frame_area,
            estimates: epc,
        })
    }
    /// Retrieve parts.
    #[inline]
    pub fn into_parts(self) -> NilsParts<PID, CID, TID> {
        (self.psus, self.tracts, self.frame_area, self.estimates)
    }
    /// Returns a reference to the estimates.
    #[inline]
    pub fn estimates(&self) -> &CategorySums<CID> { &self.estimates }
    /// Sorts the `tract_ids` and constructs tract ranges over the psus.
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
    /// Filteres the categories, so that only categories with some non zero values remain.
    /// Sorts the categories according to PSU, and returns the sorted filtered categories together
    /// with the range of categories belonging to each PSU.
    #[must_use]
    #[inline]
    fn category_ranges(&self) -> (Vec<CID>, Vec<Range<usize>>)
    where
        CID: Copy + Ord,
    {
        let mut categories: Vec<CID> =
            Vec::with_capacity(self.psus.iter().map(PsuData::categories_len).sum());
        let mut ranges: Vec<Range<usize>> = Vec::with_capacity(self.psus.len());

        for psu in self.psus.iter() {
            let start = categories.len();
            // Add only cats that have some nz values
            categories.extend(psu.categories_iter().filter_map(|cid| {
                let nz_tracts = self.estimates.categories.get(cid)?.non_zero_tracts;
                (nz_tracts > 0).then_some(*cid)
            }));
            let end = categories.len();
            ranges.push(start..end);
        }

        (categories, ranges)
    }

    /// Returns an estimated covariance matrix.
    /// # Errors
    /// Returns an error if the psus doesn't contain any categories, or if the `frame_area` is
    /// invalid.
    #[expect(clippy::missing_panics_doc, reason = "panic implies bug")]
    #[inline]
    pub fn covariance_estimate(&self) -> Result<CovarianceMatrix<CID>, NilsError>
    where
        PID: Copy + Eq,
        CID: Copy + Hash + Ord,
        TID: Copy + Eq + Display + Hash,
    {
        // Prepare covariance matrix structure
        let mut cov_mat = CovarianceMatrix::from_psustore(&self.psus);

        cov_mat.set_estimation_strategy(CovarianceStrategy::SimpleRandomSample);

        // Get vector of tract ids
        let mut tract_ids: Vec<TID> = self.tracts.iter().map(Tract::tract_id).copied().collect();
        let tract_ranges = self.tract_ranges(&mut tract_ids);

        // Prepare category sum container
        let (categories, category_ranges) = self.category_ranges();
        // Sums cumulate over each PSU (running sums)
        let mut sums: CategorySums<CID> = categories.iter().copied().collect();

        // loop from smallest to largest psu
        // We should calculate the covariance of category A and B by including the tracts that
        // are part of A and B (intersect). Hence, if A < B, then tracts in A should be counted.
        // We can do this by having an inner loop from the smallest category, where we identify
        // the tracts of this cat, and then comparing to all larger categories
        for (psu_ord, (psu_id, tract_range)) in tract_ranges.into_iter().enumerate() {
            let tracts_subset = &tract_ids[..tract_range.end];

            // Successively add tracts to the totals.
            sums.sum_tracts(
                tract_ids[tract_range.clone()]
                    .iter()
                    .map(|tid| self.tracts.get(tid).expect("tract id to exist")),
            );

            let psu = self.psus.get_psu(&psu_id).expect("psu id to exist");
            let psu_size = psu.size().get().to_f64().expect("u32 -> f64");
            let area_frac = self.frame_area.get() / psu_size;
            let area_const = area_frac * (psu_size / (psu_size - 1.0));

            // Outer loop of cats from current psu
            for cat_ord in category_ranges[psu_ord].clone() {
                let cat_curr = &categories[cat_ord];

                let mean_curr = match sums.get_sum_nz(cat_curr) {
                    Some(v) => v / psu_size,
                    _ => continue,
                };

                // First we calculate for the current category
                let area_const_curr = area_const * area_frac;
                let var_curr = tracts_subset
                    .iter()
                    .map(|tid| {
                        (self.tracts.get_category_value(tid, cat_curr).unwrap_or(0.0) - mean_curr)
                            .powi(2)
                    })
                    .sum::<f64>()
                    * area_const_curr;
                cov_mat.set((cat_curr, cat_curr), var_curr);

                // Then we calculate for categories in current PSU.
                // We clone so we dont have to visit the same category twice
                for cat_small in &categories[cat_ord..category_ranges[psu_ord].end] {
                    let mean_small = match sums.get_sum_nz(cat_small) {
                        Some(v) => v / psu_size,
                        _ => continue,
                    };
                    let var_small = tracts_subset
                        .iter()
                        .map(|tid| {
                            let vc = self.tracts.get_category_value(tid, cat_curr).unwrap_or(0.0)
                                - mean_curr;
                            let vs = self
                                .tracts
                                .get_category_value(tid, cat_small)
                                .unwrap_or(0.0)
                                - mean_small;
                            vc * vs
                        })
                        .sum::<f64>()
                        * area_const_curr;
                    cov_mat.set((cat_curr, cat_small), var_small);
                }

                // Finally we calculate for large categories
                for psu_inner_ord in (psu_ord + 1)..self.psus.len() {
                    let psu_size_large = self
                        .psus
                        .get_nth_psu(psu_inner_ord)
                        .expect("psu to exist")
                        .size()
                        .get()
                        .to_f64()
                        .expect("u32 -> f64");
                    let area_const_large = area_const * (self.frame_area.get() / psu_size_large);

                    for cat_large in &categories[category_ranges[psu_inner_ord].clone()] {
                        // We're looking for mean in intersect set, hence psu_size is the mean size
                        let mean_large = match sums.get_sum_nz(cat_large) {
                            Some(v) => v / psu_size,
                            _ => continue,
                        };

                        let var_large = tracts_subset
                            .iter()
                            .map(|tid| {
                                let vc =
                                    self.tracts.get_category_value(tid, cat_curr).unwrap_or(0.0)
                                        - mean_curr;
                                let vl = self
                                    .tracts
                                    .get_category_value(tid, cat_large)
                                    .unwrap_or(0.0)
                                    - mean_large;
                                vc * vl
                            })
                            .sum::<f64>()
                            * area_const_large;
                        cov_mat.set((cat_curr, cat_large), var_large);
                    }
                }
            }
        }

        Ok(cov_mat)
    }
    /// Returns an estimated covariance matrix using the nearest neighbour variance estimator.
    ///
    /// Spreading matrix assumed to be in same order as insertion order of tracts.
    /// # Errors
    /// Returns an error if the psus doesn't contain any categories, or if the `frame_area` is
    /// invalid.
    /// # Panics
    /// Panics if `u32` cannot be converted into `usize`.
    #[inline]
    pub fn covariance_estimate_nn<P>(
        &self,
        spreading: &SpreadingOptions<P>,
    ) -> Result<CovarianceMatrix<CID>, NilsError>
    where
        PID: Copy + Eq,
        CID: Copy + Hash + Ord,
        TID: Copy + Eq + Display + Hash,
        P: PointSet<Id = TID>,
    {
        // Prepare covariance matrix structure
        let mut cov_mat = CovarianceMatrix::from_psustore(&self.psus);
        cov_mat.set_estimation_strategy(CovarianceStrategy::NearestNeighbours);
        // Get vector of tract ids,
        let mut tract_ids: Vec<TID> = self.tracts.iter().map(Tract::tract_id).copied().collect();
        // Create tree
        let mut searcher = Searcher::new(
            spreading,
            &mut tract_ids,
            self.psus.get_max_psu().expect("there to be psus"),
        )?;

        let tract_ranges = self.tract_ranges(&mut tract_ids);
        let (categories, category_ranges) = self.category_ranges();
        let mut sums = CategorySums::with_capacity(categories.len());

        // loop from smallest to largest psu
        // We should calculate the covariance of category A and B by including the tracts that
        // are part of A and B (intersect). Hence, if A < B, then tracts in A should be counted.
        // We can do this by having an inner loop from the smallest category, where we identify
        // the tracts of this cat, and then comparing to all larger categories
        for (psu_ord, (psu_id, tract_range)) in tract_ranges.into_iter().enumerate() {
            let range_end = tract_range.end;

            // Remove all tract_ids from the tree (except the first)
            // We will add tract_ids to the tree again, as we loop through the psus
            // Anytime we are not in the first psu, we need to re-add the units to the tree
            if psu_ord == 0 {
                searcher.remove_units(&tract_ids[tract_range.end..]);
            } else {
                searcher.insert_units(&tract_ids[tract_range]);
            }

            sums.categories
                .initialize(categories[category_ranges[psu_ord].start..].iter());

            // Smaller psu info
            let psu = self.psus.get_psu(&psu_id).expect("psu id to exist");
            let psu_size = psu.size().get().to_f64().expect("u32 -> f64");
            let psu_nn = psu.nn_size().get().to_f64().expect("u32 -> f64");
            let area_frac = self.frame_area.get() / psu_size;

            // In normal variance est, we loop through psus->cats->tracts.
            // This we can do since fetching tracts is not too expensive, so looping
            // cats->tracts vs tracts->cats doesnt really matter.
            // Here, tracts is expensive, since we need to find neighbours.
            // Thus, instead of psu_outer->cat_outer->psu_inner->cat_inner->tracts, we go
            // psu_outer->tracts->cat_outer->psu_inner->cat_inner.

            // Set searcher size .. subtract 1 b/c we skip self
            searcher.set_nominal_size(psu);

            for tract_id in &tract_ids[..range_end] {
                let tract = self.tracts.get(tract_id).expect("tract to exist");

                searcher.search(tract_id);

                // Start by summing all categories .. we can skip any 0 sum category as cov
                // contribution will be 0
                sums.categories.reset_to_default();
                sums.sum_tracts_running(once(tract));
                sums.sum_tracts_running(searcher.neighbour_tracts(&self.tracts));

                // Outer loop of cats from current psu
                for cat_ord in category_ranges[psu_ord].clone() {
                    let cat_curr = &categories[cat_ord];
                    let deviance_curr = {
                        // If all units are 0, the covs will be 0
                        let mean = match sums.get_sum_nz(cat_curr) {
                            Some(v) => v / psu_nn,
                            _ => continue,
                        };
                        let val = tract.totals().get(cat_curr).copied().unwrap_or(0.0);
                        val - mean
                    };

                    // First we calculate for the current category
                    cov_mat.add((cat_curr, cat_curr), deviance_curr.powi(2));

                    // Then we calculate for other categories
                    for cat_other in &categories[(cat_ord + 1)..] {
                        let deviance_other = {
                            // If all units are 0, the covs will be 0
                            let mean = match sums.get_sum_nz(cat_other) {
                                Some(v) => v / psu_nn,
                                _ => continue,
                            };
                            let val = tract.totals().get(cat_other).copied().unwrap_or(0.0);
                            val - mean
                        };

                        cov_mat.add((cat_curr, cat_other), deviance_curr * deviance_other);
                    }
                }
            }

            // Outer loop of cats from current psu
            for cat_ord in category_ranges[psu_ord].clone() {
                let cat_curr = &categories[cat_ord];
                let area_const = area_frac * (psu_nn / (psu_nn - 1.0));

                // Calculate for categories in current PSU.
                let area_const_curr = area_const * area_frac;
                for cat_small in &categories[cat_ord..category_ranges[psu_ord].end] {
                    cov_mat[(cat_curr, cat_small)] *= area_const_curr;
                }

                // Calculate for categories in larger PSUs
                for psu_inner_ord in (psu_ord + 1)..self.psus.len() {
                    let psu_large = self.psus.get_nth_psu(psu_inner_ord).expect("psu to exist");
                    let psu_size_large = psu_large.size().get().to_f64().expect("u32 -> f64");
                    let area_const_large = area_const * (self.frame_area.get() / psu_size_large);

                    for cat_large in &categories[category_ranges[psu_inner_ord].clone()] {
                        cov_mat[(cat_curr, cat_large)] *= area_const_large;
                    }
                }
            }
        }

        Ok(cov_mat)
    }
}

/// Tree-searcher utility.
#[must_use]
struct Searcher<'bdata, P>
where
    P: PointSet,
{
    /// The tree structure.
    tree: Tree<'bdata, P>,
    /// The searcher.
    searcher: KNearestNeighbourSearcher<P>,
}
impl<'bdata, P, TID> Searcher<'bdata, P>
where
    P: PointSet<Id = TID>,
{
    /// Constructs a new tree-searcher.
    /// # Panics
    /// Panic implies that `nn_size` is too low (1).
    #[inline]
    fn new<PID, CID>(
        data: &'bdata SpreadingOptions<P>,
        ids: &mut [TID],
        max_psu: &PsuData<PID, CID>,
    ) -> Result<Self, NilsError> {
        let tree = Tree::new(data, ids)?;
        let max_nn_size = max_psu.nn_size().get().to_usize().expect("u32 -> usize");
        let searcher = KNearestNeighbourSearcher::new(
            NonZeroUsize::new(max_nn_size - 1).expect("nn_size - 1 > 0"),
            tree.data(),
        );
        Ok(Self { tree, searcher })
    }
    /// Removes units from the tree.
    #[inline]
    fn remove_units(&mut self, ids: &[TID])
    where
        TID: Copy,
    {
        for tract_id in ids {
            self.tree
                .remove_unit(*tract_id)
                .expect("tract_id to exist in data");
        }
    }
    /// Inserts units into the tree.
    #[inline]
    fn insert_units(&mut self, ids: &[TID])
    where
        TID: Copy,
    {
        for tract_id in ids {
            self.tree
                .insert_unit(*tract_id)
                .expect("tract_id to exist in data");
        }
    }
    /// Sets the nominal size to search for (excludes the search unit). Nominal size is decided from
    /// `nn_size` in provided PSU (-1 for the search unit).
    /// # Panics
    /// Panic implies that `nn_size` is too low (1).
    #[inline]
    fn set_nominal_size<PID, CID>(&mut self, psu: &PsuData<PID, CID>) {
        let nominal_size: NonZeroUsize = (psu.nn_size().get() - 1)
            .to_usize()
            .expect("u32 -> usize")
            .try_into()
            .expect("non zero");
        self.searcher.set_nominal_size(nominal_size);
    }
    /// Searches for the `nn_size - 1` nearest neighbours to `tract_id`.
    /// # Panics
    /// Panics if the `tract_id` cannot be found in the tree data (BUG), or if no neighbours can be
    /// found.
    #[inline]
    fn search(&mut self, tract_id: &TID)
    where
        TID: Copy,
    {
        self.searcher
            .reset_from_unit(self.tree.data(), *tract_id)
            .expect("unit to exist in data")
            .search(&self.tree)
            .expect("");
    }
    /// Returns an iterator of the nearest neighbours to the search unit.
    #[inline]
    fn neighbours<'bitem>(&'bitem self) -> impl Iterator<Item = &'bitem TID>
    where
        TID: Copy + 'bitem,
    {
        self.searcher.neighbours().iter().map(Neighbour::id)
    }
    /// Returns an iterator over the tract neighbours.
    #[inline]
    fn neighbour_tracts<'bitem, PID, CID>(
        &'bitem self,
        tracts: &'bitem TractStore<TID, PID, CID>,
    ) -> impl Iterator<Item = &'bitem Tract<TID, PID, CID>>
    where
        TID: Copy + Eq + Hash,
    {
        self.neighbours()
            .map(|tid| tracts.get(tid).expect("tract to exist"))
    }
}
