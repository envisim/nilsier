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

//! Savvy-R-wrappers for [`nilsier`]

#![expect(clippy::wildcard_imports, reason = "need everything")]

mod sexp;

use std::num::NonZeroU32;

use envisim_utils::matrix::Dimensions;
use envisim_utils::sampling_options::SpreadingOptions;
use envisim_utils::utils::SliceView;
use nilsier::psu::PsuHeader;
use nilsier::tract::{
    Area,
    TractHeaderEntry,
    TractValueEntry,
};
use nilsier::{
    CovarianceMatrix,
    CovarianceStrategy,
    EstimatePerCategory,
    Nils,
};
use num_traits::ToPrimitive;
use savvy::{
    ListSexp,
    OwnedIntegerSexp,
    OwnedListSexp,
    OwnedRealSexp,
    OwnedStringSexp,
    Sexp,
    savvy,
    savvy_err,
};

use crate::sexp::*;
use crate::spreading_data::*;
use crate::tract_id::*;

mod tract_id {
    //! Tract id module

    use std::cmp::Ordering;
    use std::fmt::{
        Display,
        Formatter,
        Result as fmtResult,
    };
    use std::hash::{
        Hash,
        Hasher,
    };

    use crate::sexp::*;

    /// Id pair
    #[derive(Copy, Clone, Debug)]
    pub struct TractId {
        /// Internal id (order)
        internal: usize,
        /// Id given by user
        id: i32,
    }
    impl TractId {
        /// Return the internal id order
        #[inline]
        pub fn internal(&self) -> usize { self.internal }
        /// Return the id
        #[expect(dead_code, reason = "accessor")]
        #[inline]
        pub fn id(&self) -> i32 { self.id }
        /// Constructs an iterator from an `sexp` of ids
        #[inline]
        pub fn from_sexp(
            sexp: &IntegerSexpFatPtr,
        ) -> impl ExactSizeIterator<Item = Self> + DoubleEndedIterator + Clone + use<'_> {
            sexp.iter().copied().enumerate().map(Into::into)
        }
    }
    impl From<(usize, i32)> for TractId {
        #[inline]
        fn from((internal, id): (usize, i32)) -> Self { Self { internal, id } }
    }
    impl Eq for TractId {}
    impl PartialEq for TractId {
        #[inline]
        fn eq(&self, other: &Self) -> bool { self.id == other.id }
    }
    impl Ord for TractId {
        #[inline]
        fn cmp(&self, other: &Self) -> Ordering { self.id.cmp(&other.id) }
    }
    impl PartialOrd for TractId {
        #[inline]
        fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
    }
    impl Hash for TractId {
        #[inline]
        fn hash<H>(&self, state: &mut H)
        where
            H: Hasher,
        {
            self.id.hash(state);
        }
    }
    impl Display for TractId {
        #[inline]
        fn fmt(&self, f: &mut Formatter) -> fmtResult {
            write!(f, "TractId{{ id: {}, ord: {} }}", self.id, self.internal)
        }
    }
}

mod spreading_data {
    //! Spreading data module

    use std::num::NonZeroUsize;

    use envisim_utils::matrix::Dimensions;
    use envisim_utils::utils::PointSet;

    use super::TractList;
    use crate::sexp::*;
    use crate::tract_id::*;

    /// Struct mapping matrix with ids
    #[must_use]
    pub struct SpreadingData {
        /// ids
        ids: IntegerSexpFatPtr,
        /// data in matrix form
        data: RMatrix,
    }
    impl SpreadingData {
        /// Constructs new
        #[inline]
        pub fn new(list: TractList) -> Option<Self> {
            list.auxiliaries.map(|data| Self {
                ids: list.tracts,
                data,
            })
        }
        /// Returns an iterator over the ids
        #[inline]
        pub fn id_iter(
            &self,
        ) -> impl ExactSizeIterator<Item = TractId> + DoubleEndedIterator + use<'_> {
            TractId::from_sexp(&self.ids)
        }
        /// Returns true if `id` exists in the collection
        #[inline]
        pub fn contains(&self, id: TractId) -> bool { id.internal() < self.ids.len() }
    }

    impl PointSet for SpreadingData {
        type Value = f64;
        type Id = TractId;
        #[inline]
        fn len(&self) -> NonZeroUsize { self.data.nrow() }
        #[inline]
        fn ids(&self) -> impl ExactSizeIterator<Item = Self::Id> + DoubleEndedIterator {
            self.id_iter()
        }
        #[inline]
        fn dimensions(&self) -> NonZeroUsize { self.data.ncol() }
        #[inline]
        fn contains(&self, id: Self::Id) -> bool { self.contains(id) }
        #[inline]
        fn get_coord(&self, id: Self::Id, dim: usize) -> Option<Self::Value> {
            self.data.get_coord(id.internal(), dim)
        }
        #[inline]
        fn get_coords(
            &self,
            id: Self::Id,
        ) -> Option<impl ExactSizeIterator<Item = &Self::Value> + DoubleEndedIterator> {
            self.data.get_coords(id.internal())
        }
    }
}

/// `ListSexp` representation of Psus
#[must_use]
struct PsuList {
    /// psu ids
    psus: IntegerSexpFatPtr,
    /// sizes
    sizes: IntegerSexpFatPtr,
    /// supplied nearest neighbour sizes
    nn_sizes: Option<IntegerSexpFatPtr>,
}
impl PsuList {
    /// Extract from `list`
    #[inline]
    fn from_list(list: &ListSexp) -> savvy::Result<Self> {
        let ids: IntegerSexpFatPtr = list
            .get("psu")
            .ok_or(savvy_err!("cannot find list item 'psus'"))?
            .try_into()?;

        let sizes: IntegerSexpFatPtr = list
            .get("size")
            .ok_or(savvy_err!("cannot find list item 'sizes'"))?
            .try_into()?;

        if ids.len() != sizes.len() {
            return Err(savvy_err!("'psus' and 'sizes' must match in length"));
        }

        let nn_sizes = match list.get("nn_size") {
            Some(nn) => {
                let nn: IntegerSexpFatPtr = nn.try_into()?;
                if ids.len() != nn.len() {
                    return Err(savvy_err!("'psus' and 'nn_sizes' must match in length"));
                }
                Some(nn)
            }
            None => None,
        };

        Ok(Self {
            psus: ids,
            sizes,
            nn_sizes,
        })
    }
    /// Construct new `Nils`
    #[inline]
    fn as_nils(
        &self,
        cap: usize,
        frame_area: Area,
        tract_area: Area,
    ) -> savvy::Result<Nils<i32, i32, TractId>> {
        let headers: Vec<PsuHeader<i32>> = self
            .psus
            .iter()
            .zip(self.sizes.iter())
            .map(|(id, size)| {
                let v = size.to_u32().and_then(NonZeroU32::new)?;
                Some(PsuHeader::new(*id, v))
            })
            .collect::<Option<Vec<PsuHeader<i32>>>>()
            .ok_or(savvy_err!("'sizes' must be positive"))?;

        let nn_sizes = match &self.nn_sizes {
            Some(nn) => nn
                .iter()
                .map(|n| n.to_u32().and_then(NonZeroU32::new))
                .collect::<Option<Vec<NonZeroU32>>>()
                .ok_or(savvy_err!("nn_sizes must be positive"))?
                .into_iter()
                .into(),
            None => None,
        };

        let nils = Nils::new(headers.into_iter(), nn_sizes, frame_area, tract_area, cap)?;
        Ok(nils)
    }
}

/// `ListSexp` representation of categories
#[must_use]
struct CategoryList {
    /// category ids
    categories: IntegerSexpFatPtr,
    /// psu ids
    psus: IntegerSexpFatPtr,
}
impl CategoryList {
    /// Extract from `list`
    #[inline]
    fn from_list(list: &ListSexp) -> savvy::Result<Self> {
        let categories: IntegerSexpFatPtr = list
            .get("category")
            .ok_or(savvy_err!("cannot find list item 'categories'"))?
            .try_into()?;
        let psus: IntegerSexpFatPtr = list
            .get("psu")
            .ok_or(savvy_err!("cannot find list item 'psus'"))?
            .try_into()?;

        if categories.len() != psus.len() {
            return Err(savvy_err!("cat/psu-map has inconsistent length"));
        }

        Ok(Self { categories, psus })
    }
    /// Add categories to `Nils`
    #[inline]
    fn nils<TID>(&self, nils: &mut Nils<i32, i32, TID>) -> savvy::Result<()> {
        for (cat, psu) in self.categories.iter().zip(self.psus.iter()) {
            nils.add_category_to_psu(psu, *cat)?;
        }
        Ok(())
    }
}

/// `ListSexp` representation of Tracts
#[must_use]
struct TractList {
    /// tract ids
    tracts: IntegerSexpFatPtr,
    /// psu ids
    psus: IntegerSexpFatPtr,
    /// auxiliary information
    auxiliaries: Option<RMatrix>,
}
impl TractList {
    /// Extract from `list`
    #[inline]
    fn from_list(list: &ListSexp) -> savvy::Result<Self> {
        let tracts: IntegerSexpFatPtr = list
            .get("tract")
            .ok_or(savvy_err!("cannot find list item 'tract'"))?
            .try_into()?;
        let psus: IntegerSexpFatPtr = list
            .get("psu")
            .ok_or(savvy_err!("cannot find list item 'psu'"))?
            .try_into()?;

        if tracts.len() != psus.len() {
            return Err(savvy_err!("tract/psu-map has inconsistent length"));
        }

        let auxiliaries = match list.get("auxiliaries") {
            Some(aux) => {
                let data = realsexp_to_matrix(aux.try_into()?)?;

                if tracts.len() != data.nrow().get() {
                    return Err(savvy_err!("tract/auxiliaries has inconsistent length"));
                }

                Some(data)
            }
            None => None,
        };

        Ok(Self {
            tracts,
            psus,
            auxiliaries,
        })
    }
    /// Add tracts to `Nils`
    #[inline]
    fn nils<CID>(&self, nils: &mut Nils<i32, CID, TractId>) -> savvy::Result<()> {
        nils.add_tracts(
            TractId::from_sexp(&self.tracts)
                .zip(self.psus.iter())
                .map(|(tract, psu)| TractHeaderEntry::new(tract, *psu)),
        )?;
        Ok(())
    }
}

/// `ListSexp` representation of tract entries
struct TractEntryList {
    /// tract ids
    tracts: IntegerSexpFatPtr,
    /// category ids
    categories: IntegerSexpFatPtr,
    /// design weights
    dws: RealSexpFatPtr,
    /// values
    values: RealSexpFatPtr,
}
impl TractEntryList {
    /// Extract from `list`
    #[inline]
    fn from_list(list: &ListSexp) -> savvy::Result<Self> {
        let tract_ids: IntegerSexpFatPtr = list
            .get("tract")
            .ok_or(savvy_err!("cannot find list item 'tract'"))?
            .try_into()?;
        let cat_ids: IntegerSexpFatPtr = list
            .get("category")
            .ok_or(savvy_err!("cannot find list item 'category'"))?
            .try_into()?;
        let dw_vec: RealSexpFatPtr = list
            .get("dw")
            .ok_or(savvy_err!("cannot find list item 'dw'"))?
            .try_into()?;
        let value_vec: RealSexpFatPtr = list
            .get("value")
            .ok_or(savvy_err!("cannot find list item 'value'"))?
            .try_into()?;

        if tract_ids.len() != cat_ids.len() {
            return Err(savvy_err!("tract/cat has inconsistent length"));
        } else if tract_ids.len() != dw_vec.len() {
            return Err(savvy_err!("tract/dw has inconsistent length"));
        } else if tract_ids.len() != value_vec.len() {
            return Err(savvy_err!("tract/value has inconsistent length"));
        }

        Ok(Self {
            tracts: tract_ids,
            categories: cat_ids,
            dws: dw_vec,
            values: value_vec,
        })
    }
    /// Add tract entries to `Nils`
    #[inline]
    fn nils<PID>(&self, nils: &mut Nils<PID, i32, TractId>) -> savvy::Result<()> {
        let cats = self.categories.data();
        let dws = self.dws.data();
        let values = self.values.data();

        for (i, tract) in self.tracts.iter().enumerate() {
            // Hacky as tract value entry doesnt need the internal part of the tract id
            let tid = TractId::from((0_usize, *tract));
            nils.add_value_entry(TractValueEntry::new(tid, cats[i], dws[i], values[i])?)?;
        }

        Ok(())
    }
}

/// Constructs an `OwnedListSexp` from `estimates` and `covariances`
#[inline]
fn as_result_list(
    estimates: &EstimatePerCategory<i32>,
    covariances: &CovarianceMatrix<i32>,
) -> savvy::Result<OwnedListSexp> {
    let mut list = OwnedListSexp::new(7, true)?;

    // Set estimate
    let sexp_estimate = OwnedRealSexp::try_from_scalar(estimates.estimate())?;
    list.set_name_and_value(0, "estimate", sexp_estimate)?;

    // Set variance estimate
    let sexp_variance = OwnedRealSexp::try_from_scalar(covariances.variance())?;
    list.set_name_and_value(1, "variance", sexp_variance)?;

    // Set estimates per category
    let sexp_category_estimates =
        OwnedRealSexp::try_from_iter(estimates.estimate_per_category.iter().map(|v| *v.get()))?;
    list.set_name_and_value(2, "category_estimates", sexp_category_estimates)?;

    // Set covariance matrix
    let cov_mat = covariances.to_matrix().0;
    let cov_mat_dim = [
        cov_mat
            .nrow()
            .get()
            .to_i32()
            .ok_or_else(|| savvy_err!("covmat.nrow cannot be converted to i32"))?,
        cov_mat
            .ncol()
            .get()
            .to_i32()
            .ok_or_else(|| savvy_err!("covmat.ncol cannot be converted to i32"))?,
    ];
    let mut sexp_category_covariances = OwnedRealSexp::try_from_slice(cov_mat.data().data())?;
    sexp_category_covariances.set_dim(&cov_mat_dim)?;
    list.set_name_and_value(3, "category_covariances", sexp_category_covariances)?;

    // Set number of positive tracts
    let positive_tracts = estimates
        .positive_tracts
        .to_i32()
        .ok_or_else(|| savvy_err!("positive tracts cannot be converted to i32"))?;
    let sexp_positive_tracts = OwnedIntegerSexp::try_from_scalar(positive_tracts)?;
    list.set_name_and_value(4, "positive_tracts", sexp_positive_tracts)?;

    // Set number of positive tracts per category
    let positive_tracts_per_category = estimates
        .positive_tracts_per_category
        .iter()
        .map(|v| v.get().to_i32())
        .collect::<Option<Vec<i32>>>()
        .ok_or_else(|| savvy_err!("positive tracts per category cannot be converted to i32"))?;
    let sexp_positive_tracts_per_category =
        OwnedIntegerSexp::try_from_slice(&positive_tracts_per_category)?;
    list.set_name_and_value(
        5,
        "positive_tracts_per_category",
        sexp_positive_tracts_per_category,
    )?;

    // Set covariance estimation strategy
    let sexp_covariance_estimation_strategy =
        OwnedStringSexp::try_from_scalar(match covariances.estimation_strategy() {
            CovarianceStrategy::NearestNeighbours => "nearest_neighbours",
            CovarianceStrategy::SimpleRandomSample => "srs",
            _ => "unknown",
        })?;
    list.set_name_and_value(
        6,
        "covariance_estimation_strategy",
        sexp_covariance_estimation_strategy,
    )?;

    Ok(list)
}

/// Estimates according to NILS design
///
/// @keywords internal
/// @noRd
#[savvy]
fn rust_nils_estimate(
    psus: ListSexp,
    categories: ListSexp,
    tracts: ListSexp,
    values: ListSexp,
    frame_area: f64,
    tract_area: f64,
    variance_strategy: &str,
) -> savvy::Result<Sexp> {
    // Prep data
    let frame_area = Area::new(frame_area)?;
    let tract_area = Area::new(tract_area)?;
    let psus = PsuList::from_list(&psus)?;
    let categories = CategoryList::from_list(&categories)?;
    let tracts = TractList::from_list(&tracts)?;
    let values = TractEntryList::from_list(&values)?;

    // Construct Nils
    let mut nils = psus.as_nils(tracts.tracts.len(), frame_area, tract_area)?;
    categories.nils(&mut nils)?;
    tracts.nils(&mut nils)?;
    values.nils(&mut nils)?;

    // Estimate per category
    let estimates = nils.estimate_per_category();

    // Covariance matrix

    let covariances = match (variance_strategy, SpreadingData::new(tracts)) {
        ("nearest_neighbours", Some(sd)) => {
            let so = SpreadingOptions::new(sd).set_bucket_size(30)?;
            nils.covariance_estimate_nn(&estimates, &so)?
        }
        _ => nils.covariance_estimate(&estimates)?,
    };

    let list = as_result_list(&estimates, &covariances)?;
    list.into()
}
