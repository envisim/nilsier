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

//! Savvy-R-wrappers for [`nilsier`].

#![expect(clippy::wildcard_imports, reason = "need everything")]

mod sexp;
mod spreading_data;
mod tract_id;

use std::num::NonZeroU32;

use envisim_utils::matrix::Dimensions;
use envisim_utils::sampling_options::SpreadingOptions;
use envisim_utils::utils::{
    DataView,
    SliceView,
};
use nilsier::psu::{
    PsuHeader,
    PsuStore,
};
use nilsier::tract::{
    Tract,
    TractStore,
    TractValueEntry,
};
use nilsier::{
    Area,
    CategorySums,
    CovarianceMatrix,
    CovarianceStrategy,
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

/// `ListSexp` representation of Psus.
#[must_use]
struct PsuList {
    /// PSU IDs.
    psus: IntegerSexpFatPtr,
    /// Sizes.
    sizes: IntegerSexpFatPtr,
    /// Supplied nearest neighbour sizes.
    nn_sizes: Option<IntegerSexpFatPtr>,
}
impl PsuList {
    /// Extract from `list`.
    #[inline]
    fn from_list(list: &ListSexp) -> savvy::Result<Self> {
        let ids: IntegerSexpFatPtr = list
            .get("psu")
            .ok_or(savvy_err!("cannot find list item 'psu'"))?
            .try_into()?;

        let sizes: IntegerSexpFatPtr = list
            .get("size")
            .ok_or(savvy_err!("cannot find list item 'size'"))?
            .try_into()?;

        if ids.len() != sizes.len() {
            return Err(savvy_err!("'psu' and 'size' must match in length"));
        }

        let nn_sizes = match list.get("nn_size") {
            Some(nn) => {
                let nn: IntegerSexpFatPtr = nn.try_into()?;
                if ids.len() != nn.len() {
                    return Err(savvy_err!("'psu' and 'nn_size' must match in length"));
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
    /// Construct new `PsuStore`.
    #[inline]
    fn as_psu_store(&self, categories: &CategoryList) -> savvy::Result<PsuStore<i32, i32>> {
        let headers: Vec<PsuHeader<i32>> = self
            .psus
            .values()
            .zip(self.sizes.values())
            .map(|(id, size)| {
                let v = size.to_u32().and_then(NonZeroU32::new)?;
                Some(PsuHeader::new(*id, v))
            })
            .collect::<Option<Vec<PsuHeader<i32>>>>()
            .ok_or(savvy_err!("'sizes' must be positive"))?;

        let nn_sizes = match &self.nn_sizes {
            Some(nn) => nn
                .values()
                .map(|n| n.to_u32().and_then(NonZeroU32::new))
                .collect::<Option<Vec<NonZeroU32>>>()
                .ok_or(savvy_err!("nn_sizes must be positive"))?
                .into_iter()
                .into(),
            None => None,
        };

        let mut store = PsuStore::new(headers, nn_sizes)?;

        // add categories
        for (cat, psu) in categories.categories.values().zip(categories.psus.values()) {
            store.insert_category(psu, *cat)?;
        }

        Ok(store)
    }
}

/// `ListSexp` representation of categories.
#[must_use]
struct CategoryList {
    /// category ids.
    categories: IntegerSexpFatPtr,
    /// psu ids.
    psus: IntegerSexpFatPtr,
}
impl CategoryList {
    /// Extract from `list`.
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
}

/// `ListSexp` representation of Tracts.
#[must_use]
struct TractList {
    /// tract ids.
    tracts: IntegerSexpFatPtr,
    /// psu ids.
    psus: IntegerSexpFatPtr,
    /// auxiliary information.
    auxiliaries: Option<RMatrix>,
}
impl TractList {
    /// Extract from `list`.
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
    /// Construct a `TractStore`.
    #[inline]
    fn as_tract_store(&self, tract_area: Area) -> savvy::Result<TractStore<TractId, i32, i32>> {
        // Prep store
        let mut store = TractStore::with_capacity(self.tracts.len(), tract_area);

        for tract in TractId::from_sexp(&self.tracts)
            .zip(self.psus.values())
            .map(|(tract, psu)| Tract::new(tract, *psu))
        {
            store.insert(tract)?;
        }

        Ok(store)
    }
}

/// `ListSexp` representation of tract entries.
struct TractEntryList {
    /// tract ids.
    tracts: IntegerSexpFatPtr,
    /// category ids.
    categories: IntegerSexpFatPtr,
    /// design weights.
    dws: RealSexpFatPtr,
    /// values.
    values: RealSexpFatPtr,
}
impl TractEntryList {
    /// Extract from `list`.
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
    /// Add entries to tract store.
    #[inline]
    fn add_entries_to_store<PID>(
        &self,
        store: &mut TractStore<TractId, PID, i32>,
    ) -> savvy::Result<()> {
        // Add entries
        let cats = self.categories.slice();
        let dws = self.dws.slice();
        let values = self.values.slice();

        for (i, tract) in self.tracts.entries() {
            // Hacky as tract value entry doesnt need the internal part of the tract id
            let tid = TractId::from((0_usize, *tract));
            let entry = TractValueEntry::new(tid, cats[i], dws[i], values[i])?;
            store.add_value_from_entry(&entry)?;
        }

        Ok(())
    }
}

/// Constructs an `OwnedListSexp` from `estimates` and `covariances`.
/// Sets the first 7 slots of `list`.
#[inline]
fn as_result_list(
    mut list: OwnedListSexp,
    estimates: &CategorySums<i32>,
    covariances: &CovarianceMatrix<i32>,
) -> savvy::Result<OwnedListSexp> {
    // Set estimate
    let sexp_estimate = OwnedRealSexp::try_from_scalar(estimates.total())?;
    list.set_name_and_value(0, "estimate", sexp_estimate)?;

    // Set variance estimate
    let sexp_variance = OwnedRealSexp::try_from_scalar(covariances.variance())?;
    list.set_name_and_value(1, "variance", sexp_variance)?;

    // Set estimates per category
    let sexp_category_estimates =
        OwnedRealSexp::try_from_iter(estimates.categories.iter().map(|v| v.get().sum))?;
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
    let mut sexp_category_covariances = OwnedRealSexp::try_from_slice(cov_mat.data())?;
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
        .categories
        .iter()
        .map(|v| v.get().positive_tracts.to_i32())
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

/// Estimates according to NILS design.
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
    let frame_area = Area::new(frame_area).ok_or(savvy_err!("frame area must be positive"))?;
    let tract_area = Area::new(tract_area).ok_or(savvy_err!("tract area must be positive"))?;
    let tracts = TractList::from_list(&tracts)?;

    // Construct PSU store
    let psu_store = {
        let store = PsuList::from_list(&psus)?;
        let categories = CategoryList::from_list(&categories)?;
        store.as_psu_store(&categories)?
    };
    let tract_store = {
        let mut store = tracts.as_tract_store(tract_area)?;
        let values = TractEntryList::from_list(&values)?;
        values.add_entries_to_store(&mut store)?;
        store
    };
    let nils = Nils::new(psu_store, tract_store, frame_area)?;

    // Covariance matrix
    let covariances = match (variance_strategy, SpreadingData::new(tracts)) {
        ("nearest_neighbours", Some(sd)) => {
            let so = SpreadingOptions::new(sd).set_bucket_size(30)?;
            nils.covariance_estimate_nn(&so)?
        }
        _ => nils.covariance_estimate()?,
    };

    let list = as_result_list(OwnedListSexp::new(7, true)?, nils.estimates(), &covariances)?;
    list.into()
}

/// Estimate a ratio according to NILS design.
///
/// @keywords internal
/// @noRd
#[savvy]
fn rust_nils_estimate_ratio(
    psus: ListSexp,
    categories: ListSexp,
    tracts: ListSexp,
    values_y: ListSexp,
    values_z: ListSexp,
    frame_area: f64,
    tract_area: f64,
    variance_strategy: &str,
) -> savvy::Result<Sexp> {
    // Prep data
    let frame_area = Area::new(frame_area).ok_or(savvy_err!("frame area must be positive"))?;
    let tract_area = Area::new(tract_area).ok_or(savvy_err!("tract area must be positive"))?;
    let tracts = TractList::from_list(&tracts)?;
    let tract_store_base = tracts.as_tract_store(tract_area)?;

    let psu_store = {
        let store = PsuList::from_list(&psus)?;
        let categories = CategoryList::from_list(&categories)?;
        store.as_psu_store(&categories)?
    };

    // Estimate y's
    let (psu_store, mut tract_store_y, _, estimates_y) = {
        let tract_store = {
            let mut store = tract_store_base.clone(); // Clone base instead of re-creating
            let values = TractEntryList::from_list(&values_y)?;
            values.add_entries_to_store(&mut store)?;
            store
        };
        Nils::new(psu_store, tract_store, frame_area)?.into_parts()
    };

    // Estimate z's
    let (psu_store, tract_store_z, _, estimates_z) = {
        let tract_store = {
            let mut store = tract_store_base; // Use base instead of re-creating
            let values = TractEntryList::from_list(&values_z)?;
            values.add_entries_to_store(&mut store)?;
            store
        };
        Nils::new(psu_store, tract_store, frame_area)?.into_parts()
    };

    // Change estimates_y to be estimates_r by dividing with _z
    let totals = (estimates_y.total(), estimates_z.total());
    let mut ratio = totals.0 / totals.1;
    let mut estimates_r = estimates_y.clone();
    for cat in estimates_r.categories.iter_mut() {
        let cid = cat.cat_id();
        cat.get_mut().sum /= totals.1;
    }

    // Change tract_store_y to be tract_store_r by doing y - R z for every non-zero z
    for tract in tract_store_z.iter() {
        let tid = *tract.tract_id();

        for cat in tract.totals().iter() {
            let cid = cat.cat_id();
            // tract_z has already divided by area, must multiply so it's not done twice
            let z_val = cat.get() * tract_area.get();
            // let r_val = estimates_r.get_sum_nz(cid).unwrap_or(0.0);
            let r_val = ratio;
            let zr = z_val * r_val;
            let entry = TractValueEntry::new(tid, *cid, 1.0, -zr)?; // Subtract zr
            tract_store_y.add_value_from_entry(&entry)?;
        }
    }

    let nils = Nils::new(psu_store, tract_store_y, frame_area)?;

    // Covariance matrix
    let covariances = match (variance_strategy, SpreadingData::new(tracts)) {
        ("nearest_neighbours", Some(sd)) => {
            let so = SpreadingOptions::new(sd).set_bucket_size(30)?;
            nils.covariance_estimate_nn(&so)?
        }
        _ => nils.covariance_estimate()?,
    };

    let mut list = as_result_list(OwnedListSexp::new(8, true)?, &estimates_r, &covariances)?;
    let sexp_totals = OwnedRealSexp::try_from_slice(&[totals.0, totals.1])?;
    list.set_name_and_value(7, "total_estimates", sexp_totals)?;
    list.into()
}
