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

//! Handling of tracts

use std::borrow::Borrow;
use std::collections::hash_map::Entry;
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};
use std::hash::Hash;
use std::ops::Deref;

use rustc_hash::{
    FxBuildHasher,
    FxHashMap,
};
use thiserror::Error;

use crate::category::{
    CatError,
    CategoryStore,
};

#[non_exhaustive]
#[derive(Error, Debug, Clone)]
pub enum TractError {
    #[error("Tract ID ({0}) not found")]
    TractIdNotFound(String),
    #[error("Tract ID ({0}) already exist")]
    TractIdCollision(String),
    #[error("Area ({0}) must be positive")]
    InvalidArea(f64),
    #[error("Design weight ({0}) must be positive")]
    InvalidDesignWeight(f64),
    #[error("Value ({0}) must be non-negative")]
    InvalidValue(f64),
    #[error(transparent)]
    Category(#[from] CatError),
}
/// Shorthand for `Result` with [`TractError`] error type.
type TractResult<T> = Result<T, TractError>;

/// Connects tract to a PSU
#[must_use]
#[derive(Clone, Copy, Debug)]
pub struct TractHeaderEntry<TID, PID> {
    /// tract id of header
    tract_id: TID,
    /// psu id of header
    psu_id: PID,
}
impl<TID, PID> TractHeaderEntry<TID, PID> {
    /// Constructs a new header entry
    #[inline]
    pub fn new(tract_id: TID, psu_id: PID) -> Self { Self { tract_id, psu_id } }
    /// Returns the tract ID of the header
    #[inline]
    pub fn tract_id(&self) -> &TID { &self.tract_id }
    /// Sets the tract ID of the header
    #[inline]
    pub fn set_tract_id(&mut self, tract_id: TID) { self.tract_id = tract_id; }
    /// Returns the PSU ID of the header
    #[inline]
    pub fn psu_id(&self) -> &PID { &self.psu_id }
    /// Sets the PSU ID of the header
    #[inline]
    pub fn set_psu_id(&mut self, psu_id: PID) { self.psu_id = psu_id; }
}
impl<TID, PID> Borrow<TID> for TractHeaderEntry<TID, PID> {
    #[inline]
    fn borrow(&self) -> &TID { &self.tract_id }
}

/// Contains values from a tract
#[must_use]
#[derive(Clone, Copy, Debug)]
pub struct TractValueEntry<TID, CID> {
    /// Tract ID
    tract_id: TID,
    /// Category ID
    cat_id: CID,
    /// Design weight
    design_weight: f64,
    /// Measured value
    value: f64,
}
impl<TID, CID> TractValueEntry<TID, CID> {
    /// Constructs a new entry.
    /// # Errors
    /// Returns an error if `design_weight` is non-positive or `value` is negative.
    #[inline]
    pub fn new(tract_id: TID, cat_id: CID, design_weight: f64, value: f64) -> TractResult<Self> {
        let mut tve = Self {
            tract_id,
            cat_id,
            design_weight: 0.0,
            value: 0.0,
        };
        tve.set_design_weight(design_weight)?;
        tve.set_value(value)?;
        Ok(tve)
    }
    /// Returns the tract ID.
    #[inline]
    pub fn tract_id(&self) -> &TID { &self.tract_id }
    /// Sets the tract ID.
    #[inline]
    pub fn set_tract_id(&mut self, tract_id: TID) { self.tract_id = tract_id; }
    /// Returns the category ID.
    #[inline]
    pub fn cat_id(&self) -> &CID { &self.cat_id }
    /// Sets the category ID.
    #[inline]
    pub fn set_cat_id(&mut self, cat_id: CID) { self.cat_id = cat_id; }
    /// Returns the design weight.
    #[must_use]
    #[inline]
    pub fn design_weight(&self) -> f64 { self.design_weight }
    /// Sets the design weight.
    /// # Errors
    /// Returns an error if `design_weight` is non-positive.
    #[inline]
    pub fn set_design_weight(&mut self, design_weight: f64) -> TractResult<()> {
        if !design_weight.is_finite() && design_weight <= 0.0 {
            Err(TractError::InvalidDesignWeight(design_weight))
        } else {
            self.design_weight = design_weight;
            Ok(())
        }
    }
    /// Returns the measured value.
    #[must_use]
    #[inline]
    pub fn value(&self) -> f64 { self.value }
    /// Returns the design-weighted measured value.
    #[must_use]
    #[inline]
    pub fn weighted_value(&self) -> f64 { self.value * self.design_weight }
    /// Sets the measured value.
    /// # Errors
    /// Returns an error if `value` is negative.
    #[inline]
    pub fn set_value(&mut self, value: f64) -> TractResult<()> {
        if !value.is_finite() && value < 0.0 {
            Err(TractError::InvalidValue(value))
        } else {
            self.value = value;
            Ok(())
        }
    }
}
impl<TID, CID> Borrow<TID> for TractValueEntry<TID, CID> {
    #[inline]
    fn borrow(&self) -> &TID { &self.tract_id }
}

#[must_use]
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Area(f64);
impl Area {
    /// Checks if `area` is a valid `Area`.
    /// # Errors
    /// Returns an error if `area` is non-positive.
    #[inline]
    fn check(area: f64) -> TractResult<()> {
        if !area.is_finite() && area <= 0.0 {
            Err(TractError::InvalidArea(area))
        } else {
            Ok(())
        }
    }
    /// Constructs a new `Area` from `area`
    /// # Errors
    /// Returns an error if `area` is non-positive.
    #[inline]
    pub fn new(area: f64) -> TractResult<Self> {
        Self::check(area)?;
        Ok(Self(area))
    }
    /// Returns the area as a `f64`
    #[must_use]
    #[inline]
    pub fn get(&self) -> f64 { self.0 }
    /// Sets the internal area to `area`.
    /// # Errors
    /// Returns an error if `area` is non-positive.
    #[inline]
    pub fn set(&mut self, area: f64) -> TractResult<()> {
        Self::check(area)?;
        self.0 = area;
        Ok(())
    }
}
impl Deref for Area {
    type Target = f64;
    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl TryFrom<f64> for Area {
    type Error = TractError;
    #[inline]
    fn try_from(value: f64) -> Result<Self, Self::Error> { Self::new(value) }
}
impl Display for Area {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult { write!(f, "Area ({})", self.0) }
}

#[must_use]
#[derive(Clone, Debug)]
pub struct Tract<TID, PID, CID> {
    /// Tract identifier
    tract_id: TID,
    /// Smallest PSU that includes this tract
    psu_id: PID,
    /// Category totals
    totals: CategoryStore<CID>,
    /// Tract area
    area: Area,
}
impl<TID, PID, CID> Tract<TID, PID, CID> {
    /// Constructs a new tract info container
    #[inline]
    pub fn new(header: TractHeaderEntry<TID, PID>, area: Area) -> Self {
        let TractHeaderEntry { tract_id, psu_id } = header;
        let totals = CategoryStore::new();
        Self {
            tract_id,
            psu_id,
            totals,
            area,
        }
    }
    /// Returns the identifier of the tract.
    #[inline]
    pub fn tract_id(&self) -> &TID { &self.tract_id }
    /// Returns the smallest PSU that includes this tract.
    #[inline]
    pub fn psu_id(&self) -> &PID { &self.psu_id }
    /// Returns the area of the tract
    #[inline]
    pub fn area(&self) -> &Area { &self.area }
    /// Returns the plot values of the tract.
    #[inline]
    pub fn totals(&self) -> &CategoryStore<CID> { &self.totals }
    /// Adds an entry to the tract
    #[inline]
    pub fn add(&mut self, entry: TractValueEntry<TID, CID>)
    where
        CID: Ord,
    {
        let value = entry.weighted_value() / self.area.get();
        self.totals.add_value((entry.cat_id, value));
    }
}
impl<TID, PID, CID> Borrow<TID> for Tract<TID, PID, CID> {
    #[inline]
    fn borrow(&self) -> &TID { &self.tract_id }
}

/// A set of tracts
#[must_use]
#[derive(Clone, Debug, Default)]
pub struct TractStore<TID, PID, CID> {
    /// Set of tracts
    tracts: FxHashMap<TID, Tract<TID, PID, CID>>,
}
impl<TID, PID, CID> TractStore<TID, PID, CID> {
    /// Constructs a new, empty, store with some allocated `capacity`.
    #[inline]
    pub fn with_capactity(capacity: usize) -> Self {
        Self {
            tracts: FxHashMap::with_capacity_and_hasher(capacity, FxBuildHasher),
        }
    }
    /// Inserts a `tract` into the store.
    /// # Errors
    /// Returns an error if the tracts ID already exists.
    #[inline]
    pub fn insert(&mut self, tract: Tract<TID, PID, CID>) -> TractResult<()>
    where
        TID: Copy + Display + Eq + Hash,
    {
        let id = *tract.tract_id();
        match self.tracts.entry(id) {
            Entry::Vacant(e) => {
                e.insert(tract);
                Ok(())
            }
            Entry::Occupied(_) => Err(TractError::TractIdCollision(id.to_string())),
        }
    }
    /// Returns a reference to a tract by ID.
    #[inline]
    pub fn get(&self, tract_id: &TID) -> Option<&Tract<TID, PID, CID>>
    where
        TID: Eq + Hash,
    {
        self.tracts.get(tract_id)
    }
    /// Returns the stored value of a category in a tract by IDs.
    /// If no `cat_id` is found for the tract, the value defaults to `0.0`.
    /// # Errors
    /// Returns an error if the tract ID cannot be found in the store.
    #[inline]
    pub fn get_category_value(&self, tract_id: &TID, cat_id: &CID) -> TractResult<f64>
    where
        TID: Display + Eq + Hash,
        CID: Ord,
    {
        let tract = self
            .get(tract_id)
            .ok_or_else(|| TractError::TractIdNotFound(tract_id.to_string()))?;
        Ok(tract.totals().get(cat_id).copied().unwrap_or(0.0))
    }
    /// Returns an iterator over the tracts.
    #[must_use]
    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = (&TID, &Tract<TID, PID, CID>)> + Clone {
        self.tracts.iter()
    }
    /// Returns the number of tracts in the store.
    #[must_use]
    #[inline]
    pub fn len(&self) -> usize { self.tracts.len() }
    /// Returns `true` if there is no tracts in the store.
    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool { self.tracts.is_empty() }
    /// Adds the content of `entry` to a tract in the store.
    /// # Errors
    /// Returns an error if the tract cannot be found.
    #[inline]
    pub fn add_value_from_entry(&mut self, entry: TractValueEntry<TID, CID>) -> TractResult<()>
    where
        TID: Copy + Display + Eq + Hash,
        CID: Ord,
    {
        let tract_id = *entry.tract_id();
        self.tracts
            .get_mut(&tract_id)
            .map(|tract| tract.add(entry))
            .ok_or(TractError::TractIdNotFound(tract_id.to_string()))?;
        Ok(())
    }
}
