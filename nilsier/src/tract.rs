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
use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};
use std::ops::Deref;

use anyhow::{
    Result,
    ensure,
};
use rustc_hash::{
    FxBuildHasher,
    FxHashMap,
};

use crate::category::{
    CatId,
    CategoryStore,
};
use crate::macros::newtype_id;
use crate::psu::PsuId;

#[non_exhaustive]
#[derive(Debug, Clone, Copy)]
pub enum TractError {
    TractIdNotFound(TractId),
    TractIdCollision(TractId),
    InvalidArea(f64),
    InvalidDesignWeight(f64),
    InvalidValue(f64),
}
#[expect(clippy::absolute_paths, reason = "conflict with anyhow")]
impl std::error::Error for TractError {}
impl Display for TractError {
    #[expect(clippy::enum_glob_use, reason = "simple function")]
    #[inline]
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        use TractError::*;
        match self {
            TractIdNotFound(id) => write!(f, "{id} not found"),
            TractIdCollision(id) => write!(f, "{id} already exists"),
            InvalidArea(area) => write!(f, "{area} must be positive"),
            InvalidDesignWeight(dw) => write!(f, "Design weight ({dw}) must be positive"),
            InvalidValue(val) => write!(f, "{val} must be non-negative"),
        }
    }
}

/// Connects tract to a PSU
#[must_use]
#[derive(Clone, Copy, Debug)]
pub struct TractHeaderEntry {
    /// tract id of header
    tract_id: TractId,
    /// psu id of header
    psu_id: PsuId,
}
impl TractHeaderEntry {
    /// Constructs a new header entry
    #[inline]
    pub fn new(tract_id: TractId, psu_id: PsuId) -> Self { Self { tract_id, psu_id } }
    /// Returns the tract ID of the header
    #[inline]
    pub fn tract_id(&self) -> TractId { self.tract_id }
    /// Sets the tract ID of the header
    #[inline]
    pub fn set_tract_id(&mut self, tract_id: TractId) { self.tract_id = tract_id; }
    /// Returns the PSU ID of the header
    #[inline]
    pub fn psu_id(&self) -> PsuId { self.psu_id }
    /// Sets the PSU ID of the header
    #[inline]
    pub fn set_psu_id(&mut self, psu_id: PsuId) { self.psu_id = psu_id; }
}
impl Borrow<TractId> for TractHeaderEntry {
    #[inline]
    fn borrow(&self) -> &TractId { &self.tract_id }
}

/// Contains values from a tract
#[must_use]
#[derive(Clone, Copy, Debug)]
pub struct TractValueEntry {
    /// Tract ID
    tract_id: TractId,
    /// Category ID
    cat_id: CatId,
    /// Design weight
    design_weight: f64,
    /// Measured value
    value: f64,
}
impl TractValueEntry {
    /// Constructs a new entry.
    /// # Errors
    /// Returns an error if `design_weight` is non-positive or `value` is negative.
    #[inline]
    pub fn new(tract_id: TractId, cat_id: CatId, design_weight: f64, value: f64) -> Result<Self> {
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
    pub fn tract_id(&self) -> TractId { self.tract_id }
    /// Sets the tract ID.
    #[inline]
    pub fn set_tract_id(&mut self, tract_id: TractId) { self.tract_id = tract_id; }
    /// Returns the category ID.
    #[inline]
    pub fn cat_id(&self) -> CatId { self.cat_id }
    /// Sets the category ID.
    #[inline]
    pub fn set_cat_id(&mut self, cat_id: CatId) { self.cat_id = cat_id; }
    /// Returns the design weight.
    #[must_use]
    #[inline]
    pub fn design_weight(&self) -> f64 { self.design_weight }
    /// Sets the design weight.
    /// # Errors
    /// Returns an error if `design_weight` is non-positive.
    #[inline]
    pub fn set_design_weight(&mut self, design_weight: f64) -> Result<()> {
        ensure!(
            design_weight.is_finite() && 0.0 < design_weight,
            TractError::InvalidDesignWeight(design_weight)
        );
        self.design_weight = design_weight;
        Ok(())
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
    pub fn set_value(&mut self, value: f64) -> Result<()> {
        ensure!(
            value.is_finite() && 0.0 <= value,
            TractError::InvalidValue(value)
        );
        self.value = value;
        Ok(())
    }
}
impl Borrow<TractId> for TractValueEntry {
    #[inline]
    fn borrow(&self) -> &TractId { &self.tract_id }
}

newtype_id!(TractId, i32, "Tract ID");

#[must_use]
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Area(f64);
impl Area {
    /// Checks if `area` is a valid `Area`.
    /// # Errors
    /// Returns an error if `area` is non-positive.
    #[inline]
    fn check(area: f64) -> Result<()> {
        ensure!(
            area.is_finite() && 0.0 < area,
            TractError::InvalidArea(area)
        );
        Ok(())
    }
    /// Constructs a new `Area` from `area`
    /// # Errors
    /// Returns an error if `area` is non-positive.
    #[inline]
    pub fn new(area: f64) -> Result<Self> {
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
    pub fn set(&mut self, area: f64) -> Result<()> {
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
#[expect(clippy::absolute_paths, reason = "conflicting imports")]
impl TryFrom<f64> for Area {
    type Error = anyhow::Error;
    #[inline]
    fn try_from(value: f64) -> std::result::Result<Self, Self::Error> { Self::new(value) }
}
impl Display for Area {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult { write!(f, "Area ({})", self.0) }
}

#[must_use]
#[derive(Clone, Debug)]
pub struct Tract {
    /// Tract identifier
    tract_id: TractId,
    /// Smallest PSU that includes this tract
    psu_id: PsuId,
    /// Category totals
    totals: CategoryStore,
    /// Tract area
    area: Area,
}
impl Tract {
    /// Constructs a new tract info container
    #[inline]
    pub fn new(header: TractHeaderEntry, area: Area) -> Self {
        Self {
            tract_id: header.tract_id(),
            psu_id: header.psu_id(),
            area,
            totals: CategoryStore::default(),
        }
    }
    /// Returns the identifier of the tract.
    #[inline]
    pub fn tract_id(&self) -> TractId { self.tract_id }
    /// Returns the smallest PSU that includes this tract.
    #[inline]
    pub fn psu_id(&self) -> PsuId { self.psu_id }
    /// Returns the area of the tract
    #[inline]
    pub fn area(&self) -> Area { self.area }
    /// Returns the plot values of the tract.
    #[inline]
    pub fn totals(&self) -> &CategoryStore { &self.totals }
    /// Adds an entry to the tract
    #[inline]
    pub fn add(&mut self, entry: TractValueEntry) {
        let value = entry.weighted_value() / self.area.get();
        self.totals.add_value((entry.cat_id(), value));
    }
}
impl Borrow<TractId> for Tract {
    #[inline]
    fn borrow(&self) -> &TractId { &self.tract_id }
}

/// A set of tracts
#[must_use]
#[derive(Clone, Debug, Default)]
pub struct TractStore {
    /// Set of tracts
    tracts: FxHashMap<TractId, Tract>,
}
impl TractStore {
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
    pub fn insert(&mut self, tract: Tract) -> Result<()> {
        ensure!(
            !self.tracts.contains_key(&tract.tract_id()),
            TractError::TractIdCollision(tract.tract_id())
        );
        self.tracts.insert(tract.tract_id(), tract);
        Ok(())
    }
    /// Returns a reference to a tract by ID.
    /// # Errors
    /// Returns an error if the tract ID cannot be found in the store.
    #[inline]
    pub fn get(&self, tract_id: TractId) -> Result<&Tract> {
        self.tracts
            .get(&tract_id)
            .ok_or_else(|| TractError::TractIdNotFound(tract_id).into())
    }
    /// Returns the stored value of a category in a tract by IDs.
    /// # Errors
    /// Returns an error if the tract ID or if the category ID cannot be found in the store.
    #[inline]
    pub fn get_category_value(&self, tract_id: TractId, cat_id: CatId) -> Result<f64> {
        self.get(tract_id)
            .and_then(|tract| tract.totals().get(cat_id))
    }
    /// Returns an iterator over the tracts.
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = (&TractId, &Tract)> + Clone { self.tracts.iter() }
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
    pub fn add_value_from_entry(&mut self, entry: TractValueEntry) -> Result<()> {
        let tract_id = entry.tract_id();
        self.tracts
            .get_mut(&tract_id)
            .map(|tract| tract.add(entry))
            .ok_or(TractError::TractIdNotFound(tract_id))?;
        Ok(())
    }
}
