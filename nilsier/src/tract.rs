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
use std::convert::Infallible;
use std::fmt::Display;
use std::hash::Hash;

use rustc_hash::{
    FxBuildHasher,
    FxHashMap,
};
use thiserror::Error;

use crate::category::CategoryStore;
use crate::utils::Area;

/// Tract errors
#[non_exhaustive]
#[derive(Error, Debug)]
pub enum TractError {
    /// Design weight must be positive
    #[error("Design weight ({0}) must be positive")]
    InvalidDesignWeight(f64),
    /// Value must be non-negative
    #[error("Value ({0}) must be non-negative")]
    InvalidValue(f64),
    /// ID not found
    #[error("Tract ID ({0}) not found")]
    IdNotFound(String),
    /// ID collision
    #[error("Tract ID ({0}) collision")]
    IdCollision(String),
}
impl TractError {
    /// Returns `IdNotFound` error for TID
    #[must_use]
    #[inline]
    fn id_not_found<TID>(id: &TID) -> Self
    where
        TID: Display,
    {
        Self::IdNotFound(id.to_string())
    }
    /// Returns `IdCollision` error for TID
    #[must_use]
    #[inline]
    fn id_collision<TID>(id: &TID) -> Self
    where
        TID: Display,
    {
        Self::IdCollision(id.to_string())
    }
}
impl From<Infallible> for TractError {
    #[expect(
        clippy::unreachable,
        reason = "Infallible should never be converted to an Error"
    )]
    #[inline]
    fn from(_: Infallible) -> Self { unreachable!() }
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
    pub fn new(
        tract_id: TID,
        cat_id: CID,
        design_weight: f64,
        value: f64,
    ) -> Result<Self, TractError> {
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
    pub fn set_design_weight(&mut self, design_weight: f64) -> Result<(), TractError> {
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
    pub fn set_value(&mut self, value: f64) -> Result<(), TractError> {
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

/// A tract
#[must_use]
#[derive(Clone, Debug)]
pub struct Tract<TID, PID, CID> {
    /// Tract identifier
    tract_id: TID,
    /// Smallest PSU that includes this tract
    psu_id: PID,
    /// Category totals
    totals: CategoryStore<CID, f64>,
}
impl<TID, PID, CID> Tract<TID, PID, CID> {
    /// Constructs a new tract info container
    #[inline]
    pub fn new(tract_id: TID, psu_id: PID) -> Self {
        Self {
            tract_id,
            psu_id,
            totals: CategoryStore::new(),
        }
    }
    /// Returns the identifier of the tract.
    #[inline]
    pub fn tract_id(&self) -> &TID { &self.tract_id }
    /// Returns the smallest PSU that includes this tract.
    #[inline]
    pub fn psu_id(&self) -> &PID { &self.psu_id }
    /// Returns the plot values of the tract.
    #[inline]
    pub fn totals(&self) -> &CategoryStore<CID, f64> { &self.totals }
    /// Adds an entry to the tract
    #[inline]
    pub fn add(&mut self, entry: TractValueEntry<TID, CID>, area: Area)
    where
        CID: Ord,
    {
        let value = entry.weighted_value() / area.get();
        self.totals.add_value((entry.cat_id, value));
    }
}
impl<TID, PID, CID> Borrow<TID> for Tract<TID, PID, CID> {
    #[inline]
    fn borrow(&self) -> &TID { &self.tract_id }
}

/// A set of tracts
#[must_use]
#[derive(Clone, Debug)]
pub struct TractStore<TID, PID, CID> {
    /// Set of tracts
    tracts: FxHashMap<TID, Tract<TID, PID, CID>>,
    /// Tract area
    tract_area: Area,
}
impl<TID, PID, CID> TractStore<TID, PID, CID> {
    /// Constructs a new, empty, store with some allocated `capacity`.
    #[inline]
    pub fn with_capacity(capacity: usize, tract_area: Area) -> Self {
        Self {
            tracts: FxHashMap::with_capacity_and_hasher(capacity, FxBuildHasher),
            tract_area,
        }
    }
    /// Inserts a `tract` into the store.
    /// # Errors
    /// Returns an error if the tracts ID already exists.
    #[inline]
    pub fn insert(&mut self, tract: Tract<TID, PID, CID>) -> Result<(), TractError>
    where
        TID: Copy + Display + Eq + Hash,
    {
        let id = *tract.tract_id();
        match self.tracts.entry(id) {
            Entry::Vacant(e) => {
                e.insert(tract);
                Ok(())
            }
            Entry::Occupied(_) => Err(TractError::id_collision(&id)),
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
    #[inline]
    pub fn get_category_value(&self, tract_id: &TID, cat_id: &CID) -> Option<f64>
    where
        TID: Display + Eq + Hash,
        CID: Ord,
    {
        let tract = self.get(tract_id)?;
        Some(tract.totals().get(cat_id).copied().unwrap_or(0.0))
    }
    /// Returns the area of a tract
    #[inline]
    pub fn area(&self) -> &Area { &self.tract_area }
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
    pub fn add_value_from_entry(
        &mut self,
        entry: TractValueEntry<TID, CID>,
    ) -> Result<(), TractError>
    where
        TID: Copy + Display + Eq + Hash,
        CID: Ord,
    {
        let tract_id = entry.tract_id();
        let tract = self
            .tracts
            .get_mut(tract_id)
            .ok_or(TractError::id_not_found(tract_id))?;
        tract.add(entry, self.tract_area);
        Ok(())
    }
}
