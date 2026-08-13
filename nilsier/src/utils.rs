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

//! Utilities

use std::fmt::{
    Display,
    Formatter,
    Result as FmtResult,
};
use std::ops::Deref;

use envisim_utils::kd_tree::TreeError;
use thiserror::Error;

/// Nils errors
#[non_exhaustive]
#[derive(Error, Debug)]
pub enum NilsError {
    /// Categories must be added
    #[error("no categories have been added")]
    NoCategoriesAdded,
    /// The supplied number of tracts does not match the prescribed number of tracts
    #[error("the supplied number of tracts {0} does not match the prescribed number of tracts {1}")]
    IncorrectNumberOfTracts(u32, u32),
    /// See [`TreeError`]
    #[error(transparent)]
    Tree(#[from] TreeError),
}

/// An area representation
#[must_use]
#[repr(transparent)]
#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub struct Area(f64);
impl Area {
    /// Checks if `area` is a valid `Area`.
    #[must_use]
    #[inline]
    fn check(area: f64) -> bool { area.is_finite() && area > 0.0 }
    /// Constructs a new `Area` from `area`
    /// # Errors
    /// Returns an error if `area` is non-positive.
    #[must_use]
    #[inline]
    pub fn new(area: f64) -> Option<Self> { Self::check(area).then_some(Self(area)) }
    /// Returns the area as a `f64`
    #[must_use]
    #[inline]
    pub fn get(&self) -> f64 { self.0 }
}
impl Deref for Area {
    type Target = f64;
    #[inline]
    fn deref(&self) -> &Self::Target { &self.0 }
}
impl Display for Area {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult { write!(f, "Area ({})", self.0) }
}
