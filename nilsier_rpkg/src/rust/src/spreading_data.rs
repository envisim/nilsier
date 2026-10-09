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

//! Spreading data.

use std::num::NonZeroUsize;

use envisim_utils::matrix::Dimensions;
use envisim_utils::utils::{
    DataView,
    PointSet,
};

use super::TractList;
use crate::sexp::*;
use crate::tract_id::*;

/// Struct mapping matrix with ids.
#[must_use]
pub struct SpreadingData {
    /// ids.
    ids: IntegerSexpFatPtr,
    /// data in matrix form.
    data: RMatrix,
}
impl SpreadingData {
    /// Constructs new.
    #[inline]
    pub fn new(list: TractList) -> Option<Self> {
        list.auxiliaries.map(|data| Self {
            ids: list.tracts,
            data,
        })
    }
    /// Returns an iterator over the ids.
    #[inline]
    pub fn id_iter(&self) -> impl ExactSizeIterator<Item = TractId> + Clone + use<'_> {
        TractId::from_sexp(&self.ids)
    }
    /// Returns true if `id` exists in the collection.
    #[inline]
    pub fn contains(&self, id: TractId) -> bool { id.internal() < self.ids.len() }
}

impl PointSet for SpreadingData {
    type Value = f64;
    type Id = TractId;
    #[inline]
    fn len(&self) -> NonZeroUsize { self.data.nrow() }
    #[inline]
    fn ids(&self) -> impl ExactSizeIterator<Item = Self::Id> + Clone { self.id_iter() }
    #[inline]
    fn dimensions(&self) -> NonZeroUsize { self.data.ncol() }
    #[inline]
    fn contains(&self, id: Self::Id) -> bool { self.contains(id) }
    #[inline]
    fn coord(&self, id: Self::Id, dim: usize) -> Option<&Self::Value> {
        self.data.coord(id.internal(), dim)
    }
    #[inline]
    unsafe fn coord_unchecked(&self, id: Self::Id, dim: usize) -> &Self::Value {
        self.data.coord_unchecked(id.internal(), dim)
    }
    #[inline]
    fn coords(
        &self,
        id: Self::Id,
    ) -> Option<impl ExactSizeIterator<Item = &Self::Value> + DoubleEndedIterator + Clone> {
        self.data.coords(id.internal())
    }
}
