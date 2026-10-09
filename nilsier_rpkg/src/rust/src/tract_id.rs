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

//! Tract id module.

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

use envisim_utils::utils::DataView;

use crate::sexp::*;

// We're sending around internal and external ids. The alternative would be to have spreading_data
// store a hashmap and lookup internal id every time, but this would probably cost more.
/// Id pair.
#[derive(Copy, Clone, Debug)]
pub struct TractId {
    /// Internal id (order).
    internal: usize,
    /// Id given by user.
    id: i32,
}
impl TractId {
    /// Return the internal id order.
    #[inline]
    pub fn internal(&self) -> usize { self.internal }
    /// Return the id.
    #[expect(dead_code, reason = "accessor")]
    #[inline]
    pub fn id(&self) -> i32 { self.id }
    /// Constructs an iterator from an `sexp` of ids.
    #[inline]
    pub fn from_sexp(
        sexp: &IntegerSexpFatPtr,
    ) -> impl ExactSizeIterator<Item = Self> + Clone + use<'_> {
        sexp.entries().map(|(id, v)| TractId::from((id, *v)))
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
