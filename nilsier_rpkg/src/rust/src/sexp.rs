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

//! Matrix utils.

#![expect(dead_code, reason = "not all impl uses every method")]

use std::slice::from_raw_parts;

use envisim_utils::matrix::MatrixBase;
use envisim_utils::utils::{
    ConstructableDataView,
    ContiguousDataView,
    DataView,
    SliceView,
};
use num_traits::ToPrimitive;
use savvy::{
    IntegerSexp,
    RealSexp,
    Sexp,
    savvy_err,
};
use savvy_ffi::{
    INTEGER,
    REAL,
};

/// Constructs a new fat ptr variant of a `Sexp` variant.
macro_rules! impl_sexp_fat_ptr {
    ($name:ident, $sexp:ident, $data:ty, $access:ident) => {
        /// Wrapper for `$sexp`.
        #[must_use]
        pub struct $name {
            /// Underlying reference to data.
            #[expect(dead_code, reason = "structure should own data")]
            sexp: $sexp,
            /// Pointer to data.
            ptr: *const $data,
            /// Length of data.
            len: usize,
        }

        impl $name {
            /// Constructs the fat pointer from a `$sexp`.
            #[inline]
            pub fn from_sexp(sexp: $sexp) -> savvy::Result<Self> {
                if sexp.is_empty() {
                    return Err(savvy_err!("sexp is empty"));
                }

                Ok(Self {
                    // SAFETY:
                    // Retrieving a pointer to the underlying data is mimicking
                    // savvy::RealSexp::as_slice() behaviour.
                    ptr: unsafe { $access(sexp.0) },
                    len: sexp.len(),
                    sexp,
                })
            }
        }

        impl TryFrom<Sexp> for $name {
            type Error = savvy::Error;
            #[inline]
            fn try_from(sexp: Sexp) -> Result<Self, Self::Error> {
                Self::from_sexp(sexp.try_into()?)
            }
        }

        impl TryFrom<$sexp> for $name {
            type Error = savvy::Error;
            #[inline]
            fn try_from(sexp: $sexp) -> Result<Self, Self::Error> { Self::from_sexp(sexp) }
        }

        impl DataView for $name {
            type Id = usize;
            type Value = $data;
            #[inline]
            fn ids(&self) -> impl ExactSizeIterator<Item = Self::Id> + Clone { 0..self.len }
            #[inline]
            fn values(&self) -> impl ExactSizeIterator<Item = &Self::Value> + Clone {
                self.slice().iter()
            }
            #[inline]
            fn entries(&self) -> impl ExactSizeIterator<Item = (Self::Id, &Self::Value)> + Clone {
                self.slice().iter().enumerate()
            }
            #[inline]
            fn contains(&self, id: Self::Id) -> bool { id < self.len }
            #[inline]
            fn get(&self, id: Self::Id) -> Option<&Self::Value> { self.slice().get(id) }
            #[inline]
            fn len(&self) -> usize { self.len }
        }
        impl SliceView for $name {
            #[inline]
            fn slice(&self) -> &[Self::Value] {
                if self.len == 0 {
                    return &[];
                }
                // SAFETY:
                // Reconstructing the slice from the fat pointer mimicking
                // savvy::RealSexp::as_slice() behaviour.
                unsafe { from_raw_parts(self.ptr, self.len) }
            }
        }
        impl ContiguousDataView for $name {}
        impl ConstructableDataView for $name {
            type ConstructableContainer<V> = Box<[V]>;
            #[inline]
            fn from_iter<I, V>(iter: I) -> Self::ConstructableContainer<V>
            where
                I: Iterator<Item = (Self::Id, V)>,
            {
                iter.map(|(_, v)| v).collect()
            }
            #[inline]
            fn try_from_iter<I, V, E>(iter: I) -> Result<Self::ConstructableContainer<V>, E>
            where
                I: Iterator<Item = Result<(Self::Id, V), E>>,
            {
                iter.map(|r| r.map(|(_, v)| v)).collect()
            }
        }
    };
}

impl_sexp_fat_ptr!(RealSexpFatPtr, RealSexp, f64, REAL);
impl_sexp_fat_ptr!(IntegerSexpFatPtr, IntegerSexp, i32, INTEGER);

/// Constructs a `RMatrix` from `RealSexp`.
#[inline]
pub fn realsexp_to_matrix(sexp: RealSexp) -> savvy::Result<RMatrix> {
    let rows = match sexp
        .get_dim()
        .ok_or_else(|| savvy_err!("object have no dimensions"))?
    {
        [_, _, _, ..] => Err(savvy_err!("object have too many dimensions")),
        [r, ..] => r.to_usize().ok_or_else(|| savvy_err!("i32 -> usize")),
        _ => Err(savvy_err!("object have no dimensions")),
    }?;

    Ok(MatrixBase::new(sexp.try_into()?, rows).expect("rows to be NonZeroUsize"))
}

/// Type alias for `Matrix` using `RMatrixData`.
pub type RMatrix = MatrixBase<RealSexpFatPtr>;
