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

//! Matrix utils

#![expect(dead_code, reason = "not all impl uses every method")]

use std::slice::{
    Iter,
    from_raw_parts,
};

use envisim_utils::matrix::MatrixBase;
use envisim_utils::utils::SliceView;
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

/// Constructs a new fat ptr variant of a `Sexp` variant
macro_rules! impl_sexp_fat_ptr {
    ($name:ident, $sexp:ident, $data:ty, $access:ident) => {
        /// Wrapper for `$sexp`
        #[must_use]
        pub struct $name {
            /// Underlying reference to data
            #[expect(dead_code, reason = "structure should own data")]
            sexp: $sexp,
            /// Pointer to data
            ptr: *const $data,
            /// Length of data
            len: usize,
        }

        impl $name {
            /// Constructs the fat pointer from a `$sexp`
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
            /// Returns the number of elements of the sexp
            #[inline]
            pub fn len(&self) -> usize { self.len }
            /// Returns true if the sexp is empty
            #[inline]
            pub fn is_empty(&self) -> bool { self.len == 0 }
            /// Returns true if the sexp is empty
            #[inline]
            pub fn iter(&self) -> Iter<'_, $data> { self.data().iter() }
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

        impl SliceView for $name {
            type Elem = $data;
            #[inline]
            fn data(&self) -> &[Self::Elem] {
                if self.len == 0 {
                    return &[];
                }
                // SAFETY:
                // Reconstructing the slice from the fat pointer mimicking
                // savvy::RealSexp::as_slice() behaviour.
                unsafe { from_raw_parts(self.ptr, self.len) }
            }
        }
    };
}

impl_sexp_fat_ptr!(RealSexpFatPtr, RealSexp, f64, REAL);
impl_sexp_fat_ptr!(IntegerSexpFatPtr, IntegerSexp, i32, INTEGER);

/// Constructs a `RMatrix` from `RealSexp`
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
