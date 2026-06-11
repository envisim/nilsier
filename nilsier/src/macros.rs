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

//! Macro utilities

#[macro_export]
macro_rules! newtype_id {
    ($name:ident, $inner:ty, $display:expr) => {
        #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
        #[repr(transparent)]
        #[must_use]
        pub struct $name($inner);

        impl $name {
            pub(crate) const NAME: &'static str = $display;
            #[inline]
            pub fn new(value: $inner) -> Self { Self(value) }
            #[must_use]
            #[inline]
            pub fn get(&self) -> $inner { self.0 }
            #[inline]
            pub fn to_string(&self) -> String { format!("{} ({})", Self::NAME, self.0) }
        }

        impl std::ops::Deref for $name {
            type Target = $inner;
            #[inline]
            fn deref(&self) -> &Self::Target { &self.0 }
        }

        impl std::ops::DerefMut for $name {
            #[inline]
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
        }

        impl From<$inner> for $name {
            #[inline]
            fn from(value: $inner) -> Self { Self::new(value) }
        }

        impl From<$name> for $inner {
            #[inline]
            fn from(value: $name) -> Self { value.0 }
        }

        impl std::fmt::Display for $name {
            #[inline]
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", self.to_string())
            }
        }
    };
}
pub use newtype_id;

#[macro_export]
macro_rules! newtype_id_generic {
    ($name:ident, $display:expr) => {
        #[repr(transparent)]
        #[must_use]
        #[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub struct PsuId<T>(T)
        where
            T: Identifier;

        impl<T> $name<T>
        where
            T: Identifier,
        {
            /// Constructs a new identifier from a `value`
            #[inline]
            pub fn new(value: T) -> Self { Self(value) }
            /// Returns the inner value
            #[must_use]
            #[inline]
            pub fn get(&self) -> T { self.0 }
        }

        impl<T> std::ops::Deref for $name<T>
        where
            T: Identifier,
        {
            type Target = T;
            #[inline]
            fn deref(&self) -> &Self::Target { &self.0 }
        }
        impl<T> std::ops::DerefMut for $name<T>
        where
            T: Identifier,
        {
            #[inline]
            fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
        }

        impl<T> From<T> for $name<T>
        where
            T: Identifier,
        {
            #[inline]
            fn from(value: T) -> Self { Self::new(value) }
        }

        impl<T> From<$name> for T
        where
            T: Identifier,
        {
            #[inline]
            fn from(value: $name) -> T { value.0 }
        }

        impl<T> std::fmt::Debug for $name<T>
        where
            T: Identifier,
        {
            #[inline]
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} ({:?})", $display, self.0)
            }
        }

        impl<T> std::fmt::Display for $name<T>
        where
            T: Identifier,
        {
            #[inline]
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{} ({})", $display, self.0)
            }
        }
    };
}
pub use newtype_id_generic;
