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

//! Handling of categories (or classes) defined on plots.

use std::mem::replace;

use thiserror::Error;

#[non_exhaustive]
#[derive(Error, Debug, Clone)]
pub enum CatError {
    #[error("CAT ID ({0}) not found")]
    CatIdNotFound(String),
    #[error("CAT ID ({0}) already exist")]
    CatIdCollision(String),
}

#[must_use]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct CatIdPair<CID>(CID, CID);
impl<CID> CatIdPair<CID> {
    /// Constructs a new category pair
    #[inline]
    pub fn new(cat_a: CID, cat_b: CID) -> Self
    where
        CID: Ord,
    {
        if cat_b < cat_a {
            Self(cat_b, cat_a)
        } else {
            Self(cat_a, cat_b)
        }
    }
    /// Returns the first member of the pair
    #[inline]
    pub fn get_a(&self) -> CID
    where
        CID: Copy,
    {
        self.0
    }
    /// Returns the second member of the pair
    #[inline]
    pub fn get_b(&self) -> CID
    where
        CID: Copy,
    {
        self.1
    }
}
impl<CID> From<(CID, CID)> for CatIdPair<CID>
where
    CID: Ord,
{
    #[inline]
    fn from((cat_a, cat_b): (CID, CID)) -> Self { Self::new(cat_a, cat_b) }
}

/// PSU of first occurance, i.e. at which sample the category starts to be inventoried
#[must_use]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct CategoryValue<CID> {
    /// Category (key)
    cat_id: CID,
    /// Value
    value: f64,
}
impl<CID> CategoryValue<CID> {
    /// Constructs a new container without value
    #[inline]
    pub fn new(cat_id: CID, value: Option<f64>) -> Self where {
        Self {
            cat_id,
            value: value.unwrap_or(0.0),
        }
    }
    /// Returns the category id
    #[inline]
    pub fn cat_id(&self) -> CID
    where
        CID: Copy,
    {
        self.cat_id
    }
    /// Reurns the value
    #[must_use]
    #[inline]
    pub fn get(&self) -> f64 { self.value }
    /// Returns a mutable reference to the value
    #[must_use]
    #[inline]
    pub fn get_mut(&mut self) -> &mut f64 { &mut self.value }
    /// Returns `true` if the value is 0.0
    #[must_use]
    #[inline]
    fn is_nil(&self) -> bool { !self.value.is_finite() || self.value <= 0.0 }
    /// Adds to the value
    #[inline]
    pub fn add(&mut self, value: f64) { self.value += value; }
}
impl<ID> From<(ID, Option<f64>)> for CategoryValue {
    #[inline]
    fn from((cat_id, value): (ID, Option<f64>)) -> Self { Self::new(cat_id, value) }
}
impl<ID> From<(ID, f64)> for CategoryValue
where
    ID: Into<CatId>,
{
    #[inline]
    fn from((cat_id, value): (ID, f64)) -> Self { Self::new(cat_id, Some(value)) }
}

/// Stores multiple categories in category-order
#[must_use]
#[derive(Clone, Debug, Default)]
pub struct CategoryStore {
    /// Store
    store: Vec<CategoryValue>,
}
impl CategoryStore {
    /// Returns the index of a category
    /// # Errors
    /// Returns an error if the category does not exist
    #[inline]
    fn index_of_category(&self, cat_id: CatId) -> Result<usize> {
        self.store
            .binary_search_by_key(&cat_id, CategoryValue::cat_id)
            .map_err(|_| CatError::CatIdNotFound(cat_id).into())
    }
    /// Returns the value of a category.
    /// # Errors
    /// Reuturns an error if the category does not exist.
    #[inline]
    pub fn get(&self, cat_id: CatId) -> Result<f64> {
        self.index_of_category(cat_id)
            .map(|idx| self.store[idx].get())
    }
    /// Returns a mutable reference to the value of a category.
    /// # Errors
    /// Returns an error if the category does not exist
    #[inline]
    pub fn get_mut(&mut self, cat_id: CatId) -> Result<&mut f64> {
        self.index_of_category(cat_id)
            .map(|idx| self.store[idx].get_mut())
    }
    /// Inserts a category-value pair into the store.
    /// Returns `Some`, if a pair with the same category id already exists.
    #[inline]
    pub fn insert<PAIR>(&mut self, pair: PAIR) -> Option<CategoryValue>
    where
        PAIR: Into<CategoryValue>,
    {
        let new = pair.into();
        match self
            .store
            .binary_search_by_key(&new.cat_id(), CategoryValue::cat_id)
        {
            Ok(idx) => Some(replace(&mut self.store[idx], new)),
            Err(idx) => {
                self.store.insert(idx, new);
                None
            }
        }
    }
    /// Removes a category-value pair from the store
    /// # Errors
    /// Returns an error if the category does not exist
    #[inline]
    pub fn remove(&mut self, cat_id: CatId) -> Result<CategoryValue> {
        self.index_of_category(cat_id)
            .map(|idx| self.store.remove(idx))
    }
    /// Returns an iterator over the store
    #[inline]
    pub fn iter(&self) -> impl Iterator<Item = &CategoryValue> + Clone { self.store.iter() }
    /// Returns a mutable iterator over the store
    #[inline]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut CategoryValue> { self.store.iter_mut() }
    /// Returns the number of pairs in the store
    #[must_use]
    #[inline]
    pub fn len(&self) -> usize { self.store.len() }
    /// Returns `true` if the store is empty
    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool { self.store.is_empty() }
    /// Adds a value to a category, or inserts it if it already exists.
    #[inline]
    pub fn add_value<PAIR>(&mut self, pair: PAIR)
    where
        PAIR: Into<CategoryValue>,
    {
        let new = pair.into();
        match self
            .store
            .binary_search_by_key(&new.cat_id(), CategoryValue::cat_id)
        {
            Ok(idx) => {
                self.store[idx].add(new.get());
            }
            Err(idx) => {
                self.store.insert(idx, new);
            }
        }
    }
    /// Returns the sum of the values in the sore
    #[must_use]
    #[inline]
    pub fn sum(&self) -> f64 { self.store.iter().map(|cv| cv.value).sum() }
    /// Returns `true` if all pairs are nil
    #[must_use]
    #[inline]
    pub fn is_nil(&self) -> bool { self.store.iter().all(CategoryValue::is_nil) }
}
impl FromIterator<CatId> for CategoryStore {
    #[inline]
    fn from_iter<T: IntoIterator<Item = CatId>>(iter: T) -> Self {
        let mut store = Self::default();
        for cat_id in iter {
            store.insert((cat_id, None));
        }
        store
    }
}
