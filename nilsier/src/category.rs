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

use std::cmp::Ordering;
use std::iter::Sum;
use std::mem::replace;

use envisim_utils::utils::Number;
use num_traits::ConstZero;
use thiserror::Error;

#[non_exhaustive]
#[derive(Error, Debug, Clone)]
pub enum CatError {
    #[error("CAT ID ({0}) not found")]
    CatIdNotFound(String),
    #[error("CAT ID ({0}) already exist")]
    CatIdCollision(String),
}
// /// Shorthand for `Result` with [`CatError`] error type.
// type CatResult<T> = Result<T, CatError>;

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
    pub fn get_a(&self) -> &CID { &self.0 }
    /// Returns the second member of the pair
    #[inline]
    pub fn get_b(&self) -> &CID { &self.1 }
    /// Returns true if the members are equal
    #[inline]
    pub fn is_same(&self) -> bool
    where
        CID: Eq,
    {
        self.0 == self.1
    }
    /// Deconstructs the pair and returns the inner categories
    #[inline]
    pub fn into_inner(self) -> (CID, CID) { (self.0, self.1) }
}
impl<CID> From<(CID, CID)> for CatIdPair<CID>
where
    CID: Ord,
{
    #[inline]
    fn from((cat_a, cat_b): (CID, CID)) -> Self { Self::new(cat_a, cat_b) }
}
impl<CID> From<(&CID, &CID)> for CatIdPair<CID>
where
    CID: Copy + Ord,
{
    #[inline]
    fn from((cat_a, cat_b): (&CID, &CID)) -> Self { Self::new(*cat_a, *cat_b) }
}

/// PSU of first occurance, i.e. at which sample the category starts to be inventoried
#[must_use]
#[derive(Debug, Clone, Copy)]
pub struct CategoryValue<CID, VAL> {
    /// Category (key)
    cat_id: CID,
    /// Value
    value: VAL,
}
impl<CID, VAL> CategoryValue<CID, VAL> {
    /// Constructs a new container without value
    #[inline]
    pub fn new(cat_id: CID, value: Option<VAL>) -> Self
    where
        VAL: ConstZero,
    {
        Self {
            cat_id,
            value: value.unwrap_or(VAL::ZERO),
        }
    }
    /// Returns the category id
    #[inline]
    pub fn cat_id(&self) -> &CID { &self.cat_id }
    /// Reurns the value
    #[must_use]
    #[inline]
    pub fn get(&self) -> &VAL { &self.value }
    /// Returns a mutable reference to the value
    #[must_use]
    #[inline]
    pub fn get_mut(&mut self) -> &mut VAL { &mut self.value }
    /// Returns `true` if the value is 0.0
    #[must_use]
    #[inline]
    fn is_nil(&self) -> bool
    where
        VAL: Number,
    {
        !self.value.is_pos_finite()
    }
    /// Adds to the value
    #[inline]
    pub fn add(&mut self, value: VAL)
    where
        VAL: Number,
    {
        self.value += value;
    }
}
impl<CID, VAL> From<(CID, VAL)> for CategoryValue<CID, VAL>
where
    VAL: ConstZero,
{
    #[inline]
    fn from((cat_id, value): (CID, VAL)) -> Self { Self::new(cat_id, Some(value)) }
}
impl<CID, VAL> From<(CID, Option<VAL>)> for CategoryValue<CID, VAL>
where
    VAL: ConstZero,
{
    #[inline]
    fn from((cat_id, value): (CID, Option<VAL>)) -> Self { Self::new(cat_id, value) }
}
impl<CID, VAL> PartialEq for CategoryValue<CID, VAL>
where
    CID: Eq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.cat_id == other.cat_id }
}
impl<CID, VAL> Eq for CategoryValue<CID, VAL> where CID: Eq {}
impl<CID, VAL> Ord for CategoryValue<CID, VAL>
where
    CID: Ord,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering { self.cat_id.cmp(&other.cat_id) }
}
impl<CID, VAL> PartialOrd for CategoryValue<CID, VAL>
where
    CID: Ord,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

/// Stores multiple categories in category-order
#[must_use]
#[derive(Clone, Debug)]
pub struct CategoryStore<CID, VAL> {
    /// Store
    store: Vec<CategoryValue<CID, VAL>>,
}
impl<CID, VAL> CategoryStore<CID, VAL> {
    /// Constructs a new, empty, store
    #[inline]
    pub fn new() -> Self {
        Self {
            store: Vec::default(),
        }
    }
    /// Constructs a new, empty, store, with some capacity
    #[inline]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            store: Vec::with_capacity(capacity),
        }
    }
    /// Returns the index of a category
    /// # Errors
    /// Returns an error if the category does not exist
    #[inline]
    fn index_of_category(&self, cat_id: &CID) -> Option<usize>
    where
        CID: Ord,
    {
        self.store.binary_search_by(|cv| cv.cat_id.cmp(cat_id)).ok()
    }
    /// Returns the value of a category.
    #[inline]
    pub fn get(&self, cat_id: &CID) -> Option<&VAL>
    where
        CID: Ord,
    {
        self.index_of_category(cat_id)
            .map(|idx| self.store[idx].get())
    }
    /// Returns a mutable reference to the value of a category.
    #[inline]
    pub fn get_mut(&mut self, cat_id: &CID) -> Option<&mut VAL>
    where
        CID: Ord,
    {
        self.index_of_category(cat_id)
            .map(|idx| self.store[idx].get_mut())
    }
    /// Inserts a category-value pair into the store.
    /// Returns `Some`, if a pair with the same category id already exists.
    #[inline]
    pub fn insert<KEYVAL>(&mut self, pair: KEYVAL) -> Option<CategoryValue<CID, VAL>>
    where
        CID: Ord,
        KEYVAL: Into<CategoryValue<CID, VAL>>,
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
    /// Removes a category-value pair from the store.
    /// Returns `None` if `cat_id` was not present in the collection.
    #[inline]
    pub fn remove(&mut self, cat_id: &CID) -> Option<CategoryValue<CID, VAL>>
    where
        CID: Ord,
    {
        self.index_of_category(cat_id)
            .map(|idx| self.store.remove(idx))
    }
    /// Clears the store
    #[inline]
    pub fn clear(&mut self) { self.store.clear() }
    /// Returns an iterator over the store
    #[must_use]
    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &CategoryValue<CID, VAL>> + Clone {
        self.store.iter()
    }
    /// Returns a mutable iterator over the store
    #[must_use]
    #[inline]
    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut CategoryValue<CID, VAL>> {
        self.store.iter_mut()
    }
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
        CID: Ord,
        VAL: Number,
        PAIR: Into<CategoryValue<CID, VAL>>,
    {
        let new = pair.into();
        match self.store.binary_search(&new) {
            Ok(idx) => {
                self.store[idx].add(*new.get());
            }
            Err(idx) => {
                self.store.insert(idx, new);
            }
        }
    }
    /// Returns the sum of the values in the sore
    #[must_use]
    #[inline]
    pub fn sum(&self) -> VAL
    where
        VAL: Copy + Sum<VAL>,
    {
        self.store.iter().map(|cv| cv.value).sum()
    }
    /// Returns `true` if all pairs are nil
    #[must_use]
    #[inline]
    pub fn is_nil(&self) -> bool
    where
        VAL: Number,
    {
        self.store.iter().all(CategoryValue::is_nil)
    }
}
impl<CID, VAL> Default for CategoryStore<CID, VAL> {
    #[inline]
    fn default() -> Self { Self::new() }
}
impl<CID, VAL> FromIterator<CID> for CategoryStore<CID, VAL>
where
    CID: Ord,
    VAL: ConstZero,
{
    #[inline]
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = CID>,
    {
        let mut store = Self::new();
        for cat_id in iter {
            store.insert((cat_id, None));
        }
        store
    }
}
