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

//! Handling of Psus, i.e. hierarchical samples within Nils.

use std::cmp::Ordering;
use std::fmt::{
    Debug,
    Display,
};
pub use std::num::NonZeroU32;

use num_traits::ToPrimitive;
use thiserror::Error;

use crate::category::CatError;

#[non_exhaustive]
#[derive(Error, Debug, Clone)]
pub enum PsuError {
    #[error("size must be positive")]
    InvalidSize,
    #[error("neighbourhood size must be positive")]
    InvalidNeighbourhoodSize,
    #[error("PSU ID ({0}) not found")]
    PsuIdNotFound(String),
    #[error("PSU ID ({0}) already exist")]
    PsuIdCollision(String),
    #[error(transparent)]
    Category(#[from] CatError),
}
/// Shorthand for `Result` with [`PsuError`] error type.
type PsuResult<T> = Result<T, PsuError>;

/// PSU information header
#[must_use]
#[derive(Debug, Clone, Copy)]
pub struct PsuHeader<PID> {
    /// Id of PSU
    psu_id: PID,
    /// Size of PSU (sample size)
    size: NonZeroU32,
}
impl<PID> PsuHeader<PID> {
    /// Constructs a new PSU information header
    #[inline]
    pub fn new(psu_id: PID, size: NonZeroU32) -> Self { Self { psu_id, size } }
    /// Tries to construct a new PSU information header
    /// # Errors
    /// If `size` or `nn_size` cannot be converted, i.e. are 0.
    #[inline]
    pub fn try_new<SIZE, NN>(psu_id: PID, size: SIZE) -> PsuResult<Self>
    where
        SIZE: TryInto<NonZeroU32>,
    {
        let size = size.try_into().map_err(|_| PsuError::InvalidSize)?;
        Ok(Self::new(psu_id, size))
    }
    /// Returns the PSU identifier
    #[inline]
    pub fn psu_id(&self) -> &PID { &self.psu_id }
    /// Returns the sample size of the PSU
    #[must_use]
    #[inline]
    pub fn size(&self) -> &NonZeroU32 { &self.size }
}
impl<PID> PartialEq for PsuHeader<PID>
where
    PID: Eq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.psu_id == other.psu_id }
}
impl<PID> Eq for PsuHeader<PID> where PID: Eq {}
impl<PID> PartialOrd for PsuHeader<PID>
where
    PID: Ord,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}
impl<PID> Ord for PsuHeader<PID>
where
    PID: Ord,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering { self.size.cmp(&other.size) }
}

/// PSU information
#[must_use]
#[derive(Debug, Clone)]
pub struct PsuData<PID, CID> {
    /// PSU header info
    header: PsuHeader<PID>,
    /// Size of nearest neighbourhood
    nn_size: NonZeroU32,
    /// Categories that appears in Psus of smaller sizes
    categories: Vec<CID>,
}
impl<PID, CID> PsuData<PID, CID>
// where
//     PID: Identifier,
{
    /// Default nearest neighbour size
    const NN_DEFAULT_SIZE: NonZeroU32 = NonZeroU32::new(4).expect("4 > 0");
    /// Constructs a new PSU information header
    #[inline]
    pub fn new(header: PsuHeader<PID>, nn_size: NonZeroU32) -> Self {
        Self {
            header,
            nn_size,
            categories: Vec::default(),
        }
    }
    /// Tries to construct a new PSU information header
    /// # Errors
    /// If `nn_size` cannot be converted, i.e. are 0.
    #[inline]
    pub fn try_new<NN>(header: PsuHeader<PID>, nn_size: NN) -> PsuResult<Self>
    where
        NN: TryInto<NonZeroU32>,
    {
        let nn_size = nn_size
            .try_into()
            .map_err(|_| PsuError::InvalidNeighbourhoodSize)?;
        Ok(Self::new(header, nn_size))
    }
    /// Returns a reference to the header
    #[inline]
    pub fn header(&self) -> &PsuHeader<PID> { &self.header }
    /// Returns the PSU identifier
    #[inline]
    pub fn psu_id(&self) -> &PID { self.header.psu_id() }
    /// Returns the sample size of the PSU
    #[must_use]
    #[inline]
    pub fn size(&self) -> &NonZeroU32 { self.header.size() }
    /// Returns the number of nearest neighbours to be used in spatially balanced variance
    /// estimation
    #[must_use]
    #[inline]
    pub fn nn_size(&self) -> &NonZeroU32 { &self.nn_size }
    /// Returns an iterator over the categories of the PSU
    #[must_use]
    #[inline]
    pub fn categories_iter(&self) -> impl ExactSizeIterator<Item = &CID> + Clone + '_ {
        self.categories.iter()
    }
    /// Returns the number of categories in the PSU. A category is "in the PSU" if the category
    /// appears in this PSU and smaller PSUs, but not in larger PSUs.
    #[must_use]
    #[inline]
    pub fn categories_len(&self) -> usize { self.categories.len() }
    /// Returns `true` if the PSU contains a category.
    #[must_use]
    #[inline]
    fn contains_category(&self, cat_id: &CID) -> bool
    where
        CID: Ord,
    {
        self.categories.binary_search(cat_id).is_ok()
    }
    /// Returns `true` if the value was newly inserted.
    #[must_use]
    #[inline]
    fn insert_category(&mut self, cat_id: CID) -> bool
    where
        CID: Ord,
    {
        match self.categories.binary_search(&cat_id) {
            Err(idx) => {
                self.categories.insert(idx, cat_id);
                true
            }
            _ => false,
        }
    }
    /// Returns `true` if the value was present.
    #[must_use]
    #[inline]
    fn remove_category(&mut self, cat_id: &CID) -> bool
    where
        CID: Ord,
    {
        match self.categories.binary_search(cat_id) {
            Ok(idx) => {
                let _removed_id = self.categories.remove(idx);
                true
            }
            _ => false,
        }
    }
}
impl<PID, CID> PartialEq for PsuData<PID, CID>
where
    PID: Eq,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.header == other.header }
}
impl<PID, CID> Eq for PsuData<PID, CID> where PID: Eq {}
impl<PID, CID> Ord for PsuData<PID, CID>
where
    PID: Ord,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering { self.header.cmp(&other.header) }
}
impl<PID, CID> PartialOrd for PsuData<PID, CID>
where
    PID: Ord,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

/// `PsuStore` maps a primary sampling unit identifier to its sample size.
/// The map is ordered by sample size, as the PSUs are assumed to be in descending order, i.e. each
/// sample is a drawn from the sample before.
#[must_use]
#[derive(Debug, Default, Clone)]
pub struct PsuStore<PID, CID> {
    // psus probably very small (about 10?)
    // store sorted by size
    // categories medium sized (about 100?)
    /// Psu storage, sorted by [`PsuHeader`] (size)
    psus: Vec<PsuData<PID, CID>>,
}

impl<PID, CID> PsuStore<PID, CID> {
    /// Returns a reference to a PSU by identifier.
    #[must_use]
    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &PsuData<PID, CID>> + Clone {
        self.psus.iter()
    }
    /// Returns an iterator over the categories in the store
    #[inline]
    pub fn category_iter(&self) -> impl Iterator<Item = (&CID, &PsuData<PID, CID>)> + Clone {
        self.psus
            .iter()
            .flat_map(|pd| pd.categories.iter().map(move |cat_id| (cat_id, pd)))
    }
    /// Returns the number of PSUs in the store
    #[must_use]
    #[inline]
    pub fn len(&self) -> usize { self.psus.len() }
    /// Returns `true` if the PSU collection is empty
    #[must_use]
    #[inline]
    pub fn is_empty(&self) -> bool { self.psus.is_empty() }
    /// Returns a reference to the header info of the PSU id
    /// # Errors
    /// Returns an error if `psu_id` is not found.
    #[inline]
    pub fn get_psu(&self, psu_id: &PID) -> Option<&PsuData<PID, CID>>
    where
        PID: Eq,
    {
        self.psus.iter().find(|dt| psu_id == dt.psu_id())
    }
    /// Returns a mutable reference to the header info of the PSU id
    /// # Errors
    /// Returns an error if `psu_id` is not found.
    #[inline]
    fn get_psu_mut(&mut self, psu_id: &PID) -> Option<&mut PsuData<PID, CID>>
    where
        PID: Eq,
    {
        self.psus.iter_mut().find(|dt| psu_id == dt.psu_id())
    }
    /// Returns the smallest PSU. Every other PSU is assumed to be a superset of this.
    #[must_use]
    #[inline]
    pub fn get_min_psu(&self) -> Option<&PsuData<PID, CID>> { self.psus.first() }
    /// Returns the largest PSU. Every other PSU is assumed to be a subset of this.
    #[must_use]
    #[inline]
    pub fn get_max_psu(&self) -> Option<&PsuData<PID, CID>> { self.psus.last() }
    /// Returns the index of a PSU.
    /// # Errors
    /// Returns an error if the `psu_id` is not found
    #[inline]
    pub fn order_of_psu(&self, psu_id: &PID) -> Option<usize>
    where
        PID: Eq,
    {
        self.psus.iter().position(|dt| psu_id == dt.psu_id())
    }
    /// Returns an iterator over all PSUs smaller than the given PSU, starting with the smallest and
    /// ending with the given PSU.
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn subset_psu(
        &self,
        psu_id: &PID,
    ) -> Option<impl ExactSizeIterator<Item = &PsuData<PID, CID>> + Clone>
    where
        PID: Eq,
    {
        let idx = self.order_of_psu(psu_id)?;
        Some(self.psus[..=idx].iter())
    }
    /// Returns an iterator over all PSUs larger than the given PSU, starting with the given PSU.
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn superset_psu(
        &self,
        psu_id: &PID,
    ) -> Option<impl ExactSizeIterator<Item = &PsuData<PID, CID>> + Clone>
    where
        PID: Eq,
    {
        let idx = self.order_of_psu(psu_id)?;
        Some(self.psus[idx..].iter())
    }
    /// Adds a new category to a psu
    /// # Errors
    /// Returns an error if `psu_id` does not exist, or if `cat_id` already exists.
    #[inline]
    pub fn insert_category(&mut self, psu_id: &PID, cat_id: CID) -> PsuResult<bool>
    where
        PID: Eq + Display,
        CID: Ord + Display,
    {
        if self.psus.iter().any(|pd| pd.contains_category(&cat_id)) {
            return Err(CatError::CatIdCollision(cat_id.to_string()).into());
        }

        self.get_psu_mut(psu_id)
            .ok_or(PsuError::PsuIdNotFound(psu_id.to_string()))
            .map(|pd| pd.insert_category(cat_id))
    }
    /// Removes a category from a PSU
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn remove_category(&mut self, psu_id: &PID, cat_id: &CID) -> Option<bool>
    where
        PID: Eq,
        CID: Ord,
    {
        self.get_psu_mut(psu_id)
            .map(|pd| pd.remove_category(cat_id))
    }
    /// Returns the PSU of a `cat_id`.
    /// # Errors
    /// Returns an error if `cat_id` is not found in any PSU.
    #[inline]
    pub fn get_psu_from_category(&self, cat_id: &CID) -> Option<&PsuData<PID, CID>>
    where
        CID: Ord,
    {
        self.psus.iter().find(|pd| pd.contains_category(cat_id))
    }
    /// Returns `true` if the `psu_id` contains `cat_id`.
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn psu_contains_category(&self, psu_id: &PID, cat_id: &CID) -> Option<bool>
    where
        PID: Eq,
        CID: Ord,
    {
        self.get_psu(psu_id).map(|pd| pd.contains_category(cat_id))
    }
    /// Initializes the storage by `(key, size)` entries
    /// # Errors
    /// Returns an error if the iterator sizes does not match.
    #[expect(clippy::missing_panics_doc, reason = "panic implies bug")]
    #[inline]
    pub fn new<I, J>(entries: I, nns: Option<J>) -> PsuResult<Self>
    where
        PID: Ord,
        I: IntoIterator<Item = PsuHeader<PID>>,
        I::IntoIter: ExactSizeIterator,
        J: IntoIterator<Item = NonZeroU32>,
        J::IntoIter: ExactSizeIterator,
    {
        let entries = entries.into_iter();
        let mut map: Vec<PsuData<PID, CID>> = if let Some(nn) = nns {
            let nn = nn.into_iter();
            if entries.len() != nn.len() {
                return Err(PsuError::InvalidNeighbourhoodSize);
            }

            // Zip in nns and construct vector
            let mut map: Vec<PsuData<PID, CID>> =
                entries.zip(nn).map(|(h, n)| PsuData::new(h, n)).collect();
            map.sort();
            map
        } else {
            // Construct without nns
            let mut headers: Vec<PsuHeader<PID>> = entries.collect();
            headers.sort();
            let min_size_frac = 4.0
                / headers
                    .first()
                    // if default is used, headers were empty...just any size goes
                    .map_or(PsuData::<PID, CID>::NN_DEFAULT_SIZE, |ph| *ph.size())
                    .get()
                    .to_f64()
                    .expect("u32 -> f64");
            headers
                .into_iter()
                .map(|h| {
                    let nn_size = (min_size_frac * h.size().get().to_f64().expect("u32 -> f64"))
                        .round()
                        .to_u32()
                        .expect("f64 -> u32");
                    PsuData::<PID, CID>::try_new(h, nn_size)
                })
                .collect::<PsuResult<Vec<PsuData<PID, CID>>>>()?
        };

        // Remove duplicate keys
        map.dedup();
        Ok(Self { psus: map })
    }
}
