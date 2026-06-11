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
use std::fmt::Debug;
pub use std::num::NonZeroU32;

use num_traits::ToPrimitive;
use thiserror::Error;

use crate::category::{
    CatError,
    CatId,
};
use crate::utils::Identifier;

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
    pub fn psu_id(&self) -> PID
    where
        PID: Copy,
    {
        self.psu_id
    }
    /// Returns the sample size of the PSU
    #[must_use]
    #[inline]
    pub fn size(&self) -> NonZeroU32 { self.size }
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
    pub fn psu_id(&self) -> PID
    where
        PID: Copy,
    {
        self.header.psu_id()
    }
    /// Returns the sample size of the PSU
    #[must_use]
    #[inline]
    pub fn size(&self) -> NonZeroU32 { self.header.size() }
    /// Returns the number of nearest neighbours to be used in spatially balanced variance
    /// estimation
    #[must_use]
    #[inline]
    pub fn nn_size(&self) -> NonZeroU32 { self.nn_size }
    /// Returns an iterator over the categories of the PSU
    #[must_use]
    #[inline]
    pub fn categories_iter(&self) -> impl ExactSizeIterator<Item = CID> + Clone + '_ {
        self.categories.iter().copied()
    }
    /// Returns the number of categories in the PSU. A category is "in the PSU" if the category
    /// appears in this PSU and smaller PSUs, but not in larger PSUs.
    #[must_use]
    #[inline]
    pub fn categories_len(&self) -> usize { self.categories.len() }
    /// Returns `true` if the PSU contains a category.
    #[must_use]
    #[inline]
    fn contains_category<CID>(&self, cat_id: CID) -> bool {
        self.categories.binary_search(&cat_id).is_ok()
    }
    /// Returns `true` if the value was newly inserted.
    #[must_use]
    #[inline]
    fn insert_category<CID>(&mut self, cat_id: CID) -> bool {
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
    fn remove_category(&mut self, cat_id: CatId) -> bool {
        match self.categories.binary_search(&cat_id) {
            Ok(idx) => {
                let _removed_id = self.categories.remove(idx);
                true
            }
            _ => false,
        }
    }
}
impl<P> PartialEq for PsuData<P>
where
    P: Identifier,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool { self.header == other.header }
}
impl<P> Eq for PsuData<P> where P: Identifier {}
impl<P> Ord for PsuData<P>
where
    P: Identifier,
{
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering
    where
        P: Identifier,
    {
        self.header.cmp(&other.header)
    }
}
impl<P> PartialOrd for PsuData<P>
where
    P: Identifier,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { Some(self.cmp(other)) }
}

/// `PsuStore` maps a primary sampling unit identifier to its sample size.
/// The map is ordered by sample size, as the PSUs are assumed to be in descending order, i.e. each
/// sample is a drawn from the sample before.
#[must_use]
#[derive(Debug, Default, Clone)]
pub struct PsuStore<P>
where
    P: Identifier,
{
    // psus probably very small (about 10?)
    // store sorted by size
    // categories medium sized (about 100?)
    /// Psu storage, sorted by [`PsuHeader`] (size)
    psus: Vec<PsuData<P>>,
}

impl<P> PsuStore<P>
where
    P: Identifier,
{
    /// Returns a reference to a PSU by identifier.
    #[must_use]
    #[inline]
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &PsuData<P>> + Clone { self.psus.iter() }
    /// Returns an iterator over the categories in the store
    #[inline]
    pub fn category_iter(&self) -> impl Iterator<Item = (CatId, &PsuData<P>)> + Clone {
        self.psus
            .iter()
            .flat_map(|pd| pd.categories.iter().map(move |&cat_id| (cat_id, pd)))
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
    pub fn get_psu(&self, psu_id: PsuId<P>) -> Option<&PsuData<P>> {
        self.psus.iter().find(|dt| psu_id == dt.psu_id())
    }
    /// Returns a mutable reference to the header info of the PSU id
    /// # Errors
    /// Returns an error if `psu_id` is not found.
    #[inline]
    fn get_psu_mut(&mut self, psu_id: PsuId<P>) -> Option<&mut PsuData<P>> {
        self.psus.iter_mut().find(|dt| psu_id == dt.psu_id())
    }
    /// Returns the smallest PSU. Every other PSU is assumed to be a superset of this.
    #[must_use]
    #[inline]
    pub fn get_min_psu(&self) -> Option<&PsuData<P>> { self.psus.first() }
    /// Returns the largest PSU. Every other PSU is assumed to be a subset of this.
    #[must_use]
    #[inline]
    pub fn get_max_psu(&self) -> Option<&PsuData<P>> { self.psus.last() }
    /// Returns the index of a PSU.
    /// # Errors
    /// Returns an error if the `psu_id` is not found
    #[inline]
    pub fn order_of_psu(&self, psu_id: PsuId<P>) -> Option<usize> {
        self.psus.iter().position(|dt| psu_id == dt.psu_id())
    }
    // /// Adds a new PSU. Any PSU larger than this is assumed to be a superset, and any PSU
    // /// smaller than this is assumed to be a subset.
    // fn insert_psu(&mut self, new: PsuData) -> Result<usize> {
    //     // Check if key already exists, as we want to remove and return it if not
    //     ensure!(
    //         self.order_of_psu(new.psu_id()).is_err(),
    //         PsuError::PsuIdCollision(new.psu_id())
    //     );

    //     // Find the correct spot for the new pair
    //     let pos = self
    //         .psus
    //         .partition_point(|dt| dt.size().cmp(&new.size()).is_lt());
    //     self.psus.insert(pos, new);
    //     Ok(pos)
    // }
    // /// Removes a PSU by identifier.
    // #[inline]
    // fn remove_psu(&mut self, psu_id: PsuId) -> Result<PsuData> {
    //     let idx = self.order_of_psu(psu_id)?;
    //     Ok(self.psus.remove(idx))
    // }
    /// Returns an iterator over all PSUs smaller than the given PSU, starting with the smallest and
    /// ending with the given PSU.
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn subset_psu(
        &self,
        psu_id: PsuId<P>,
    ) -> Option<impl ExactSizeIterator<Item = &PsuData<P>> + Clone> {
        let idx = self.order_of_psu(psu_id)?;
        Some(self.psus[..=idx].iter())
    }
    /// Returns an iterator over all PSUs larger than the given PSU, starting with the given PSU.
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn superset_psu(
        &self,
        psu_id: PsuId<P>,
    ) -> Option<impl ExactSizeIterator<Item = &PsuData<P>> + Clone> {
        let idx = self.order_of_psu(psu_id)?;
        Some(self.psus[idx..].iter())
    }
    /// Adds a new category to a psu
    /// # Errors
    /// Returns an error if `psu_id` does not exist, or if `cat_id` already exists.
    #[inline]
    pub fn insert_category(&mut self, psu_id: PsuId<P>, cat_id: CatId) -> PsuResult<bool> {
        if self.psus.iter().any(|pd| pd.contains_category(cat_id)) {
            return Err(CatError::CatIdCollision(cat_id).into());
        }

        self.get_psu_mut(psu_id)
            .ok_or(PsuError::PsuIdNotFound(psu_id.to_string()))
            .map(|pd| pd.insert_category(cat_id))
    }
    /// Removes a category from a PSU
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn remove_category(&mut self, psu_id: PsuId<P>, cat_id: CatId) -> Option<bool> {
        self.get_psu_mut(psu_id)
            .map(|pd| pd.remove_category(cat_id))
    }
    /// Returns the PSU of a `cat_id`.
    /// # Errors
    /// Returns an error if `cat_id` is not found in any PSU.
    #[inline]
    pub fn get_psu_from_category(&self, cat_id: CatId) -> Option<&PsuData<P>> {
        self.psus.iter().find(|pd| pd.contains_category(cat_id))
    }
    /// Returns `true` if the `psu_id` contains `cat_id`.
    /// # Errors
    /// Returns an error if `psu_id` does not exist.
    #[inline]
    pub fn psu_contains_category(&self, psu_id: PsuId<P>, cat_id: CatId) -> Option<bool> {
        self.get_psu(psu_id).map(|pd| pd.contains_category(cat_id))
    }
    // #[inline]
    // fn psu_superset_contains_category(&self, psu_id: PsuId, cat_id: CatId) -> Result<bool> {
    //     // Until we have found psu_id, we're amongst the subsets
    //     let mut found_psu_id = false;
    //     for psu in self.psus.iter() {
    //         if psu.psu_id() == psu_id {
    //             found_psu_id = true;
    //         }

    //         if found_psu_id {
    //             // Return true if we find cat
    //             if psu.contains_category(cat_id) {
    //                 return Ok(true);
    //             }
    //         } else {
    //             // Return false if we find cat
    //             if psu.contains_category(cat_id) {
    //                 return Ok(false);
    //             }
    //         }
    //     }

    //     ensure!(found_psu_id, PsuError::PsuIdNotFound(psu_id));
    //     bail!(CatError::CatIdNotFound(cat_id));
    // }
    /// Initializes the storage by `(key, size)` entries
    #[expect(clippy::missing_panics_doc, reason = "panic implies bug")]
    #[expect(
        clippy::cast_possible_truncation,
        clippy::cast_sign_loss,
        clippy::as_conversions,
        reason = "rounded, wont truncate"
    )]
    #[inline]
    pub fn new<I, J>(entries: I, nns: Option<J>) -> PsuResult<Self>
    where
        I: ExactSizeIterator<Item = PsuHeader<P>>,
        J: ExactSizeIterator<Item = NonZeroU32>,
    {
        let mut map: Vec<PsuData<P>> = if let Some(nn) = nns {
            if entries.len() != nn.len() {
                return Err(PsuError::InvalidNeighbourhoodSize);
            }

            // Zip in nns and construct vector
            let mut map: Vec<PsuData<P>> =
                entries.zip(nn).map(|(h, n)| PsuData::new(h, n)).collect();
            map.sort();
            map
        } else {
            // Construct without nns
            let mut headers: Vec<PsuHeader<P>> = entries.collect();
            headers.sort();
            let min_size_frac = 4.0
                / headers
                    .first()
                    .map(|h| h.size())
                    // if default is used, headers were empty...just any size goes
                    .unwrap_or(PsuData::NN_DEFAULT_SIZE)
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
                    PsuData::try_new(h, nn_size)
                })
                .collect()?
        };

        // Remove duplicate keys
        map.dedup();
        Ok(Self { psus: map })
    }
}
