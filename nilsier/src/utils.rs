use std::fmt::{
    Debug,
    Display,
};
use std::hash::Hash;

pub trait Identifier: Copy + Eq + Ord + Hash + Debug + Display {}
