use std::fmt;
use std::fmt::Debug;

use crate::serial::SerialGen;

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct HashEqId {
    index: usize,
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct AlphaId {
    index: usize,
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct BetaId {
    index: usize,
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct InterId {
    index: usize,
}

index_id!(HashEqId, AlphaId, BetaId, InterId);

pub type HashEqIdGen = SerialGen<usize, HashEqId>;
pub type AlphaIdGen = SerialGen<usize, AlphaId>;
pub type BetaIdGen = SerialGen<usize, BetaId>;
pub type InterIdGen = SerialGen<usize, InterId>;
