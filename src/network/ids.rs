use std::fmt;
use std::fmt::Debug;


#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct HashEqId{
    id: usize
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct AlphaId {
    pub (crate) id: usize
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct BetaId {
    pub (crate) id: usize
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct InterBetaId {
    pub (crate) id: usize
}

index_id!(HashEqId, AlphaId, BetaId, InterBetaId);