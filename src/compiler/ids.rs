use std::fmt;
use std::fmt::Debug;

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct HashEqId{
    pub (crate) id: usize
}

impl Debug for HashEqId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<HashEqId> for usize {
    fn into(self) -> HashEqId {
        HashEqId{id: self}
    }
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct AlphaId {
    pub (crate) id: usize
}

impl Debug for AlphaId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<AlphaId> for usize {
    fn into(self) -> AlphaId {
        AlphaId {id: self}
    }
}


#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct BetaId {
    pub (crate) id: usize
}

impl Debug for BetaId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<BetaId> for usize {
    fn into(self) -> BetaId {
        BetaId {id: self}
    }
}