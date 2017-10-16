use string_interner::{StringInterner, Symbol};
use std::fmt;
use std::fmt::Debug;
use std::any::TypeId;

use network::ids::*;

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct SymbolId {
    id: usize,
}

impl Symbol for SymbolId {
    fn from_usize(val: usize) -> Self {
        SymbolId{id: val}
    }

    fn to_usize(self) -> usize {
        self.id
    }
}

impl Debug for SymbolId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

pub type StringCache = StringInterner<SymbolId>;

pub trait AlphaMemoryId {}

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum MemoryId {
    HashEq(HashEqId),
    Alpha(AlphaId),
    Beta(BetaId)
}

impl Into<MemoryId> for HashEqId {
    fn into(self) -> MemoryId {
        MemoryId::HashEq(self)
    }
}

impl AlphaMemoryId for HashEqId {}

impl Into<MemoryId> for AlphaId {
    fn into(self) -> MemoryId {
        MemoryId::Alpha(self)
    }
}

impl AlphaMemoryId for AlphaId {}

impl Into<MemoryId> for BetaId {
    fn into(self) -> MemoryId {
        MemoryId::Beta(self)
    }
}

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum InterMemoryId {
    HashEq(TypeId, HashEqId),
    Alpha(TypeId, AlphaId),
    Beta(TypeId, BetaId),
    Inter(InterId)
}