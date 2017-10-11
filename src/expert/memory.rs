use string_interner::{StringInterner, Symbol};
use std::fmt;
use std::fmt::Debug;

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

pub type KStringInterner = StringInterner<SymbolId>;
