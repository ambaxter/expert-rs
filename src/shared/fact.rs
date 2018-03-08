use std::hash::Hash;
use std;
use std::fmt;
use std::fmt::Debug;
use std::any::TypeId;
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ::runtime::memory::{StringCache, SymbolId};

pub trait Introspect {
    fn static_type_id() -> TypeId;
}

#[derive(Copy, Clone)]
pub enum Getters<I: Fact> {
    BOOL(fn(&I) -> &bool),
    NUMBER(fn(&I) -> &OrdVar<d128>),
    STR(fn(&I) -> &str),
    TIME(fn(&I) -> &NaiveTime),
    DATE(fn(&I) -> &Date<Utc>),
    DATETIME(fn(&I) -> &DateTime<Utc>),
}

impl<I: Fact> Debug for Getters<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Getters::*;
        write!(f, "Getters(")?;
        match self {
            &BOOL(accessor) => write!(f, "BOOL({:#x})", accessor as usize)?,
            &NUMBER(accessor) => write!(f, "NUMBER({:#x})", accessor as usize)?,
            &STR(accessor) => write!(f, "STR({:#x})", accessor as usize)?,
            &TIME(accessor) => write!(f, "TIME({:#x})", accessor as usize)?,
            &DATE(accessor) => write!(f, "DATE({:#x})", accessor as usize)?,
            &DATETIME(accessor) => write!(f, "DATETIME({:#x})", accessor as usize)?,
            _ => {}
        }
        write!(f, ")")
    }
}

pub enum HashEqField {
    BOOL(usize, bool),
    NUMBER(usize, OrdVar<d128>),
    STR(usize, SymbolId),
    TIME(usize, NaiveTime),
    DATE(usize, Date<Utc>),
    DATETIME(usize, DateTime<Utc>),
}


pub trait Fact: Introspect + Eq + Hash
    where Self: std::marker::Sized {

    type HashEq: Hash + Eq + Clone + Debug;
    fn getter(field: &str) -> Option<Getters<Self>>;
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>>;
}