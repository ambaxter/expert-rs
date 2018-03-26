use std::hash::Hash;
use std;
use std::fmt;
use std::fmt::Debug;
use std::any::TypeId;
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ::runtime::memory::{StringCache, SymbolId};
use ordered_float::NotNaN;
use super::context::LocalContext;

pub trait Introspect {
    fn static_type_id() -> TypeId;
}

#[derive(Copy, Clone)]
pub enum Getters<I: Fact> {
    BOOL(fn(&I) -> &bool),
    I8(fn(&I) -> &i8),
    I16(fn(&I) -> &i16),
    I32(fn(&I) -> &i32),
    I64(fn(&I) -> &i64),
    U8(fn(&I) -> &u8),
    U16(fn(&I) -> &u16),
    U32(fn(&I) -> &u32),
    U64(fn(&I) -> &u64),
    F32(fn(&I) -> &NotNaN<f32>),
    F64(fn(&I) -> &NotNaN<f64>),
    D128(fn(&I) -> &OrdVar<d128>),
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
            &I8(accessor) => write!(f, "I8({:#x})", accessor as usize)?,
            &I16(accessor) => write!(f, "I16({:#x})", accessor as usize)?,
            &I32(accessor) => write!(f, "I32({:#x})", accessor as usize)?,
            &I64(accessor) => write!(f, "I64({:#x})", accessor as usize)?,
            &U8(accessor) => write!(f, "U8({:#x})", accessor as usize)?,
            &U16(accessor) => write!(f, "U16({:#x})", accessor as usize)?,
            &U32(accessor) => write!(f, "U32({:#x})", accessor as usize)?,
            &U64(accessor) => write!(f, "U64({:#x})", accessor as usize)?,
            &F32(accessor) => write!(f, "F32({:#x})", accessor as usize)?,
            &F64(accessor) => write!(f, "F64({:#x})", accessor as usize)?,
            &D128(accessor) => write!(f, "D128({:#x})", accessor as usize)?,
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
    I8(usize, i8),
    I16(usize, i16),
    I32(usize, i32),
    I64(usize, i64),
    U8(usize, u8),
    U16(usize, u16),
    U32(usize, u32),
    U64(usize, u64),
    F32(usize, NotNaN<f32>),
    F64(usize, NotNaN<f64>),
    D128(usize, OrdVar<d128>),
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

pub trait FactField {
    #[inline]
    fn resolve<C: LocalContext>(context: &C, sym: SymbolId) -> &Self;
}

macro_rules! impl_fact_field {
    ($($id:ty => $getter:ident),+) => {
        $(
            impl FactField for $id {
                #[inline]
                fn resolve<C: LocalContext>(context: &C, sym: SymbolId) -> &Self {
                    context.$getter(sym)
                }
            }
        )*
    };
}

impl_fact_field!(
    bool => get_bool,
    i8 => get_i8,
    i16 => get_i16,
    i32 => get_i32,
    i64 => get_i64,
    u8 => get_u8,
    u16 => get_u16,
    u32 => get_u32,
    u64 => get_u64,
    NotNaN<f32> => get_f32,
    NotNaN<f64> => get_f64,
    d128 => get_d128,
    str => get_str,
    NaiveTime => get_time,
    Date<Utc> => get_date,
    DateTime<Utc> => get_datetime
);