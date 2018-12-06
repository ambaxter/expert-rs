use std::hash::Hash;
use std;
use std::fmt;
use std::fmt::Debug;
use ord_subset::OrdVar;
use decimal::d128;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use crate::runtime::memory::SymbolId;
use ordered_float::NotNaN;
use super::context::BetaContext;
use crate::runtime::memory::StringCache;
use crate::shared::nodes::alpha::HashEqField;
use crate::shared::nodes::alpha::AlphaNode;
use std::hash::Hasher;
use std::cmp::Ordering;
use enum_index;
use enum_index::EnumIndex;
use std::any::Any;

#[derive(Copy, EnumIndex)]
pub enum Getter<T: Fact> {
    BOOL(fn(&T) -> &bool),
    I8(fn(&T) -> &i8),
    I16(fn(&T) -> &i16),
    I32(fn(&T) -> &i32),
    I64(fn(&T) -> &i64),
    I128(fn(&T) -> &i128),
    U8(fn(&T) -> &u8),
    U16(fn(&T) -> &u16),
    U32(fn(&T) -> &u32),
    U64(fn(&T) -> &u64),
    U128(fn(&T) -> &u128),
    F32(fn(&T) -> &NotNaN<f32>),
    F64(fn(&T) -> &NotNaN<f64>),
    D128(fn(&T) -> &OrdVar<d128>),
    STR(fn(&T) -> &str),
    TIME(fn(&T) -> &NaiveTime),
    DATE(fn(&T) -> &Date<Utc>),
    DATETIME(fn(&T) -> &DateTime<Utc>),
}

impl<T: Fact> Getter<T> {
    fn hash_self<H: Hasher>(ord: usize, getter: usize, state: &mut H) {
        ord.hash(state);
        getter.hash(state);
    }
}

impl<T: Fact> Debug for Getter<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Getter::*;
        write!(f, "Getter(")?;
        match *self {
            BOOL(accessor) => write!(f, "BOOL({:#x})", accessor as usize)?,
            I8(accessor) => write!(f, "I8({:#x})", accessor as usize)?,
            I16(accessor) => write!(f, "I16({:#x})", accessor as usize)?,
            I32(accessor) => write!(f, "I32({:#x})", accessor as usize)?,
            I64(accessor) => write!(f, "I64({:#x})", accessor as usize)?,
            I128(accessor) => write!(f, "I128({:#x})", accessor as usize)?,
            U8(accessor) => write!(f, "U8({:#x})", accessor as usize)?,
            U16(accessor) => write!(f, "U16({:#x})", accessor as usize)?,
            U32(accessor) => write!(f, "U32({:#x})", accessor as usize)?,
            U64(accessor) => write!(f, "U64({:#x})", accessor as usize)?,
            U128(accessor) => write!(f, "U128({:#x})", accessor as usize)?,
            F32(accessor) => write!(f, "F32({:#x})", accessor as usize)?,
            F64(accessor) => write!(f, "F64({:#x})", accessor as usize)?,
            D128(accessor) => write!(f, "D128({:#x})", accessor as usize)?,
            STR(accessor) => write!(f, "STR({:#x})", accessor as usize)?,
            TIME(accessor) => write!(f, "TIME({:#x})", accessor as usize)?,
            DATE(accessor) => write!(f, "DATE({:#x})", accessor as usize)?,
            DATETIME(accessor) => write!(f, "DATETIME({:#x})", accessor as usize)?,
        }
        write!(f, ")")
    }
}

macro_rules! getter_derive {
    ($($t:ident),+ ) => {

        impl<T: Fact> Clone for Getter<T> {
            fn clone(&self) -> Self {
                use self::Getter::*;
                match *self {
                    $(
                    $t(getter) => $t(getter),
                    )*
                }
            }
        }

        impl <T:Fact> Hash for Getter<T> {
            fn hash < H: Hasher > ( & self, state: & mut H) {
                use self::Getter::*;
                    match *self {
                    $ ( $ t(getter) => Self::hash_self(self.enum_index(), getter as usize, state),
                    )*
                }
            }
        }

        impl<T:Fact> PartialEq for Getter<T> {
            fn eq(&self, other: &Self) -> bool {
                use self::Getter::*;
                    match (self, other) {
                    $( (&$t(getter1), &$t(getter2)) => {
                        (getter1 as usize) == (getter2 as usize)
                    },)*
                    _ => false
                }
            }
        }

        impl<T: Fact> Eq for Getter<T> {}

        impl<T:Fact> Ord for Getter<T> {
            fn cmp(&self, other: &Self) -> Ordering {
            use self::Getter::*;
                match(self, other) {
                    $( (&$t(getter1), &$t(getter2)) => {
                        (getter1 as usize).cmp(&(getter2 as usize))
                    },)*
                    _ => self.enum_index().cmp(&other.enum_index())
                }
            }
        }

        impl<T:Fact> PartialOrd for Getter<T> {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
        pub enum FactFieldType {
            $(
               $t,
            )*
        }

        pub trait GetFieldType {
            fn get_field_type(&self) -> FactFieldType;
        }

        impl<T: Fact> GetFieldType for Getter<T> {
            fn get_field_type(&self) -> FactFieldType {
                use self::Getter::*;
                match self {
                    $(
                        $t(_) => FactFieldType::$t,
                    )*
                }
            }
        }
    };
}

getter_derive!(
        BOOL,
        I8, I16, I32, I64, I128,
        U8, U16, U32, U64, U128,
        F32, F64, D128,
        STR ,
        TIME, DATE, DATETIME
    );


impl FactFieldType {

    pub fn is_compatible(self, other: Self) -> bool {
        self.is_number_compatible(other)
    }

    pub fn is_number_compatible(self, other: Self) -> bool {
        match (self.is_number(), other.is_number()) {
            (true, true) => true,
            _ => false
        }
    }

    pub fn is_number(self) -> bool {
        use self::FactFieldType::*;
        match self {
            I8 | I16 | I32 | I64 | I128
            | U8 | U16 | U32 | U64 | U128
            | F32 | F64 | D128 => true,
            _ => false
        }
    }
}

pub trait Fact: 'static + Eq + Hash + Any
    where Self: std::marker::Sized {

    type HashEq: Hash + Eq + Clone + Debug;

    fn getter(field: &str) -> Option<Getter<Self>>;
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>>;
    fn create_hash_eq(conditions: &[AlphaNode<Self>]) -> Self::HashEq;
}

pub trait FactField {
    fn get_field_type() -> FactFieldType;
}

pub trait RefField : FactField {
    fn resolve<C: BetaContext>(context: &C, sym: SymbolId) -> &Self;
}

pub trait CastField : FactField {
    fn resolve<C: BetaContext>(context: &C, sym: SymbolId) -> Self;
}

macro_rules! impl_ref_field {
    ($($id:ty => $getter:ident => $field_type:ident),+) => {
        $(
            impl FactField for $id {
                fn get_field_type() -> FactFieldType {
                    FactFieldType::$field_type
                }
            }

            impl RefField for $id {
                #[inline]
                fn resolve<C: BetaContext>(context: &C, sym: SymbolId) -> &Self {
                    context.$getter(sym)
                }
            }
        )*
    };
}

macro_rules! impl_cast_field {
    ($($id:ty => $getter:ident => $field_type:ident),+) => {
        $(
            impl FactField for $id {
                fn get_field_type() -> FactFieldType {
                    FactFieldType::$field_type
                }
            }

            impl CastField for $id {
                #[inline]
                fn resolve<C: BetaContext>(context: &C, sym: SymbolId) -> Self {
                    context.$getter(sym)
                }
            }
        )*
    };
}

impl_ref_field!(
    bool => get_bool => BOOL,
    str => get_str => STR,
    NaiveTime => get_time => TIME,
    Date<Utc> => get_date => DATE,
    DateTime<Utc> => get_datetime => DATETIME
);

impl_cast_field!(
    i8 => get_i8 => I8,
    i16 => get_i16 => I16,
    i32 => get_i32 => I32,
    i64 => get_i64 => I64,
    i128 => get_i128 => I128,
    u8 => get_u8 => U8,
    u16 => get_u16 => U16,
    u32 => get_u32 => U32,
    u64 => get_u64 => U64,
    u128 => get_u128 => U128,
    NotNaN<f32> => get_f32 => F32,
    NotNaN<f64> => get_f64 => F64,
    OrdVar<d128> => get_d128 => D128
);

impl FactField for SymbolId {
    fn get_field_type() -> FactFieldType {
        FactFieldType::STR
    }
}