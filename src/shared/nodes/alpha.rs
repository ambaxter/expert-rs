use ord_subset::OrdVar;
use decimal::d128;
use ordered_float::NotNaN;
use std::hash::{Hash, Hasher};
use super::tests::*;
use crate::runtime::memory::SymbolId;
use chrono::NaiveTime;
use chrono::Date;
use chrono::Utc;
use chrono::DateTime;
use crate::shared::fact::Fact;
use crate::shared::fact::FactField;
use crate::shared::context::AlphaContext;
use std::fmt::Debug;
use std::fmt;
use crate::shared::nodes::beta::{BetaNode, IsAlpha};
use enum_index;
use enum_index::EnumIndex;

pub trait IsHashEq {
    fn is_hash_eq(&self) -> bool;
}

pub trait AlphaTestField<T: FactField + ?Sized > {
    fn alpha_test_field<C: AlphaContext>(&self, value: &T, context: &C) -> bool;
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BoolTest {
    Eq(Truth, EqTest, bool)
}

impl IsHashEq for BoolTest {
    fn is_hash_eq(&self) -> bool {
        use self::BoolTest::*;
        match self {
            Eq(..) => true
        }
    }
}

impl AlphaTestField<bool> for BoolTest {
    fn alpha_test_field<C: AlphaContext>(&self, value: &bool, _: &C) -> bool {
        use self::BoolTest::*;
        match *self {
            Eq(truth, ref test, ref to) => (truth, test).test(value, to)
        }
    }
}

macro_rules! alpha_number_test {
    ($($id:ty => $test:ident),+) => {
     $(
             #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test {
                Ord(Truth, OrdTest, $id),
                Btwn(Truth, BetweenTest, $id, $id),
                Eq(Truth, EqTest, $id)
            }

            impl IsHashEq for $test {
                fn is_hash_eq(&self) -> bool {
                    use self::$test::*;
                    match self {
                        Eq(Truth::Is, EqTest::Eq,_) => true,
                        Eq(Truth::Not, EqTest::Ne,_) => true,
                        _ => false
                    }
                }
            }

            impl AlphaTestField<$id> for $test {
                fn alpha_test_field<C: AlphaContext>(&self, value: &$id, _: &C) -> bool {
                    use self::$test::*;
                    match *self {
                        Ord(truth, ref test, ref to) => (truth, test).test(value, to),
                        Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
                        Eq(truth, ref test, ref to) => (truth, test).test(value, to)
                    }
                }
            }
     )*
    };
}

macro_rules! alpha_float_test {
    ($($id:ty => $test:ident),+) => {
     $(
             #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test {
                Ord(Truth, OrdTest, $id),
                Btwn(Truth, BetweenTest, $id, $id),
                ApproxEq(Truth, ApproxEqTest, $id)
            }

            impl IsHashEq for $test {
                fn is_hash_eq(&self) -> bool {
                    false
                }
            }

            impl AlphaTestField<$id> for $test {
                fn alpha_test_field<C: AlphaContext>(&self, value: &$id, _: &C) -> bool {
                    use self::$test::*;
                    match *self {
                        Ord(truth, ref test, ref to) => (truth, test).test(value, to),
                        Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
                        ApproxEq(truth, ref test, ref to) => (truth, test).test(value, to)
                    }
                }
            }
     )*
    };
}

alpha_number_test!(
    i8 => I8Test,
    i16 => I16Test,
    i32 => I32Test,
    i64 => I64Test,
    i128 => I128Test,
    u8 => U8Test,
    u16 => U16Test,
    u32 => U32Test,
    u64 => U64Test,
    u128 => U128Test,
    OrdVar<d128> => D128Test
    );

alpha_float_test!(
    NotNaN<f32> => F32Test,
    NotNaN<f64> => F64Test
);

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrTest {
    Ord(Truth, OrdTest, SymbolId),
    Btwn(Truth, BetweenTest, SymbolId, SymbolId),
    Eq(Truth, EqTest, SymbolId),
    Str(Truth, StrArrayTest, SymbolId)
}

impl IsHashEq for StrTest {
    fn is_hash_eq(&self) -> bool {
        use self::StrTest::*;
        match self {
            Eq(Truth::Is, EqTest::Eq, _) => true,
            Eq(Truth::Not, EqTest::Ne, _) => true,
            _ => false
        }
    }
}

impl AlphaTestField<str> for StrTest {
    fn alpha_test_field<C: AlphaContext>(&self, value: &str, context: &C) -> bool {
        use self::StrTest::*;
        let string_cache = context.get_string_cache();
        match *self {
            Ord(truth, ref test, ref to) => (truth, test).test(value, string_cache.resolve(*to).unwrap()),
            Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, string_cache.resolve(*from).unwrap(), string_cache.resolve(*to).unwrap()),
            Eq(truth, ref test, ref to) => (truth, test).test(value, string_cache.resolve(*to).unwrap()),
            Str(truth, ref test, ref to) => (truth, test).test(value, string_cache.resolve(*to).unwrap()),

        }
    }
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TimeTest {
    Ord(Truth, OrdTest, NaiveTime),
    Btwn(Truth, BetweenTest, NaiveTime, NaiveTime),
    Eq(Truth, EqTest, NaiveTime)
}

impl IsHashEq for TimeTest {
    fn is_hash_eq(&self) -> bool {
        use self::TimeTest::*;
        match self {
            Eq(Truth::Is, EqTest::Eq, _) => true,
            Eq(Truth::Not, EqTest::Ne, _) => true,
            _ => false
        }
    }
}

impl AlphaTestField<NaiveTime> for TimeTest {
    fn alpha_test_field<C: AlphaContext>(&self, value: &NaiveTime, _: &C) -> bool {
        use self::TimeTest::*;
        match *self {
            Ord(truth, ref test, ref to) => (truth, test).test(value, to),
            Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
            Eq(truth, ref test, ref to) => (truth, test).test(value, to)
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTest {
    Ord(Truth, OrdTest, Date<Utc>),
    Btwn(Truth, BetweenTest, Date<Utc>, Date<Utc>),
    Eq(Truth, EqTest, Date<Utc>)
}

impl IsHashEq for DateTest {
    fn is_hash_eq(&self) -> bool {
        use self::DateTest::*;
        match self {
            Eq(Truth::Is, EqTest::Eq, _) => true,
            Eq(Truth::Not, EqTest::Ne, _) => true,
            _ => false
        }
    }
}

impl AlphaTestField<Date<Utc>> for DateTest {
    fn alpha_test_field<C: AlphaContext>(&self, value: &Date<Utc>, _: &C) -> bool {
        use self::DateTest::*;
        match *self {
            Ord(truth, ref test, ref to) => (truth, test).test(value, to),
            Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
            Eq(truth, ref test, ref to) => (truth, test).test(value, to)
        }
    }
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTimeTest {
    Ord(Truth, OrdTest, DateTime<Utc>),
    Btwn(Truth, BetweenTest, DateTime<Utc>, DateTime<Utc>),
    Eq(Truth, EqTest, DateTime<Utc>)
}

impl IsHashEq for DateTimeTest {
    fn is_hash_eq(&self) -> bool {
        use self::DateTimeTest::*;
        match self {
            Eq(Truth::Is, EqTest::Eq, _) => true,
            Eq(Truth::Not, EqTest::Ne, _) => true,
            _ => false
        }
    }
}

impl AlphaTestField<DateTime<Utc>> for DateTimeTest {
    fn alpha_test_field<C: AlphaContext>(&self, value: &DateTime<Utc>, _: &C) -> bool {
        use self::DateTimeTest::*;
        match *self {
            Ord(truth, ref test, ref to) => (truth, test).test(value, to),
            Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
            Eq(truth, ref test, ref to) => (truth, test).test(value, to)
        }
    }
}

#[derive(Copy, EnumIndex)]
pub enum AlphaNode<T: Fact> {
    HASHEQ,
    BOOL(fn(&T) -> &bool, BoolTest),
    I8(fn(&T) -> &i8, I8Test),
    I16(fn(&T) -> &i16, I16Test),
    I32(fn(&T) -> &i32, I32Test),
    I64(fn(&T) -> &i64, I64Test),
    I128(fn(&T) -> &i128, I128Test),
    U8(fn(&T) -> &u8, U8Test),
    U16(fn(&T) -> &u16, U16Test),
    U32(fn(&T) -> &u32, U32Test),
    U64(fn(&T) -> &u64, U64Test),
    U128(fn(&T) -> &u128, U128Test),
    F32(fn(&T) -> &NotNaN<f32>, F32Test),
    F64(fn(&T) -> &NotNaN<f64>, F64Test),
    D128(fn(&T) -> &OrdVar<d128>, D128Test),
    STR(fn(&T) -> &str, StrTest),
    TIME(fn(&T) -> &NaiveTime, TimeTest),
    DATE(fn(&T) -> &Date<Utc>, DateTest),
    DATETIME(fn(&T) -> &DateTime<Utc>, DateTimeTest),
}

macro_rules! alpha_derive {
    ($($t:ident),+ ) => {
        impl<T: Fact> Clone for AlphaNode<T> {
            fn clone(&self) -> Self {
                use self::AlphaNode::*;
                match self {
                    HASHEQ => HASHEQ,
                    $(
                    $t(getter, test) => $t(*getter, test.clone()),
                    )*
                }
            }
        }

        impl <T:Fact> Hash for AlphaNode<T> {
            fn hash < H: Hasher > ( &self, state: & mut H) {
                use self::AlphaNode::*;
                    match self {
                    HASHEQ => self.enum_index().hash(state),
                    $(
                    $t(getter, test) => Self::hash_self(self.enum_index(), (*getter) as usize, test, state),
                    )*
                }
            }
        }

        impl<T:Fact> PartialEq for AlphaNode<T> {
            fn eq(&self, other: &Self) -> bool {
                use self::AlphaNode::*;
                    match (self, other) {
                    (HASHEQ, HASHEQ) => true,
                    $( ($t(getter1, test1), $t(getter2, test2)) => {
                        (*getter1 as usize) == (*getter2 as usize) && test1 == test2
                    },)*
                    _ => false
                }
            }
        }

        impl<T: Fact> Eq for AlphaNode<T> {}

        impl<T:Fact> IsHashEq for AlphaNode<T> {
            fn is_hash_eq(&self) -> bool {
                use self::AlphaNode::*;
                match self {
                    $(
                    $t(_, test) => test.is_hash_eq(),
                    )*
                    _ => unreachable!(),
                }
            }
        }

        impl<T: Fact> From<BetaNode<T>> for AlphaNode<T> {
            fn from(node: BetaNode<T>) -> AlphaNode<T> {
                use self::BetaNode::*;
                match node {
                    $(
                    $t(getter, test) if test.is_alpha() => AlphaNode::$t(getter, test.into()),
                    )*
                    _ => unreachable!("Into AlphaNode with unsupported config")
                }
            }
        }
    };
}

alpha_derive!(
    BOOL,
    I8, I16, I32, I64, I128,
    U8, U16, U32, U64, U128,
    F32, F64, D128,
    STR,
    TIME, DATE, DATETIME
    );

impl<I: Fact> Debug for AlphaNode<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AlphaNode::*;
        write!(f, "Getter(")?;
        match self {
            HASHEQ => write!(f, "HASHEQ")?,
            BOOL(getter, test) => write!(f, "BOOL({:#x}) - {:?}", (*getter) as usize, test)?,
            I8(getter, test) => write!(f, "I8({:#x}) - {:?}", (*getter) as usize, test)?,
            I16(getter, test) => write!(f, "I16({:#x}) - {:?}", (*getter) as usize, test)?,
            I32(getter, test) => write!(f, "I32({:#x}) - {:?}", (*getter) as usize, test)?,
            I64(getter, test) => write!(f, "I64({:#x}) - {:?}", (*getter) as usize, test)?,
            I128(getter, test) => write!(f, "I128({:#x}) - {:?}", (*getter) as usize, test)?,
            U8(getter, test) => write!(f, "U8({:#x}) - {:?}", (*getter) as usize, test)?,
            U16(getter, test) => write!(f, "U16({:#x}) - {:?}", (*getter) as usize, test)?,
            U32(getter, test) => write!(f, "U32({:#x}) - {:?}", (*getter) as usize, test)?,
            U64(getter, test) => write!(f, "U64({:#x}) - {:?}", (*getter) as usize, test)?,
            U128(getter, test) => write!(f, "U128({:#x}) - {:?}", (*getter) as usize, test)?,
            F32(getter, test) => write!(f, "F32({:#x}) - {:?}", (*getter) as usize, test)?,
            F64(getter, test) => write!(f, "F64({:#x}) - {:?}", (*getter) as usize, test)?,
            D128(getter, test) => write!(f, "D128({:#x}) - {:?}", (*getter) as usize, test)?,
            STR(getter, test) => write!(f, "STR({:#x}) - {:?}", (*getter) as usize, test)?,
            TIME(getter, test) => write!(f, "TIME({:#x}) - {:?}", (*getter) as usize, test)?,
            DATE(getter, test) => write!(f, "DATE({:#x}) - {:?}", (*getter) as usize, test)?,
            DATETIME(getter, test) => write!(f, "DATETIME({:#x}) - {:?}", (*getter) as usize, test)?,
        }
        write!(f, ")")
    }
}

impl<T: Fact> AlphaNode<T> {
    fn hash_self<H: Hasher, K: Hash>(ord: usize, getter: usize, test: &K, state: &mut H) {
        ord.hash(state);
        getter.hash(state);
        test.hash(state);
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

macro_rules! from_alphanode_to_hasheq {
    ($($id:tt => $test:tt),+) => {
        impl<T:Fact> From<AlphaNode<T>> for HashEqField {
            fn from(node: AlphaNode<T>) -> Self {
                use self::AlphaNode::*;
                match node {
                    BOOL(getter, BoolTest::Eq(truth, EqTest::Eq, to)) => HashEqField::BOOL(getter as usize, truth.is_not() ^ to),
                    BOOL(getter, BoolTest::Eq(truth, EqTest::Ne, to)) => HashEqField::BOOL(getter as usize, truth.is_not() ^ ! to),
                    $(
                    $id(getter, $test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::$id(getter as usize, to),
                    $id(getter, $test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::$id(getter as usize, to),
                    )*
                    _ => unreachable!("Into HashEqField With Unsupported Config")
                }
            }
        }
    };
}

from_alphanode_to_hasheq!(
    I8 => I8Test,
    I16 => I16Test,
    I32 => I32Test,
    I64 => I64Test,
    U8 => U8Test,
    U16 => U16Test,
    U32 => U32Test,
    U64 => U64Test,
    D128 => D128Test,
    STR => StrTest,
    TIME => TimeTest,
    DATE => DateTest,
    DATETIME => DateTimeTest
);
