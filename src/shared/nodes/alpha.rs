use ord_subset::OrdVar;
use decimal::d128;
use ordered_float::NotNaN;
use std::hash::{Hash, Hasher};
use super::tests::*;
use runtime::memory::SymbolId;
use chrono::NaiveTime;
use chrono::Date;
use chrono::Utc;
use chrono::DateTime;
use shared::fact::Fact;
use shared::fact::FactField;
use shared::context::AlphaContext;
use shared::fact::HashEqField;
use std::fmt::Debug;
use std::fmt;

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
        match self {
            &Eq(truth, ref test, ref to) => (truth, test).test(value, to)
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
                    match self {
                        &Ord(truth, ref test, ref to) => (truth, test).test(value, to),
                        &Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
                        &Eq(truth, ref test, ref to) => (truth, test).test(value, to)
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
                    match self {
                        &Ord(truth, ref test, ref to) => (truth, test).test(value, to),
                        &Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
                        &ApproxEq(truth, ref test, ref to) => (truth, test).test(value, to)
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
    u8 => U8Test,
    u16 => U16Test,
    u32 => U32Test,
    u64 => U64Test,
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
        match self {
            &Ord(truth, ref test, ref to) => (truth, test).test(value, string_cache.resolve(*to).unwrap()),
            &Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, string_cache.resolve(*from).unwrap(), string_cache.resolve(*to).unwrap()),
            &Eq(truth, ref test, ref to) => (truth, test).test(value, string_cache.resolve(*to).unwrap()),
            &Str(truth, ref test, ref to) => (truth, test).test(value, string_cache.resolve(*to).unwrap()),

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
        match self {
            &Ord(truth, ref test, ref to) => (truth, test).test(value, to),
            &Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
            &Eq(truth, ref test, ref to) => (truth, test).test(value, to)
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
        match self {
            &Ord(truth, ref test, ref to) => (truth, test).test(value, to),
            &Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
            &Eq(truth, ref test, ref to) => (truth, test).test(value, to)
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
        match self {
            &Ord(truth, ref test, ref to) => (truth, test).test(value, to),
            &Btwn(truth, ref test, ref from, ref to) => (truth, test).test(value, from, to),
            &Eq(truth, ref test, ref to) => (truth, test).test(value, to)
        }
    }
}

#[derive(Copy, Clone)]
pub enum AlphaNode<T: Fact> {
    BOOL(fn(&T) -> &bool, BoolTest),
    I8(fn(&T) -> &i8, I8Test),
    I16(fn(&T) -> &i16, I16Test),
    I32(fn(&T) -> &i32, I32Test),
    I64(fn(&T) -> &i64, I64Test),
    U8(fn(&T) -> &u8, U8Test),
    U16(fn(&T) -> &u16, U16Test),
    U32(fn(&T) -> &u32, U32Test),
    U64(fn(&T) -> &u64, U64Test),
    F32(fn(&T) -> &NotNaN<f32>, F32Test),
    F64(fn(&T) -> &NotNaN<f64>, F64Test),
    D128(fn(&T) -> &OrdVar<d128>, D128Test),
    STR(fn(&T) -> &str, StrTest),
    TIME(fn(&T) -> &NaiveTime, TimeTest),
    DATE(fn(&T) -> &Date<Utc>, DateTest),
    DATETIME(fn(&T) -> &DateTime<Utc>, DateTimeTest),
}

macro_rules! test_hash {
    ($($t:ident => $ord:expr),+ ) => {
        impl <T:Fact> Hash for AlphaNode<T> {
            fn hash < H: Hasher > ( & self, state: & mut H) {
                use self::AlphaNode::*;
                    match self {
                    $ ( & $ t(getter, ref test) => Self::hash_self($ord, getter as usize, test, state),
                    )*
                }
            }
        }
    };
}

test_hash!(
        BOOL => 0,
        I8 => 1, I16 => 2, I32 => 3, I64 => 4,
        U8 => 5, U16 => 6, U32 => 7, U64 => 8,
        F32 => 9, F64 => 10, D128 => 11,
        STR => 12,
        TIME => 13, DATE => 14, DATETIME => 15
    );

macro_rules! test_eq {
    ($($t:ident),+ ) => {
        impl<T:Fact> PartialEq for AlphaNode<T> {
            fn eq(&self, other: &Self) -> bool {
                use self::AlphaNode::*;
                    match (self, other) {
                    $( (&$t(getter1, ref test1), &$t(getter2, ref test2)) => {
                        (getter1 as usize) == (getter2 as usize) && test1 == test2
                    },)*
                    _ => false
                }
            }
        }
    };
}

test_eq!(
    BOOL,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64, D128,
    STR,
    TIME, DATE, DATETIME
    );

impl<T: Fact> Eq for AlphaNode<T> {}

impl<I: Fact> Debug for AlphaNode<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AlphaNode::*;
        write!(f, "Getter(")?;
        match self {
            &BOOL(getter, test) => write!(f, "BOOL({:#x}) - {:?}", getter as usize, test)?,
            &I8(getter, test) => write!(f, "I8({:#x}) - {:?}", getter as usize, test)?,
            &I16(getter, test) => write!(f, "I16({:#x}) - {:?}", getter as usize, test)?,
            &I32(getter, test) => write!(f, "I32({:#x}) - {:?}", getter as usize, test)?,
            &I64(getter, test) => write!(f, "I64({:#x}) - {:?}", getter as usize, test)?,
            &U8(getter, test) => write!(f, "U8({:#x}) - {:?}", getter as usize, test)?,
            &U16(getter, test) => write!(f, "U16({:#x}) - {:?}", getter as usize, test)?,
            &U32(getter, test) => write!(f, "U32({:#x}) - {:?}", getter as usize, test)?,
            &U64(getter, test) => write!(f, "U64({:#x}) - {:?}", getter as usize, test)?,
            &F32(getter, test) => write!(f, "F32({:#x}) - {:?}", getter as usize, test)?,
            &F64(getter, test) => write!(f, "F64({:#x}) - {:?}", getter as usize, test)?,
            &D128(getter, test) => write!(f, "D128({:#x}) - {:?}", getter as usize, test)?,
            &STR(getter, test) => write!(f, "STR({:#x}) - {:?}", getter as usize, test)?,
            &TIME(getter, test) => write!(f, "TIME({:#x}) - {:?}", getter as usize, test)?,
            &DATE(getter, test) => write!(f, "DATE({:#x}) - {:?}", getter as usize, test)?,
            &DATETIME(getter, test) => write!(f, "DATETIME({:#x}) - {:?}", getter as usize, test)?,
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

impl<T:Fact> Into<HashEqField> for AlphaNode<T> {
    fn into(self) -> HashEqField {
        use self::AlphaNode::*;
        match self {
            BOOL(getter, BoolTest::Eq(truth, EqTest::Eq, to)) => HashEqField::BOOL(getter as usize, truth.is_not() ^ to),
            BOOL(getter, BoolTest::Eq(truth, EqTest::Ne, to)) => HashEqField::BOOL(getter as usize, truth.is_not() ^ !to),
            I8(getter, I8Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::I8(getter as usize, to),
            I8(getter, I8Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::I8(getter as usize, to),
            I16(getter, I16Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::I16(getter as usize, to),
            I16(getter, I16Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::I16(getter as usize, to),
            I32(getter, I32Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::I32(getter as usize, to),
            I32(getter, I32Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::I32(getter as usize, to),
            I64(getter, I64Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::I64(getter as usize, to),
            I64(getter, I64Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::I64(getter as usize, to),
            U8(getter, U8Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::U8(getter as usize, to),
            U8(getter, U8Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::U8(getter as usize, to),
            U16(getter, U16Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::U16(getter as usize, to),
            U16(getter, U16Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::U16(getter as usize, to),
            U32(getter, U32Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::U32(getter as usize, to),
            U32(getter, U32Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::U32(getter as usize, to),
            U64(getter, U64Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::U64(getter as usize, to),
            U64(getter, U64Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::U64(getter as usize, to),
            D128(getter, D128Test::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::D128(getter as usize, to),
            D128(getter, D128Test::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::D128(getter as usize, to),
            STR(getter, StrTest::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::STR(getter as usize, to),
            STR(getter, StrTest::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::STR(getter as usize, to),
            TIME(getter, TimeTest::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::TIME(getter as usize, to),
            TIME(getter, TimeTest::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::TIME(getter as usize, to),
            DATE(getter, DateTest::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::DATE(getter as usize, to),
            DATE(getter, DateTest::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::DATE(getter as usize, to),
            DATETIME(getter, DateTimeTest::Eq(Truth::Is, EqTest::Eq, to)) => HashEqField::DATETIME(getter as usize, to),
            DATETIME(getter, DateTimeTest::Eq(Truth::Not, EqTest::Ne, to)) => HashEqField::DATETIME(getter as usize, to),
            _ => unreachable!("Into HashEqField With Unsupported Config")
        }
    }
}

impl<T: Fact> IsHashEq for AlphaNode<T> {
    fn is_hash_eq(&self) -> bool {
        use self::AlphaNode::*;
        match self {
            BOOL(_, ref test) => test.is_hash_eq(),
            I8(_, ref test) => test.is_hash_eq(),
            I16(_, ref test) => test.is_hash_eq(),
            I32(_, ref test) => test.is_hash_eq(),
            I64(_, ref test) => test.is_hash_eq(),
            U8(_, ref test) => test.is_hash_eq(),
            U16(_, ref test) => test.is_hash_eq(),
            U32(_, ref test) => test.is_hash_eq(),
            U64(_, ref test) => test.is_hash_eq(),
            F32(_, ref test) => test.is_hash_eq(),
            F64(_, ref test) => test.is_hash_eq(),
            D128(_, ref test) => test.is_hash_eq(),
            STR(_, ref test) => test.is_hash_eq(),
            TIME(_, ref test) => test.is_hash_eq(),
            DATE(_, ref test) => test.is_hash_eq(),
            DATETIME(_, ref test) => test.is_hash_eq(),
        }
    }
}
