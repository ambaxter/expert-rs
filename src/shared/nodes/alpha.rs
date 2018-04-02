use ord_subset::OrdVar;
use decimal::d128;
use ordered_float::NotNaN;
use super::tests::*;
use runtime::memory::SymbolId;
use chrono::NaiveTime;
use chrono::Date;
use chrono::Utc;
use chrono::DateTime;
use shared::fact::Fact;
use shared::fact::FactField;
use shared::context::AlphaContext;

pub trait AlphaTestField<T: FactField + ?Sized > {
    fn beta_test_field<C: AlphaContext>(&self, value: &T, context: &C) -> bool;
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BoolTest {
    EQ(EqTest, bool)
}

macro_rules! alpha_number_test {
    ($($id:ty => $test:ident),+) => {
     $(
             #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test {
                ORD(OrdTest, $id),
                BTWN(BetweenTest, $id, $id),
                EQ(EqTest, $id)
            }
     )*
    };
}

macro_rules! alpha_float_test {
    ($($id:ty => $test:ident),+) => {
     $(
             #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test {
                ORD(OrdTest, $id),
                BTWN(BetweenTest, $id, $id),
                APPROX_EQ(ApproxEqTest, $id)
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
    ORD(OrdTest, SymbolId),
    BTWN(BetweenTest, SymbolId, SymbolId),
    EQ(EqTest, SymbolId),
    STR(StrArrayTest, SymbolId)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TimeTest {
    ORD(OrdTest, NaiveTime),
    BTWN(BetweenTest, NaiveTime, NaiveTime),
    EQ(EqTest, NaiveTime)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTest {
    ORD(OrdTest, Date<Utc>),
    BTWN(BetweenTest, Date<Utc>, Date<Utc>),
    EQ(EqTest, Date<Utc>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTimeTest {
    ORD(OrdTest, DateTime<Utc>),
    BTWN(BetweenTest, DateTime<Utc>, DateTime<Utc>),
    EQ(EqTest, DateTime<Utc>)
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