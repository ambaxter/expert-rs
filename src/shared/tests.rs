use std::hash::{Hash, Hasher};
use num::{Integer, Float};
use ordered_float::NotNaN;
use float_cmp::ApproxEqUlps;
use runtime::memory::SymbolId;
use traits::Fact;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum SLimit<T: Hash + Eq + Ord + Copy + Clone, S: Hash + Eq + Ord + Copy + Clone> {
    St(T),
    Local(S),
    Global(S),
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DLimit<T: Hash + Eq + Ord + Copy + Clone, S: Hash + Eq + Ord + Copy + Clone> {
    St(T, T),
    StLocal(T, S),
    StGlobal(T, S),
    LocalSt(S, T),
    GlobalSt(S, T),
    Local(S, S),
    LocalGlobal(S, S),
    GlobalLocal(S, S),
    Global(S, S),
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum PartialOrdTest {
    Lt,
    Le,
    Gt,
    Ge,
}

impl PartialOrdTest {
    pub fn test<T: PartialOrd + ?Sized>(&self, val: &T, to: &T) -> bool {
        use self::PartialOrdTest::*;
        match self {
            &Lt => val < to,
            &Le => val <= to,
            &Gt => val > to,
            &Ge => val >= to,
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BetweenTest {
    GtLt,
    GeLt,
    GtLe,
    GeLe
}

impl BetweenTest {
    pub fn test<T: PartialOrd + ?Sized>(&self, val: &T, from: &T, to: &T) -> bool {
        use self::BetweenTest::*;
        match self {
            &GtLt => val > from && val < to,
            &GeLt => val >= from && val < to,
            &GtLe => val > from && val <= to,
            &GeLe => val >= from && val <= to,
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum EqTest {
    Eq,
    Ne
}

impl EqTest {
    pub fn test<T: Eq + ?Sized>(&self, val: &T, to: &T) -> bool {
        use self::EqTest::*;
        match self {
            &Eq => val == to,
            &Ne => val != to,
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum ApproxEqTest {
    ApproxEq,
    ApproxNe,
}

impl ApproxEqTest {
    pub fn test<T: Float>(&self, val: &T, to: &T) -> bool {
        use self::ApproxEqTest::*;
        match self {
            &ApproxEq => val.to_f64().unwrap().approx_eq_ulps(&to.to_f64().unwrap(), 2),
            &ApproxNe => val.to_f64().unwrap().approx_ne_ulps(&to.to_f64().unwrap(), 2),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum ArrayTest {
    Contains,
    StartsWith,
    EndsWith
}

impl ArrayTest {
    pub fn test(&self, val: &str, to: &str) -> bool {
        use self::ArrayTest::*;
        match self {
            &Contains => val.contains(to),
            &StartsWith => val.starts_with(to),
            &EndsWith => val.ends_with(to)
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BoolTest<S: Hash + Eq + Ord + Copy + Clone> {
    EQ(EqTest, SLimit<bool, S>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum NumTest<T: Integer + Hash + Copy + Clone, S: Hash + Eq + Ord + Copy + Clone> {
    ORD(PartialOrdTest, SLimit<T, S>),
    BTWN(BetweenTest, DLimit<T, S>),
    EQ(EqTest, SLimit<T, S>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum FlTest<T: Hash + Eq + Ord + Copy + Clone, S: Hash + Eq + Ord + Copy + Clone> {
    ORD(PartialOrdTest, SLimit<T, S>),
    BTWN(BetweenTest, DLimit<T, S>),
    APPROXEQ(ApproxEqTest, SLimit<T, S>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrTest<S: Hash + Eq + Ord + Copy + Clone> {
    ORD(PartialOrdTest, SLimit<S, S>),
    BTWN(BetweenTest, DLimit<S, S>),
    EQ(EqTest, SLimit<S, S>),
    ARRAY(ArrayTest, SLimit<S, S>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TestRepr<'a> {
    BOOL(&'a str, BoolTest<&'a str>),
    I8(&'a str, NumTest<i8, &'a str>),
    I16(&'a str, NumTest<i16, &'a str>),
    I32(&'a str, NumTest<i32, &'a str>),
    I64(&'a str, NumTest<i64, &'a str>),
    U8(&'a str, NumTest<u8, &'a str>),
    U16(&'a str, NumTest<u16, &'a str>),
    U32(&'a str, NumTest<u32, &'a str>),
    U64(&'a str, NumTest<u64, &'a str>),
    ISIZE(&'a str, NumTest<isize, &'a str>),
    USIZE(&'a str, NumTest<usize, &'a str>),
    F32(&'a str, FlTest<NotNaN<f32>, &'a str>),
    F64(&'a str, FlTest<NotNaN<f64>, &'a str>),
    STR(&'a str, StrTest<&'a str>),
}

pub enum TestData<T: Fact> {
    // Add AlphaMemory?
    BOOL(fn(&T) -> &bool, BoolTest<SymbolId>),
    I8(fn(&T) -> &i8, NumTest<i8, SymbolId>),
    I16(fn(&T) -> &i16, NumTest<i16, SymbolId>),
    I32(fn(&T) -> &i32, NumTest<i32, SymbolId>),
    I64(fn(&T) -> &i64, NumTest<i64, SymbolId>),
    U8(fn(&T) -> &u8, NumTest<u8, SymbolId>),
    U16(fn(&T) -> &u16, NumTest<u16, SymbolId>),
    U32(fn(&T) -> &u32, NumTest<u32, SymbolId>),
    U64(fn(&T) -> &u64, NumTest<u64, SymbolId>),
    ISIZE(fn(&T) -> &isize, NumTest<isize, SymbolId>),
    USIZE(fn(&T) -> &usize, NumTest<usize, SymbolId>),
    F32(fn(&T) -> &f32, FlTest<NotNaN<f32>, SymbolId>),
    F64(fn(&T) -> &f64, FlTest<NotNaN<f64>, SymbolId>),
    STR(fn(&T) -> &str, StrTest<SymbolId>),
}



macro_rules! test_hash {
    ($($t:ident => $ord:expr),+ ) => {
        impl<T:Fact>Hash for TestData<T> {
            fn hash < H: Hasher > ( & self, state: & mut H) {
                use self::TestData::*;
                    match self {
                    $ ( & $ t(accessor, ref test) => Self::hash_self($ord, accessor as usize, test, state),
                    )*
                }
            }
        }
    };
}


test_hash!(BOOL => 0,
    I8 => 1, I16 => 2, I32 => 3, I64 => 4,
    U8 => 5, U16 => 6, U32 => 7, U64 => 8,
    ISIZE => 9, USIZE => 10,
    F32 => 11, F64 => 12,
    STR => 13
);

macro_rules! test_eq {
    ($($t:ident),+ ) => {
        impl<T:Fact> PartialEq for TestData<T> {
            fn eq(&self, other: &Self) -> bool {
                use self::TestData::*;
                    match (self, other) {
                    $( (&$t(accessor1, ref test1), &$t(accessor2, ref test2)) => {
                        (accessor1 as usize) == (accessor2 as usize) && test1 == test2
                    },)*
                    _ => false
                }
            }
        }
    };
}

test_eq!(BOOL,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    STR);

impl<T: Fact> Eq for TestData<T> {}

impl<T: Fact> TestData<T> {
    fn hash_self<H: Hasher, K: Hash>(ord: usize, accessor: usize, test: &K, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        test.hash(state);
    }
}