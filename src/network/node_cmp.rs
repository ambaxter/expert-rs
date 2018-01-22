use num::Float;
use float_cmp::ApproxEqUlps;
use std::hash::{Hash, Hasher};
use traits::Fact;
use ordered_float::NotNaN;
use runtime::memory::{StringCache, SymbolId};

pub trait DynId { }

impl DynId for SymbolId {}

pub trait RuleContext<I: DynId> {
    fn resolve_i8(&self, id: I) -> &i8;
    fn resolve_i16(&self, id: I) -> &i16;
    fn resolve_i32(&self, id: I) -> &i32;
    fn resolve_i64(&self, id: I) -> &i64;
    fn resolve_u8(&self, id: I) -> &u8;
    fn resolve_u16(&self, id: I) -> &u16;
    fn resolve_u32(&self, id: I) -> &u32;
    fn resolve_u64(&self, id: I) -> &u64;
    fn resolve_isize(&self, id: I) -> &isize;
    fn resolve_usize(&self, id: I) -> &usize;
    fn resolve_f32(&self, id: I) -> &NotNaN<f32>;
    fn resolve_f64(&self, id: I) -> &NotNaN<f64>;
    fn resolve_str(&self, id: I) -> &str;
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum SLimit<T: Hash + Eq + Ord + Copy + Clone> {
    St(T),
    Local(SymbolId),
    Global(SymbolId),
}
/*
// Impl Specialization?

impl<T: Hash + Eq + Ord + Copy + Clone> SLimit<T> {
    pub fn resolve(&self, local_context: &RuleContext<SymbolId>, global_context: &RuleContext<SymbolId>) -> &str {
        use self::SLimit::*;
        match self {
            &St(ref to) => to,
            &Local(ref local) => RuleContext::resolve_str(local_context, *local),
            &Global(ref global) => RuleContext::resolve_str(global_context, *global)
        }
    }
}*/

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DLimit<T: Hash + Eq + Ord + Copy + Clone> {
    St(T, T),
    StLocal(T, SymbolId),
    StGlobal(T, SymbolId),
    LocalSt(SymbolId, T),
    GlobalSt(SymbolId, T),
    Local(SymbolId, SymbolId),
    LocalGlobal(SymbolId, SymbolId),
    GlobalLocal(SymbolId, SymbolId),
    Global(SymbolId, SymbolId),
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
pub enum NumTest<T: Hash + Eq + Ord + Copy + Clone> {
    ORD(PartialOrdTest, SLimit<T>),
    BTWN(BetweenTest, DLimit<T>),
    EQ(EqTest, SLimit<T>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum FlTest<T: Hash + Eq + Ord + Copy + Clone> {
    ORD(PartialOrdTest, SLimit<T>),
    BTWN(BetweenTest, DLimit<T>),
    APPROXEQ(ApproxEqTest, SLimit<T>)
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrTest<T: Hash + Eq + Ord + Copy + Clone> {
    ORD(PartialOrdTest, SLimit<T>),
    BTWN(BetweenTest, DLimit<T>),
    EQ(EqTest, SLimit<T>),
    ARRAY(ArrayTest, SLimit<T>)
}

pub enum TestData<T: Fact> {
    // Add AlphaMemory
    // Add bool
    I8(fn(&T) -> &i8, NumTest<i8>),
    I16(fn(&T) -> &i16, NumTest<i16>),
    I32(fn(&T) -> &i32, NumTest<i32>),
    I64(fn(&T) -> &i64, NumTest<i64>),
    U8(fn(&T) -> &u8, NumTest<u8>),
    U16(fn(&T) -> &u16, NumTest<u16>),
    U32(fn(&T) -> &u32, NumTest<u32>),
    U64(fn(&T) -> &u64, NumTest<u64>),
    ISIZE(fn(&T) -> &isize, NumTest<isize>),
    USIZE(fn(&T) -> &usize, NumTest<usize>),
    F32(fn(&T) -> &f32, FlTest<NotNaN<f32>>),
    F64(fn(&T) -> &f64, FlTest<NotNaN<f64>>),
    STR(fn(&T) -> &str, StrTest<SymbolId>),
}

impl<T: Fact> TestData<T> {
    fn hash_self<H: Hasher, K: Hash>(ord: usize, accessor: usize, test: &K, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        test.hash(state);
    }

    pub fn test(&self, fact: &T, context: &RuleContext<SymbolId>, str_cache: &StringCache) -> bool {
        use self::TestData::*;
        match self {
            /*
            // I8
            &I8(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i8(*id)
                };
                test.test(val, to)
            },
            &I8(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_i8(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_i8(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_i8(*from_id), context.resolve_i8(*to_id)),
                };
                test.test(val, from, to)
            },
            &I8(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i8(*id)
                };
                test.test(val, to)
            },
            // I16
            &I16(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i16(*id)
                };
                test.test(val, to)
            },
            &I16(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_i16(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_i16(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_i16(*from_id), context.resolve_i16(*to_id)),
                };
                test.test(val, from, to)
            },
            &I16(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i16(*id)
                };
                test.test(val, to)
            },
            // I32
            &I32(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i32(*id)
                };
                test.test(val, to)
            },
            &I32(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_i32(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_i32(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_i32(*from_id), context.resolve_i32(*to_id)),
                };
                test.test(val, from, to)
            },
            &I32(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i32(*id)
                };
                test.test(val, to)
            },
            // I64
            &I64(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i64(*id)
                };
                test.test(val, to)
            },
            &I64(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_i64(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_i64(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_i64(*from_id), context.resolve_i64(*to_id)),
                };
                test.test(val, from, to)
            },
            &I64(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_i64(*id)
                };
                test.test(val, to)
            },
            // U8
            &U8(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u8(*id)
                };
                test.test(val, to)
            },
            &U8(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_u8(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_u8(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_u8(*from_id), context.resolve_u8(*to_id)),
                };
                test.test(val, from, to)
            },
            &U8(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u8(*id)
                };
                test.test(val, to)
            },
            // U16
            &U16(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u16(*id)
                };
                test.test(val, to)
            },
            &U16(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_u16(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_u16(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_u16(*from_id), context.resolve_u16(*to_id)),
                };
                test.test(val, from, to)
            },
            &U16(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u16(*id)
                };
                test.test(val, to)
            },
            // U32
            &U32(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u32(*id)
                };
                test.test(val, to)
            },
            &U32(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_u32(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_u32(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_u32(*from_id), context.resolve_u32(*to_id)),
                };
                test.test(val, from, to)
            },
            &U32(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u32(*id)
                };
                test.test(val, to)
            },
            // U64
            &U64(accessor, NumTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u64(*id)
                };
                test.test(val, to)
            },
            &U64(accessor, NumTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_u64(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_u64(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_u64(*from_id), context.resolve_u64(*to_id)),
                };
                test.test(val, from, to)
            },
            &U64(accessor, NumTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_u64(*id)
                };
                test.test(val, to)
            },
            // F32
            &F32(accessor, FlTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_f32(*id)
                };
                test.test(val, to)
            },
            &F32(accessor, FlTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_f32(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_f32(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_f32(*from_id), context.resolve_f32(*to_id)),
                };
                test.test(val, from, to)
            },
            &F32(accessor, FlTest::APPROXEQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_f32(*id)
                };
                test.test(val, to)
            },
            // F64
            &F64(accessor, FlTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_f64(*id)
                };
                test.test(val, to)
            },
            &F64(accessor, FlTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (from, to),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (from, context.resolve_f64(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_f64(*from_id), to),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_f64(*from_id), context.resolve_f64(*to_id)),
                };
                test.test(val, from, to)
            },
            &F64(accessor, FlTest::APPROXEQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => to,
                    &Limit::Dyn(ref id) => context.resolve_f64(*id)
                };
                test.test(val, to)
            },
            // STR
            &STR(accessor, StrTest::ORD(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => str_cache.resolve(*to).expect("to string not found in cache"),
                    &Limit::Dyn(ref id) => context.resolve_str(*id)
                };
                test.test(val, to)
            },
            &STR(accessor, StrTest::BTWN(ref test), Limits::D(ref limit1, ref limit2)) => {
                let val = accessor(fact);
                let (from, to) = match (limit1, limit2) {
                    (&Limit::St(ref from), &Limit::St(ref to)) => (str_cache.resolve(*from).expect("from string not found in cache"), str_cache.resolve(*to).expect("to string not found in cache")),
                    (&Limit::St(ref from), &Limit::Dyn(ref to_id)) => (str_cache.resolve(*from).expect("from string not found in cache"), context.resolve_str(*to_id)),
                    (&Limit::Dyn(ref from_id), &Limit::St(ref to)) => (context.resolve_str(*from_id), str_cache.resolve(*to).expect("to string not found in cache")),
                    (&Limit::Dyn(ref from_id), &Limit::Dyn(ref to_id)) => (context.resolve_str(*from_id), context.resolve_str(*to_id)),
                };
                test.test(val, from, to)
            },
            &STR(accessor, StrTest::EQ(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                match limit {
                    &Limit::St(to) => test.test(&Some(to), &str_cache.get(val)),
                    &Limit::Dyn(ref to_id) => test.test(val, context.resolve_str(*to_id))
                }
            },
            &STR(accessor, StrTest::ARRAY(ref test), Limits::S(ref limit)) => {
                let val = accessor(fact);
                let to = match limit {
                    &Limit::St(ref to) => str_cache.resolve(*to).expect("to string not found in cache"),
                    &Limit::Dyn(ref id) => context.resolve_str(*id)
                };
                test.test(val, to)
            },*/
            _ => false
        }
    }
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


test_hash!(I8 => 0, I16 => 1, I32 => 2, I64 => 3,
    U8 => 4, U16 => 5, U32 => 6, U64 => 7,
    ISIZE => 8, USIZE => 9,
    F32 => 10, F64 => 11,
    STR => 12
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

test_eq!(I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    STR);

impl<T: Fact> Eq for TestData<T> {}
