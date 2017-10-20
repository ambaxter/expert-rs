use std::hash::{Hash, Hasher};
use std::fmt;
use std::fmt::Debug;
use traits::Insert;
use ordered_float::NotNaN;
use runtime::memory::SymbolId;
use num::Float;
use float_cmp::ApproxEqUlps;
use runtime::memory::StringCache;

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum CLimits<T: Hash + Eq + Ord + Clone> {
    S(T),
    D(T, T)
}

#[derive(Clone)]
pub enum OrdData<T: Insert>{
    I8(fn(&T) -> &i8, CLimits<i8>),
    I16(fn(&T) -> &i16, CLimits<i16>),
    I32(fn(&T) -> &i32, CLimits<i32>),
    I64(fn(&T) -> &i64, CLimits<i64>),
    U8(fn(&T) -> &u8, CLimits<u8>),
    U16(fn(&T) -> &u16, CLimits<u16>),
    U32(fn(&T) -> &u32, CLimits<u32>),
    U64(fn(&T) -> &u64, CLimits<u64>),
    ISIZE(fn(&T) -> &isize, CLimits<isize>),
    USIZE(fn(&T) -> &usize, CLimits<usize>),
}

impl<T: Insert> OrdData<T> {
    fn hash_self<H: Hasher, L: Hash>(ord: usize, accessor: usize, limits: &L, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        limits.hash(state);
    }
}

impl<T: Insert> Hash for OrdData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::OrdData::*;
        match self {
            &I8(accessor, ref limits) => {
                Self::hash_self(0, accessor as usize, limits, state);
            },
            &I16(accessor, ref limits) => {
                Self::hash_self(1, accessor as usize, limits, state);
            },
            &I32(accessor, ref limits) => {
                Self::hash_self(2, accessor as usize, limits, state);
            },
            &I64(accessor, ref limits) => {
                Self::hash_self(3, accessor as usize, limits, state);
            },
            &U8(accessor, ref limits) => {
                Self::hash_self(4, accessor as usize, limits, state);
            },
            &U16(accessor, ref limits) => {
                Self::hash_self(5, accessor as usize, limits, state);
            },
            &U32(accessor, ref limits) => {
                Self::hash_self(6, accessor as usize, limits, state);
            },
            &U64(accessor, ref limits) => {
                Self::hash_self(7, accessor as usize, limits, state);
            },
            &ISIZE(accessor, ref limits) => {
                Self::hash_self(8, accessor as usize, limits, state);
            },
            &USIZE(accessor, ref limits) => {
                Self::hash_self(9, accessor as usize, limits, state);
            }
        }
    }
}

impl<T: Insert> PartialEq for OrdData<T> {
    fn eq(&self, other: &Self) -> bool {
        use self::OrdData::*;
        match (self, other) {
            (&I8(accessor1, ref limits1), &I8(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&I16(accessor1, ref limits1), &I16(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&I32(accessor1, ref limits1), &I32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&I64(accessor1, ref limits1), &I64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U8(accessor1, ref limits1), &U8(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U16(accessor1, ref limits1), &U16(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U32(accessor1, ref limits1), &U32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U64(accessor1, ref limits1), &U64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&ISIZE(accessor1, ref limits1), &ISIZE(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&USIZE(accessor1, ref limits1), &USIZE(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            _ => false
        }
    }
}

impl<T: Insert> Eq for OrdData<T> {}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub enum OrdTest {
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    GtLt,
    GeLt,
    GtLe,
    GeLe
}

impl OrdTest {
    fn test<T: Hash + Eq + Ord + Clone>(&self, val: &T, limits: &CLimits<T>) -> bool {
        use self::OrdTest::*;
        use self::CLimits::*;
        match (self, limits) {
            (&Ne, &S(ref to)) => {
                val != to
            },
            (&Lt, &S(ref to)) => {
                val < to
            },
            (&Le, &S(ref to)) => {
                val <= to
            },
            (&Gt, &S(ref to)) => {
                val > to
            },
            (&Ge, &S(ref to)) => {
                val >= to
            },
            (&GtLt, &D(ref from, ref to)) => {
                val > from && val < to
            },
            (&GeLt, &D(ref from, ref to)) => {
                val >= from && val < to
            },
            (&GtLe, &D(ref from, ref to)) => {
                val > from && val <= to
            },
            (&GeLe, &D(ref from, ref to)) => {
                val >= from && val <= to
            },
            _ => unreachable!("Unexpected condition test combination.")
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum FLimits<T: Float> {
    S(NotNaN<T>),
    D(NotNaN<T>, NotNaN<T>)
}

// TODO: Why do we need to do this? Seems silly as NotNaN already provides hash
impl<T: Float> Hash for FLimits<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::FLimits::*;
        match self {
            &S(ref to) => to.hash(state),
            &D(ref from, ref to) => {
                from.hash(state);
                to.hash(state);
            },

        }
    }
}

#[derive(Clone)]
pub enum FlData<T: Insert>{
    F32(fn(&T) -> &f32, FLimits<f32>),
    F64(fn(&T) -> &f64, FLimits<f64>),
}

impl<T: Insert> FlData<T> {
    fn hash_self<H: Hasher, L: Hash>(ord: usize, accessor: usize, limits: &L, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        limits.hash(state);
    }
}

impl<T: Insert> Hash for FlData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::FlData::*;
        match self {
            &F32(accessor, ref limits) => {
                Self::hash_self(0, accessor as usize, limits, state);
            },
            &F64(accessor, ref limits) => {
                Self::hash_self(1, accessor as usize, limits, state);
            },
        }
    }
}

impl<T: Insert> PartialEq for FlData<T> {
    fn eq(&self, other: &Self) -> bool {
        use self::FlData::*;
        match (self, other) {
            (&F32(accessor1, ref limits1), &F32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&F64(accessor1, ref limits1), &F64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            _ => false

        }
    }
}

impl<T: Insert> Eq for FlData<T> {}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub enum FlTest {
    ApproxEq,
    ApproxNe,
    Lt,
    Le,
    Gt,
    Ge,
    GtLt,
    GeLt,
    GtLe,
    GeLe
}

impl FlTest {
    fn test<T: Float>(&self, val: &T, limits: &FLimits<T>) -> bool {
        use self::FlTest::*;
        use self::FLimits::*;
        match (self, limits) {
            (&ApproxEq, &S(ref to)) => {
                (val.to_f64().unwrap()).approx_eq_ulps(&to.to_f64().unwrap(), 2)
            },
            (&ApproxNe, &S(ref to)) => {
                (val.to_f64().unwrap()).approx_ne_ulps(&to.to_f64().unwrap(), 2)
            },
            (&Lt, &S(ref to)) => {
                val < to
            },
            (&Le, &S(ref to)) => {
                val <= to
            },
            (&Gt, &S(ref to)) => {
                val > to
            },
            (&Ge, &S(ref to)) => {
                val >= to
            },
            (&GtLt, &D(ref from, ref to)) => {
                val > from && val < to
            },
            (&GeLt, &D(ref from, ref to)) => {
                val >= from && val < to
            },
            (&GtLe, &D(ref from, ref to)) => {
                val > from && val <= to
            },
            (&GeLe, &D(ref from, ref to)) => {
                val >= from && val <= to
            },
            _ => unreachable!("Unexpected condition test combination.")
        }
    }
}

#[derive(Clone)]
pub enum StrData<T: Insert> {
    REF(fn(&T) -> &str, CLimits<SymbolId>),
}

impl<T: Insert> StrData<T> {
    fn hash_self<H: Hasher, L: Hash>(ord: usize, accessor: usize, limits: &L, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        limits.hash(state);
    }
}

impl<T: Insert> Hash for StrData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::StrData::*;
        match self {
            &REF(accessor, ref limits) => {
                Self::hash_self(0, accessor as usize, limits, state);
            },
        }
    }
}

impl<T: Insert> PartialEq for StrData<T> {
    fn eq(&self, other: &Self) -> bool {
        use self::StrData::*;
        match (self, other) {
            (&REF(accessor1, ref limits1), &REF(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            _ => false
        }
    }
}


impl<T: Insert> Eq for StrData<T> {}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub enum StrTest {
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    GtLt,
    GeLt,
    GtLe,
    GeLe,
    Contains,
    StartsWith,
    EndsWith
}

impl StrTest {
    fn test<T: Hash + Eq + Ord + Clone>(&self, val: &str, limits: &CLimits<SymbolId>, cache: &StringCache) -> bool {
        use self::StrTest::*;
        use self::CLimits::*;
        match (self, limits) {
            (&Ne, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val != to_str
            },
            (&Lt, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val < to_str
            },
            (&Le, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val <= to_str
            },
            (&Gt, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val > to_str
            },
            (&Ge, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val >= to_str
            },
            (&GtLt, &D(from, to)) => {
                let from_str = cache.resolve(from).unwrap();
                let to_str = cache.resolve(to).unwrap();
                val > from_str && val < to_str
            },
            (&GeLt, &D(from, to)) => {
                let from_str = cache.resolve(from).unwrap();
                let to_str = cache.resolve(to).unwrap();
                val >= from_str && val < to_str
            },
            (&GtLe, &D(from, to)) => {
                let from_str = cache.resolve(from).unwrap();
                let to_str = cache.resolve(to).unwrap();
                val > from_str && val <= to_str
            },
            (&GeLe, &D(from, to)) => {
                let from_str = cache.resolve(from).unwrap();
                let to_str = cache.resolve(to).unwrap();
                val >= from_str && val <= to_str
            },
            (&Contains, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val.contains(to_str)
            },
            (&StartsWith, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val.starts_with(to_str)
            },
            (&EndsWith, &S(to)) => {
                let to_str = cache.resolve(to).unwrap();
                val.ends_with(to_str)
            },
            _ => unreachable!("Unexpected condition test combination.")
        }
    }
}

#[derive(Hash, Eq, PartialEq)]
pub enum AlphaTest<T: Insert> {
    HashEq,
    Ord(OrdData<T>, OrdTest),
    Fl(FlData<T>, FlTest),
    Str(StrData<T>, StrTest),
}

impl<T: Insert> AlphaTest<T> {
    pub fn is_hash_eq(&self) -> bool {
        use self::AlphaTest::*;
        match self {
            &HashEq => true,
            _ => false
        }
    }
}

impl<T: Insert> Debug for AlphaTest<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AlphaTest::*;
        write!(f, "Test{{");
        match self {
            &HashEq => {
                write!(f, "HashEq")?
            },
            &Ord(ref data, ref test) => {
                write!(f, "Ord")?
            },
            &Fl(ref data, ref test) => {
                write!(f, "Fl")?
            },
            &Str(ref data, ref test) => {
                write!(f, "Str")?
            }
        }
        write!(f, "}}")
    }
}
