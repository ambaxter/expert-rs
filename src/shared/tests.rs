use std::hash::{Hash, Hasher};
use std::string::ToString;
use num::{Integer, Float, ToPrimitive, NumCast};
use num::cast;
use ordered_float::NotNaN;
use float_cmp::ApproxEqUlps;
use runtime::memory::{StringCache, SymbolId};
use ::shared::fact::{Getters, Fact};
use errors::CompileError;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ord_subset::OrdVar;
use decimal::d128;
use std::fmt;
use std::fmt::Debug;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum SLimit<T, S> {
    St(T),
    Local(S),
}

impl<T, S> SLimit<T, S>
    where S: Clone + Into<String> + AsRef<str>, T: Clone {

    pub fn intern(&self, cache: &mut StringCache) -> SLimit<T, SymbolId> {
        use self::SLimit::*;
        match self {
            &St(ref t) => St(t.clone()),
            &Local(ref s) => Local(cache.get_or_intern(s.clone()))
        }
    }
}

impl<S> SLimit<S, S>
    where S: Clone + Into<String> + AsRef<str> {

    pub fn intern_all(&self, cache: &mut StringCache) -> SLimit<SymbolId, SymbolId> {
        use self::SLimit::*;
        match self {
            &St(ref t) => St(cache.get_or_intern(t.clone())),
            &Local(ref s) => Local(cache.get_or_intern(s.clone()))
        }
    }
}


pub trait STest<T: ?Sized>{
    fn test(&self, val: &T, to: &T) -> bool;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DLimit<T, S> {
    St(T, T),
    StLocal(T, S),
    LocalSt(S, T),
    Local(S, S),
}

impl<T, S> DLimit<T, S>
    where S: Clone + Into<String> + AsRef<str>, T: Clone {

    pub fn intern(&self, cache: &mut StringCache) -> DLimit<T, SymbolId> {
        use self::DLimit::*;
        match self {
            &St(ref t1, ref t2) => St(t1.clone(), t2.clone()),
            &StLocal(ref t, ref s) => StLocal(t.clone(), cache.get_or_intern(s.clone())),
            &LocalSt(ref s, ref t) => LocalSt(cache.get_or_intern(s.clone()), t.clone()),
            &Local(ref s1, ref s2) => Local(cache.get_or_intern(s1.clone()), cache.get_or_intern(s2.clone())),
        }
    }
}

impl<S> DLimit<S, S>
    where S: Clone + Into<String> + AsRef<str> {

    pub fn intern_all(&self, cache: &mut StringCache) -> DLimit<SymbolId, SymbolId> {
        use self::DLimit::*;
        match self {
            &St(ref t1, ref t2) => St(cache.get_or_intern(t1.clone()), cache.get_or_intern(t2.clone())),
            &StLocal(ref t, ref s) => StLocal(cache.get_or_intern(t.clone()), cache.get_or_intern(s.clone())),
            &LocalSt(ref s, ref t) => LocalSt(cache.get_or_intern(s.clone()), cache.get_or_intern(t.clone())),
            &Local(ref s1, ref s2) => Local(cache.get_or_intern(s1.clone()), cache.get_or_intern(s2.clone())),
        }
    }
}

pub trait DTest<T: ?Sized>{
    fn test(&self, val: &T, from: &T, to: &T) -> bool;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum OrdTest {
    Lt,
    Le,
    Gt,
    Ge,
}

impl<T> STest<T> for OrdTest
    where T: Ord + ?Sized{
    fn test(&self, val: &T, to: &T) -> bool {
        use self::OrdTest::*;
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

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum EqTest {
    Eq,
    Ne
}

impl<T> STest<T> for EqTest
    where T: Eq + ?Sized {
    fn test(&self, val: &T, to: &T) -> bool {
        use self::EqTest::*;
        match self {
            &Eq => val == to,
            &Ne => val != to,
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrArrayTest {
    Contains,
    StartsWith,
    EndsWith
}

impl<T> STest<T> for StrArrayTest
    where T: AsRef<str> + ?Sized {
    fn test(&self, val: &T, to: &T) -> bool {
        use self::StrArrayTest::*;
        match self {
            &Contains => val.as_ref().contains(to.as_ref()),
            &StartsWith => val.as_ref().starts_with(to.as_ref()),
            &EndsWith => val.as_ref().ends_with(to.as_ref())
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BoolTest<S> {
    EQ(EqTest, SLimit<bool, S>)
}

impl<S> BoolTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    pub fn intern(&self, cache: &mut StringCache) -> BoolTest<SymbolId> {
        use self::BoolTest::*;
        match self {
            &EQ(test, ref limit) => EQ(test, limit.intern(cache))
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum NumberTest<S> {
    ORD(OrdTest, SLimit<OrdVar<d128>, S>),
    BTWN(BetweenTest, DLimit<OrdVar<d128>, S>),
    EQ(EqTest, SLimit<OrdVar<d128>, S>)
}

impl<S> NumberTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    pub fn intern(&self, cache: &mut StringCache) -> NumberTest<SymbolId> {
        use self::NumberTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.intern(cache)),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrTest<S> {
    ORD(OrdTest, SLimit<S, S>),
    BTWN(BetweenTest, DLimit<S, S>),
    EQ(EqTest, SLimit<S, S>),
    StrArrayTest(StrArrayTest, SLimit<S, S>)
}

impl<S> StrTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    pub fn intern(&self, cache: &mut StringCache) -> StrTest<SymbolId> {
        use self::StrTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.intern_all(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.intern_all(cache)),
            &EQ(test, ref limit) => EQ(test, limit.intern_all(cache)),
            &StrArrayTest(test, ref limit) => StrArrayTest(test, limit.intern_all(cache)),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TimeTest<S> {
    ORD(OrdTest, SLimit<NaiveTime, S>),
    BTWN(BetweenTest, DLimit<NaiveTime, S>),
    EQ(EqTest, SLimit<NaiveTime, S>)
}

impl<S> TimeTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    pub fn intern(&self, cache: &mut StringCache) -> TimeTest<SymbolId> {
        use self::TimeTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.intern(cache)),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTest<S> {
    ORD(OrdTest, SLimit<Date<Utc>, S>),
    BTWN(BetweenTest, DLimit<Date<Utc>, S>),
    EQ(EqTest, SLimit<Date<Utc>, S>)
}

impl<S> DateTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    pub fn intern(&self, cache: &mut StringCache) -> DateTest<SymbolId> {
        use self::DateTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.intern(cache)),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTimeTest<S> {
    ORD(OrdTest, SLimit<DateTime<Utc>, S>),
    BTWN(BetweenTest, DLimit<DateTime<Utc>, S>),
    EQ(EqTest, SLimit<DateTime<Utc>, S>)
}

impl<S> DateTimeTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    pub fn intern(&self, cache: &mut StringCache) -> DateTimeTest<SymbolId> {
        use self::DateTimeTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.intern(cache)),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TestRepr<'a> {
    BOOL(&'a str, BoolTest<&'a str>),
    NUMBER(&'a str, NumberTest<&'a str>),
    STR(&'a str, StrTest<&'a str>),
    TIME(&'a str, TimeTest<&'a str>),
    DATE(&'a str, DateTest<&'a str>),
    DATETIME(&'a str, DateTimeTest<&'a str>),
}

impl<'a> TestRepr<'a> {
    pub fn field(&self) -> &str {
        use self::TestRepr::*;
        match self {
            &BOOL(field, _) => field,
            &NUMBER(field, _) => field,
            &STR(field, _) => field,
            &TIME(field, _) => field,
            &DATE(field, _) => field,
            &DATETIME(field, _) => field,
        }
    }

    pub fn field_type(&self) -> &'static str {
        use self::TestRepr::*;
        match self {
            &BOOL(..) => "BOOL",
            &NUMBER(..) => "NUMBER",
            &STR(..) => "STR",
            &TIME(..) => "TIME",
            &DATE(..) => "DATE",
            &DATETIME(..) => "DATETIME",
        }
    }

    pub fn compile<T: Fact>(&self) -> Result<TestData<T>, CompileError> {
        let getter = T::getter(self.field())
            .ok_or_else(|| CompileError::MissingGetter { getter: self.field().to_owned() })?;
        match (&getter, self) {
            //(&Getters::BOOL(accessor), TestRepr::BOOL(_, ref test)) => Ok(TestData::Bool(accessor, test.com))
            _ => Err(CompileError::IncorrectGetter {
                getter: self.field().to_owned(),
                to: self.field_type().to_owned(),
                from: format!("{:?}", getter),
            }),
        }
    }
}

#[derive(Copy, Clone)]
pub enum TestData<T: Fact> {
    BOOL(fn(&T) -> &bool, BoolTest<SymbolId>),
    NUMBER(fn(&T) -> &OrdVar<d128>, NumberTest<SymbolId>),
    STR(fn(&T) -> &str, StrTest<SymbolId>),
    TIME(fn(&T) -> &NaiveTime, TimeTest<SymbolId>),
    DATE(fn(&T) -> &Date<Utc>, DateTest<SymbolId>),
    DATETIME(fn(&T) -> &DateTime<Utc>, DateTimeTest<SymbolId>),
}

impl<T: Fact> TestData<T> {
    fn hash_self<H: Hasher, K: Hash>(ord: usize, getter: usize, test: &K, state: &mut H) {
        ord.hash(state);
        getter.hash(state);
        test.hash(state);
    }
}

macro_rules! test_hash {
    ($($t:ident => $ord:expr),+ ) => {
        impl <T:Fact> Hash for TestData<T> {
            fn hash < H: Hasher > ( & self, state: & mut H) {
                use self::TestData::*;
                    match self {
                    $ ( & $ t(getter, ref test) => Self::hash_self($ord, getter as usize, test, state),
                    )*
                }
            }
        }
    };
}

test_hash!(
        BOOL => 0, NUMBER => 1, STR => 2, TIME => 3,
        DATE => 4, DATETIME => 5
    );

macro_rules! test_eq {
    ($($t:ident),+ ) => {
        impl<T:Fact> PartialEq for TestData<T> {
            fn eq(&self, other: &Self) -> bool {
                use self::TestData::*;
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

test_eq!(BOOL, NUMBER, STR, TIME, DATE, DATETIME);

impl<T: Fact> Eq for TestData<T> {}


impl<I: Fact> Debug for TestData<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::TestData::*;
        write!(f, "Getters(")?;
        match self {
            &BOOL(accessor, test) => write!(f, "BOOL({:#x}) - {:?}", accessor as usize, test)?,
            &NUMBER(accessor, test) => write!(f, "NUMBER({:#x}) - {:?}", accessor as usize, test)?,
            &STR(accessor, test) => write!(f, "STR({:#x}) - {:?}", accessor as usize, test)?,
            &TIME(accessor, test) => write!(f, "TIME({:#x}) - {:?}", accessor as usize, test)?,
            &DATE(accessor, test) => write!(f, "DATE({:#x}) - {:?}", accessor as usize, test)?,
            &DATETIME(accessor, test) => write!(f, "DATETIME({:#x}) - {:?}", accessor as usize, test)?,
            _ => {}
        }
        write!(f, ")")
    }
}

/*





#[cfg(test)]
mod tests {

    #[test]
    pub fn bool_test() {
        use shared::tests::{EqTest, SLimit, BoolTest, TestValue};
        let t: BoolTest<&str> = BoolTest::EQ(EqTest::Ne, SLimit::St(true));
        assert!(t.test_value(&false));
    }
}*/
