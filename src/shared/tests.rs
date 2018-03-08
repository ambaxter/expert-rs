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

pub trait IsHashEq {
    fn is_hash_eq(&self) -> bool;
}

pub trait CloneHashEq {
    type Output;

    fn clone_hash_eq(&self) -> Self::Output;
}

pub trait StringIntern {
    type Output;

    fn string_intern(&self, u: &mut StringCache) -> Self::Output;
}

pub trait StringInternAll {
    type Output;

    fn string_intern_all(&self, u: &mut StringCache) -> Self::Output;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum SLimit<T, S> {
    St(T),
    Local(S),
}

impl<T, S> CloneHashEq for SLimit<T, S>
    where T: Clone {
    type Output = T;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::SLimit::*;
        match self {
            &St(ref t) => t.clone(),
            &Local(ref s) => unreachable!("clone_hash_eq on a local variable")
        }
    }
}

impl<T, S> IsHashEq for SLimit<T, S> {
    fn is_hash_eq(&self) -> bool {
        use self::SLimit::*;
        match self {
            &St(_) => true,
            &Local(_) => false,
        }
    }
}

impl<T, S> StringIntern for SLimit<T, S>
    where S: Clone + Into<String> + AsRef<str>, T: Clone {
    type Output = SLimit<T, SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::SLimit::*;
        match self {
            &St(ref t) => St(t.clone()),
            &Local(ref s) => Local(cache.get_or_intern(s.clone()))
        }
    }
}

impl<S> StringInternAll for SLimit<S, S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = SLimit<SymbolId, SymbolId>;

    fn string_intern_all(&self, cache: &mut StringCache) -> Self::Output {
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

impl<T, S> StringIntern for DLimit<T, S>
    where S: Clone + Into<String> + AsRef<str>, T: Clone {
    type Output = DLimit<T, SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DLimit::*;
        match self {
            &St(ref t1, ref t2) => St(t1.clone(), t2.clone()),
            &StLocal(ref t, ref s) => StLocal(t.clone(), cache.get_or_intern(s.clone())),
            &LocalSt(ref s, ref t) => LocalSt(cache.get_or_intern(s.clone()), t.clone()),
            &Local(ref s1, ref s2) => Local(cache.get_or_intern(s1.clone()), cache.get_or_intern(s2.clone())),
        }
    }
}

impl<S> StringInternAll for DLimit<S, S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = DLimit<SymbolId, SymbolId>;

    fn string_intern_all(&self, cache: &mut StringCache) -> Self::Output {
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

impl IsHashEq for EqTest {
    fn is_hash_eq(&self) -> bool {
        use self::EqTest::*;
        match self {
            &Eq => true,
            &Ne => false,
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

impl<S> StringIntern for BoolTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = BoolTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::BoolTest::*;
        match self {
            &EQ(test, ref limit) => EQ(test, limit.string_intern(cache))
        }
    }
}

impl<S> IsHashEq for BoolTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::BoolTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_hash_eq()
        }
    }
}


impl<S> CloneHashEq for BoolTest<S> {
    type Output = bool;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::BoolTest::*;
        match self {
            &EQ(_, ref limit) => limit.clone_hash_eq(),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum NumberTest<S> {
    ORD(OrdTest, SLimit<OrdVar<d128>, S>),
    BTWN(BetweenTest, DLimit<OrdVar<d128>, S>),
    EQ(EqTest, SLimit<OrdVar<d128>, S>)
}

impl<S> StringIntern for NumberTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = NumberTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::NumberTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.string_intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.string_intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.string_intern(cache)),
        }
    }
}

impl<S> IsHashEq for NumberTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::NumberTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_hash_eq(),
            _ => false
        }
    }
}

impl<S> CloneHashEq for NumberTest<S> {
    type Output = OrdVar<d128>;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::NumberTest::*;
        match self {
            &EQ(_, ref limit) => limit.clone_hash_eq(),
            _ => unreachable!("clone_hash_eq on non hash_eq tests"),
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

impl<S> StringIntern for StrTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = StrTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::StrTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.string_intern_all(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.string_intern_all(cache)),
            &EQ(test, ref limit) => EQ(test, limit.string_intern_all(cache)),
            &StrArrayTest(test, ref limit) => StrArrayTest(test, limit.string_intern_all(cache)),
        }
    }
}

impl<S> IsHashEq for StrTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::StrTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_hash_eq(),
            _ => false
        }
    }
}

impl<S> CloneHashEq for StrTest<S>
    where S: Clone {
    type Output = S;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::StrTest::*;
        match self {
            &EQ(_, ref limit) => limit.clone_hash_eq(),
            _ => unreachable!("clone_hash_eq on non hash_eq tests"),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TimeTest<S> {
    ORD(OrdTest, SLimit<NaiveTime, S>),
    BTWN(BetweenTest, DLimit<NaiveTime, S>),
    EQ(EqTest, SLimit<NaiveTime, S>)
}

impl<S> StringIntern for TimeTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = TimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::TimeTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.string_intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.string_intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.string_intern(cache)),
        }
    }
}

impl<S> IsHashEq for TimeTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::TimeTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_hash_eq(),
            _ => false
        }
    }
}

impl<S> CloneHashEq for TimeTest<S> {
    type Output = NaiveTime;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::TimeTest::*;
        match self {
            &EQ(_, ref limit) => limit.clone_hash_eq(),
            _ => unreachable!("clone_hash_eq on non hash_eq tests"),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTest<S> {
    ORD(OrdTest, SLimit<Date<Utc>, S>),
    BTWN(BetweenTest, DLimit<Date<Utc>, S>),
    EQ(EqTest, SLimit<Date<Utc>, S>)
}

impl<S> StringIntern for DateTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = DateTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.string_intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.string_intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.string_intern(cache)),
        }
    }
}

impl<S> IsHashEq for DateTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::DateTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_hash_eq(),
            _ => false
        }
    }
}

impl<S> CloneHashEq for DateTest<S> {
    type Output = Date<Utc>;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::DateTest::*;
        match self {
            &EQ(_, ref limit) => limit.clone_hash_eq(),
            _ => unreachable!("clone_hash_eq on non hash_eq tests"),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTimeTest<S> {
    ORD(OrdTest, SLimit<DateTime<Utc>, S>),
    BTWN(BetweenTest, DLimit<DateTime<Utc>, S>),
    EQ(EqTest, SLimit<DateTime<Utc>, S>)
}

impl<S> StringIntern for  DateTimeTest<S>
    where S: Clone + Into<String> + AsRef<str> {
    type Output = DateTimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTimeTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.string_intern(cache)),
            &BTWN(test, ref limit) => BTWN(test, limit.string_intern(cache)),
            &EQ(test, ref limit) => EQ(test, limit.string_intern(cache)),
        }
    }
}

impl<S> IsHashEq for DateTimeTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::DateTimeTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_hash_eq(),
            _ => false
        }
    }
}

impl<S> CloneHashEq for DateTimeTest<S> {
    type Output = DateTime<Utc>;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::DateTimeTest::*;
        match self {
            &EQ(_, ref limit) => limit.clone_hash_eq(),
            _ => unreachable!("clone_hash_eq on non hash_eq tests"),
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

    pub fn compile<T: Fact>(&self, cache: &mut StringCache) -> Result<TestData<T>, CompileError> {
        let getter = T::getter(self.field())
            .ok_or_else(|| CompileError::MissingGetter { getter: self.field().to_owned() })?;
        match (&getter, self) {
            (&Getters::BOOL(accessor), &TestRepr::BOOL(_, ref test)) =>
                Ok(TestData::BOOL(accessor, test.string_intern(cache))),
            (&Getters::NUMBER(accessor), &TestRepr::NUMBER(_, ref test)) =>
                Ok(TestData::NUMBER(accessor, test.string_intern(cache))),
            (&Getters::STR(accessor), &TestRepr::STR(_, ref test)) =>
                Ok(TestData::STR(accessor, test.string_intern(cache))),
            (&Getters::TIME(accessor), &TestRepr::TIME(_, ref test)) =>
                Ok(TestData::TIME(accessor, test.string_intern(cache))),
            (&Getters::DATE(accessor), &TestRepr::DATE(_, ref test)) =>
                Ok(TestData::DATE(accessor, test.string_intern(cache))),
            (&Getters::DATETIME(accessor), &TestRepr::DATETIME(_, ref test)) =>
                Ok(TestData::DATETIME(accessor, test.string_intern(cache))),
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

#[cfg(test)]
mod tests {

    #[test]
    pub fn str_test() {
        use shared::tests::SLimit;
        let s: SLimit<&'static str, &'static str> = SLimit::St("Test");
    }
}
