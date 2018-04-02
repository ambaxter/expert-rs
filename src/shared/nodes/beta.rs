use std::hash::{Hash, Hasher};
use std::string::ToString;
use num::{Integer, Float, ToPrimitive, NumCast};
use num::cast;
use ordered_float::NotNaN;
use runtime::memory::{StringCache, SymbolId};
use super::super::fact::{Getters, Fact, FactField, RefField, CastField};
use errors::CompileError;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ord_subset::OrdVar;
use decimal::d128;
use std::fmt;
use std::fmt::Debug;
use string_interner::Symbol;
use shared::context::BetaContext;
use super::tests::*;

pub trait IsHashEq {
    fn is_hash_eq(&self) -> bool;
}

pub trait IsStatic {
    fn is_static(&self) -> bool;
}

pub trait CloneHashEq {
    type Output;

    fn clone_hash_eq(&self) -> Self::Output;
}

pub trait StringIntern {
    type Output;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output;
}

pub trait MapStatic<T, U> {
    type Output;

    fn map_static<F>(&self, func: F) -> Self::Output
        where F: FnMut(&T) -> U;
}

pub trait MapDynamic<T, U> {
    type Output;

    fn map_dynamic<F>(&self, func: F) -> Self::Output
        where F: FnMut(&T) -> U;
}

pub trait MapAll<T, U> {
    type Output;

    fn map_all<F>(&self, func: F) -> Self::Output
        where F: FnMut(&T) -> U;
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum SLimit<T, S> {
    St(T),
    Dyn(S),
}

impl<T> SLimit<T, SymbolId>
    where T: RefField {

    pub fn test_field_ref<C: BetaContext, E: STest<T> >(&self, value: &T, test: &E, context: &C) -> bool {
        use self::SLimit::*;
        match self {
            &St(ref to) => test.test(value, to),
            &Dyn(ref s_to) => test.test(value, T::resolve(context, *s_to))
        }
    }
}

impl<'a> SLimit<&'a str, SymbolId> {

    pub fn test_field_str<C: BetaContext, E: STest<&'a str> >(&self, value: &'a str, test: &E, context: &'a C) -> bool {
        use self::SLimit::*;
        match self {
            &St(ref to) => test.test(&value, to),
            &Dyn(ref s_to) => test.test(&value, & str::resolve(context, *s_to))
        }
    }
}


impl<T> SLimit<T, SymbolId>
    where T: CastField {

    pub fn test_field_cast<C: BetaContext, E: STest<T> >(&self, value: &T, test: &E, context: &C) -> bool {
        use self::SLimit::*;
        match self {
            &St(ref to) => test.test(value, to),
            &Dyn(ref s_to) => test.test(value, &T::resolve(context, *s_to))
        }
    }
}

impl<T, S> CloneHashEq for SLimit<T, S>
    where T: Clone {
    type Output = T;

    fn clone_hash_eq(&self) -> Self::Output {
        use self::SLimit::*;
        match self {
            &St(ref t) => t.clone(),
            &Dyn(ref s) => unreachable!("clone_hash_eq on a local variable")
        }
    }
}

impl<T, S> IsStatic for SLimit<T, S> {
    fn is_static(&self) -> bool {
        use self::SLimit::*;
        match self {
            &St(_) => true,
            &Dyn(_) => false,
        }
    }
}

impl<T, U, S> MapStatic<T, U> for SLimit<T, S>
    where S: Clone {
    type Output = SLimit<U, S>;

    fn map_static<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::SLimit::*;
        match self {
            &St(ref t) => St(func(t)),
            &Dyn(ref s) => Dyn(s.clone())
        }
    }
}

impl<T, U, S> MapDynamic<S, U> for SLimit<T, S>
    where T: Clone {
    type Output = SLimit<T, U>;

    fn map_dynamic<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&S) -> U {

        use self::SLimit::*;
        match self {
            &St(ref t) => St(t.clone()),
            &Dyn(ref s) => Dyn(func(s))
        }
    }
}

impl<T, U> MapAll<T, U> for SLimit<T, T> {
    type Output = SLimit<U, U>;

    fn map_all<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::SLimit::*;
        match self {
            &St(ref t) => St(func(t)),
            &Dyn(ref s) => Dyn(func(s))
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DLimit<T, S> {
    St(T, T),
    StDyn(T, S),
    DynSt(S, T),
    Dyn(S, S),
}

impl<T> DLimit<T, SymbolId>
    where T: RefField {

    pub fn test_field_ref<C: BetaContext, E: DTest<T> >(&self, value: &T, test: &E, context: &C) -> bool {
        use self::DLimit::*;
        match self {
            &St(ref from, ref to) => test.test(value, from, to),
            &StDyn(ref from, ref s_to) => test.test(value, from, T::resolve(context, *s_to)),
            &DynSt(ref s_from, ref to) => test.test(value, T::resolve(context, *s_from), to),
            &Dyn(ref s_from, ref s_to) => test.test(value, T::resolve(context, *s_from), T::resolve(context, *s_to))
        }
    }
}

impl<'a> DLimit<&'a str, SymbolId> {

    pub fn test_field_str<C: BetaContext, E: DTest<&'a str> >(&self, value: &'a str, test: &E, context: &'a C) -> bool {
        use self::DLimit::*;
        match self {
            &St(ref from, ref to) => test.test(&value, from, to),
            &StDyn(ref from, ref s_to) => test.test(&value, from, & str::resolve(context, *s_to)),
            &DynSt(ref s_from, ref to) => test.test(&value, & str::resolve(context, *s_from), to),
            &Dyn(ref s_from, ref s_to) => test.test(&value, & str::resolve(context, *s_from), & str::resolve(context, *s_to))
        }
    }
}

impl<T> DLimit<T, SymbolId>
    where T: CastField {

    pub fn test_field_cast<C: BetaContext, E: DTest<T> >(&self, value: &T, test: &E, context: &C) -> bool {
        use self::DLimit::*;
        match self {
            &St(ref from, ref to) => test.test(value, from, to),
            &StDyn(ref from, ref s_to) => test.test(value, from, &T::resolve(context, *s_to)),
            &DynSt(ref s_from, ref to) => test.test(value, &T::resolve(context, *s_from), to),
            &Dyn(ref s_from, ref s_to) => test.test(value, &T::resolve(context, *s_from), &T::resolve(context, *s_to))
        }
    }
}

impl<T, S> IsStatic for DLimit<T, S> {
    fn is_static(&self) -> bool {
        use self::DLimit::*;
        match self {
            &St(_, _) => true,
            _ => false,
        }
    }
}

impl<T, U, S> MapStatic<T, U> for DLimit<T, S>
    where S: Clone {
    type Output = DLimit<U, S>;

    fn map_static<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::DLimit::*;
        match self {
            &St(ref from, ref to) => St(func(from), func(to)),
            &StDyn(ref from, ref s_to) => StDyn(func(from), s_to.clone()),
            &DynSt(ref s_from, ref to) => DynSt(s_from.clone(), func(to)),
            &Dyn(ref s_from, ref s_to) => Dyn(s_from.clone(), s_to.clone()),
        }
    }
}

impl<T, U, S> MapDynamic<S, U> for DLimit<T, S>
    where T: Clone {
    type Output = DLimit<T, U>;

    fn map_dynamic<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&S) -> U {

        use self::DLimit::*;
        match self {
            &St(ref t1, ref t2) => St(t1.clone(), t2.clone()),
            &StDyn(ref t, ref s) => StDyn(t.clone(), func(s)),
            &DynSt(ref s, ref t) => DynSt(func(s), t.clone()),
            &Dyn(ref s1, ref s2) => Dyn(func(s1), func(s2)),
        }
    }
}

impl<T, U> MapAll<T, U> for DLimit<T, T> {
    type Output = DLimit<U, U>;

    fn map_all<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::DLimit::*;
        match self {
            &St(ref from, ref to) => St(func(from), func(to)),
            &StDyn(ref from, ref s_to) => StDyn(func(from), func(s_to)),
            &DynSt(ref s_from, ref to) => DynSt(func(s_from), func(to)),
            &Dyn(ref s_from, ref s_to) => Dyn(func(s_from), func(s_to)),
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

impl IsHashEq for ApproxEqTest {
    fn is_hash_eq(&self) -> bool {
        false
    }
}

pub trait BetaTestField<T: FactField + ?Sized > {
    fn beta_test_field<C: BetaContext>(&self, value: &T, context: &C) -> bool;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BoolTest<S> {
    EQ(EqTest, SLimit<bool, S>)
}

impl<S> StringIntern for BoolTest<S>
    where S: AsRef<str> {
    type Output = BoolTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::BoolTest::*;
        match self {
            &EQ(test, ref limit) => EQ(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned())))
        }
    }
}

impl<S> IsHashEq for BoolTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::BoolTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_static()
        }
    }
}

impl<S> IsStatic for BoolTest<S> {
    fn is_static(&self) -> bool {
        use self::BoolTest::*;
        match self {
            &EQ(_, ref limit) => limit.is_static()
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

impl BetaTestField<bool> for BoolTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &bool, context: &C) -> bool {
        use self::BoolTest::*;
        match self {
            &EQ(ref test, ref limit) => limit.test_field_ref(value, test, context)
        }
    }
}

macro_rules! beta_number_test {
    ($($id:ty => $test:ident),+) => {
        $(
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test<S> {
                ORD(OrdTest, SLimit<$id, S>),
                BTWN(BetweenTest, DLimit<$id, S>),
                EQ(EqTest, SLimit<$id, S>)
            }


            impl<S> StringIntern for $test<S>
                where S: AsRef<str> {
                type Output = $test<SymbolId>;

                fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
                    use self::$test::*;
                    match self {
                        &ORD(test, ref limit) => ORD(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &BTWN(test, ref limit) => BTWN(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &EQ(test, ref limit) => EQ(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                    }
                }
            }

            impl<S> IsHashEq for $test<S> {
                fn is_hash_eq(&self) -> bool {
                    use self::$test::*;
                    match self {
                        &EQ(test, ref limit) => test.is_hash_eq() && limit.is_static(),
                        _ => false
                    }
                }
            }

            impl<S> IsStatic for $test<S> {
                fn is_static(&self) -> bool {
                    use self::$test::*;
                    match self {
                        &ORD(_, ref limit) => limit.is_static(),
                        &BTWN(_, ref limit) => limit.is_static(),
                        &EQ(_, ref limit) => limit.is_static()
                    }
                }
            }

            impl<S> CloneHashEq for $test<S> {
                type Output = $id;

                fn clone_hash_eq(&self) -> Self::Output {
                    use self::$test::*;
                    match self {
                        &EQ(_, ref limit) => limit.clone_hash_eq(),
                        _ => unreachable!("clone_hash_eq on non hash_eq tests"),
                    }
                }
            }

            impl BetaTestField<$id> for $test<SymbolId> {
                fn beta_test_field<C: BetaContext>(&self, value: &$id, context: &C) -> bool {
                    use self::$test::*;
                    match self {
                        &ORD(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &EQ(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &BTWN(ref test, ref limit) => limit.test_field_cast(value, test, context),

                    }
                }
            }
        )*
    };
}

macro_rules! beta_float_test {
    ($($id:ty => $test:ident),+) => {
        $(
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test<S> {
                ORD(OrdTest, SLimit<$id, S>),
                BTWN(BetweenTest, DLimit<$id, S>),
                APPROX_EQ(ApproxEqTest, SLimit<$id, S>)
            }


            impl<S> StringIntern for $test<S>
                where S: AsRef<str> {
                type Output = $test<SymbolId>;

                fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
                    use self::$test::*;
                    match self {
                        &ORD(test, ref limit) => ORD(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &BTWN(test, ref limit) => BTWN(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &APPROX_EQ(test, ref limit) => APPROX_EQ(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                    }
                }
            }

            impl<S> IsHashEq for $test<S> {
                fn is_hash_eq(&self) -> bool {
                    false
                }
            }

            impl<S> IsStatic for $test<S> {
                fn is_static(&self) -> bool {
                    use self::$test::*;
                    match self {
                        &ORD(_, ref limit) => limit.is_static(),
                        &BTWN(_, ref limit) => limit.is_static(),
                        &APPROX_EQ(_, ref limit) => limit.is_static()
                    }
                }
            }

            impl<S> CloneHashEq for $test<S> {
                type Output = $id;

                fn clone_hash_eq(&self) -> Self::Output {
                    unreachable!("clone_hash_eq on non hash_eq tests")
                }
            }

            impl BetaTestField<$id> for $test<SymbolId> {
                fn beta_test_field<C: BetaContext>(&self, value: &$id, context: &C) -> bool {
                    use self::$test::*;
                    match self {
                        &ORD(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &BTWN(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &APPROX_EQ(ref test, ref limit) => limit.test_field_cast(value, test, context),

                    }
                }
            }
        )*
    };
}

beta_number_test!(
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

beta_float_test!(
    NotNaN<f32> => F32Test,
    NotNaN<f64> => F64Test
);


#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum StrTest<S> {
    ORD(OrdTest, SLimit<S, S>),
    BTWN(BetweenTest, DLimit<S, S>),
    EQ(EqTest, SLimit<S, S>),
    STR(StrArrayTest, SLimit<S, S>)
}

impl<S> StringIntern for StrTest<S>
    where S: AsRef<str> {
    type Output = StrTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::StrTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &BTWN(test, ref limit) => BTWN(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &EQ(test, ref limit) => EQ(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &STR(test, ref limit) => STR(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsHashEq for StrTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::StrTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_static(),
            _ => false
        }
    }
}

impl<S> IsStatic for StrTest<S> {
    fn is_static(&self) -> bool {
        use self::StrTest::*;
        match self {
            &ORD(_, ref limit) => limit.is_static(),
            &BTWN(_, ref limit) => limit.is_static(),
            &EQ(_, ref limit) => limit.is_static(),
            &STR(test, ref limit) => limit.is_static()
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

//TODO: This ended up being pretty painful. Is there a better way to handle this?
impl BetaTestField<str> for StrTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &str, context: &C) -> bool {
        use self::StrTest::*;
        let string_cache = context.get_string_cache();
        match self {
            &ORD(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
            &BTWN(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
            &EQ(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
            &STR(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
        }
    }
}


//TODO: Handle StrTest

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TimeTest<S> {
    ORD(OrdTest, SLimit<NaiveTime, S>),
    BTWN(BetweenTest, DLimit<NaiveTime, S>),
    EQ(EqTest, SLimit<NaiveTime, S>)
}

impl<S> StringIntern for TimeTest<S>
    where S: AsRef<str> {
    type Output = TimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::TimeTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &BTWN(test, ref limit) => BTWN(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &EQ(test, ref limit) => EQ(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsHashEq for TimeTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::TimeTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_static(),
            _ => false
        }
    }
}

impl<S> IsStatic for TimeTest<S> {
    fn is_static(&self) -> bool {
        use self::TimeTest::*;
        match self {
            &ORD(_, ref limit) => limit.is_static(),
            &BTWN(_, ref limit) => limit.is_static(),
            &EQ(_, ref limit) => limit.is_static()
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

impl BetaTestField<NaiveTime> for TimeTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &NaiveTime, context: &C) -> bool {
        use self::TimeTest::*;
        match self {
            &ORD(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &BTWN(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &EQ(ref test, ref limit) => limit.test_field_ref(value, test, context)
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
    where S: AsRef<str> {
    type Output = DateTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &BTWN(test, ref limit) => BTWN(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &EQ(test, ref limit) => EQ(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsHashEq for DateTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::DateTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_static(),
            _ => false
        }
    }
}

impl<S> IsStatic for DateTest<S> {
    fn is_static(&self) -> bool {
        use self::DateTest::*;
        match self {
            &ORD(_, ref limit) => limit.is_static(),
            &BTWN(_, ref limit) => limit.is_static(),
            &EQ(_, ref limit) => limit.is_static()
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

impl BetaTestField<Date<Utc>> for DateTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &Date<Utc>, context: &C) -> bool {
        use self::DateTest::*;
        match self {
            &ORD(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &BTWN(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &EQ(ref test, ref limit) => limit.test_field_ref(value, test, context)
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
    where S: AsRef<str> {
    type Output = DateTimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTimeTest::*;
        match self {
            &ORD(test, ref limit) => ORD(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &BTWN(test, ref limit) => BTWN(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &EQ(test, ref limit) => EQ(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsHashEq for DateTimeTest<S> {
    fn is_hash_eq(&self) -> bool {
        use self::DateTimeTest::*;
        match self {
            &EQ(test, ref limit) => test.is_hash_eq() && limit.is_static(),
            _ => false
        }
    }
}

impl<S> IsStatic for DateTimeTest<S> {
    fn is_static(&self) -> bool {
        use self::DateTimeTest::*;
        match self {
            &ORD(_, ref limit) => limit.is_static(),
            &BTWN(_, ref limit) => limit.is_static(),
            &EQ(_, ref limit) => limit.is_static()
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

impl BetaTestField<DateTime<Utc>> for DateTimeTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &DateTime<Utc>, context: &C) -> bool {
        use self::DateTimeTest::*;
        match self {
            &ORD(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &BTWN(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &EQ(ref test, ref limit) => limit.test_field_ref(value, test, context)
        }
    }
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct SDynLimit<S> {
    pub(crate) limit: S
}

impl<S> StringIntern for SDynLimit<S>
    where S: AsRef<str> {
    type Output = SDynLimit<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        SDynLimit{limit: cache.get_or_intern(self.limit.as_ref().to_owned())}
    }
}

impl<T, S: Symbol> Into<SLimit<T, S>> for SDynLimit<S> {
    fn into(self) -> SLimit<T, S> {
        SLimit::Dyn(self.limit)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum SDynTests {
    ORD(OrdTest),
    EQ(EqTest),
    STR(StrArrayTest)
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct DDynLimit<S> {
    pub(crate) l: S,
    pub(crate) r: S
}

impl<S> StringIntern for DDynLimit<S>
    where S: AsRef<str> {
    type Output = DDynLimit<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        DDynLimit{l: cache.get_or_intern(self.l.as_ref().to_owned()), r: cache.get_or_intern(self.r.as_ref().to_owned())}
    }
}

impl<T, S: Symbol> Into<DLimit<T, S>> for DDynLimit<S> {
    fn into(self) -> DLimit<T, S> {
        DLimit::Dyn(self.l, self.r)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DDynTests {
    BTWN(BetweenTest)
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum TestRepr<S: AsRef<str>> {
    BOOL(S, BoolTest<S>),
    I8(S, I8Test<S>),
    I16(S, I16Test<S>),
    I32(S, I32Test<S>),
    I64(S, I64Test<S>),
    U8(S, U8Test<S>),
    U16(S, U16Test<S>),
    U32(S, U32Test<S>),
    U64(S, U64Test<S>),
    F32(S, F32Test<S>),
    F64(S, F64Test<S>),
    D128(S, D128Test<S>),
    STR(S, StrTest<S>),
    TIME(S, TimeTest<S>),
    DATE(S, DateTest<S>),
    DATETIME(S, DateTimeTest<S>),
    SDYN(S, SDynTests, SDynLimit<S>),
    DDYN(S, DDynTests, DDynLimit<S>),
}

impl<S: AsRef<str>> TestRepr<S>  {
    pub fn field(&self) -> &str {
        use self::TestRepr::*;
        match self {
            &BOOL(ref field, _) => field.as_ref(),
            &I8(ref field, _) => field.as_ref(),
            &I16(ref field, _) => field.as_ref(),
            &I32(ref field, _) => field.as_ref(),
            &I64(ref field, _) => field.as_ref(),
            &U8(ref field, _) => field.as_ref(),
            &U16(ref field, _) => field.as_ref(),
            &U32(ref field, _) => field.as_ref(),
            &U64(ref field, _) => field.as_ref(),
            &F32(ref field, _) => field.as_ref(),
            &F64(ref field, _) => field.as_ref(),
            &D128(ref field, _) => field.as_ref(),
            &STR(ref field, _) => field.as_ref(),
            &TIME(ref field, _) => field.as_ref(),
            &DATE(ref field, _) => field.as_ref(),
            &DATETIME(ref field, _) => field.as_ref(),
            &SDYN(ref field, ..) => field.as_ref(),
            &DDYN(ref field, ..) => field.as_ref(),
        }
    }

    pub fn field_type(&self) -> &'static str {
        use self::TestRepr::*;
        match self {
            &BOOL(..) => "BOOL",
            &I8(..) => "I8",
            &I16(..) => "I16",
            &I32(..) => "I32",
            &I64(..) => "I64",
            &U8(..) => "U8",
            &U16(..) => "U16",
            &U32(..) => "U32",
            &U64(..) => "U64",
            &F32(..) => "F32",
            &F64(..) => "F64",
            &D128(..) => "D128",
            &STR(..) => "STR",
            &TIME(..) => "TIME",
            &DATE(..) => "DATE",
            &DATETIME(..) => "DATETIME",
            &SDYN(..) => "SDYN",
            &DDYN(..) => "DDYN",
        }
    }


    //TODO: So much casting and coercion
    pub fn compile<T: Fact>(&self, cache: &mut StringCache) -> Result<BetaNode<T>, CompileError> {
        let getter = T::getter(self.field())
            .ok_or_else(|| CompileError::MissingGetter { getter: self.field().to_owned() })?;
        match (&getter, self) {
            // BOOL
            (&Getters::BOOL(accessor), &TestRepr::BOOL(_, ref test)) =>
                Ok(BetaNode::BOOL(accessor, test.string_intern(cache))),
            (&Getters::BOOL(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::BOOL(accessor, BoolTest::EQ(test, limit.string_intern(cache).into()))),

            // I8
            (&Getters::I8(accessor), &TestRepr::I8(_, ref test)) =>
                Ok(BetaNode::I8(accessor, test.string_intern(cache))),
            (&Getters::I8(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::I8(accessor, I8Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::I8(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::I8(accessor, I8Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::I8(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::I8(accessor, I8Test::BTWN(test, limit.string_intern(cache).into()))),

            // I16
            (&Getters::I16(accessor), &TestRepr::I16(_, ref test)) =>
                Ok(BetaNode::I16(accessor, test.string_intern(cache))),
            (&Getters::I16(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::I16(accessor, I16Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::I16(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::I16(accessor, I16Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::I16(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::I16(accessor, I16Test::BTWN(test, limit.string_intern(cache).into()))),

            // I32
            (&Getters::I32(accessor), &TestRepr::I32(_, ref test)) =>
                Ok(BetaNode::I32(accessor, test.string_intern(cache))),
            (&Getters::I32(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::I32(accessor, I32Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::I32(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::I32(accessor, I32Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::I32(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::I32(accessor, I32Test::BTWN(test, limit.string_intern(cache).into()))),

            // I64
            (&Getters::I64(accessor), &TestRepr::I64(_, ref test)) =>
                Ok(BetaNode::I64(accessor, test.string_intern(cache))),
            (&Getters::I64(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::I64(accessor, I64Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::I64(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::I64(accessor, I64Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::I64(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::I64(accessor, I64Test::BTWN(test, limit.string_intern(cache).into()))),


            // U8
            (&Getters::U8(accessor), &TestRepr::U8(_, ref test)) =>
                Ok(BetaNode::U8(accessor, test.string_intern(cache))),
            (&Getters::U8(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::U8(accessor, U8Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::U8(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::U8(accessor, U8Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::U8(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::U8(accessor, U8Test::BTWN(test, limit.string_intern(cache).into()))),

            // U16
            (&Getters::U16(accessor), &TestRepr::U16(_, ref test)) =>
                Ok(BetaNode::U16(accessor, test.string_intern(cache))),
            (&Getters::U16(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::U16(accessor, U16Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::U16(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::U16(accessor, U16Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::U16(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::U16(accessor, U16Test::BTWN(test, limit.string_intern(cache).into()))),
            
            // U32
            (&Getters::U32(accessor), &TestRepr::U32(_, ref test)) =>
                Ok(BetaNode::U32(accessor, test.string_intern(cache))),
            (&Getters::U32(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::U32(accessor, U32Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::U32(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::U32(accessor, U32Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::U32(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::U32(accessor, U32Test::BTWN(test, limit.string_intern(cache).into()))),

            // U64
            (&Getters::U64(accessor), &TestRepr::U64(_, ref test)) =>
                Ok(BetaNode::U64(accessor, test.string_intern(cache))),
            (&Getters::U64(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::U64(accessor, U64Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::U64(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::U64(accessor, U64Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::U64(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::U64(accessor, U64Test::BTWN(test, limit.string_intern(cache).into()))),
            
            // F32
            (&Getters::F32(accessor), &TestRepr::F32(_, ref test)) =>
                Ok(BetaNode::F32(accessor, test.string_intern(cache))),
            (&Getters::F32(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::F32(accessor, F32Test::APPROX_EQ(test.into(), limit.string_intern(cache).into()))),
            (&Getters::F32(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::F32(accessor, F32Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::F32(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::F32(accessor, F32Test::BTWN(test, limit.string_intern(cache).into()))),

            // F64
            (&Getters::F64(accessor), &TestRepr::F64(_, ref test)) =>
                Ok(BetaNode::F64(accessor, test.string_intern(cache))),
            (&Getters::F64(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::F64(accessor, F64Test::APPROX_EQ(test.into(), limit.string_intern(cache).into()))),
            (&Getters::F64(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::F64(accessor, F64Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::F64(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::F64(accessor, F64Test::BTWN(test, limit.string_intern(cache).into()))),


            // D128
            (&Getters::D128(accessor), &TestRepr::D128(_, ref test)) =>
                Ok(BetaNode::D128(accessor, test.string_intern(cache))),
            (&Getters::D128(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::D128(accessor, D128Test::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::D128(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::D128(accessor, D128Test::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::D128(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::D128(accessor, D128Test::BTWN(test, limit.string_intern(cache).into()))),

            // STR
            (&Getters::STR(accessor), &TestRepr::STR(_, ref test)) =>
                Ok(BetaNode::STR(accessor, test.string_intern(cache))),
            (&Getters::STR(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::STR(accessor, StrTest::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::STR(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::STR(accessor, StrTest::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::STR(accessor), &TestRepr::SDYN(_, SDynTests::STR(test), ref limit)) =>
                Ok(BetaNode::STR(accessor, StrTest::STR(test, limit.string_intern(cache).into()))),
            (&Getters::STR(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::STR(accessor, StrTest::BTWN(test, limit.string_intern(cache).into()))),

            //TIME
            (&Getters::TIME(accessor), &TestRepr::TIME(_, ref test)) =>
                Ok(BetaNode::TIME(accessor, test.string_intern(cache))),
            (&Getters::TIME(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::TIME(accessor, TimeTest::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::TIME(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::TIME(accessor, TimeTest::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::TIME(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::TIME(accessor, TimeTest::BTWN(test, limit.string_intern(cache).into()))),

            // DATE
            (&Getters::DATE(accessor), &TestRepr::DATE(_, ref test)) =>
                Ok(BetaNode::DATE(accessor, test.string_intern(cache))),
            (&Getters::DATE(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::DATE(accessor, DateTest::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::DATE(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::DATE(accessor, DateTest::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::DATE(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::DATE(accessor, DateTest::BTWN(test, limit.string_intern(cache).into()))),

            // DATETIME
            (&Getters::DATETIME(accessor), &TestRepr::DATETIME(_, ref test)) =>
                Ok(BetaNode::DATETIME(accessor, test.string_intern(cache))),
            (&Getters::DATETIME(accessor), &TestRepr::SDYN(_, SDynTests::EQ(test), ref limit)) =>
                Ok(BetaNode::DATETIME(accessor, DateTimeTest::EQ(test, limit.string_intern(cache).into()))),
            (&Getters::DATETIME(accessor), &TestRepr::SDYN(_, SDynTests::ORD(test), ref limit)) =>
                Ok(BetaNode::DATETIME(accessor, DateTimeTest::ORD(test, limit.string_intern(cache).into()))),
            (&Getters::DATETIME(accessor), &TestRepr::DDYN(_, DDynTests::BTWN(test), ref limit)) =>
                Ok(BetaNode::DATETIME(accessor, DateTimeTest::BTWN(test, limit.string_intern(cache).into()))),

            _ => Err(CompileError::IncorrectGetter {
                getter: self.field().to_owned(),
                to: self.field_type().to_owned(),
                from: format!("{:?}", getter),
            }),
        }
    }
}

#[derive(Copy, Clone)]
pub enum BetaNode<T: Fact> {
    BOOL(fn(&T) -> &bool, BoolTest<SymbolId>),
    I8(fn(&T) -> &i8, I8Test<SymbolId>),
    I16(fn(&T) -> &i16, I16Test<SymbolId>),
    I32(fn(&T) -> &i32, I32Test<SymbolId>),
    I64(fn(&T) -> &i64, I64Test<SymbolId>),
    U8(fn(&T) -> &u8, U8Test<SymbolId>),
    U16(fn(&T) -> &u16, U16Test<SymbolId>),
    U32(fn(&T) -> &u32, U32Test<SymbolId>),
    U64(fn(&T) -> &u64, U64Test<SymbolId>),
    F32(fn(&T) -> &NotNaN<f32>, F32Test<SymbolId>),
    F64(fn(&T) -> &NotNaN<f64>, F64Test<SymbolId>),
    D128(fn(&T) -> &OrdVar<d128>, D128Test<SymbolId>),
    STR(fn(&T) -> &str, StrTest<SymbolId>),
    TIME(fn(&T) -> &NaiveTime, TimeTest<SymbolId>),
    DATE(fn(&T) -> &Date<Utc>, DateTest<SymbolId>),
    DATETIME(fn(&T) -> &DateTime<Utc>, DateTimeTest<SymbolId>),
}

// Add test for StrTest, now that mapping is finished
// new Trait FactTest for BoolTest and the likes
// panic if not found


impl<T: Fact> BetaNode<T> {
    fn hash_self<H: Hasher, K: Hash>(ord: usize, getter: usize, test: &K, state: &mut H) {
        ord.hash(state);
        getter.hash(state);
        test.hash(state);
    }
}

macro_rules! test_hash {
    ($($t:ident => $ord:expr),+ ) => {
        impl <T:Fact> Hash for BetaNode<T> {
            fn hash < H: Hasher > ( & self, state: & mut H) {
                use self::BetaNode::*;
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
        impl<T:Fact> PartialEq for BetaNode<T> {
            fn eq(&self, other: &Self) -> bool {
                use self::BetaNode::*;
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

impl<T: Fact> Eq for BetaNode<T> {}


impl<I: Fact> Debug for BetaNode<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BetaNode::*;
        write!(f, "Getters(")?;
        match self {
            &BOOL(accessor, test) => write!(f, "BOOL({:#x}) - {:?}", accessor as usize, test)?,
            &I8(accessor, test) => write!(f, "I8({:#x}) - {:?}", accessor as usize, test)?,
            &I16(accessor, test) => write!(f, "I16({:#x}) - {:?}", accessor as usize, test)?,
            &I32(accessor, test) => write!(f, "I32({:#x}) - {:?}", accessor as usize, test)?,
            &I64(accessor, test) => write!(f, "I64({:#x}) - {:?}", accessor as usize, test)?,
            &U8(accessor, test) => write!(f, "U8({:#x}) - {:?}", accessor as usize, test)?,
            &U16(accessor, test) => write!(f, "U16({:#x}) - {:?}", accessor as usize, test)?,
            &U32(accessor, test) => write!(f, "U32({:#x}) - {:?}", accessor as usize, test)?,
            &U64(accessor, test) => write!(f, "U64({:#x}) - {:?}", accessor as usize, test)?,
            &F32(accessor, test) => write!(f, "F32({:#x}) - {:?}", accessor as usize, test)?,
            &F64(accessor, test) => write!(f, "F64({:#x}) - {:?}", accessor as usize, test)?,
            &D128(accessor, test) => write!(f, "D128({:#x}) - {:?}", accessor as usize, test)?,
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
        use super::SLimit;
        let s: SLimit<&'static str, &'static str> = SLimit::St("Test");
    }
}