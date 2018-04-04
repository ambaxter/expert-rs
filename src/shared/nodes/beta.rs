use std::hash::{Hash, Hasher};
use std::string::ToString;
use num::{Integer, Float, ToPrimitive, NumCast};
use num::cast;
use ordered_float::NotNaN;
use runtime::memory::{StringCache, SymbolId};
use super::super::fact::{Getter, Fact, FactField, RefField, CastField};
use errors::CompileError;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ord_subset::OrdVar;
use decimal::d128;
use std::fmt;
use std::fmt::Debug;
use string_interner::Symbol;
use shared::context::BetaContext;
use super::tests::*;
use shared::nodes::alpha::AlphaNode;

pub trait IsAlpha {
    fn is_alpha(&self) -> bool;
}

pub trait IsStatic {
    fn is_static(&self) -> bool;
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

pub trait BetaTestField<T: FactField + ?Sized > {
    fn beta_test_field<C: BetaContext>(&self, value: &T, context: &C) -> bool;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BoolTest<S> {
    Eq(EqTest, SLimit<bool, S>)
}

impl<S> StringIntern for BoolTest<S>
    where S: AsRef<str> {
    type Output = BoolTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::BoolTest::*;
        match self {
            &Eq(test, ref limit) => Eq(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned())))
        }
    }
}

impl<S> IsAlpha for BoolTest<S> {
    fn is_alpha(&self) -> bool {
        use self::BoolTest::*;
        match self {
            &Eq(_, ref limit) => limit.is_static()
        }
    }
}

impl<S> Into<super::alpha::BoolTest> for BoolTest<S> {
    fn into(self) -> super::alpha::BoolTest {
        use self::BoolTest::*;
        match self {
            Eq(test, SLimit::St(to)) => super::alpha::BoolTest::Eq(test, to),
            _ => unreachable!("Into Alpha BoolTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for BoolTest<S> {
    fn apply_not(&mut self) {
        use self::BoolTest::*;
        match self {
            &mut Eq(ref mut test, _) => test.apply_not()
        }
    }
}

impl BetaTestField<bool> for BoolTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &bool, context: &C) -> bool {
        use self::BoolTest::*;
        match self {
            &Eq(ref test, ref limit) => limit.test_field_ref(value, test, context)
        }
    }
}

macro_rules! beta_number_test {
    ($($id:ty => $test:ident),+) => {
        $(
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
            pub enum $test<S> {
                Ord(OrdTest, SLimit<$id, S>),
                Btwn(BetweenTest, DLimit<$id, S>),
                Eq(EqTest, SLimit<$id, S>)
            }


            impl<S> StringIntern for $test<S>
                where S: AsRef<str> {
                type Output = $test<SymbolId>;

                fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
                    use self::$test::*;
                    match self {
                        &Ord(test, ref limit) => Ord(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &Btwn(test, ref limit) => Btwn(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &Eq(test, ref limit) => Eq(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                    }
                }
            }

            impl<S> IsAlpha for $test<S> {
                fn is_alpha(&self) -> bool {
                    use self::$test::*;
                    match self {
                        &Ord(_, ref limit) => limit.is_static(),
                        &Btwn(_, ref limit) => limit.is_static(),
                        &Eq(_, ref limit) => limit.is_static()
                    }
                }
            }

            impl<S> Into<super::alpha::$test> for $test<S> {
                fn into(self) -> super::alpha::$test {
                    use self::$test::*;
                    match self {
                        Ord(test, SLimit::St(to)) => super::alpha::$test::Ord(test, to),
                        Btwn(test, DLimit::St(from, to)) => super::alpha::$test::Btwn(test, from, to),
                        Eq(test, SLimit::St(to)) => super::alpha::$test::Eq(test, to),
                        _ => unreachable!("Into Alpha Primitive with Unsupported Config")
                    }
                }
            }

            impl<S> ApplyNot for $test<S> {
                fn apply_not(&mut self) {
                    use self::$test::*;
                    match self {
                        &mut Ord(ref mut test, _) => test.apply_not(),
                        &mut Btwn(ref mut test, _) => test.apply_not(),
                        &mut Eq(ref mut test, _) => test.apply_not(),
                    }
                }
            }

            impl BetaTestField<$id> for $test<SymbolId> {
                fn beta_test_field<C: BetaContext>(&self, value: &$id, context: &C) -> bool {
                    use self::$test::*;
                    match self {
                        &Ord(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &Eq(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &Btwn(ref test, ref limit) => limit.test_field_cast(value, test, context),

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
                Ord(OrdTest, SLimit<$id, S>),
                Btwn(BetweenTest, DLimit<$id, S>),
                ApproxEq(ApproxEqTest, SLimit<$id, S>)
            }


            impl<S> StringIntern for $test<S>
                where S: AsRef<str> {
                type Output = $test<SymbolId>;

                fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
                    use self::$test::*;
                    match self {
                        &Ord(test, ref limit) => Ord(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &Btwn(test, ref limit) => Btwn(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                        &ApproxEq(test, ref limit) => ApproxEq(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
                    }
                }
            }

            impl<S> IsAlpha for $test<S> {
                fn is_alpha(&self) -> bool {
                    use self::$test::*;
                    match self {
                        &Ord(_, ref limit) => limit.is_static(),
                        &Btwn(_, ref limit) => limit.is_static(),
                        &ApproxEq(_, ref limit) => limit.is_static()
                    }
                }
            }

            impl<S> Into<super::alpha::$test> for $test<S> {
                fn into(self) -> super::alpha::$test {
                    use self::$test::*;
                    match self {
                        Ord(test, SLimit::St(to)) => super::alpha::$test::Ord(test, to),
                        Btwn(test, DLimit::St(from, to)) => super::alpha::$test::Btwn(test, from, to),
                        ApproxEq(test, SLimit::St(to)) => super::alpha::$test::ApproxEq(test, to),
                        _ => unreachable!("Into Alpha Float with Unsupported Config")
                    }
                }
            }

            impl<S> ApplyNot for $test<S> {
                fn apply_not(&mut self) {
                    use self::$test::*;
                    match self {
                        &mut Ord(ref mut test, _) => test.apply_not(),
                        &mut Btwn(ref mut test, _) => test.apply_not(),
                        &mut ApproxEq(ref mut test, _) => test.apply_not(),
                    }
                }
            }

            impl BetaTestField<$id> for $test<SymbolId> {
                fn beta_test_field<C: BetaContext>(&self, value: &$id, context: &C) -> bool {
                    use self::$test::*;
                    match self {
                        &Ord(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &Btwn(ref test, ref limit) => limit.test_field_cast(value, test, context),
                        &ApproxEq(ref test, ref limit) => limit.test_field_cast(value, test, context),

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
    Ord(OrdTest, SLimit<S, S>),
    Btwn(BetweenTest, DLimit<S, S>),
    Eq(EqTest, SLimit<S, S>),
    Str(StrArrayTest, SLimit<S, S>)
}

impl<S> StringIntern for StrTest<S>
    where S: AsRef<str> {
    type Output = StrTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::StrTest::*;
        match self {
            &Ord(test, ref limit) => Ord(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Btwn(test, ref limit) => Btwn(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Eq(test, ref limit) => Eq(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Str(test, ref limit) => Str(test, limit.map_all(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsAlpha for StrTest<S> {
    fn is_alpha(&self) -> bool {
        use self::StrTest::*;
        match self {
            &Ord(_, ref limit) => limit.is_static(),
            &Btwn(_, ref limit) => limit.is_static(),
            &Eq(_, ref limit) => limit.is_static(),
            &Str(test, ref limit) => limit.is_static()
        }
    }
}

impl Into<super::alpha::StrTest> for StrTest<SymbolId> {
    fn into(self) -> super::alpha::StrTest {
        use self::StrTest::*;
        match self {
            Ord(test, SLimit::St(to)) => super::alpha::StrTest::Ord(test, to),
            Btwn(test, DLimit::St(from, to)) => super::alpha::StrTest::Btwn(test, from, to),
            Eq(test, SLimit::St(to)) => super::alpha::StrTest::Eq(test, to),
            Str(test, SLimit::St(to)) => super::alpha::StrTest::Str(test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for StrTest<S> {
    fn apply_not(&mut self) {
        use self::StrTest::*;
        match self {
            &mut Ord(ref mut test, _) => test.apply_not(),
            &mut Btwn(ref mut test, _) => test.apply_not(),
            &mut Eq(ref mut test, _) => test.apply_not(),
            &mut Str(ref mut test, _) => test.apply_not(),
        }
    }
}

//TODO: This ended up being pretty painful. Is there a better way to handle this?
impl BetaTestField<str> for StrTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &str, context: &C) -> bool {
        use self::StrTest::*;
        let string_cache = context.get_string_cache();
        match self {
            &Ord(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
            &Btwn(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
            &Eq(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
            &Str(ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&value, test, context),
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum TimeTest<S> {
    Ord(OrdTest, SLimit<NaiveTime, S>),
    Btwn(BetweenTest, DLimit<NaiveTime, S>),
    Eq(EqTest, SLimit<NaiveTime, S>)
}

impl<S> StringIntern for TimeTest<S>
    where S: AsRef<str> {
    type Output = TimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::TimeTest::*;
        match self {
            &Ord(test, ref limit) => Ord(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Btwn(test, ref limit) => Btwn(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Eq(test, ref limit) => Eq(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsAlpha for TimeTest<S> {
    fn is_alpha(&self) -> bool {
        use self::TimeTest::*;
        match self {
            &Ord(_, ref limit) => limit.is_static(),
            &Btwn(_, ref limit) => limit.is_static(),
            &Eq(_, ref limit) => limit.is_static()
        }
    }
}

impl<S> Into<super::alpha::TimeTest> for TimeTest<S> {
    fn into(self) -> super::alpha::TimeTest {
        use self::TimeTest::*;
        match self {
            Ord(test, SLimit::St(to)) => super::alpha::TimeTest::Ord(test, to),
            Btwn(test, DLimit::St(from, to)) => super::alpha::TimeTest::Btwn(test, from, to),
            Eq(test, SLimit::St(to)) => super::alpha::TimeTest::Eq(test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for TimeTest<S> {
    fn apply_not(&mut self) {
        use self::TimeTest::*;
        match self {
            &mut Ord(ref mut test, _) => test.apply_not(),
            &mut Btwn(ref mut test, _) => test.apply_not(),
            &mut Eq(ref mut test, _) => test.apply_not(),
        }
    }
}

impl BetaTestField<NaiveTime> for TimeTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &NaiveTime, context: &C) -> bool {
        use self::TimeTest::*;
        match self {
            &Ord(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &Btwn(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &Eq(ref test, ref limit) => limit.test_field_ref(value, test, context)
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTest<S> {
    Ord(OrdTest, SLimit<Date<Utc>, S>),
    Btwn(BetweenTest, DLimit<Date<Utc>, S>),
    Eq(EqTest, SLimit<Date<Utc>, S>)
}

impl<S> StringIntern for DateTest<S>
    where S: AsRef<str> {
    type Output = DateTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTest::*;
        match self {
            &Ord(test, ref limit) => Ord(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Btwn(test, ref limit) => Btwn(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Eq(test, ref limit) => Eq(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsAlpha for DateTest<S> {
    fn is_alpha(&self) -> bool {
        use self::DateTest::*;
        match self {
            &Ord(_, ref limit) => limit.is_static(),
            &Btwn(_, ref limit) => limit.is_static(),
            &Eq(_, ref limit) => limit.is_static()
        }
    }
}

impl<S> Into<super::alpha::DateTest> for DateTest<S> {
    fn into(self) -> super::alpha::DateTest {
        use self::DateTest::*;
        match self {
            Ord(test, SLimit::St(to)) => super::alpha::DateTest::Ord(test, to),
            Btwn(test, DLimit::St(from, to)) => super::alpha::DateTest::Btwn(test, from, to),
            Eq(test, SLimit::St(to)) => super::alpha::DateTest::Eq(test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for DateTest<S> {
    fn apply_not(&mut self) {
        use self::DateTest::*;
        match self {
            &mut Ord(ref mut test, _) => test.apply_not(),
            &mut Btwn(ref mut test, _) => test.apply_not(),
            &mut Eq(ref mut test, _) => test.apply_not(),
        }
    }
}

impl BetaTestField<Date<Utc>> for DateTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &Date<Utc>, context: &C) -> bool {
        use self::DateTest::*;
        match self {
            &Ord(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &Btwn(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &Eq(ref test, ref limit) => limit.test_field_ref(value, test, context)
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum DateTimeTest<S> {
    Ord(OrdTest, SLimit<DateTime<Utc>, S>),
    Btwn(BetweenTest, DLimit<DateTime<Utc>, S>),
    Eq(EqTest, SLimit<DateTime<Utc>, S>)
}

impl<S> StringIntern for  DateTimeTest<S>
    where S: AsRef<str> {
    type Output = DateTimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTimeTest::*;
        match self {
            &Ord(test, ref limit) => Ord(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Btwn(test, ref limit) => Btwn(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
            &Eq(test, ref limit) => Eq(test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref().to_owned()))),
        }
    }
}

impl<S> IsAlpha for DateTimeTest<S> {
    fn is_alpha(&self) -> bool {
        use self::DateTimeTest::*;
        match self {
            &Ord(_, ref limit) => limit.is_static(),
            &Btwn(_, ref limit) => limit.is_static(),
            &Eq(_, ref limit) => limit.is_static()
        }
    }
}


impl<S> Into<super::alpha::DateTimeTest> for DateTimeTest<S> {
    fn into(self) -> super::alpha::DateTimeTest {
        use self::DateTimeTest::*;
        match self {
            Ord(test, SLimit::St(to)) => super::alpha::DateTimeTest::Ord(test, to),
            Btwn(test, DLimit::St(from, to)) => super::alpha::DateTimeTest::Btwn(test, from, to),
            Eq(test, SLimit::St(to)) => super::alpha::DateTimeTest::Eq(test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}


impl<S> ApplyNot for DateTimeTest<S> {
    fn apply_not(&mut self) {
        use self::DateTimeTest::*;
        match self {
            &mut Ord(ref mut test, _) => test.apply_not(),
            &mut Btwn(ref mut test, _) => test.apply_not(),
            &mut Eq(ref mut test, _) => test.apply_not(),
        }
    }
}

impl BetaTestField<DateTime<Utc>> for DateTimeTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &DateTime<Utc>, context: &C) -> bool {
        use self::DateTimeTest::*;
        match self {
            &Ord(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &Btwn(ref test, ref limit) => limit.test_field_ref(value, test, context),
            &Eq(ref test, ref limit) => limit.test_field_ref(value, test, context)
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
    Ord(OrdTest),
    Eq(EqTest),
    Str(StrArrayTest)
}


impl ApplyNot for SDynTests {
    fn apply_not(&mut self) {
        use self::SDynTests::*;
        match self {
            &mut Ord(ref mut test) => test.apply_not(),
            &mut Eq(ref mut test) => test.apply_not(),
            &mut Str(ref mut test) => test.apply_not(),
        }
    }
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
    Btwn(BetweenTest)
}

impl ApplyNot for DDynTests {
    fn apply_not(&mut self) {
        use self::DDynTests::*;
        match self {
            &mut Btwn(ref mut test) => test.apply_not(),
        }
    }
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
            (&Getter::BOOL(getter), &TestRepr::BOOL(_, ref test)) =>
                Ok(BetaNode::BOOL(getter, test.string_intern(cache))),
            (&Getter::BOOL(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::BOOL(getter, BoolTest::Eq(test, limit.string_intern(cache).into()))),

            // I8
            (&Getter::I8(getter), &TestRepr::I8(_, ref test)) =>
                Ok(BetaNode::I8(getter, test.string_intern(cache))),
            (&Getter::I8(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I8(getter, I8Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::I8(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I8(getter, I8Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::I8(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I8(getter, I8Test::Btwn(test, limit.string_intern(cache).into()))),

            // I16
            (&Getter::I16(getter), &TestRepr::I16(_, ref test)) =>
                Ok(BetaNode::I16(getter, test.string_intern(cache))),
            (&Getter::I16(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I16(getter, I16Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::I16(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I16(getter, I16Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::I16(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I16(getter, I16Test::Btwn(test, limit.string_intern(cache).into()))),

            // I32
            (&Getter::I32(getter), &TestRepr::I32(_, ref test)) =>
                Ok(BetaNode::I32(getter, test.string_intern(cache))),
            (&Getter::I32(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I32(getter, I32Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::I32(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I32(getter, I32Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::I32(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I32(getter, I32Test::Btwn(test, limit.string_intern(cache).into()))),

            // I64
            (&Getter::I64(getter), &TestRepr::I64(_, ref test)) =>
                Ok(BetaNode::I64(getter, test.string_intern(cache))),
            (&Getter::I64(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I64(getter, I64Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::I64(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I64(getter, I64Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::I64(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I64(getter, I64Test::Btwn(test, limit.string_intern(cache).into()))),


            // U8
            (&Getter::U8(getter), &TestRepr::U8(_, ref test)) =>
                Ok(BetaNode::U8(getter, test.string_intern(cache))),
            (&Getter::U8(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U8(getter, U8Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::U8(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U8(getter, U8Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::U8(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U8(getter, U8Test::Btwn(test, limit.string_intern(cache).into()))),

            // U16
            (&Getter::U16(getter), &TestRepr::U16(_, ref test)) =>
                Ok(BetaNode::U16(getter, test.string_intern(cache))),
            (&Getter::U16(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U16(getter, U16Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::U16(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U16(getter, U16Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::U16(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U16(getter, U16Test::Btwn(test, limit.string_intern(cache).into()))),
            
            // U32
            (&Getter::U32(getter), &TestRepr::U32(_, ref test)) =>
                Ok(BetaNode::U32(getter, test.string_intern(cache))),
            (&Getter::U32(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U32(getter, U32Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::U32(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U32(getter, U32Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::U32(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U32(getter, U32Test::Btwn(test, limit.string_intern(cache).into()))),

            // U64
            (&Getter::U64(getter), &TestRepr::U64(_, ref test)) =>
                Ok(BetaNode::U64(getter, test.string_intern(cache))),
            (&Getter::U64(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U64(getter, U64Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::U64(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U64(getter, U64Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::U64(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U64(getter, U64Test::Btwn(test, limit.string_intern(cache).into()))),
            
            // F32
            (&Getter::F32(getter), &TestRepr::F32(_, ref test)) =>
                Ok(BetaNode::F32(getter, test.string_intern(cache))),
            (&Getter::F32(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::F32(getter, F32Test::ApproxEq(test.into(), limit.string_intern(cache).into()))),
            (&Getter::F32(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::F32(getter, F32Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::F32(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::F32(getter, F32Test::Btwn(test, limit.string_intern(cache).into()))),

            // F64
            (&Getter::F64(getter), &TestRepr::F64(_, ref test)) =>
                Ok(BetaNode::F64(getter, test.string_intern(cache))),
            (&Getter::F64(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::F64(getter, F64Test::ApproxEq(test.into(), limit.string_intern(cache).into()))),
            (&Getter::F64(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::F64(getter, F64Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::F64(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::F64(getter, F64Test::Btwn(test, limit.string_intern(cache).into()))),


            // D128
            (&Getter::D128(getter), &TestRepr::D128(_, ref test)) =>
                Ok(BetaNode::D128(getter, test.string_intern(cache))),
            (&Getter::D128(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::D128(getter, D128Test::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::D128(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::D128(getter, D128Test::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::D128(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::D128(getter, D128Test::Btwn(test, limit.string_intern(cache).into()))),

            // STR
            (&Getter::STR(getter), &TestRepr::STR(_, ref test)) =>
                Ok(BetaNode::STR(getter, test.string_intern(cache))),
            (&Getter::STR(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::STR(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::STR(getter), &TestRepr::SDYN(_, SDynTests::Str(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Str(test, limit.string_intern(cache).into()))),
            (&Getter::STR(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Btwn(test, limit.string_intern(cache).into()))),

            //TIME
            (&Getter::TIME(getter), &TestRepr::TIME(_, ref test)) =>
                Ok(BetaNode::TIME(getter, test.string_intern(cache))),
            (&Getter::TIME(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::TIME(getter, TimeTest::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::TIME(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::TIME(getter, TimeTest::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::TIME(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::TIME(getter, TimeTest::Btwn(test, limit.string_intern(cache).into()))),

            // DATE
            (&Getter::DATE(getter), &TestRepr::DATE(_, ref test)) =>
                Ok(BetaNode::DATE(getter, test.string_intern(cache))),
            (&Getter::DATE(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::DATE(getter, DateTest::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::DATE(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::DATE(getter, DateTest::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::DATE(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::DATE(getter, DateTest::Btwn(test, limit.string_intern(cache).into()))),

            // DATETIME
            (&Getter::DATETIME(getter), &TestRepr::DATETIME(_, ref test)) =>
                Ok(BetaNode::DATETIME(getter, test.string_intern(cache))),
            (&Getter::DATETIME(getter), &TestRepr::SDYN(_, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::DATETIME(getter, DateTimeTest::Eq(test, limit.string_intern(cache).into()))),
            (&Getter::DATETIME(getter), &TestRepr::SDYN(_, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::DATETIME(getter, DateTimeTest::Ord(test, limit.string_intern(cache).into()))),
            (&Getter::DATETIME(getter), &TestRepr::DDYN(_, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::DATETIME(getter, DateTimeTest::Btwn(test, limit.string_intern(cache).into()))),

            _ => Err(CompileError::IncorrectGetter {
                getter: self.field().to_owned(),
                to: self.field_type().to_owned(),
                from: format!("{:?}", getter),
            }),
        }
    }
}

impl<S: AsRef<str>> ApplyNot for TestRepr<S> {
    fn apply_not(&mut self) {
        use self::TestRepr::*;
        match self {
            &mut BOOL(_, ref mut test) => test.apply_not(),
            &mut I8(_, ref mut test) => test.apply_not(),
            &mut I16(_, ref mut test) => test.apply_not(),
            &mut I32(_, ref mut test) => test.apply_not(),
            &mut I64(_, ref mut test) => test.apply_not(),
            &mut U8(_, ref mut test) => test.apply_not(),
            &mut U16(_, ref mut test) => test.apply_not(),
            &mut U32(_, ref mut test) => test.apply_not(),
            &mut U64(_, ref mut test) => test.apply_not(),
            &mut F32(_, ref mut test) => test.apply_not(),
            &mut F64(_, ref mut test) => test.apply_not(),
            &mut D128(_, ref mut test) => test.apply_not(),
            &mut STR(_, ref mut test) => test.apply_not(),
            &mut TIME(_, ref mut test) => test.apply_not(),
            &mut DATE(_, ref mut test) => test.apply_not(),
            &mut DATETIME(_, ref mut test) => test.apply_not(),
            &mut SDYN(_, ref mut test, _) => test.apply_not(),
            &mut DDYN(_, ref mut test, _) => test.apply_not(),
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

impl<T:Fact> IsAlpha for BetaNode<T> {
    fn is_alpha(&self) -> bool {
        use self::BetaNode::*;
        match self {
            &BOOL(_, test) => test.is_alpha(),
            &I8(_, test) => test.is_alpha(),
            &I16(_, test) => test.is_alpha(),
            &I32(_, test) => test.is_alpha(),
            &I64(_, test) => test.is_alpha(),
            &U8(_, test) => test.is_alpha(),
            &U16(_, test) => test.is_alpha(),
            &U32(_, test) => test.is_alpha(),
            &U64(_, test) => test.is_alpha(),
            &F32(_, test) => test.is_alpha(),
            &F64(_, test) => test.is_alpha(),
            &D128(_, test) => test.is_alpha(),
            &STR(_, test) => test.is_alpha(),
            &TIME(_, test) => test.is_alpha(),
            &DATE(_, test) => test.is_alpha(),
            &DATETIME(_, test) => test.is_alpha(),
        }
    }
}

impl<T: Fact> Into<AlphaNode<T>> for BetaNode<T> {
    fn into(self) -> AlphaNode<T> {
        use self::BetaNode::*;
        match self {
            BOOL(getter, test) if test.is_alpha() => AlphaNode::BOOL(getter, test.into()),
            I8(getter, test) if test.is_alpha() => AlphaNode::I8(getter, test.into()),
            I16(getter, test) if test.is_alpha() => AlphaNode::I16(getter, test.into()),
            I32(getter, test) if test.is_alpha() => AlphaNode::I32(getter, test.into()),
            I64(getter, test) if test.is_alpha() => AlphaNode::I64(getter, test.into()),
            U8(getter, test) if test.is_alpha() => AlphaNode::U8(getter, test.into()),
            U16(getter, test) if test.is_alpha() => AlphaNode::U16(getter, test.into()),
            U32(getter, test) if test.is_alpha() => AlphaNode::U32(getter, test.into()),
            U64(getter, test) if test.is_alpha() => AlphaNode::U64(getter, test.into()),
            F32(getter, test) if test.is_alpha() => AlphaNode::F32(getter, test.into()),
            F64(getter, test) if test.is_alpha() => AlphaNode::F64(getter, test.into()),
            D128(getter, test) if test.is_alpha() => AlphaNode::D128(getter, test.into()),
            STR(getter, test) if test.is_alpha() => AlphaNode::STR(getter, test.into()),
            TIME(getter, test) if test.is_alpha() => AlphaNode::TIME(getter, test.into()),
            DATE(getter, test) if test.is_alpha() => AlphaNode::DATE(getter, test.into()),
            DATETIME(getter, test) if test.is_alpha() => AlphaNode::DATETIME(getter, test.into()),
            _ => unreachable!("Into AlphaNode with unsupported config")
        }
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
