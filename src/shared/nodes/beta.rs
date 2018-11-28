use std::hash::{Hash, Hasher};
use std::string::ToString;
use num::{Integer, Float, ToPrimitive, NumCast};
use num::cast;
use ordered_float::NotNaN;
use runtime::memory::{StringCache, SymbolId};
use super::super::fact::{Getter, Fact, FactField, RefField, CastField, FactFieldType, GetFieldType};
use errors::CompileError;
use chrono::{NaiveTime, Date, DateTime, Duration, Utc};
use ord_subset::OrdVar;
use decimal::d128;
use std::fmt;
use std::fmt::Debug;
use string_interner::Symbol;
use shared::context::BetaContext;
use super::tests::*;
use enum_index;
use enum_index::EnumIndex;
use std::cmp::Ordering;
use std::collections::HashSet;
use std::collections::HashMap;


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

pub trait CollectRequired {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>);
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum SLimit<T, S> {
    St(T),
    Dyn(S),
}

impl<T> SLimit<T, SymbolId>
    where T: RefField {

    pub fn test_field_ref<C: BetaContext, E: STest<T> >(&self, test: &E, value: &T, context: &C) -> bool {
        use self::SLimit::*;
        match *self {
            St(ref to) => test.test(value, to),
            Dyn(ref s_to) => test.test(value, T::resolve(context, *s_to))
        }
    }
}

impl<'a> SLimit<&'a str, SymbolId> {

    pub fn test_field_str<C: BetaContext, E: STest<&'a str> >(&self, test: &E, value: &'a str, context: &'a C) -> bool {
        use self::SLimit::*;
        match *self {
            St(ref to) => test.test(&value, to),
            Dyn(ref s_to) => test.test(&value, & str::resolve(context, *s_to))
        }
    }
}

impl<T> SLimit<T, SymbolId>
    where T: CastField {

    pub fn test_field_cast<C: BetaContext, E: STest<T> >(&self, test: &E, value: &T, context: &C) -> bool {
        use self::SLimit::*;
        match *self {
            St(ref to) => test.test(value, to),
            Dyn(ref s_to) => test.test(value, &T::resolve(context, *s_to))
        }
    }
}

impl<T, S> IsStatic for SLimit<T, S> {
    fn is_static(&self) -> bool {
        use self::SLimit::*;
        match *self {
            St(_) => true,
            Dyn(_) => false,
        }
    }
}

impl<T, U, S> MapStatic<T, U> for SLimit<T, S>
    where S: Clone {
    type Output = SLimit<U, S>;

    fn map_static<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::SLimit::*;
        match *self {
            St(ref t) => St(func(t)),
            Dyn(ref s) => Dyn(s.clone())
        }
    }
}

impl<T, U, S> MapDynamic<S, U> for SLimit<T, S>
    where T: Clone {
    type Output = SLimit<T, U>;

    fn map_dynamic<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&S) -> U {

        use self::SLimit::*;
        match *self {
            St(ref t) => St(t.clone()),
            Dyn(ref s) => Dyn(func(s))
        }
    }
}

impl<T, U> MapAll<T, U> for SLimit<T, T> {
    type Output = SLimit<U, U>;

    fn map_all<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::SLimit::*;
        match *self {
            St(ref t) => St(func(t)),
            Dyn(ref s) => Dyn(func(s))
        }
    }
}

impl<T> CollectRequired for SLimit<T, SymbolId>
    where T: FactField {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::SLimit::*;

        if let Dyn(ref s) = *self {
            symbols.entry(*s).or_insert_with( Default::default).insert(T::get_field_type());
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum DLimit<T, S> {
    St(T, T),
    StDyn(T, S),
    DynSt(S, T),
    Dyn(S, S),
}

impl<T> DLimit<T, SymbolId>
    where T: RefField {

    pub fn test_field_ref<C: BetaContext, E: DTest<T> >(&self, test: &E, value: &T, context: &C) -> bool {
        use self::DLimit::*;
        match *self {
            St(ref from, ref to) => test.test(value, from, to),
            StDyn(ref from, ref s_to) => test.test(value, from, T::resolve(context, *s_to)),
            DynSt(ref s_from, ref to) => test.test(value, T::resolve(context, *s_from), to),
            Dyn(ref s_from, ref s_to) => test.test(value, T::resolve(context, *s_from), T::resolve(context, *s_to))
        }
    }
}

impl<'a> DLimit<&'a str, SymbolId> {

    pub fn test_field_str<C: BetaContext, E: DTest<&'a str> >(&self, test: &E, value: &'a str, context: &'a C) -> bool {
        use self::DLimit::*;
        match *self {
            St(ref from, ref to) => test.test(&value, from, to),
            StDyn(ref from, ref s_to) => test.test(&value, from, & str::resolve(context, *s_to)),
            DynSt(ref s_from, ref to) => test.test(&value, & str::resolve(context, *s_from), to),
            Dyn(ref s_from, ref s_to) => test.test(&value, & str::resolve(context, *s_from), & str::resolve(context, *s_to))
        }
    }
}

impl<T> DLimit<T, SymbolId>
    where T: CastField {

    pub fn test_field_cast<C: BetaContext, E: DTest<T> >(&self, test: &E, value: &T, context: &C) -> bool {
        use self::DLimit::*;
        match *self {
            St(ref from, ref to) => test.test(value, from, to),
            StDyn(ref from, ref s_to) => test.test(value, from, &T::resolve(context, *s_to)),
            DynSt(ref s_from, ref to) => test.test(value, &T::resolve(context, *s_from), to),
            Dyn(ref s_from, ref s_to) => test.test(value, &T::resolve(context, *s_from), &T::resolve(context, *s_to))
        }
    }
}

impl<T, S> IsStatic for DLimit<T, S> {
    fn is_static(&self) -> bool {
        use self::DLimit::*;
        match *self {
            St(_, _) => true,
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
        match *self {
            St(ref from, ref to) => St(func(from), func(to)),
            StDyn(ref from, ref s_to) => StDyn(func(from), s_to.clone()),
            DynSt(ref s_from, ref to) => DynSt(s_from.clone(), func(to)),
            Dyn(ref s_from, ref s_to) => Dyn(s_from.clone(), s_to.clone()),
        }
    }
}

impl<T, U, S> MapDynamic<S, U> for DLimit<T, S>
    where T: Clone {
    type Output = DLimit<T, U>;

    fn map_dynamic<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&S) -> U {

        use self::DLimit::*;
        match *self {
            St(ref t1, ref t2) => St(t1.clone(), t2.clone()),
            StDyn(ref t, ref s) => StDyn(t.clone(), func(s)),
            DynSt(ref s, ref t) => DynSt(func(s), t.clone()),
            Dyn(ref s1, ref s2) => Dyn(func(s1), func(s2)),
        }
    }
}

impl<T, U> MapAll<T, U> for DLimit<T, T> {
    type Output = DLimit<U, U>;

    fn map_all<F>(&self, mut func: F) -> Self::Output
        where F: FnMut(&T) -> U {

        use self::DLimit::*;
        match *self {
            St(ref from, ref to) => St(func(from), func(to)),
            StDyn(ref from, ref s_to) => StDyn(func(from), func(s_to)),
            DynSt(ref s_from, ref to) => DynSt(func(s_from), func(to)),
            Dyn(ref s_from, ref s_to) => Dyn(func(s_from), func(s_to)),
        }
    }
}

impl<T> CollectRequired for DLimit<T, SymbolId>
    where T: FactField {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::DLimit::*;
        match *self {
            StDyn(_, ref s_to) => {symbols.entry(*s_to).or_insert_with(Default::default).insert(T::get_field_type());},
            DynSt(ref s_from, _) => {symbols.entry(*s_from).or_insert_with(Default::default).insert(T::get_field_type());},
            Dyn(ref s_from, ref s_to) => {
                symbols.entry(*s_from).or_insert_with( Default::default).insert(T::get_field_type());
                symbols.entry(*s_to).or_insert_with( Default::default).insert(T::get_field_type());
            },
            _ => {}
        }
    }
}

pub trait BetaTestField<T: FactField + ?Sized > {
    fn beta_test_field<C: BetaContext>(&self, value: &T, context: &C) -> bool;
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum BoolTest<S> {
    Eq(Truth, EqTest, SLimit<bool, S>)
}

impl<S> StringIntern for BoolTest<S>
    where S: AsRef<str> {
    type Output = BoolTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::BoolTest::*;
        match *self {
            Eq(truth, test, ref limit) => Eq(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref())))
        }
    }
}

impl<S> IsAlpha for BoolTest<S> {
    fn is_alpha(&self) -> bool {
        use self::BoolTest::*;
        match *self {
            Eq(.., ref limit) => limit.is_static()
        }
    }
}

impl<S> Into<super::alpha::BoolTest> for BoolTest<S> {
    fn into(self) -> super::alpha::BoolTest {
        use self::BoolTest::*;
        match self {
            Eq(truth, test, SLimit::St(to)) => super::alpha::BoolTest::Eq(truth, test, to),
            _ => unreachable!("Into Alpha BoolTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for BoolTest<S> {
    fn apply_not(&mut self) {
        use self::BoolTest::*;
        match *self {
            Eq(ref mut truth, ..) => truth.apply_not()
        }
    }
}

impl BetaTestField<bool> for BoolTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &bool, context: &C) -> bool {
        use self::BoolTest::*;
        match *self {
            Eq(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value,  context)
        }
    }
}

impl CollectRequired for BoolTest<SymbolId> {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::BoolTest::*;
        match *self {
            Eq(.., ref limit) => limit.collect_required(symbols)
        }
    }
}

macro_rules! beta_number_test {
    ($($id:ty => $test:ident),+) => {
        $(
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
            pub enum $test<S> {
                Ord(Truth, OrdTest, SLimit<$id, S>),
                Btwn(Truth, BetweenTest, DLimit<$id, S>),
                Eq(Truth, EqTest, SLimit<$id, S>)
            }


            impl<S> StringIntern for $test<S>
                where S: AsRef<str> {
                type Output = $test<SymbolId>;

                fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
                    use self::$test::*;
                    match *self {
                        Ord(truth, test, ref limit) => Ord(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
                        Btwn(truth, test, ref limit) => Btwn(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
                        Eq(truth, test, ref limit) => Eq(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
                    }
                }
            }

            impl<S> IsAlpha for $test<S> {
                fn is_alpha(&self) -> bool {
                    use self::$test::*;
                    match *self {
                        Ord(.., ref limit) => limit.is_static(),
                        Btwn(.., ref limit) => limit.is_static(),
                        Eq(.., ref limit) => limit.is_static()
                    }
                }
            }

            impl<S> Into<super::alpha::$test> for $test<S> {
                fn into(self) -> super::alpha::$test {
                    use self::$test::*;
                    match self {
                        Ord(truth, test, SLimit::St(to)) => super::alpha::$test::Ord(truth, test, to),
                        Btwn(truth, test, DLimit::St(from, to)) => super::alpha::$test::Btwn(truth, test, from, to),
                        Eq(truth, test, SLimit::St(to)) => super::alpha::$test::Eq(truth, test, to),
                        _ => unreachable!("Into Alpha Primitive with Unsupported Config")
                    }
                }
            }

            impl<S> ApplyNot for $test<S> {
                fn apply_not(&mut self) {
                    use self::$test::*;
                    match *self {
                        Ord(ref mut truth, ..) => truth.apply_not(),
                        Btwn(ref mut truth, ..) => truth.apply_not(),
                        Eq(ref mut truth, ..) => truth.apply_not(),
                    }
                }
            }

            impl BetaTestField<$id> for $test<SymbolId> {
                fn beta_test_field<C: BetaContext>(&self, value: &$id, context: &C) -> bool {
                    use self::$test::*;
                    match *self {
                        Ord(truth, ref test, ref limit) => limit.test_field_cast(&(truth, test), value, context),
                        Eq(truth, ref test, ref limit) => limit.test_field_cast(&(truth, test), value, context),
                        Btwn(truth, ref test, ref limit) => limit.test_field_cast(&(truth, test), value, context),

                    }
                }
            }

            impl CollectRequired for $test<SymbolId> {
                fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
                    use self::$test::*;
                    match *self {
                        Ord(.., ref limit) => limit.collect_required(symbols),
                        Btwn(.., ref limit) => limit.collect_required(symbols),
                        Eq(.., ref limit) => limit.collect_required(symbols)
                    }
                }
            }
        )*
    };
}

macro_rules! beta_float_test {
    ($($id:ty => $test:ident),+) => {
        $(
            #[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
            pub enum $test<S> {
                Ord(Truth, OrdTest, SLimit<$id, S>),
                Btwn(Truth, BetweenTest, DLimit<$id, S>),
                ApproxEq(Truth, ApproxEqTest, SLimit<$id, S>)
            }


            impl<S> StringIntern for $test<S>
                where S: AsRef<str> {
                type Output = $test<SymbolId>;

                fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
                    use self::$test::*;
                    match *self {
                        Ord(truth, test, ref limit) => Ord(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
                        Btwn(truth, test, ref limit) => Btwn(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
                        ApproxEq(truth, test, ref limit) => ApproxEq(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
                    }
                }
            }

            impl<S> IsAlpha for $test<S> {
                fn is_alpha(&self) -> bool {
                    use self::$test::*;
                    match *self {
                        Ord(.., ref limit) => limit.is_static(),
                        Btwn(.., ref limit) => limit.is_static(),
                        ApproxEq(.., ref limit) => limit.is_static()
                    }
                }
            }

            impl<S> Into<super::alpha::$test> for $test<S> {
                fn into(self) -> super::alpha::$test {
                    use self::$test::*;
                    match self {
                        Ord(truth, test, SLimit::St(to)) => super::alpha::$test::Ord(truth, test, to),
                        Btwn(truth, test, DLimit::St(from, to)) => super::alpha::$test::Btwn(truth, test, from, to),
                        ApproxEq(truth, test, SLimit::St(to)) => super::alpha::$test::ApproxEq(truth, test, to),
                        _ => unreachable!("Into Alpha Float with Unsupported Config")
                    }
                }
            }

            impl<S> ApplyNot for $test<S> {
                fn apply_not(&mut self) {
                    use self::$test::*;
                    match *self {
                        Ord(ref mut truth, ..) => truth.apply_not(),
                        Btwn(ref mut truth, ..) => truth.apply_not(),
                        ApproxEq(ref mut truth, ..) => truth.apply_not(),
                    }
                }
            }

            impl BetaTestField<$id> for $test<SymbolId> {
                fn beta_test_field<C: BetaContext>(&self, value: &$id, context: &C) -> bool {
                    use self::$test::*;
                    match *self {
                        Ord(truth, ref test, ref limit) => limit.test_field_cast(&(truth, test), value, context),
                        Btwn(truth, ref test, ref limit) => limit.test_field_cast(&(truth, test), value, context),
                        ApproxEq(truth, ref test, ref limit) => limit.test_field_cast(&(truth, test), value, context),

                    }
                }
            }

            impl CollectRequired for $test<SymbolId> {
                fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
                    use self::$test::*;
                    match *self {
                        Ord(.., ref limit) => limit.collect_required(symbols),
                        Btwn(.., ref limit) => limit.collect_required(symbols),
                        ApproxEq(.., ref limit) => limit.collect_required(symbols)
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
    i128 => I128Test,
    u8 => U8Test,
    u16 => U16Test,
    u32 => U32Test,
    u64 => U64Test,
    u128 => U128Test,
    OrdVar<d128> => D128Test
    );

beta_float_test!(
    NotNaN<f32> => F32Test,
    NotNaN<f64> => F64Test
);


#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum StrTest<S> {
    Ord(Truth, OrdTest, SLimit<S, S>),
    Btwn(Truth, BetweenTest, DLimit<S, S>),
    Eq(Truth, EqTest, SLimit<S, S>),
    Str(Truth, StrArrayTest, SLimit<S, S>)
}

impl<S> StringIntern for StrTest<S>
    where S: AsRef<str> {
    type Output = StrTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::StrTest::*;
        match *self {
            Ord(truth, test, ref limit) => Ord(truth, test, limit.map_all(|s| cache.get_or_intern(s.as_ref()))),
            Btwn(truth, test, ref limit) => Btwn(truth, test, limit.map_all(|s| cache.get_or_intern(s.as_ref()))),
            Eq(truth, test, ref limit) => Eq(truth, test, limit.map_all(|s| cache.get_or_intern(s.as_ref()))),
            Str(truth, test, ref limit) => Str(truth, test, limit.map_all(|s| cache.get_or_intern(s.as_ref()))),
        }
    }
}

impl<S> IsAlpha for StrTest<S> {
    fn is_alpha(&self) -> bool {
        use self::StrTest::*;
        match *self {
            Ord(.., ref limit) => limit.is_static(),
            Btwn(.., ref limit) => limit.is_static(),
            Eq(.., ref limit) => limit.is_static(),
            Str(.., ref limit) => limit.is_static()
        }
    }
}

impl Into<super::alpha::StrTest> for StrTest<SymbolId> {
    fn into(self) -> super::alpha::StrTest {
        use self::StrTest::*;
        match self {
            Ord(truth, test, SLimit::St(to)) => super::alpha::StrTest::Ord(truth, test, to),
            Btwn(truth, test, DLimit::St(from, to)) => super::alpha::StrTest::Btwn(truth, test, from, to),
            Eq(truth, test, SLimit::St(to)) => super::alpha::StrTest::Eq(truth, test, to),
            Str(truth, test, SLimit::St(to)) => super::alpha::StrTest::Str(truth, test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for StrTest<S> {
    fn apply_not(&mut self) {
        use self::StrTest::*;
        match *self {
            Ord(ref mut truth, ..) => truth.apply_not(),
            Btwn(ref mut truth, ..) => truth.apply_not(),
            Eq(ref mut truth, ..) => truth.apply_not(),
            Str(ref mut truth, ..) => truth.apply_not(),
        }
    }
}

//TODO: This ended up being pretty painful. Is there a better way to handle this?
impl BetaTestField<str> for StrTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &str, context: &C) -> bool {
        use self::StrTest::*;
        let string_cache = context.get_string_cache();
        match *self {
            Ord(truth, ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&(truth, test), &value, context),
            Btwn(truth, ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&(truth, test), &value, context),
            Eq(truth, ref test, ref limit) => limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&(truth, test), &value, context),
            Str(truth, ref test, ref limit) => truth.is_not() ^ limit
                .map_static(|s| string_cache.resolve(*s).unwrap())
                .test_field_str(&(truth, test), &value, context),
        }
    }
}

impl CollectRequired for StrTest<SymbolId> {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::StrTest::*;
        match *self {
            Ord(.., ref limit) => limit.collect_required(symbols),
            Btwn(.., ref limit) => limit.collect_required(symbols),
            Eq(.., ref limit) => limit.collect_required(symbols),
            Str(.., ref limit) => limit.collect_required(symbols),
        }
    }
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum TimeTest<S> {
    Ord(Truth, OrdTest, SLimit<NaiveTime, S>),
    Btwn(Truth, BetweenTest, DLimit<NaiveTime, S>),
    Eq(Truth, EqTest, SLimit<NaiveTime, S>)
}

impl<S> StringIntern for TimeTest<S>
    where S: AsRef<str> {
    type Output = TimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::TimeTest::*;
        match *self {
            Ord(truth, test, ref limit) => Ord(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
            Btwn(truth, test, ref limit) => Btwn(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
            Eq(truth, test, ref limit) => Eq(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
        }
    }
}

impl<S> IsAlpha for TimeTest<S> {
    fn is_alpha(&self) -> bool {
        use self::TimeTest::*;
        match *self {
            Ord(.., ref limit) => limit.is_static(),
            Btwn(.., ref limit) => limit.is_static(),
            Eq(.., ref limit) => limit.is_static()
        }
    }
}

impl<S> Into<super::alpha::TimeTest> for TimeTest<S> {
    fn into(self) -> super::alpha::TimeTest {
        use self::TimeTest::*;
        match self {
            Ord(truth, test, SLimit::St(to)) => super::alpha::TimeTest::Ord(truth, test, to),
            Btwn(truth, test, DLimit::St(from, to)) => super::alpha::TimeTest::Btwn(truth, test, from, to),
            Eq(truth, test, SLimit::St(to)) => super::alpha::TimeTest::Eq(truth, test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for TimeTest<S> {
    fn apply_not(&mut self) {
        use self::TimeTest::*;
        match *self {
            Ord(ref mut truth, ..) => truth.apply_not(),
            Btwn(ref mut truth, ..) => truth.apply_not(),
            Eq(ref mut truth, ..) => truth.apply_not(),
        }
    }
}

impl BetaTestField<NaiveTime> for TimeTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &NaiveTime, context: &C) -> bool {
        use self::TimeTest::*;
        match *self {
            Ord(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value, context),
            Btwn(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value,  context),
            Eq(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value,  context)
        }
    }
}

impl CollectRequired for TimeTest<SymbolId> {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::TimeTest::*;
        match *self {
            Ord(.., ref limit) => limit.collect_required(symbols),
            Btwn(.., ref limit) => limit.collect_required(symbols),
            Eq(.., ref limit) => limit.collect_required(symbols)
        }
    }
}


#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum DateTest<S> {
    Ord(Truth, OrdTest, SLimit<Date<Utc>, S>),
    Btwn(Truth, BetweenTest, DLimit<Date<Utc>, S>),
    Eq(Truth, EqTest, SLimit<Date<Utc>, S>)
}

impl<S> StringIntern for DateTest<S>
    where S: AsRef<str> {
    type Output = DateTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTest::*;
        match *self {
            Ord(truth, test, ref limit) => Ord(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
            Btwn(truth, test, ref limit) => Btwn(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
            Eq(truth, test, ref limit) => Eq(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
        }
    }
}

impl<S> IsAlpha for DateTest<S> {
    fn is_alpha(&self) -> bool {
        use self::DateTest::*;
        match *self {
            Ord(.., ref limit) => limit.is_static(),
            Btwn(.., ref limit) => limit.is_static(),
            Eq(.., ref limit) => limit.is_static()
        }
    }
}

impl<S> Into<super::alpha::DateTest> for DateTest<S> {
    fn into(self) -> super::alpha::DateTest {
        use self::DateTest::*;
        match self {
            Ord(truth, test, SLimit::St(to)) => super::alpha::DateTest::Ord(truth, test, to),
            Btwn(truth, test, DLimit::St(from, to)) => super::alpha::DateTest::Btwn(truth, test, from, to),
            Eq(truth, test, SLimit::St(to)) => super::alpha::DateTest::Eq(truth, test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}

impl<S> ApplyNot for DateTest<S> {
    fn apply_not(&mut self) {
        use self::DateTest::*;
        match *self {
            Ord(ref mut truth, ..) => truth.apply_not(),
            Btwn(ref mut truth, ..) => truth.apply_not(),
            Eq(ref mut truth, ..) => truth.apply_not(),
        }
    }
}

impl BetaTestField<Date<Utc>> for DateTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &Date<Utc>, context: &C) -> bool {
        use self::DateTest::*;
        match *self {
            Ord(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value, context),
            Btwn(truth, ref test, ref limit) =>limit.test_field_ref(&(truth, test), value, context),
            Eq(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value, context)
        }
    }
}

impl CollectRequired for DateTest<SymbolId> {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::DateTest::*;
        match *self {
            Ord(.., ref limit) => limit.collect_required(symbols),
            Btwn(.., ref limit) => limit.collect_required(symbols),
            Eq(.., ref limit) => limit.collect_required(symbols)
        }
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub enum DateTimeTest<S> {
    Ord(Truth, OrdTest, SLimit<DateTime<Utc>, S>),
    Btwn(Truth, BetweenTest, DLimit<DateTime<Utc>, S>),
    Eq(Truth, EqTest, SLimit<DateTime<Utc>, S>)
}

impl<S> StringIntern for  DateTimeTest<S>
    where S: AsRef<str> {
    type Output = DateTimeTest<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        use self::DateTimeTest::*;
        match *self {
            Ord(truth, test, ref limit) => Ord(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
            Btwn(truth, test, ref limit) => Btwn(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
            Eq(truth, test, ref limit) => Eq(truth, test, limit.map_dynamic(|s| cache.get_or_intern(s.as_ref()))),
        }
    }
}

impl<S> IsAlpha for DateTimeTest<S> {
    fn is_alpha(&self) -> bool {
        use self::DateTimeTest::*;
        match *self {
            Ord(.., ref limit) => limit.is_static(),
            Btwn(.., ref limit) => limit.is_static(),
            Eq(.., ref limit) => limit.is_static()
        }
    }
}


impl<S> Into<super::alpha::DateTimeTest> for DateTimeTest<S> {
    fn into(self) -> super::alpha::DateTimeTest {
        use self::DateTimeTest::*;
        match self {
            Ord(truth, test, SLimit::St(to)) => super::alpha::DateTimeTest::Ord(truth, test, to),
            Btwn(truth, test, DLimit::St(from, to)) => super::alpha::DateTimeTest::Btwn(truth, test, from, to),
            Eq(truth, test, SLimit::St(to)) => super::alpha::DateTimeTest::Eq(truth, test, to),
            _ => unreachable!("Into Alpha StrTest with Unsupported Config")
        }
    }
}


impl<S> ApplyNot for DateTimeTest<S> {
    fn apply_not(&mut self) {
        use self::DateTimeTest::*;
        match *self {
            Ord(ref mut truth, ..) => truth.apply_not(),
            Btwn(ref mut truth, ..) => truth.apply_not(),
            Eq(ref mut truth, ..) => truth.apply_not(),
        }
    }
}

impl BetaTestField<DateTime<Utc>> for DateTimeTest<SymbolId> {
    fn beta_test_field<C: BetaContext>(&self, value: &DateTime<Utc>, context: &C) -> bool {
        use self::DateTimeTest::*;
        match *self {
            Ord(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value, context),
            Btwn(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value, context),
            Eq(truth, ref test, ref limit) => limit.test_field_ref(&(truth, test), value, context)
        }
    }
}

impl CollectRequired for DateTimeTest<SymbolId> {
    fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
        use self::DateTimeTest::*;
        match *self {
            Ord(.., ref limit) => limit.collect_required(symbols),
            Btwn(.., ref limit) => limit.collect_required(symbols),
            Eq(.., ref limit) => limit.collect_required(symbols)
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
        SDynLimit{limit: cache.get_or_intern(self.limit.as_ref())}
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

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct DDynLimit<S> {
    pub(crate) l: S,
    pub(crate) r: S
}

impl<S> StringIntern for DDynLimit<S>
    where S: AsRef<str> {
    type Output = DDynLimit<SymbolId>;

    fn string_intern(&self, cache: &mut StringCache) -> Self::Output {
        DDynLimit{l: cache.get_or_intern(self.l.as_ref()), r: cache.get_or_intern(self.r.as_ref())}
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

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum TestRepr<S: AsRef<str>> {
    BOOL(S, BoolTest<S>),
    I8(S, I8Test<S>),
    I16(S, I16Test<S>),
    I32(S, I32Test<S>),
    I64(S, I64Test<S>),
    I128(S, I128Test<S>),
    U8(S, U8Test<S>),
    U16(S, U16Test<S>),
    U32(S, U32Test<S>),
    U64(S, U64Test<S>),
    U128(S, U128Test<S>),
    F32(S, F32Test<S>),
    F64(S, F64Test<S>),
    D128(S, D128Test<S>),
    STR(S, StrTest<S>),
    TIME(S, TimeTest<S>),
    DATE(S, DateTest<S>),
    DATETIME(S, DateTimeTest<S>),
    SDYN(S, Truth, SDynTests, SDynLimit<S>),
    DDYN(S, Truth, DDynTests, DDynLimit<S>),
}

impl<S: AsRef<str>> TestRepr<S>  {
    pub fn field(&self) -> &str {
        use self::TestRepr::*;
        match self {
            BOOL(field, _) => field.as_ref(),
            I8(field, _) => field.as_ref(),
            I16(field, _) => field.as_ref(),
            I32(field, _) => field.as_ref(),
            I64(field, _) => field.as_ref(),
            I128(field, _) => field.as_ref(),
            U8(field, _) => field.as_ref(),
            U16(field, _) => field.as_ref(),
            U32(field, _) => field.as_ref(),
            U64(field, _) => field.as_ref(),
            U128(field, _) => field.as_ref(),
            F32(field, _) => field.as_ref(),
            F64(field, _) => field.as_ref(),
            D128(field, _) => field.as_ref(),
            STR(field, _) => field.as_ref(),
            TIME(field, _) => field.as_ref(),
            DATE(field, _) => field.as_ref(),
            DATETIME(field, _) => field.as_ref(),
            SDYN(field, ..) => field.as_ref(),
            DDYN(field, ..) => field.as_ref(),
        }
    }

    pub fn field_type(&self) -> &'static str {
        use self::TestRepr::*;
        match self {
            BOOL(..) => "BOOL",
            I8(..) => "I8",
            I16(..) => "I16",
            I32(..) => "I32",
            I64(..) => "I64",
            I128(..) => "I128",
            U8(..) => "U8",
            U16(..) => "U16",
            U32(..) => "U32",
            U64(..) => "U64",
            U128(..) => "U128",
            F32(..) => "F32",
            F64(..) => "F64",
            D128(..) => "D128",
            STR(..) => "STR",
            TIME(..) => "TIME",
            DATE(..) => "DATE",
            DATETIME(..) => "DATETIME",
            SDYN(..) => "SDYN",
            DDYN(..) => "DDYN",
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
            (&Getter::BOOL(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::BOOL(getter, BoolTest::Eq(truth, test, limit.string_intern(cache).into()))),

            // I8
            (&Getter::I8(getter), &TestRepr::I8(_, ref test)) =>
                Ok(BetaNode::I8(getter, test.string_intern(cache))),
            (&Getter::I8(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I8(getter, I8Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I8(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I8(getter, I8Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I8(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I8(getter, I8Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // I16
            (&Getter::I16(getter), &TestRepr::I16(_, ref test)) =>
                Ok(BetaNode::I16(getter, test.string_intern(cache))),
            (&Getter::I16(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I16(getter, I16Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I16(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I16(getter, I16Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I16(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I16(getter, I16Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // I32
            (&Getter::I32(getter), &TestRepr::I32(_, ref test)) =>
                Ok(BetaNode::I32(getter, test.string_intern(cache))),
            (&Getter::I32(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I32(getter, I32Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I32(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I32(getter, I32Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I32(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I32(getter, I32Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // I64
            (&Getter::I64(getter), &TestRepr::I64(_, ref test)) =>
                Ok(BetaNode::I64(getter, test.string_intern(cache))),
            (&Getter::I64(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I64(getter, I64Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I64(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I64(getter, I64Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I64(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I64(getter, I64Test::Btwn(truth, test, limit.string_intern(cache).into()))),


            // I128
            (&Getter::I128(getter), &TestRepr::I128(_, ref test)) =>
                Ok(BetaNode::I128(getter, test.string_intern(cache))),
            (&Getter::I128(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::I128(getter, I128Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I128(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::I128(getter, I128Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::I128(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::I128(getter, I128Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // U8
            (&Getter::U8(getter), &TestRepr::U8(_, ref test)) =>
                Ok(BetaNode::U8(getter, test.string_intern(cache))),
            (&Getter::U8(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U8(getter, U8Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U8(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U8(getter, U8Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U8(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U8(getter, U8Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // U16
            (&Getter::U16(getter), &TestRepr::U16(_, ref test)) =>
                Ok(BetaNode::U16(getter, test.string_intern(cache))),
            (&Getter::U16(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U16(getter, U16Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U16(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U16(getter, U16Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U16(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U16(getter, U16Test::Btwn(truth, test, limit.string_intern(cache).into()))),
            
            // U32
            (&Getter::U32(getter), &TestRepr::U32(_, ref test)) =>
                Ok(BetaNode::U32(getter, test.string_intern(cache))),
            (&Getter::U32(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U32(getter, U32Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U32(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U32(getter, U32Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U32(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U32(getter, U32Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // U64
            (&Getter::U64(getter), &TestRepr::U64(_, ref test)) =>
                Ok(BetaNode::U64(getter, test.string_intern(cache))),
            (&Getter::U64(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U64(getter, U64Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U64(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U64(getter, U64Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U64(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U64(getter, U64Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // U128
            (&Getter::U128(getter), &TestRepr::U128(_, ref test)) =>
                Ok(BetaNode::U128(getter, test.string_intern(cache))),
            (&Getter::U128(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::U128(getter, U128Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U128(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::U128(getter, U128Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::U128(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::U128(getter, U128Test::Btwn(truth, test, limit.string_intern(cache).into()))),


            // F32
            (&Getter::F32(getter), &TestRepr::F32(_, ref test)) =>
                Ok(BetaNode::F32(getter, test.string_intern(cache))),
            (&Getter::F32(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::F32(getter, F32Test::ApproxEq(truth, test.into(), limit.string_intern(cache).into()))),
            (&Getter::F32(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::F32(getter, F32Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::F32(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::F32(getter, F32Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // F64
            (&Getter::F64(getter), &TestRepr::F64(_, ref test)) =>
                Ok(BetaNode::F64(getter, test.string_intern(cache))),
            (&Getter::F64(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::F64(getter, F64Test::ApproxEq(truth, test.into(), limit.string_intern(cache).into()))),
            (&Getter::F64(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::F64(getter, F64Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::F64(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::F64(getter, F64Test::Btwn(truth, test, limit.string_intern(cache).into()))),


            // D128
            (&Getter::D128(getter), &TestRepr::D128(_, ref test)) =>
                Ok(BetaNode::D128(getter, test.string_intern(cache))),
            (&Getter::D128(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::D128(getter, D128Test::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::D128(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::D128(getter, D128Test::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::D128(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::D128(getter, D128Test::Btwn(truth, test, limit.string_intern(cache).into()))),

            // STR
            (&Getter::STR(getter), &TestRepr::STR(_, ref test)) =>
                Ok(BetaNode::STR(getter, test.string_intern(cache))),
            (&Getter::STR(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::STR(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::STR(getter), &TestRepr::SDYN(_, truth, SDynTests::Str(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Str(truth, test, limit.string_intern(cache).into()))),
            (&Getter::STR(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::STR(getter, StrTest::Btwn(truth, test, limit.string_intern(cache).into()))),

            //TIME
            (&Getter::TIME(getter), &TestRepr::TIME(_, ref test)) =>
                Ok(BetaNode::TIME(getter, test.string_intern(cache))),
            (&Getter::TIME(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::TIME(getter, TimeTest::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::TIME(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::TIME(getter, TimeTest::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::TIME(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::TIME(getter, TimeTest::Btwn(truth, test, limit.string_intern(cache).into()))),

            // DATE
            (&Getter::DATE(getter), &TestRepr::DATE(_, ref test)) =>
                Ok(BetaNode::DATE(getter, test.string_intern(cache))),
            (&Getter::DATE(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::DATE(getter, DateTest::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::DATE(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::DATE(getter, DateTest::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::DATE(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::DATE(getter, DateTest::Btwn(truth, test, limit.string_intern(cache).into()))),

            // DATETIME
            (&Getter::DATETIME(getter), &TestRepr::DATETIME(_, ref test)) =>
                Ok(BetaNode::DATETIME(getter, test.string_intern(cache))),
            (&Getter::DATETIME(getter), &TestRepr::SDYN(_, truth, SDynTests::Eq(test), ref limit)) =>
                Ok(BetaNode::DATETIME(getter, DateTimeTest::Eq(truth, test, limit.string_intern(cache).into()))),
            (&Getter::DATETIME(getter), &TestRepr::SDYN(_, truth, SDynTests::Ord(test), ref limit)) =>
                Ok(BetaNode::DATETIME(getter, DateTimeTest::Ord(truth, test, limit.string_intern(cache).into()))),
            (&Getter::DATETIME(getter), &TestRepr::DDYN(_, truth, DDynTests::Btwn(test), ref limit)) =>
                Ok(BetaNode::DATETIME(getter, DateTimeTest::Btwn(truth, test, limit.string_intern(cache).into()))),

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
        match *self {
            BOOL(_, ref mut test) => test.apply_not(),
            I8(_, ref mut test) => test.apply_not(),
            I16(_, ref mut test) => test.apply_not(),
            I32(_, ref mut test) => test.apply_not(),
            I64(_, ref mut test) => test.apply_not(),
            I128(_, ref mut test) => test.apply_not(),
            U8(_, ref mut test) => test.apply_not(),
            U16(_, ref mut test) => test.apply_not(),
            U32(_, ref mut test) => test.apply_not(),
            U64(_, ref mut test) => test.apply_not(),
            U128(_, ref mut test) => test.apply_not(),
            F32(_, ref mut test) => test.apply_not(),
            F64(_, ref mut test) => test.apply_not(),
            D128(_, ref mut test) => test.apply_not(),
            STR(_, ref mut test) => test.apply_not(),
            TIME(_, ref mut test) => test.apply_not(),
            DATE(_, ref mut test) => test.apply_not(),
            DATETIME(_, ref mut test) => test.apply_not(),
            SDYN(_, ref mut truth, ..) => truth.apply_not(),
            DDYN(_, ref mut truth, ..) => truth.apply_not(),
        }
    }
}

#[derive(Copy, EnumIndex)]
pub enum BetaNode<T: Fact> {
    BOOL(fn(&T) -> &bool, BoolTest<SymbolId>),
    I8(fn(&T) -> &i8, I8Test<SymbolId>),
    I16(fn(&T) -> &i16, I16Test<SymbolId>),
    I32(fn(&T) -> &i32, I32Test<SymbolId>),
    I64(fn(&T) -> &i64, I64Test<SymbolId>),
    I128(fn(&T) -> &i128, I128Test<SymbolId>),
    U8(fn(&T) -> &u8, U8Test<SymbolId>),
    U16(fn(&T) -> &u16, U16Test<SymbolId>),
    U32(fn(&T) -> &u32, U32Test<SymbolId>),
    U64(fn(&T) -> &u64, U64Test<SymbolId>),
    U128(fn(&T) -> &u128, U128Test<SymbolId>),
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

macro_rules! beta_derive {
    ($($t:ident),+ ) => {

        impl<T: Fact> Clone for BetaNode<T> {
            fn clone(&self) -> Self {
                use self::BetaNode::*;
                match *self {
                    $(
                    $t(getter, ref test) => $t(getter, test.clone()),
                    )*
                }
            }
        }

        impl <T:Fact> Hash for BetaNode<T> {
            fn hash < H: Hasher > ( & self, state: & mut H) {
                use self::BetaNode::*;
                    match *self {
                    $ ( $ t(getter, ref test) => Self::hash_self(self.enum_index(), getter as usize, test, state),
                    )*
                }
            }
        }

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

        impl<T: Fact> Eq for BetaNode<T> {}

        impl<T:Fact> Ord for BetaNode<T> {
            fn cmp(&self, other: &Self) -> Ordering {
            use self::BetaNode::*;
                match(self, other) {
                    $( (&$t(getter1, ref test1), &$t(getter2, ref test2)) => {
                        (getter1 as usize).cmp(&(getter2 as usize)).then_with(|| test1.cmp(test2))
                    },)*
                    _ => self.enum_index().cmp(&other.enum_index())
                }
            }
        }

        impl<T:Fact> PartialOrd for BetaNode<T> {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                Some(self.cmp(other))
            }
        }

        impl<T:Fact> IsAlpha for BetaNode<T> {
            fn is_alpha(&self) -> bool {
                use self::BetaNode::*;
                match *self {
                    $(
                    $t(_, test) => test.is_alpha(),
                    )*
                }
            }
        }

        impl<T: Fact> ApplyNot for BetaNode<T> {
            fn apply_not(&mut self) {
                use self::BetaNode::*;
                match *self {
                    $(
                    $t(_, ref mut test) => test.apply_not(),
                    )*
                }
            }
        }

        impl<T:Fact> CollectRequired for BetaNode<T> {
            fn collect_required(&self, symbols: &mut HashMap<SymbolId, HashSet<FactFieldType>>) {
                use self::BetaNode::*;
                match *self {
                    $(
                    $t(_, test) => test.collect_required(symbols),
                    )*
                }
            }
        }

        impl<T: Fact> GetFieldType for BetaNode<T> {
            fn get_field_type(&self) -> FactFieldType {
                use self::BetaNode::*;
                match self {
                    $(
                        $t(..) => FactFieldType::$t,
                    )*
                }
            }
        }

    };
}

beta_derive!(
        BOOL,
        I8, I16, I32, I64, I128,
        U8, U16, U32, U64, U128,
        F32, F64, D128,
        STR ,
        TIME, DATE, DATETIME
    );

impl<I: Fact> Debug for BetaNode<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::BetaNode::*;
        write!(f, "Getter(")?;
        match *self {
            BOOL(getter, test) => write!(f, "BOOL({:#x}) - {:?}", getter as usize, test)?,
            I8(getter, test) => write!(f, "I8({:#x}) - {:?}", getter as usize, test)?,
            I16(getter, test) => write!(f, "I16({:#x}) - {:?}", getter as usize, test)?,
            I32(getter, test) => write!(f, "I32({:#x}) - {:?}", getter as usize, test)?,
            I64(getter, test) => write!(f, "I64({:#x}) - {:?}", getter as usize, test)?,
            I128(getter, test) => write!(f, "I128({:#x}) - {:?}", getter as usize, test)?,
            U8(getter, test) => write!(f, "U8({:#x}) - {:?}", getter as usize, test)?,
            U16(getter, test) => write!(f, "U16({:#x}) - {:?}", getter as usize, test)?,
            U32(getter, test) => write!(f, "U32({:#x}) - {:?}", getter as usize, test)?,
            U64(getter, test) => write!(f, "U64({:#x}) - {:?}", getter as usize, test)?,
            U128(getter, test) => write!(f, "U128({:#x}) - {:?}", getter as usize, test)?,
            F32(getter, test) => write!(f, "F32({:#x}) - {:?}", getter as usize, test)?,
            F64(getter, test) => write!(f, "F64({:#x}) - {:?}", getter as usize, test)?,
            D128(getter, test) => write!(f, "D128({:#x}) - {:?}", getter as usize, test)?,
            STR(getter, test) => write!(f, "STR({:#x}) - {:?}", getter as usize, test)?,
            TIME(getter, test) => write!(f, "TIME({:#x}) - {:?}", getter as usize, test)?,
            DATE(getter, test) => write!(f, "DATE({:#x}) - {:?}", getter as usize, test)?,
            DATETIME(getter, test) => write!(f, "DATETIME({:#x}) - {:?}", getter as usize, test)?,
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
