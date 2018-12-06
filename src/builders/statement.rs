use std::marker::PhantomData;
use crate::traits::{Fact, RuleBuilder, Getters};
use ordered_float::NotNaN;
use crate::runtime::memory::{StringCache, SymbolId};
use crate::builders::ids::{StatementId, ConditionId};
use crate::network::tests::{CLimits, OrdTest, OrdData, FLimits, FlData, FlTest, StrData, StrTest, AlphaTest};
use std::collections::HashSet;
use crate::Result;
use failure::ResultExt;

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq, Debug)]
pub enum ValueHolder<T> {
    S(T),
    D(T, T)
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq, Debug)]
pub enum StatementValues {
    BOOL(ValueHolder<bool>),
    I8(ValueHolder<i8>),
    I16(ValueHolder<i16>),
    I32(ValueHolder<i32>),
    I64(ValueHolder<i64>),
    U8(ValueHolder<u8>),
    U16(ValueHolder<u16>),
    U32(ValueHolder<u32>),
    U64(ValueHolder<u64>),
    ISIZE(ValueHolder<isize>),
    USIZE(ValueHolder<usize>),
    F32(ValueHolder<NotNaN<f32>>),
    F64(ValueHolder<NotNaN<f64>>),
    STR(ValueHolder<SymbolId>)
}

impl StatementValues {
    pub fn is_hash_eq(&self) -> bool {
        use self::StatementValues::*;
        match self {
            &F32(_) | &F64(_) => false,
            _ => true
        }
    }
}

pub trait IntoIntern<T> {
    fn into_intern(self, cache: &mut StringCache) -> T;
}

pub trait TryInto<T> {
    fn try_into(self) -> Result<T>;
}


macro_rules! into_values {
    ($($id:ty => $sub:ident),+) => {
        $(
            impl IntoIntern<StatementValues> for $id {
                fn into_intern(self, _: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::S(self))
                }
            }

            impl IntoIntern<StatementValues> for ($id, $id) {
                fn into_intern(self, _: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::D(self.0, self.1))
                }
            }
        )*
    };
}

macro_rules! into_float_values {
    ($($id:ty => $sub:ident),+) => {
        $(
            impl IntoIntern<StatementValues> for $id {
                fn into_intern(self, _: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::S(NotNaN::from(self)))
                }
            }

            impl IntoIntern<StatementValues> for ($id, $id) {
                fn into_intern(self, _: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::D(NotNaN::from(self.0), NotNaN::from(self.1)))
                }
            }
        )*
    };
}

impl IntoIntern<StatementValues> for bool {
    fn into_intern(self, _: &mut StringCache) -> StatementValues {
        StatementValues::BOOL(ValueHolder::S(self))
    }
}

into_values!(
    i8 => I8, i16 => I16, i32 => I32, i64 => I64,
    u8 => U8, u16 => U16, u32 => U32, u64 => U64,
    isize => ISIZE, usize => USIZE,
    SymbolId => STR
 );

into_float_values!(
    f32 => F32, f64 => F64
);

impl<'a> IntoIntern<StatementValues> for &'a str {
    fn into_intern(self, cache: &mut StringCache) -> StatementValues {
        StatementValues::STR(ValueHolder::S(cache.get_or_intern(self)))
    }
}

impl<'a> IntoIntern<StatementValues> for (&'a str, &'a str) {
    fn into_intern(self, cache: &mut StringCache) -> StatementValues {
        StatementValues::STR(ValueHolder::D(cache.get_or_intern(self.0), cache.get_or_intern(self.1)))
    }
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq, Debug)]
pub enum StatementConditions {
    Eq(SymbolId, StatementValues),
    Ne(SymbolId, StatementValues),
    Lt(SymbolId, StatementValues),
    Le(SymbolId, StatementValues),
    Gt(SymbolId, StatementValues),
    Ge(SymbolId, StatementValues),
    GtLt(SymbolId, StatementValues),
    GeLt(SymbolId, StatementValues),
    GtLe(SymbolId, StatementValues),
    GeLe(SymbolId, StatementValues),
    Contains(SymbolId, StatementValues),
    StartsWith(SymbolId, StatementValues),
    EndsWith(SymbolId, StatementValues)
}

impl StatementConditions {
    pub fn is_hash_eq(&self) -> bool {
        use self::StatementConditions::*;
        match self {
            &Eq(_, values) if values.is_hash_eq() => true,
            _ => false
        }
    }

    pub fn field(&self) -> SymbolId {
        use self::StatementConditions::*;
        match self {
            &Eq(field, _) => field,
            &Ne(field, _) => field,
            &Lt(field, _) => field,
            &Le(field, _) => field,
            &Gt(field, _) => field,
            &Ge(field, _) => field,
            &GtLt(field, _) => field,
            &GeLt(field, _) => field,
            &GtLe(field, _) => field,
            &GeLe(field, _) => field,
            &Contains(field, _) => field,
            &StartsWith(field, _) => field,
            &EndsWith(field, _) => field,
        }
    }
}

pub struct StatementBuilder<I: Fact, R: RuleBuilder> {
    statement_type: PhantomData<I>,
    rule_builder: R,
    conditions: Vec<StatementConditions>
}

impl<I: Fact, R: RuleBuilder> StatementBuilder<I, R> {

    pub fn eq<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Eq(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn ne<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Ne(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn lt<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Lt(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn le<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Le(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn gt<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Gt(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn ge<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Ge(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn gtlt<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, from: T, to: T)
        where (T,T) : IntoIntern<StatementValues> {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GtLt(field_symbol, (from, to).into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn gelt<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, from: T, to: T)
        where (T,T) : IntoIntern<StatementValues> {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GeLt(field_symbol, (from, to).into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn gtle<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, from: T, to: T)
        where (T,T) : IntoIntern<StatementValues> {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GtLe(field_symbol, (from, to).into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn gele<S: Into<String> + AsRef<str>, T: IntoIntern<StatementValues>>(&mut self, field: S, from: T, to: T)
        where (T,T) : IntoIntern<StatementValues> {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GeLe(field_symbol, (from, to).into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn contains<S: Into<String> + AsRef<str>>(&mut self, field: S, to: &str) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Contains(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn starts_with<S: Into<String> + AsRef<str>>(&mut self, field: S, to: &str) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::StartsWith(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }

    pub fn ends_with<S: Into<String> + AsRef<str>>(&mut self, field: S, to: &str) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::EndsWith(field_symbol, to.into_intern(self.rule_builder.get_string_cache()))
        )
    }


    fn collapse(mut self) -> Result<R> {
        let hash_eq = I::create_hash_eq(&self.conditions, self.rule_builder.get_string_cache());

        {
            let statement_id = self.rule_builder.get_id_generator().next_statement_id();
            self.rule_builder.get_statement_ids().insert(statement_id);
            let (cache, id_gen, entry_point) = self.rule_builder.get_for_condition_collapse::<I>(hash_eq);

            entry_point.entry(AlphaTest::HashEq).or_insert_with(|| ConditionDesc::new(id_gen.next_condition_id(), None))
                .dependents.insert(statement_id);

            for c in self.conditions
                .into_iter()
                .filter(|c| !c.is_hash_eq()) {

                let field_sym = c.field();
                let getter = cache.resolve(field_sym)
                    .and_then(|s| I ::getter(s))
                    .ok_or(format_err!("Type has no getter {:?}", cache.resolve(field_sym)))?;
                let test = (getter, c).try_into()
                    .context(format_err!("Field {:?}", cache.resolve(field_sym)))?;
                entry_point.entry(test).or_insert_with(|| ConditionDesc::new(id_gen.next_condition_id(), Some(field_sym)))
                    .dependents.insert(statement_id);
            }
        }
        Ok(self.rule_builder)
    }
}


// TODO: Certainly there's a way to do this in a macro - https://users.rust-lang.org/t/cartesian-product-using-macros/10763/24
// I will figure this out eventually :/
impl<I: Fact> TryInto<AlphaTest<I>> for (Getters<I>, StatementConditions) {
    fn try_into(self) -> Result<AlphaTest<I>> {
        match self {
            //I8
            (Getters::I8(accessor), StatementConditions::Ne(_, StatementValues::I8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::I8(accessor), StatementConditions::Lt(_, StatementValues::I8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::I8(accessor), StatementConditions::Le(_, StatementValues::I8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::I8(accessor), StatementConditions::Gt(_, StatementValues::I8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::I8(accessor), StatementConditions::Ge(_, StatementValues::I8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::I8(accessor), StatementConditions::GtLt(_, StatementValues::I8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::I8(accessor), StatementConditions::GeLt(_, StatementValues::I8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::I8(accessor), StatementConditions::GtLe(_, StatementValues::I8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::I8(accessor), StatementConditions::GeLe(_, StatementValues::I8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I8(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //I16
            (Getters::I16(accessor), StatementConditions::Ne(_, StatementValues::I16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::I16(accessor), StatementConditions::Lt(_, StatementValues::I16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::I16(accessor), StatementConditions::Le(_, StatementValues::I16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::I16(accessor), StatementConditions::Gt(_, StatementValues::I16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::I16(accessor), StatementConditions::Ge(_, StatementValues::I16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::I16(accessor), StatementConditions::GtLt(_, StatementValues::I16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::I16(accessor), StatementConditions::GeLt(_, StatementValues::I16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::I16(accessor), StatementConditions::GtLe(_, StatementValues::I16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::I16(accessor), StatementConditions::GeLe(_, StatementValues::I16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I16(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //I32
            (Getters::I32(accessor), StatementConditions::Ne(_, StatementValues::I32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::I32(accessor), StatementConditions::Lt(_, StatementValues::I32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::I32(accessor), StatementConditions::Le(_, StatementValues::I32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::I32(accessor), StatementConditions::Gt(_, StatementValues::I32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::I32(accessor), StatementConditions::Ge(_, StatementValues::I32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::I32(accessor), StatementConditions::GtLt(_, StatementValues::I32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::I32(accessor), StatementConditions::GeLt(_, StatementValues::I32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::I32(accessor), StatementConditions::GtLe(_, StatementValues::I32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::I32(accessor), StatementConditions::GeLe(_, StatementValues::I32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I32(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //I64
            (Getters::I64(accessor), StatementConditions::Ne(_, StatementValues::I64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::I64(accessor), StatementConditions::Lt(_, StatementValues::I64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::I64(accessor), StatementConditions::Le(_, StatementValues::I64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::I64(accessor), StatementConditions::Gt(_, StatementValues::I64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::I64(accessor), StatementConditions::Ge(_, StatementValues::I64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::I64(accessor), StatementConditions::GtLt(_, StatementValues::I64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::I64(accessor), StatementConditions::GeLt(_, StatementValues::I64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::I64(accessor), StatementConditions::GtLe(_, StatementValues::I64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::I64(accessor), StatementConditions::GeLe(_, StatementValues::I64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::I64(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //U8
            (Getters::U8(accessor), StatementConditions::Ne(_, StatementValues::U8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::U8(accessor), StatementConditions::Lt(_, StatementValues::U8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::U8(accessor), StatementConditions::Le(_, StatementValues::U8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::U8(accessor), StatementConditions::Gt(_, StatementValues::U8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::U8(accessor), StatementConditions::Ge(_, StatementValues::U8(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::U8(accessor), StatementConditions::GtLt(_, StatementValues::U8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::U8(accessor), StatementConditions::GeLt(_, StatementValues::U8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::U8(accessor), StatementConditions::GtLe(_, StatementValues::U8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::U8(accessor), StatementConditions::GeLe(_, StatementValues::U8(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U8(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //U16
            (Getters::U16(accessor), StatementConditions::Ne(_, StatementValues::U16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::U16(accessor), StatementConditions::Lt(_, StatementValues::U16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::U16(accessor), StatementConditions::Le(_, StatementValues::U16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::U16(accessor), StatementConditions::Gt(_, StatementValues::U16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::U16(accessor), StatementConditions::Ge(_, StatementValues::U16(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::U16(accessor), StatementConditions::GtLt(_, StatementValues::U16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::U16(accessor), StatementConditions::GeLt(_, StatementValues::U16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::U16(accessor), StatementConditions::GtLe(_, StatementValues::U16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::U16(accessor), StatementConditions::GeLe(_, StatementValues::U16(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U16(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //U32
            (Getters::U32(accessor), StatementConditions::Ne(_, StatementValues::U32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::U32(accessor), StatementConditions::Lt(_, StatementValues::U32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::U32(accessor), StatementConditions::Le(_, StatementValues::U32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::U32(accessor), StatementConditions::Gt(_, StatementValues::U32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::U32(accessor), StatementConditions::Ge(_, StatementValues::U32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::U32(accessor), StatementConditions::GtLt(_, StatementValues::U32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::U32(accessor), StatementConditions::GeLt(_, StatementValues::U32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::U32(accessor), StatementConditions::GtLe(_, StatementValues::U32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::U32(accessor), StatementConditions::GeLe(_, StatementValues::U32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U32(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //U64
            (Getters::U64(accessor), StatementConditions::Ne(_, StatementValues::U64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::U64(accessor), StatementConditions::Lt(_, StatementValues::U64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::U64(accessor), StatementConditions::Le(_, StatementValues::U64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::U64(accessor), StatementConditions::Gt(_, StatementValues::U64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::U64(accessor), StatementConditions::Ge(_, StatementValues::U64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::U64(accessor), StatementConditions::GtLt(_, StatementValues::U64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::U64(accessor), StatementConditions::GeLt(_, StatementValues::U64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::U64(accessor), StatementConditions::GtLe(_, StatementValues::U64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::U64(accessor), StatementConditions::GeLe(_, StatementValues::U64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::U64(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //ISIZE
            (Getters::ISIZE(accessor), StatementConditions::Ne(_, StatementValues::ISIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::ISIZE(accessor), StatementConditions::Lt(_, StatementValues::ISIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::ISIZE(accessor), StatementConditions::Le(_, StatementValues::ISIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::ISIZE(accessor), StatementConditions::Gt(_, StatementValues::ISIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::ISIZE(accessor), StatementConditions::Ge(_, StatementValues::ISIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::ISIZE(accessor), StatementConditions::GtLt(_, StatementValues::ISIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::ISIZE(accessor), StatementConditions::GeLt(_, StatementValues::ISIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::ISIZE(accessor), StatementConditions::GtLe(_, StatementValues::ISIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::ISIZE(accessor), StatementConditions::GeLe(_, StatementValues::ISIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::ISIZE(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //USIZE
            (Getters::USIZE(accessor), StatementConditions::Ne(_, StatementValues::USIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::S(to)), OrdTest::Ne)),
            (Getters::USIZE(accessor), StatementConditions::Lt(_, StatementValues::USIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::S(to)), OrdTest::Lt)),
            (Getters::USIZE(accessor), StatementConditions::Le(_, StatementValues::USIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::S(to)), OrdTest::Le)),
            (Getters::USIZE(accessor), StatementConditions::Gt(_, StatementValues::USIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::S(to)), OrdTest::Gt)),
            (Getters::USIZE(accessor), StatementConditions::Ge(_, StatementValues::USIZE(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::S(to)), OrdTest::Ge)),
            (Getters::USIZE(accessor), StatementConditions::GtLt(_, StatementValues::USIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::D(from, to)), OrdTest::GtLt)),
            (Getters::USIZE(accessor), StatementConditions::GeLt(_, StatementValues::USIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::D(from, to)), OrdTest::GeLt)),
            (Getters::USIZE(accessor), StatementConditions::GtLe(_, StatementValues::USIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::D(from, to)), OrdTest::GtLe)),
            (Getters::USIZE(accessor), StatementConditions::GeLe(_, StatementValues::USIZE(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Ord(OrdData::USIZE(accessor, CLimits::D(from, to)), OrdTest::GeLe)),
            //F32
            (Getters::F32(accessor), StatementConditions::Eq(_, StatementValues::F32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::S(to)), FlTest::ApproxEq)),
            (Getters::F32(accessor), StatementConditions::Ne(_, StatementValues::F32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::S(to)), FlTest::ApproxNe)),
            (Getters::F32(accessor), StatementConditions::Lt(_, StatementValues::F32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::S(to)), FlTest::Lt)),
            (Getters::F32(accessor), StatementConditions::Le(_, StatementValues::F32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::S(to)), FlTest::Le)),
            (Getters::F32(accessor), StatementConditions::Gt(_, StatementValues::F32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::S(to)), FlTest::Gt)),
            (Getters::F32(accessor), StatementConditions::Ge(_, StatementValues::F32(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::S(to)), FlTest::Ge)),
            (Getters::F32(accessor), StatementConditions::GtLt(_, StatementValues::F32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::D(from, to)), FlTest::GtLt)),
            (Getters::F32(accessor), StatementConditions::GeLt(_, StatementValues::F32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::D(from, to)), FlTest::GeLt)),
            (Getters::F32(accessor), StatementConditions::GtLe(_, StatementValues::F32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::D(from, to)), FlTest::GtLe)),
            (Getters::F32(accessor), StatementConditions::GeLe(_, StatementValues::F32(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F32(accessor, FLimits::D(from, to)), FlTest::GeLe)),
            //F64
            (Getters::F64(accessor), StatementConditions::Eq(_, StatementValues::F64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::S(to)), FlTest::ApproxEq)),
            (Getters::F64(accessor), StatementConditions::Ne(_, StatementValues::F64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::S(to)), FlTest::ApproxNe)),
            (Getters::F64(accessor), StatementConditions::Lt(_, StatementValues::F64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::S(to)), FlTest::Lt)),
            (Getters::F64(accessor), StatementConditions::Le(_, StatementValues::F64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::S(to)), FlTest::Le)),
            (Getters::F64(accessor), StatementConditions::Gt(_, StatementValues::F64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::S(to)), FlTest::Gt)),
            (Getters::F64(accessor), StatementConditions::Ge(_, StatementValues::F64(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::S(to)), FlTest::Ge)),
            (Getters::F64(accessor), StatementConditions::GtLt(_, StatementValues::F64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::D(from, to)), FlTest::GtLt)),
            (Getters::F64(accessor), StatementConditions::GeLt(_, StatementValues::F64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::D(from, to)), FlTest::GeLt)),
            (Getters::F64(accessor), StatementConditions::GtLe(_, StatementValues::F64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::D(from, to)), FlTest::GtLe)),
            (Getters::F64(accessor), StatementConditions::GeLe(_, StatementValues::F64(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Fl(FlData::F64(accessor, FLimits::D(from, to)), FlTest::GeLe)),
            //STR::REF
            (Getters::STR(accessor), StatementConditions::Ne(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::Ne)),
            (Getters::STR(accessor), StatementConditions::Lt(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::Lt)),
            (Getters::STR(accessor), StatementConditions::Le(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::Le)),
            (Getters::STR(accessor), StatementConditions::Gt(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::Gt)),
            (Getters::STR(accessor), StatementConditions::Ge(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::Ge)),
            (Getters::STR(accessor), StatementConditions::GtLt(_, StatementValues::STR(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::D(from, to)), StrTest::GtLt)),
            (Getters::STR(accessor), StatementConditions::GeLt(_, StatementValues::STR(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::D(from, to)), StrTest::GeLt)),
            (Getters::STR(accessor), StatementConditions::GtLe(_, StatementValues::STR(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::D(from, to)), StrTest::GtLe)),
            (Getters::STR(accessor), StatementConditions::GeLe(_, StatementValues::STR(ValueHolder::D(from, to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::D(from, to)), StrTest::GeLe)),
            (Getters::STR(accessor), StatementConditions::Contains(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::Contains)),
            (Getters::STR(accessor), StatementConditions::StartsWith(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::StartsWith)),
            (Getters::STR(accessor), StatementConditions::EndsWith(_, StatementValues::STR(ValueHolder::S(to)))) =>
                Ok(AlphaTest::Str(StrData::REF(accessor, CLimits::S(to)), StrTest::EndsWith)),
            _ => Err(format_err!("Something went wrong during transformation! - Data - {:?}", self))
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConditionDesc {
    pub(crate) id: ConditionId,
    pub(crate) field: Option<SymbolId>,
    pub(crate) dependents: HashSet<StatementId>
}

impl ConditionDesc {
    pub fn new(id: ConditionId, field: Option<SymbolId>) -> ConditionDesc {
        ConditionDesc{id, field, dependents: Default::default()}
    }
}
