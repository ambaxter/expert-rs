use std::marker::PhantomData;
use traits::{Insert, RuleBuilder};
use ordered_float::NotNaN;
use runtime::memory::{StringCache, SymbolId};

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq, Debug)]
pub enum ValueHolder<T> {
    S(T),
    D(T, T)
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq, Debug)]
pub enum StatementValues {
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

pub trait ValueType {
    fn into_single(to: Self, cache: &mut StringCache) -> StatementValues;
    fn into_double(from: Self, to: Self, cache: &mut StringCache) -> StatementValues;
}

macro_rules! into_value_type {
    ($($id:ident => $sub:ident),+) => {
        $(
            impl ValueType for $id {
                fn into_single(to: Self, cache: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::S(to))
                }
                fn into_double(from: Self, to: Self, cache: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::D(from, to))
                }
            }
        )*
    };
}

into_value_type!(
    i8 => I8, i16 => I16, i32 => I32, i64 => I64,
    u8 => U8, u16 => U16, u32 => U32, u64 => U64,
    isize => ISIZE, usize => USIZE
 );


impl ValueType for f32 {
    fn into_single(to: Self, cache: &mut StringCache) -> StatementValues {
        StatementValues::F32(ValueHolder::S(NotNaN::from(to)))
    }

    fn into_double(from: Self, to: Self, cache: &mut StringCache) -> StatementValues {
        StatementValues::F32(ValueHolder::D(NotNaN::from(from), NotNaN::from(to)))
    }
}

impl ValueType for f64 {
    fn into_single(to: Self, cache: &mut StringCache) -> StatementValues {
        StatementValues::F64(ValueHolder::S(NotNaN::from(to)))
    }

    fn into_double(from: Self, to: Self, cache: &mut StringCache) -> StatementValues {
        StatementValues::F64(ValueHolder::D(NotNaN::from(from), NotNaN::from(to)))
    }
}

impl<'a> ValueType for &'a str {
    fn into_single(to: Self, cache: &mut StringCache) -> StatementValues {
        StatementValues::STR(ValueHolder::S(cache.get_or_intern(to)))
    }

    fn into_double(from: Self, to: Self, cache: &mut StringCache) -> StatementValues {
        StatementValues::STR(ValueHolder::D(cache.get_or_intern(from), cache.get_or_intern(to)))
    }
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq, Debug)]
pub enum StatementConditions {
    Exists,
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

pub struct StatementBuilder<I: Insert, R: RuleBuilder> {
    statement_type: PhantomData<I>,
    rule_builder: R,
    conditions: Vec<StatementConditions>
}

impl<I: Insert, R: RuleBuilder> StatementBuilder<I, R> {

    pub fn exists(&mut self) {
        self.conditions.push(StatementConditions::Exists)
    }

    pub fn eq<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Eq(field_symbol, T::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn ne<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Ne(field_symbol, T::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn lt<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Lt(field_symbol, T::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn le<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Le(field_symbol, T::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn gt<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Gt(field_symbol, T::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn ge<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Ge(field_symbol, T::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn gtlt<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, from: T, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GtLt(field_symbol, T::into_double(from, to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn gelt<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, from: T, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GeLt(field_symbol, T::into_double(from, to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn gtle<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, from: T, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GtLe(field_symbol, T::into_double(from, to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn gele<S: Into<String> + AsRef<str>, T: ValueType>(&mut self, field: S, from: T, to: T) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::GeLe(field_symbol, T::into_double(from, to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn contains<S: Into<String> + AsRef<str>>(&mut self, field: S, to: &str) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::Contains(field_symbol, <&str as ValueType>::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn starts_with<S: Into<String> + AsRef<str>>(&mut self, field: S, to: &str) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::StartsWith(field_symbol, <&str as ValueType>::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

    pub fn ends_with<S: Into<String> + AsRef<str>>(&mut self, field: S, to: &str) {
        let field_symbol = self.rule_builder.get_string_cache().get_or_intern(field);
        self.conditions.push(
            StatementConditions::EndsWith(field_symbol, <&str as ValueType>::into_single(to, self.rule_builder.get_string_cache()))
        )
    }

}