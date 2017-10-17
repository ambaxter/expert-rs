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

macro_rules! into_values {
    ($($id:ty => $sub:ident),+) => {
        $(
            impl IntoIntern<StatementValues> for $id {
                fn into_intern(self, cache: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::S(self))
                }
            }

            impl IntoIntern<StatementValues> for ($id, $id) {
                fn into_intern(self, cache: &mut StringCache) -> StatementValues {
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
                fn into_intern(self, cache: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::S(NotNaN::from(self)))
                }
            }

            impl IntoIntern<StatementValues> for ($id, $id) {
                fn into_intern(self, cache: &mut StringCache) -> StatementValues {
                    StatementValues::$sub(ValueHolder::D(NotNaN::from(self.0), NotNaN::from(self.1)))
                }
            }
        )*
    };
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
        use self::StatementValues::*;
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

pub struct StatementBuilder<I: Insert, R: RuleBuilder> {
    statement_type: PhantomData<I>,
    rule_builder: R,
    conditions: Vec<StatementConditions>
}

impl<I: Insert, R: RuleBuilder> StatementBuilder<I, R> {

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


    fn collapse(mut self) -> R {
        let hash_eq = I::create_hash_eq(&self.conditions, self.rule_builder.get_string_cache());

        if !self.conditions.is_empty() {
            let statement_id = self.rule_builder.next_statement_id();

            //TODO: Left off here
            //let entry_point = self.rule_builder.condition_map
        }

        self.rule_builder

    }
}