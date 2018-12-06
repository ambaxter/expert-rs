use std;
use std::any::TypeId;
use std::hash::Hash;
use crate::builders::statement::{ConditionDesc, StatementConditions};
use crate::network::tests::AlphaTest;
use crate::builder::StatementCondition;
use crate::runtime::memory::{SymbolId, StringCache};
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fmt;
use ordered_float::NotNaN;
use crate::builders::ids::*;

pub trait Introspect {
    fn static_type_id() -> TypeId;
}

#[derive(Copy, Clone, Debug, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub enum FieldValue {
    BOOL(SymbolId, bool),
    I8(SymbolId, i8),
    I16(SymbolId, i16),
    I32(SymbolId, i32),
    I64(SymbolId, i64),
    U8(SymbolId, u8),
    U16(SymbolId, u16),
    U32(SymbolId, u32),
    U64(SymbolId, u64),
    ISIZE(SymbolId, isize),
    USIZE(SymbolId, usize),
    F32(SymbolId, NotNaN<f32>),
    F64(SymbolId, NotNaN<f64>),
    STR(SymbolId, SymbolId),
}

impl FieldValue {
    pub fn field(&self) -> SymbolId {
        use self::FieldValue::*;
        match self {
            &BOOL(field, _) => field,
            &I8(field, _) => field,
            &I16(field, _) => field,
            &I32(field, _) => field,
            &I64(field, _) => field,
            &U8(field, _) => field,
            &U16(field, _) => field,
            &U32(field, _) => field,
            &U64(field, _) => field,
            &ISIZE(field, _) => field,
            &USIZE(field, _) => field,
            &F32(field, _) => field,
            &F64(field, _) => field,
            &STR(field, _) => field,
        }
    }
}

#[derive(Copy, Clone)]
pub enum Getters<I: Fact> {
    BOOL(fn(&I) -> &bool),
    I8(fn(&I) -> &i8),
    I16(fn(&I) -> &i16),
    I32(fn(&I) -> &i32),
    I64(fn(&I) -> &i64),
    U8(fn(&I) -> &u8),
    U16(fn(&I) -> &u16),
    U32(fn(&I) -> &u32),
    U64(fn(&I) -> &u64),
    ISIZE(fn(&I) -> &isize),
    USIZE(fn(&I) -> &usize),
    F32(fn(&I) -> &f32),
    F64(fn(&I) -> &f64),
    STR(fn(&I) -> &str),
}

impl<I: Fact> Debug for Getters<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Getters::*;
        write!(f, "Getters(")?;
        match self {
            &BOOL(accessor) => write!(f, "BOOL({:#x})", accessor as usize)?,
            &I8(accessor) => write!(f, "I8({:#x})", accessor as usize)?,
            &I16(accessor) => write!(f, "I16({:#x})", accessor as usize)?,
            &I32(accessor) => write!(f, "I32({:#x})", accessor as usize)?,
            &I64(accessor) => write!(f, "I64({:#x})", accessor as usize)?,
            &U8(accessor) => write!(f, "U8({:#x})", accessor as usize)?,
            &U16(accessor) => write!(f, "U16({:#x})", accessor as usize)?,
            &U32(accessor) => write!(f, "U32({:#x})", accessor as usize)?,
            &U64(accessor) => write!(f, "U64({:#x})", accessor as usize)?,
            &ISIZE(accessor) => write!(f, "ISIZE({:#x})", accessor as usize)?,
            &USIZE(accessor) => write!(f, "USIZE({:#x})", accessor as usize)?,
            &F32(accessor) => write!(f, "F32({:#x})", accessor as usize)?,
            &F64(accessor) => write!(f, "F64({:#x})", accessor as usize)?,
            &STR(accessor) => write!(f, "STR({:#x})", accessor as usize)?,
            _ => {}
        }
        write!(f, ")")
    }
}

pub trait Fact: Introspect + Eq + Hash
    where Self: std::marker::Sized {
    type HashEq: Hash + Eq + Clone + Debug;
    fn create_hash_eq(conditions: &Vec<StatementConditions>, cache: &StringCache) -> Self::HashEq;
    fn new_from_fields(fields: &[FieldValue], cache: &StringCache) -> Self;
    fn getter(field: &str) -> Option<Getters<Self>>;
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>>;
}

pub trait NetworkBuilder {
    fn get_id_generator(&mut self) -> &mut BuilderIdGen;
    fn get_conditions<I: Fact>(&mut self) -> &mut HashMap<I::HashEq, HashMap<AlphaTest<I>, ConditionDesc>>;
    fn get_string_cache(&mut self) -> &mut StringCache;

}

pub trait RuleBuilder {
    fn get_for_condition_collapse<I: Fact>(&mut self, hash_eq: I::HashEq) -> (&mut StringCache, &mut BuilderIdGen, &mut HashMap<AlphaTest<I>, ConditionDesc>);
    fn get_id_generator(&mut self) -> &mut BuilderIdGen;
    fn get_conditions<I: Fact>(&mut self) -> &mut HashMap<I::HashEq, HashMap<AlphaTest<I>, ConditionDesc>>;
    fn get_statement_ids(&mut self) -> &mut HashSet<StatementId>;
    fn get_string_cache(&mut self) -> &mut StringCache;
}


pub trait ReteIntrospection : Eq + Hash {
    type HashEq: Hash + Eq + Clone + Debug;

    fn static_type_id() -> TypeId;
    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<fn(&Self) -> &u64>;
    fn type_id(&self) -> TypeId;
}

