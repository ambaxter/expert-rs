use std;
use std::any::TypeId;
use std::hash::Hash;
use ::builder::StatementCondition;
use runtime::memory::StringCache;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use builders::ids::*;

pub trait Introspect {
    fn static_type_id() -> TypeId;
}

#[derive(Copy, Clone)]
pub enum Getters<I: Insert> {
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

pub trait Insert : Introspect + Eq + Hash
    where Self: std::marker::Sized {
    type HashEq: Hash + Eq + Clone + Debug;
    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<Getters<Self>>;
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>>;
}

pub trait NetworkBuilder {
    fn next_rule_id(&mut self) -> RuleId;
    fn next_statement_id(&mut self) -> StatementId;
    fn next_condition_id(&mut self) -> ConditionId;
    fn get_conditions<I: Insert>(&mut self) ->&mut HashSet<I::HashEq>;
    fn get_string_cache(&mut self) -> &mut StringCache;

}

pub trait RuleBuilder {
    fn next_statement_id(&mut self) -> StatementId;
    fn next_condition_id(&mut self) -> ConditionId;
    fn get_conditions<I: Insert>(&mut self) ->&mut HashSet<I::HashEq>;
    fn get_statement_ids(&mut self) -> &mut Vec<StatementId>;
    fn get_string_cache(&mut self) -> &mut StringCache;
}


pub trait ReteIntrospection : Eq + Hash {
    type HashEq: Hash + Eq + Clone + Debug;

    fn static_type_id() -> TypeId;
    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<fn(&Self) -> &u64>;
    fn type_id(&self) -> TypeId;
}

