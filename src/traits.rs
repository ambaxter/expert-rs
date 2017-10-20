use std;
use std::any::TypeId;
use std::hash::Hash;
use builders::statement::{ConditionDesc, StatementConditions};
use network::tests::AlphaTest;
use ::builder::StatementCondition;
use runtime::memory::StringCache;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;
use std::fmt;
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

impl<I: Insert> Debug for Getters<I> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Getters::*;
        write!(f, "Getters(")?;
        match self {
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

pub trait Insert : Introspect + Eq + Hash
    where Self: std::marker::Sized {
    type HashEq: Hash + Eq + Clone + Debug;
    fn create_hash_eq(conditions: &Vec<StatementConditions>, cache: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<Getters<Self>>;
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>>;
}

pub trait NetworkBuilder {
    fn next_rule_id(&mut self) -> RuleId;
    fn next_statement_id(&mut self) -> StatementId;
    fn next_condition_id(&mut self) -> ConditionId;
    fn get_conditions<I: Insert>(&mut self) -> &mut HashMap<I::HashEq, HashMap<AlphaTest<I>, ConditionDesc>>;
    fn get_string_cache(&mut self) -> &mut StringCache;

}

pub trait RuleBuilder {
    fn next_statement_id(&mut self) -> StatementId;
    fn next_condition_id(&mut self) -> ConditionId;
    fn get_conditions<I: Insert>(&mut self) -> &mut HashMap<I::HashEq, HashMap<AlphaTest<I>, ConditionDesc>>;
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

