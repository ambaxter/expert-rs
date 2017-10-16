use mopa;
use std;
use std::any::TypeId;
use std::hash::Hash;
use ::builder::StatementCondition;
use runtime::memory::StringCache;
use std::fmt::Debug;

pub trait Introspect {
    fn static_type_id() -> TypeId;
}

#[derive(Clone)]
pub enum Getters<T: Insert> {
    I8(fn(&T) -> &i8),
    I16(fn(&T) -> &i16),
    I32(fn(&T) -> &i32),
    I64(fn(&T) -> &i64),
    U8(fn(&T) -> &u8),
    U16(fn(&T) -> &u16),
    U32(fn(&T) -> &u32),
    U64(fn(&T) -> &u64),
    ISIZE(fn(&T) -> &isize),
    USIZE(fn(&T) -> &usize),
}

pub trait Insert : Introspect + Eq + Hash
    where Self: std::marker::Sized {
    type HashEq: Hash + Eq + Clone + Debug;
    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<Getters<Self>>;
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>>;
}



pub trait ReteIntrospection : Eq + Hash {
    type HashEq: Hash + Eq + Clone + Debug;

    fn static_type_id() -> TypeId;
    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<fn(&Self) -> &u64>;
    fn type_id(&self) -> TypeId;
}

