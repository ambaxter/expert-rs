use mopa;
use std::any::TypeId;
use std::hash::Hash;
use ::builder::StatementCondition;
use runtime::memory::StringCache;
use std::fmt::Debug;

pub trait ReteMopa: mopa::Any {}

mopafy!(ReteMopa);

pub trait ReteIntrospection : ReteMopa + Eq + Hash {
    type HashEq: Hash + Eq + Clone + Debug;

    fn static_type_id() -> TypeId;
    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> Self::HashEq;
    fn getter(field: &str) -> Option<fn(&Self) -> &u64>;
    fn type_id(&self) -> TypeId;
}

