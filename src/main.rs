#![feature(conservative_impl_trait)]

#[macro_use]
extern crate mopa;

extern crate num;

#[macro_use]
extern crate itertools;

extern crate string_interner;

extern crate ordered_float;

mod expert;
use itertools::Itertools;

use std::mem;
use std::any::TypeId;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::hash::Hash;
use std::hash::Hasher;
use expert::serial::SerialGen;
use std::cmp::Ordering;
use expert::builder::StatementCondition;
use expert::base::MemoryId;
use expert::memory::KStringInterner;
use expert::iter::OptionIter;
use expert::introspection::{ReteMopa, ReteIntrospection};


#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
struct Aspect {
    id: u64,
    aspect_type: u64,
    impact: u64
}

impl Aspect {
    fn get_id(&self) -> &u64 {
        &self.id
    }

    fn get_aspect_type(&self) -> &u64 {
        &self.aspect_type
    }

    fn get_impact(&self) -> &u64 {
        &self.impact
    }

    fn exhaustive_hash<'a>(&'a self) -> impl Iterator<Item=(Option<u64>, Option<u64>, Option<u64>)> +'a {
        iproduct!(OptionIter::new(Some(self.id)), OptionIter::new(Some(self.aspect_type)), OptionIter::new(Some(self.impact)))
    }
}

impl ReteIntrospection for Aspect {
    type HashEq = [Option<u64>; 3];
    fn static_type_id() -> TypeId {
        TypeId::of::<Self>()
    }

    fn getter(field: &str) -> Option<fn(&Self) -> &u64> {
        match field {
            "id" => Some(Self::get_id),
            "aspect_type" => Some(Self::get_aspect_type),
            "impact" => Some(Self::get_impact),
            _ => None
        }
    }

    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }


    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &KStringInterner) -> [Option<u64>; 3] {
        let mut o_id = None;
        let mut o_aspect_type = None;
        let mut o_impact = None;

        for c in conditions {
            match c {
                &StatementCondition::Exists => return [None, None, None],
                &StatementCondition::Eq{field_sym, to} => {
                    match string_interner.resolve(field_sym) {
                        Some("id") => o_id = Some(to),
                        Some("aspect_type") => o_aspect_type = Some(to),
                        Some("impact") => o_impact = Some(to),
                        _ => {}
                    }
                },
                _ => continue
            }
        }
        [o_id, o_aspect_type, o_impact]
    }
}

impl ReteMopa for Aspect {}

enum BetaJoin {
    //NOT(MemoryId),
    AND(MemoryId, MemoryId),
    //OR(MemoryId, MemoryId),
    ALL(Vec<MemoryId>),
    //ANY(Vec<MemoryId>)
}

fn main() {
    use expert::builder::KnowledgeBuilder;
    use expert::builder::ConditionData;

    let mut builder: KnowledgeBuilder<Aspect> = KnowledgeBuilder::new()
        .rule("test1")
            .when()
                .eq("id", 6).eq("aspect_type", 12).btwn("impact", 8, false, 400, true)
        .then()
            .when()
                .eq("id", 6).eq("aspect_type", 12).btwn("impact", 8, false, 400, true)
        .then()
        .end()
        .rule("test2")
            .when()
            .eq("id", 9).eq("aspect_type", 1).btwn("impact", 8, false, 400, true)
            .then()
            .when()
            .eq("id", 9).eq("aspect_type", 1)//.btwn("impact", 8, false, 400, true).btwn("impact", 9, false, 400, true)
            .then()
            .when()
            .gt("impact", 5, true)
            .then()
        .end();

    println!("builder: {:?}", &builder);

//    let base = builder.compile();

    let a = Aspect{id: 485, aspect_type: 3840, impact: 9};
    for i in a.exhaustive_hash() {
        println!("{:?}", i);
    }

}
