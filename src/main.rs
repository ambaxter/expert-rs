#![feature(generators, generator_trait, conservative_impl_trait)]

#[macro_use]
extern crate mopa;

extern crate num;

extern crate itertools;

extern crate string_interner;

extern crate ordered_float;

mod expert;
use itertools::Itertools;

use std::any::TypeId;
use std::ops::{Generator, GeneratorState};
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::hash::Hash;
use std::hash::Hasher;
use expert::serial::SerialGen;
use std::cmp::Ordering;
use expert::builder::StatementCondition;
use string_interner::DefaultStringInterner;

use expert::introspection::{ReteMopa, ReteIntrospection};


struct GenIter<T>(T);

impl<T: Generator<Return = ()>> Iterator for GenIter<T> {
    type Item = T::Yield;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.resume() {
            GeneratorState::Complete(..) => None,
            GeneratorState::Yielded(v) => Some(v),
        }
    }
}

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

    fn exhaustive_hash<'a>(&'a self) -> impl Iterator<Item=[Option<u64>; 3]> +'a {
        GenIter(move || {
            yield [Some(self.id), Some(self.aspect_type), Some(self.impact)];
            yield [Some(self.id), Some(self.aspect_type), None];
            yield [Some(self.id), None, Some(self.impact)];
            yield [Some(self.id), None, None];
            yield [None, Some(self.aspect_type), Some(self.impact)];
            yield [None, None, Some(self.impact)];
            yield [None, Some(self.aspect_type), None];
            yield [None, None, None];
        })
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


    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &DefaultStringInterner) -> [Option<u64>; 3] {
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

fn main() {
    use expert::builder::KnowledgeBuilder;

    println!("Do test output: {:?}", expert::base::new::do_test::<Aspect>());

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
    
    let base = builder.compile();

}
