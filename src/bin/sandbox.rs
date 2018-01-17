#![feature(conservative_impl_trait)]
#![feature(plugin)]
#![plugin(interpolate_idents)]


extern crate num;

#[macro_use]
extern crate itertools;

extern crate string_interner;

extern crate ordered_float;

#[macro_use]
extern crate expert;

extern crate parking_lot;

use itertools::Itertools;

use parking_lot::Mutex;

use std::mem;
use std::any::TypeId;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::hash::Hash;
use std::hash::Hasher;
use expert::serial::SerialGen;
use std::cmp::Ordering;
use expert::builder::StatementCondition;
use expert::builders::statement::{ValueHolder, StatementValues, StatementConditions};
use expert::runtime::memory::MemoryId;
use expert::runtime::memory::StringCache;
use expert::iter::OptionIter;
use expert::traits::ReteIntrospection;
use expert::traits::{Introspect, Fact, Getters, FieldValue};

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

impl Introspect for Aspect {
    fn static_type_id() -> TypeId {
        TypeId::of::<Self>()
    }
}

impl Fact for Aspect {
    type HashEq = (Option<u64>, Option<u64>, Option<u64>);

    fn getter(field: &str) -> Option<Getters<Self>> {
        use Getters::*;
        match field {
            "id" => Some(U64(Self::get_id)),
            "aspect_type" => Some(U64(Self::get_aspect_type)),
            "impact" => Some(U64(Self::get_impact)),
            _ => None
        }
    }

    fn create_hash_eq(conditions: &Vec<StatementConditions>, cache: &StringCache) -> Self::HashEq {
        use StatementConditions::*;
        use StatementValues::*;
        use ValueHolder::*;
        let mut o_id = None;
        let mut o_aspect_type = None;
        let mut o_impact = None;

        for c in conditions.iter()
            .filter(|c| c.is_hash_eq()) {
            let field = c.field();
            match (cache.resolve(field), c) {
                (Some("id"), &Eq(_, U64(S(to)))) => o_id = Some(to),
                (Some("aspect_type"), &Eq(_, U64(S(to)))) => o_aspect_type = Some(to),
                (Some("impact"), &Eq(_, U64(S(to)))) => o_impact = Some(to),
                _ => continue
            }
        }
        (o_id, o_aspect_type, o_impact)
    }
    fn exhaustive_hash(&self) -> Box<Iterator<Item=Self::HashEq>> {
        Box::new(iproduct!(OptionIter::some(self.id), OptionIter::some(self.aspect_type), OptionIter::some(self.impact)))
    }
    fn new_from_fields(fields: &[FieldValue], cache: &StringCache) -> Self {
        use self::FieldValue::*;
        let mut o_id = None;
        let mut o_aspect_type = None;
        let mut o_impact = None;

        for f in fields.iter() {
            let field = f.field();
            match (cache.resolve(field), f) {
                (Some("id"), &U64(_, val)) => o_id = Some(val),
                (Some("aspect_type"), &U64(_, val)) => o_aspect_type = Some(val),
                (Some("impact"), &U64(_, val)) => o_impact = Some(val),
                _ => {}
            }
        }

        Aspect {
            id: o_id.unwrap_or(0),
            aspect_type: o_aspect_type.unwrap_or(1),
            impact: o_impact.unwrap_or(2)
        }
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


    fn create_hash_eq(conditions: &Vec<StatementCondition>, string_interner: &StringCache) -> [Option<u64>; 3] {
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

enum BetaJoin {
    //NOT(MemoryId),
    AND(MemoryId, MemoryId),
    //OR(MemoryId, MemoryId),
    ALL(Vec<MemoryId>),
    //ANY(Vec<MemoryId>)
}

expert!(Knowledge; [Aspect]);

fn main() {
    use expert::builder::KnowledgeBuilder;
    use expert::builder::CData;
    use ordered_float::NotNaN;
    use expert::runtime::memory::SymbolId;

    println!("TestData: {:?}", std::mem::size_of::<expert::network::node_cmp::TestData<Aspect>>());
    println!("StrTest: {:?}", std::mem::size_of::<expert::network::node_cmp::StrTest<SymbolId>>());
    println!("FlTest: {:?}", std::mem::size_of::<expert::network::node_cmp::FlTest<NotNaN<f64>>>());
    println!("NumTest: {:?}", std::mem::size_of::<expert::network::node_cmp::NumTest<usize>>());

    let default = vec![false, true, false];
    let mut test_into = Vec::with_capacity(default.len());
    for c in default.iter().cloned().map(|c| Mutex::new(c)) {
        test_into.push(c);
    }

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
