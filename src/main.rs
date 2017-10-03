#![feature(generators, generator_trait, conservative_impl_trait)]

#[macro_use]
extern crate mopa;

extern crate num;

extern crate itertools;

extern crate string_interner;

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
    fn get_id(&self) -> u64 {
        self.id
    }

    fn get_aspect_type(&self) -> u64 {
        self.aspect_type
    }

    fn get_impact(&self) -> u64 {
        self.impact
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

    fn getter(field: &str) -> Option<fn(&Self) -> u64> {
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

/*
#[derive(Debug, Copy, Clone)]
enum ReteNode {
    Alpha{id: usize},
    Beta{id: usize},
    And{id: usize, l_id: usize, r_id: usize},
}

impl ReteNode {
    pub fn alpha(id: usize) -> ReteNode {
        ReteNode::Alpha{id}
    }
    pub fn beta(id: usize) -> ReteNode {
        ReteNode::Beta{id}
    }

    pub fn and(id: usize, l_node: &ReteNode, r_node: &ReteNode) -> ReteNode {
        ReteNode::And{id, l_id: l_node.id(), r_id: r_node.id()}
    }

    pub fn id(&self) -> usize {
        match *self {
            ReteNode::Alpha{id} => id,
            ReteNode::Beta{id} => id,
            ReteNode::And{id, ..} => id
        }
    }
}

struct ReteNodeBuilder {
    node_ids: SerialGen<usize>
}

impl ReteNodeBuilder {

    pub fn new() -> ReteNodeBuilder {
        ReteNodeBuilder{node_ids: Default::default()}
    }

    pub fn alpha(&mut self) -> ReteNode {
        ReteNode::alpha(self.node_ids.next())
    }

    pub fn beta(&mut self) -> ReteNode {
        ReteNode::beta(self.node_ids.next())
    }


    pub fn and(&mut self, l_node: &ReteNode, r_node: &ReteNode) -> ReteNode {
        ReteNode::and(self.node_ids.next(), l_node, r_node)
    }
}

impl PartialEq for ReteNode {
    fn eq(&self, other: &ReteNode) -> bool {
        self.id() == other.id()
    }
}

impl Eq for ReteNode {}

impl Hash for ReteNode {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id().hash(state);
    }
}

impl PartialOrd for ReteNode {
    fn partial_cmp(&self, other: &ReteNode) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ReteNode {
    fn cmp(&self, other: &ReteNode) -> Ordering {
        self.id().cmp(&other.id())
    }
}*/

fn main() {
    use expert::builder::KnowledgeBuilder;

    let f = Aspect::getter("id").unwrap();

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
            .gt("impact", 5, true)
            .then()
        .end();

    println!("builder: {:?}", &builder);

/*
    // Last POC. Worked pretty well
    let mut node_builder = ReteNodeBuilder::new();

    let mut compiler_map:HashMap<ReteNode, HashSet<usize>> = HashMap::new();

    compiler_map.insert(node_builder.alpha(), [1,2,3,4,5,6,7,8,9,10].iter().cloned().collect());
    compiler_map.insert(node_builder.beta(), [1].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [2].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [3].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [4].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [5].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [6].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [7].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [8].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [9].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [10].iter().cloned().collect());
    compiler_map.insert(node_builder.alpha(), [11].iter().cloned().collect());


    loop {

        println!("Compiler map :");

        for (k, v) in &compiler_map {
            println!("  {:?} - {:?}", k, v);
        }

        let mut intersect_map: HashMap<Vec<usize>, HashSet<(ReteNode, ReteNode)>> = HashMap::new();

        for ((ref n1, ref p1), (ref n2, ref p2)) in compiler_map
            .iter()
            .tuple_combinations()
            .filter(|&((_, ref p1), (_ , ref p2))| !p1.is_empty() && !p2.is_empty()) {

            let mut intersection: Vec<_> = p1.intersection(&p2).cloned().collect();
            if intersection.is_empty() {
                continue;
            }
            intersection.sort();

            for len in 1..intersection.len() + 1 {
                for  combination in intersection.iter().cloned().combinations(len) {
                    intersect_map.entry(combination)
                        .or_insert_with(|| HashSet::new())
                        .insert((**n1, **n2));
                }
            }

        }

        if intersect_map.is_empty() {
            break;
        }

        println!("intersect_map:");
        let mut intersections:Vec<(Vec<usize>, HashSet<(ReteNode, ReteNode)>)> = intersect_map.into_iter().collect();
        intersections.sort_by(|&(ref c1, ref s1), &(ref c2, ref s2)| c1.len().cmp(&c2.len()).then(s1.len().cmp(&s2.len())));
        for &(ref c, ref s) in &intersections {
            println!("c: {:?}, s: {:?}", c, s);
        }

        if let Some((c, s)) = intersections.pop() {
            let mut join_set: HashSet<ReteNode> = HashSet::new();
            let mut last_join_store = None;
            for &(ref r1, ref r2) in s.iter() {
                let m = (join_set.contains(r1), join_set.contains(r2));
                match m {
                    (false, false) => {
                        if let Some(ref mut r1_entries) = compiler_map.get_mut(r1) {
                            r1_entries.retain(|&x| !c.contains(&x));
                        }
                        join_set.insert(*r1);
                        if let Some(ref mut r2_entries) = compiler_map.get_mut(r2) {
                            r2_entries.retain(|&x| !c.contains(&x));
                        }
                        join_set.insert(*r2);
                        let new_join = node_builder.and(r1, r2);
                        if let Some(last_join) = last_join_store {
                            let combined_join = node_builder.and(&new_join, &last_join);
                            compiler_map.insert(last_join, Default::default());
                            compiler_map.insert(new_join, Default::default());
                            last_join_store = Some(combined_join);
                        } else {
                            last_join_store = Some(new_join);
                        }
                    },
                    (false, true) => {
                        if let Some(ref mut r1_entries) = compiler_map.get_mut(r1) {
                            r1_entries.retain(|&x| !c.contains(&x));
                        }
                        join_set.insert(*r1);
                        let last_join = last_join_store.unwrap();
                        let join = node_builder.and(r1, &last_join);
                        compiler_map.insert(last_join, Default::default());
                        last_join_store = Some(join);
                    },
                    (true, false) => {
                        if let Some(ref mut r2_entries) = compiler_map.get_mut(r2) {
                            r2_entries.retain(|&x| !c.contains(&x));
                            join_set.insert(*r2);
                        }
                        let last_join = last_join_store.unwrap();
                        let join = node_builder.and(r2, &last_join);
                        compiler_map.insert(last_join, Default::default());
                        last_join_store = Some(join);
                    },
                    _ => {}
                }
            }
            if let Some(last_join) = last_join_store {
                compiler_map.insert(last_join, c.into_iter().collect());
            }
        }
    }

    let mut entries:Vec<_> = compiler_map.into_iter().collect();
    entries.sort_by_key(|&(k, _)| k);


    println!("Compiler map result:");

    for &(ref k, ref v) in &entries {
        println!("  {:?} - {:?}", k, v);
    }*/

/*    use expert::base::{Rule, Condition};

    let mut e_map = HashMap::new();
    e_map.insert([Some(102), Some(0), None], 1);
    let aspect = Aspect{id: 102, aspect_type: 0, impact: 7};
    for ref i in aspect.exhaustive_hash() {
        println!("i: {:?}", e_map.get(i));
    }

    let rules = vec![
            Rule::new("Test")
                .add_condition_eq(Some(0), Some(2), None)
                .add_condition_eq(Some(273), Some(4), None)
                .add_operation_print("Message"),
            Rule::new("Test2")
                .add_condition_eq(Some(5), Some(3), Some(1))
                .add_condition_eq(Some(2), Some(1), Some(1))
                .add_operation_print("Test2 Message"),
            Rule::new("Empty")
                .add_condition_eq(None, None, None)
                .add_operation_print("Empty Message")
        ];
    for rule in &rules {
        println!("rule: {:?}", rule);
    }*/


}
