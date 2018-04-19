use super::prelude::Stage1Compile;
use super::super::nodes::beta::SDynLimit;
use shared::fact::Fact;
use shared::nodes::beta::BetaNode;
use shared::compiler::prelude::Stage1Node;
use runtime::memory::StringCache;
use shared::compiler::prelude::DeclareNode;
use shared::compiler::id_generator::{IdGenerator, StatementId, RuleId};
use runtime::memory::SymbolId;
use std::collections::HashSet;
use std::collections::HashMap;

struct BuilderContext {

}

// Have a single KnowledgeBuilder trait
// Have a single RuleBuilder trait
// Have a single Struct that implements both traits
// it switches between the two traits via impl trait on the entrance and exit functions
// stores current rule_id (0 by default)
// fetches each rule during modification (set name, salience, other options)
//
//pub struct KnowledgeBaseBuilder {
//
//}
//
//pub struct RuleBuilder {
//    cache: StringCache,
//}
//
//impl RuleBuilder {
//
//    pub fn when<T:Fact, N: Stage1Compile<T>>(mut self, nodes: &[N]) -> RuleBuilder {
//        Stage1Node::All(N::stage1_compile_slice(nodes, &mut self.cache).unwrap()).clean();
//        self
//    }
//
//}

// TODO Beta compile
/*
  * Store 2 different Map<k, Set<v>> to represent the relationships between parent and children
      * one for the relationship between parent node and children
      * one for relationship between child and parents
  * Store a Set<Logic<ArrayId>> and Map<ArrayId, Set<Logic<ArrayId>>> te track parent types
  * Store Map<Vec<children>, ArrayId> to prevent duplicate parents
  * take the most shared child
  * iterate parents' children to determine the set of most shared children
  * create a new intermediate node between the parent and the children. Update their information as necessary
  * continue until no more nodes are shared (up to what point?)

  * Rules will remember their entry point
  * Arrays will remember their rules
*/

pub trait KnowledgeBase {

}

pub struct ArrayKnowledgeBase {

}

impl KnowledgeBase for ArrayKnowledgeBase {}


#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
enum StatementGroupEntry {
    Statement(StatementId),
    Child(GroupId),
}

enum StatementGroup {
    All(GroupId, Vec<StatementGroupEntry>),
    Any(GroupId, Vec<StatementGroupEntry>),
    Exists(GroupId, Vec<StatementGroupEntry>),
    NotAll(GroupId, Vec<StatementGroupEntry>),
    ForAll(GroupId, StatementId, Vec<StatementGroupEntry>),
}

impl StatementGroup {
    fn all(parent: GroupId) -> StatementGroup {
        StatementGroup::All(parent, Vec::new())
    }

    fn any(parent: GroupId) -> StatementGroup {
        StatementGroup::Any(parent, Vec::new())
    }

    fn exists(parent: GroupId) -> StatementGroup {
        StatementGroup::Exists(parent, Vec::new())
    }

    fn not_all(parent: GroupId) -> StatementGroup {
        StatementGroup::NotAll(parent, Vec::new())
    }

    fn for_all(parent: GroupId, statement: StatementId) -> StatementGroup {
        StatementGroup::ForAll(parent, statement, Vec::new())
    }

    fn parent(&self) -> GroupId {
        match *self {
            StatementGroup::All(parent, _) => parent,
            StatementGroup::Any(parent, _) => parent,
            StatementGroup::Exists(parent, _) => parent,
            StatementGroup::NotAll(parent, _) => parent,
            StatementGroup::ForAll(parent, ..) => parent,
        }
    }

    fn add(&mut self, entry: StatementGroupEntry) {
        match *self {
            StatementGroup::All(_, ref mut entries) => entries.push(entry),
            StatementGroup::Any(_, ref mut entries) => entries.push(entry),
            StatementGroup::Exists(_, ref mut entries) => entries.push(entry),
            StatementGroup::NotAll(_, ref mut entries) => entries.push(entry),
            StatementGroup::ForAll(_, _, ref mut entries) => entries.push(entry),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct StatementReq {
    declares: HashSet<SymbolId>, // HashSet or vec?
    requires: HashSet<SymbolId>
}

// TODO: After we build up the groupings & requirements, cascade down the groupings to ensure that we're not screwing anything up

struct ArrayRuleData {
    id: RuleId,
    name: SymbolId,
    salience: i32,
    no_loop: bool,
    agenda: SymbolId,
    current_group: GroupId,
    statement_groups: HashMap<GroupId, StatementGroup>,
    statement_requirements: HashMap<StatementId, StatementReq>,
}

pub trait BaseBuilder {
    type RB: RuleBuilder;
    type KB: KnowledgeBase;

    fn rule<S: AsRef<str>>(self, name: S) -> Self::RB;
    fn end(self) -> Self::KB;
}

pub struct ArrayBaseBuilder {
    id_generator: IdGenerator,
    cache: StringCache,
}

impl BaseBuilder for ArrayBaseBuilder {
    type RB = ArrayRuleBuilder;
    type KB = ArrayKnowledgeBase;

    fn rule<S: AsRef<str>>(mut self, name: S) -> Self::RB {
        let id = self.id_generator.rule_ids.next();
        let name_symbol = self.cache.get_or_intern(name.as_ref());
        let agenda_symbol = self.cache.get_or_intern("MAIN");
        let root_group_id = self.id_generator.group_ids.next();
        let root_group = StatementGroup::all(root_group_id);

        let mut statement_groups = Default::default();
        statement_groups.insert(root_group_id, root_group);

        // NEXT - create group enum & add new one to the set. Need to know parent
        ArrayRuleBuilder {
            rule_data : ArrayRuleData {
                id,
                name: name_symbol,
                salience: 0,
                no_loop: false,
                agenda: agenda_symbol,
                current_group: root_group_id,
                statement_groups,
                statement_requirements: Default::default(),
            },
            base_builder: self
        }
    }

    fn end(self) -> Self::KB {
        unimplemented!()
    }
}

pub trait RuleBuilder {
    type CB: ConsequenceBuilder;

    fn salience(self, salience: i32) -> Self;
    fn agenda<S: AsRef<str>>(self, agenda: S) -> Self;
    fn no_loop(self, no_loop: bool) -> Self;
    fn when<T:Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Self;
    fn declare_when<T:Fact, S: AsRef<str>, N: Stage1Compile<T>>(self, declare: &[DeclareNode<S, S>], nodes: &[N]);
    fn all_group(self) -> Self;
    fn any_group(self) -> Self;
    fn exists_group(self) -> Self;
    fn not_group(self) -> Self;
    fn for_all_group<T:Fact, N: Stage1Compile<T>>(self, node: &[N]) -> Self;
    fn end_group(self) -> Self;
    fn then(self) -> Self::CB;
}

pub struct ArrayRuleBuilder {
    rule_data: ArrayRuleData,
    base_builder: ArrayBaseBuilder,
}

impl ArrayRuleBuilder {
    fn add_new_group(&mut self, new_group: StatementGroup) {
        let new_group_id = self.base_builder.id_generator.group_ids.next();
        let parent_id = new_group.parent();
        self.rule_data.statement_groups.get_mut(&parent_id).unwrap().push(StatementGroupEntry::Child(new_group_id));
        self.rule_data.statement_groups.insert(new_group_id, new_group);
        self.rule_data.current_group = new_group_id;
    }
}

impl RuleBuilder for ArrayRuleBuilder {
    type CB = ArrayConsequenceBuilder;

    fn salience(mut self, salience: i32) -> Self {
        self.rule_data.salience = salience;
        self
    }

    fn agenda<S: AsRef<str>>(mut self, agenda: S) -> Self {
        let agenda_symbol = self.base_builder.cache.get_or_intern(agenda.as_ref());
        self.rule_data.agenda = agenda_symbol;
        self
    }

    fn no_loop(mut self, no_loop: bool) -> Self {
        self.rule_data.no_loop = no_loop;
        self
    }

    fn when<T: Fact, N: Stage1Compile<T>>(mut self, nodes: &[N]) -> Self {
        let statement_id = self.base_builder.id_generator.statement_ids.next();
        unimplemented!()
    }

    fn declare_when<T: Fact, S: AsRef<str>, N: Stage1Compile<T>>(mut self, declare: &[DeclareNode<S, S>], nodes: &[N]) {
        let statement_id = self.base_builder.id_generator.statement_ids.next();
        unimplemented!()
    }

    fn all_group(mut self) -> Self {
        self.add_new_group(StatementGroup::all(self.rule_data.current_group));
        self
    }

    fn any_group(mut self) -> Self {
        self.add_new_group(StatementGroup::any(self.rule_data.current_group));
        self
    }

    fn exists_group(mut self) -> Self {
        self.add_new_group(StatementGroup::exists(self.rule_data.current_group));
        self
    }

    fn not_group(mut self) -> Self {
        self.add_new_group(StatementGroup::not_all(self.rule_data.current_group));
        self
    }

    fn for_all_group<T: Fact, N: Stage1Compile<T>>(mut self, node: &[N]) -> Self {
        let statement_id = self.base_builder.id_generator.statement_ids.next();
        // TODO - compile the node
        self.add_new_group(StatementGroup::for_all(self.rule_data.current_group, statement_id));
        self
    }

    fn end_group(mut self) -> Self {
        let parent_id = self.rule_data.statement_groups.get(&self.rule_data.current_group).unwrap().parent();
        if parent_id == self.rule_data.current_group {
            unreachable!("end_group at root {:?}", parent_id);
        }
        self.rule_data.current_group = parent_id;
        self
    }

    fn then(self) -> Self::CB {
        ArrayConsequenceBuilder{
            rule_data: self.rule_data,
            consequence_data: ArrayConsequenceData {},
            base_builder: self.base_builder
        }
    }
}

struct ArrayConsequenceData {

}

pub trait ConsequenceBuilder {
    type BB: BaseBuilder;
    fn end(self) -> Self::BB;
}

pub struct ArrayConsequenceBuilder {
    rule_data: ArrayRuleData,
    consequence_data: ArrayConsequenceData,
    base_builder: ArrayBaseBuilder,
}

impl ConsequenceBuilder for ArrayConsequenceBuilder {
    type BB = ArrayBaseBuilder;

    fn end(self) -> Self::BB {
        self.base_builder
    }
}

#[cfg(test)]
mod tests {
    use shared::fact::{Fact, Getter};
    use super::*;
    use super::super::as_ref::*;
    use runtime::memory::StringCache;
    use shared::compiler::prelude::Stage1Node;

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    struct Dummy {
        d: u64
    }

    impl Dummy {
        fn get_d(&self) -> &u64 {
            &self.d
        }
    }

    impl Fact for Dummy {
        type HashEq = ();

        fn getter(field: &str) -> Option<Getter<Self>> {
            match field {
                "d" => Some(Getter::U64(Dummy::get_d)),
                _ => unimplemented!()
            }
        }

        fn exhaustive_hash(&self) -> Box<Iterator<Item=<Self as Fact>::HashEq>> {
            unimplemented!()
        }
    }

    #[test]
    pub fn as_ref_test() {
//        let cache = StringCache::new();
//        let b = RuleBuilder{cache}
//            .when::<Dummy, _>(&all(
//                &[not(any(&[eq("d", 6u64)])), all(&[le("d", 64u64), le("d", dyn("ab"))])]
//            ));
    }
}