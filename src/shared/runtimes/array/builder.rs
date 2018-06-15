use shared::fact::{Fact, Getter, FactFieldType, GetFieldType};
use shared::nodes::alpha::{IsHashEq, AlphaNode};
use shared::nodes::beta::{CollectRequired, BetaNode};
use shared::compiler::prelude::Stage1Node;
use runtime::memory::StringCache;
use shared::compiler::prelude::{DrainWhere, ProvidesNode};
use shared::compiler::id_generator::{IdGenerator, StatementGroupId, StatementId, ConditionId, ConditionGroupId, RuleId};
use runtime::memory::SymbolId;
use std::collections::HashSet;
use std::collections::HashMap;
use errors::CompileError;
use std;
use std::collections::BTreeMap;
use shared::nodes::alpha::HashEqField;
use anymap::any::{IntoBox, Any, UncheckedAnyExt};
use anymap::Map;
use shared::compiler::builder::BaseBuilder;
use shared::compiler::builder::RuleBuilder;
use shared::compiler::prelude::Stage1Compile;
use shared::compiler::builder::ConsequenceBuilder;
use shared::compiler::builder::KnowledgeBase;
use bimap::BiMap;
use std::any::TypeId;
use std::fmt::Debug;

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
  * continue until no more nodes are shared (up to cd twhat point?)

  * Rules will remember their entry point
  * Arrays will remember their rules
*/


pub struct ArrayKnowledgeBase {

}

impl KnowledgeBase for ArrayKnowledgeBase {}


#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Debug)]
enum StatementGroupEntry {
    Statement(StatementId),
    Exists(StatementId),
    Absent(StatementId),
    Child(StatementGroupId),
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum StatementGroup {
    All(StatementGroupId, Vec<StatementGroupEntry>),
    Any(StatementGroupId, Vec<StatementGroupEntry>),
    ForAll(StatementGroupId, StatementId, Vec<StatementGroupEntry>),
}

impl StatementGroup {
    fn all(parent: StatementGroupId) -> StatementGroup {
        StatementGroup::All(parent, Vec::new())
    }

    fn any(parent: StatementGroupId) -> StatementGroup {
        StatementGroup::Any(parent, Vec::new())
    }

    fn for_all(parent: StatementGroupId, statement: StatementId) -> StatementGroup {
        StatementGroup::ForAll(parent, statement, Vec::new())
    }

    fn parent(&self) -> StatementGroupId {
        match *self {
            StatementGroup::All(parent, _) => parent,
            StatementGroup::Any(parent, _) => parent,
            StatementGroup::ForAll(parent, ..) => parent,
        }
    }

    fn push(&mut self, entry: StatementGroupEntry) {
        match *self {
            StatementGroup::All(_, ref mut entries) => entries.push(entry),
            StatementGroup::Any(_, ref mut entries) => entries.push(entry),
            StatementGroup::ForAll(_, _, ref mut entries) => entries.push(entry),
        }
    }

    fn should_merge(&self, other: &Self) -> bool {
        use self::StatementGroup::*;
        match (self, other) {
            (All(..), All(..)) => true,
            (ForAll(..), All(..)) => true,
            (Any(..), Any(..)) => true,
            _ => false
        }
    }

    fn can_single_entry_optimize(&self) -> bool {
        use self::StatementGroup::*;
        match self {
            StatementGroup::All(_, entries) => entries.len() == 1,
            StatementGroup::Any(_, entries) => entries.len() == 1,
            _ => false
        }
    }

    fn extract_single_entry(self) -> StatementGroupEntry {
        use self::StatementGroup::*;
        match self {
            StatementGroup::All(_, entries) => entries[0],
            StatementGroup::Any(_, entries) => entries[0],
            _ => unreachable!("extract single entry from non optimizable group {:?}", self)
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ConditionGroupType {
    Any,
    NotAny,
    All,
    NotAll
}

pub trait StatementDetails {
    fn provides_var(&self) -> Option<SymbolId>;
    fn provides_fields<'a>(&'a self) -> Box<Iterator<Item = (SymbolId, FactFieldType)> + 'a>;
    fn requires_fields(&self) -> &HashMap<SymbolId, HashSet<FactFieldType>>;
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct StatementProvides<T: Fact> {
    var: Option<SymbolId>,
    fields: HashMap<SymbolId, Getter<T>>
}

impl<T: Fact> Default for StatementProvides<T> {
    fn default() -> Self {
        StatementProvides{var: None, fields: Default::default()}
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct StatementData<T: Fact> {
    statement_provides: StatementProvides<T>,
    statement_requires: HashMap<SymbolId, HashSet<FactFieldType>>,
    condition_groups: HashMap<ConditionGroupId, ConditionGroupType>
}

impl<T: Fact> StatementDetails for StatementData<T> {
    fn provides_var(&self) -> Option<SymbolId> {
        self.statement_provides.var
    }

    fn provides_fields<'a>(&'a self) -> Box<Iterator<Item = (SymbolId, FactFieldType)> + 'a> {
        Box::new(
            self.statement_provides.fields.iter()
            .map(|(key, val)| (*key, val.get_field_type()))
        )
    }

    fn requires_fields(&self) -> &HashMap<SymbolId, HashSet<FactFieldType>> {
        &self.statement_requires
    }
}

// TODO: After we build up the groupings & requirements, cascade down the groupings to ensure that we're not screwing anything up

struct ArrayRuleData {
    id: RuleId,
    name: SymbolId,
    salience: i32,
    no_loop: bool,
    agenda_group: SymbolId,
    current_group: StatementGroupId,
    statement_groups: BTreeMap<StatementGroupId, StatementGroup>,
    statement_data: BTreeMap<StatementId, Box<StatementDetails>>,
}


struct AlphaConditionInfo {
    condition_id: ConditionId,
    dependents: HashSet<StatementId>,
}

impl AlphaConditionInfo {
    fn new(condition_id: ConditionId) -> AlphaConditionInfo {
        AlphaConditionInfo{ condition_id, dependents: HashSet::new() }
    }
}

pub trait NetworkBuilder: Any {

}

//impl<T: StdAny> NetworkBuilder for T {}

impl UncheckedAnyExt for NetworkBuilder {
    #[inline]
    unsafe fn downcast_ref_unchecked<T: 'static>(&self) -> &T {
        &*(self as *const Self as *const T)
    }

    #[inline]
    unsafe fn downcast_mut_unchecked<T: 'static>(&mut self) -> &mut T {
        &mut *(self as *mut Self as *mut T)
    }

    #[inline]
    unsafe fn downcast_unchecked<T: 'static>(self: Box<Self>) -> Box<T> {
        Box::from_raw(Box::into_raw(self) as *mut T)
    }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Debug)]
enum ConditionGroupChild {
    Condition(ConditionId),
    Group(ConditionGroupId),
}

struct BetaGraph<T: Fact> {
    rule_rel: HashMap<ConditionGroupChild, Vec<RuleId>>,
    statement_root: HashMap<StatementId, ConditionGroupChild>,
    parent_child_rel: BiMap<ConditionGroupId, Vec<ConditionGroupChild>>,
    child_group_rel: HashMap<ConditionGroupChild, ConditionGroupId>,
    test_nodes: BiMap<ConditionId, BetaNode<T>>
}

impl<T: Fact> Default for BetaGraph<T> {
    fn default() -> Self {
        BetaGraph {
            rule_rel: Default::default(),
            statement_root: Default::default(),
            parent_child_rel: Default::default(),
            child_group_rel: Default::default(),
            test_nodes: Default::default()
        }
    }
}

pub struct ArrayNetworkBuilder<T: Fact> {
    alpha_graph: HashMap<T::HashEq, HashMap<AlphaNode<T>, AlphaConditionInfo>>,
    beta_graph_map: HashMap<SymbolId, BetaGraph<T>>
}

impl<T:Fact> IntoBox<NetworkBuilder + 'static> for ArrayNetworkBuilder<T> {
    fn into_box(self) -> Box<NetworkBuilder> {
        Box::new(self)
    }
}


impl<T: Fact> ArrayNetworkBuilder<T> {
    fn new() -> ArrayNetworkBuilder<T> {
        ArrayNetworkBuilder{ alpha_graph: Default::default(), beta_graph_map: Default::default()}
    }
}

impl<T:Fact> Default for ArrayNetworkBuilder<T> {
    fn default() -> Self {
        ArrayNetworkBuilder::new()
    }
}

impl<T: Fact> NetworkBuilder for ArrayNetworkBuilder<T> {

}

pub struct ArrayBaseBuilder {
    id_generator: IdGenerator,
    cache: StringCache,
    network_builders: Map<NetworkBuilder>
}

impl ArrayBaseBuilder {
    fn insert_alpha<T: Fact>(&mut self, statement_id: StatementId, nodes: Vec<AlphaNode<T>>) {
        let hash_eq = T::create_hash_eq(&nodes);
        let (alpha_graph, id_generator) =
            (
                self.network_builders.entry::<ArrayNetworkBuilder<T>>().or_insert_with(|| Default::default())
                    .alpha_graph.entry(hash_eq).or_insert_with(|| Default::default()),
                &mut self.id_generator
            );
        for node in nodes.into_iter().filter(|n| !n.is_hash_eq()) {
            alpha_graph.entry(node)
                .or_insert_with(|| AlphaConditionInfo::new(id_generator.condition_ids.next()))
                .dependents.insert(statement_id);
        }
    }

    fn insert_beta<T: Fact>(&mut self, agenda_group: SymbolId, rule_id: RuleId, statement_id: StatementId, beta_node: Stage1Node<T>) -> HashMap<ConditionGroupId, ConditionGroupType> {
        let mut condition_groups = Default::default();
        if !beta_node.is_empty() {
            let(beta_graph, id_generator) =
                (
                    self.network_builders.entry::<ArrayNetworkBuilder<T>>().or_insert_with(|| Default::default())
                        .beta_graph_map.entry(agenda_group).or_insert_with(|| Default::default()),
                    &mut self.id_generator
                );
            // Thank you @moxian in the Rust Discord for figuring out my monumental mistake!
            use self::Stage1Node::*;
            let statement_root = match beta_node {
                Any(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::Any, beta_nodes, &mut condition_groups),
                NotAny(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::NotAny, beta_nodes, &mut condition_groups),
                All(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::All, beta_nodes, &mut condition_groups),
                NotAll(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::NotAll, beta_nodes, &mut condition_groups),
                _ => unreachable!("Should not find a test at the topmost level")
            };
            beta_graph.statement_root.insert(statement_id, statement_root);
        }
        condition_groups
    }

    fn insert_beta_group<T: Fact>(beta_graph: &mut BetaGraph<T>,
                                            id_generator: &mut IdGenerator,
                                            rule_id: RuleId,
                                            statement_id: StatementId,
                                            condition_group_type: ConditionGroupType,
                                            beta_nodes: &[Stage1Node<T>],
                                            condition_groups: &mut HashMap<ConditionGroupId, ConditionGroupType>) -> ConditionGroupChild {
        use self::Stage1Node::*;
        let mut children: Vec<ConditionGroupChild> = beta_nodes.iter()
            .map(|beta_node| Self::insert_beta_child(beta_graph, id_generator, rule_id, statement_id, beta_node, condition_groups))
            .collect();
        children.sort();
        let parent_group_id = {
            if !beta_graph.parent_child_rel.contains_right(&children) {
                let new_group_id = id_generator.condition_group_ids.next();
                beta_graph.parent_child_rel.insert(new_group_id, children.clone());
                new_group_id
            } else {
                *beta_graph.parent_child_rel.get_by_right(&children).unwrap()
            }
        };
        for child in children {
            beta_graph.child_group_rel.insert(child, parent_group_id);
        }
        condition_groups.insert(parent_group_id, condition_group_type);
        let child_id = ConditionGroupChild::Group(parent_group_id);
        beta_graph.rule_rel.entry(child_id).or_insert_with(|| Default::default()).push(rule_id);
        child_id
    }

    fn insert_beta_child<T: Fact>(beta_graph: &mut BetaGraph<T>,
                                            id_generator: &mut IdGenerator,
                                            rule_id: RuleId,
                                            statement_id: StatementId,
                                            beta_node: &Stage1Node<T>,
                                            condition_groups: &mut HashMap<ConditionGroupId, ConditionGroupType>) -> ConditionGroupChild {
        use self::Stage1Node::*;
        match beta_node {
            Any(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::Any, beta_nodes, condition_groups),
            NotAny(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::NotAny, beta_nodes, condition_groups),
            All(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::All, beta_nodes, condition_groups),
            NotAll(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::NotAll, beta_nodes, condition_groups),
            Test(ref node) => {
                if !beta_graph.test_nodes.contains_right(node) {
                    let new_condition_id = id_generator.condition_ids.next();
                    beta_graph.test_nodes.insert(new_condition_id, node.clone());
                    ConditionGroupChild::Condition(new_condition_id)
                } else {
                    ConditionGroupChild::Condition(*beta_graph.test_nodes.get_by_right(node).unwrap())
                }
            }
        }
    }
}

impl BaseBuilder for ArrayBaseBuilder {
    type RB = ArrayRuleBuilder;
    type KB = ArrayKnowledgeBase;

    fn rule<S: AsRef<str>>(mut self, name: S) -> Self::RB {
        self.rule_with_agenda(name, "MAIN")
    }

    fn rule_with_agenda<S: AsRef<str>, A: AsRef<str>>(mut self, name: S, agenda_group: A) -> Self::RB {
        let id = self.id_generator.rule_ids.next();
        let name_symbol = self.cache.get_or_intern(name.as_ref());
        let agenda_symbol = self.cache.get_or_intern(agenda_group.as_ref());
        let root_group_id = self.id_generator.statement_group_ids.next();
        let root_group = StatementGroup::all(root_group_id);

        let mut statement_groups: BTreeMap<StatementGroupId, StatementGroup> = Default::default();
        statement_groups.insert(root_group_id, root_group);

        ArrayRuleBuilder {
            rule_data : ArrayRuleData {
                id,
                name: name_symbol,
                salience: 0,
                no_loop: false,
                agenda_group: agenda_symbol,
                current_group: root_group_id,
                statement_groups,
                statement_data: Default::default(),
            },
            base_builder: self
        }
    }

    fn end(self) -> Self::KB {
        unimplemented!()
    }
}


pub struct ArrayRuleBuilder {
    rule_data: ArrayRuleData,
    base_builder: ArrayBaseBuilder,
}

impl ArrayRuleBuilder {
    fn add_new_statement<T: Fact, S: AsRef<str>, N: Stage1Compile<T>>(&mut self, provides: &[ProvidesNode<S, S>], nodes: &[N]) -> Result<(StatementId, Box<StatementDetails>), CompileError> {
        let rule_id = self.rule_data.id;
        let statement_id = self.base_builder.id_generator.statement_ids.next();

        // Retrieve the upfront declarations
        // TODO - is there a way to do this in one line?
        let provides_result: Result<HashSet<ProvidesNode<SymbolId, Getter<T>>>, CompileError>
        = provides.iter()
            .map(|d| d.compile(&mut self.base_builder.cache))
            .collect();
        let provided = provides_result?;

        // TODO: Move into TryFrom once stable
        if provided.iter()
            .filter(|p| p.is_variable())
            .count() > 1 {
            let rule = self.base_builder.cache.resolve(self.rule_data.name).unwrap();
            return Err(CompileError::MultipleVariables {rule: rule.to_owned(), statement_id});
        }

        let mut statement_provides: StatementProvides<T> = Default::default();

        for node in provided {
            match node {
                ProvidesNode::Var(s) => {statement_provides.var = Some(s);},
                ProvidesNode::Field(s, g) => {statement_provides.fields.insert(s, g);},
            }
        }

        let mut beta_nodes = Stage1Node::All(Stage1Compile::stage1_compile_slice(nodes, &mut self.base_builder.cache)?).clean();
        let  alpha_nodes = beta_nodes.collect_alpha();
        self.base_builder.insert_alpha(statement_id, alpha_nodes);

        // For speed purposes, requires & condition_groups should be done in the same step?
        let mut statement_requires = Default::default();
        beta_nodes.collect_required(&mut statement_requires);

        let condition_groups =
            self.base_builder.insert_beta(self.rule_data.agenda_group, rule_id, statement_id, beta_nodes);

        let statement_details = Box::new(StatementData {
            statement_provides,
            statement_requires,
            condition_groups,
        });

        Ok((statement_id, statement_details))
    }

    fn add_new_group(&mut self, new_group: StatementGroup) {

        let parent_id = new_group.parent();
        let mut has_group_id = None;
        {
            let parent_group = self.rule_data.statement_groups.get_mut(&parent_id).unwrap();
            if !parent_group.should_merge(&new_group) {
                let new_group_id = self.base_builder.id_generator.statement_group_ids.next();
                parent_group.push(StatementGroupEntry::Child(new_group_id));
                has_group_id = Some(new_group_id);
            }
        }
        if let Some(new_group_id) = has_group_id {
            self.rule_data.statement_groups.insert(new_group_id, new_group);
            self.rule_data.current_group = new_group_id;
        };

    }

    // ALL - there may only be a single source of a particular ID
    // ANY - all must supply the same IDs
    fn test(&mut self) {

    }
}

impl RuleBuilder for ArrayRuleBuilder {
    type CB = ArrayConsequenceBuilder;

    fn salience(mut self, salience: i32) -> Self {
        self.rule_data.salience = salience;
        self
    }

    fn no_loop(mut self, no_loop: bool) -> Self {
        self.rule_data.no_loop = no_loop;
        self
    }

    fn when<T: Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Result<Self, CompileError> {
        self.provides_when::<T, &'static str, N>(&[], nodes)
    }

    fn provides_when<T: Fact, S: AsRef<str>, N: Stage1Compile<T>>(mut self, provides: &[ProvidesNode<S, S>], nodes: &[N]) -> Result<Self, CompileError> {
        let (statement_id, statement_details) = self.add_new_statement(provides, nodes)?;
        let statement_group = self.rule_data.current_group;
        self.rule_data.statement_groups.get_mut(&statement_group)
            .unwrap()
            .push(StatementGroupEntry::Statement(statement_id));

        self.rule_data.statement_data.insert(statement_id, statement_details);
        Ok(self)
    }

    fn when_exists<T: 'static + Fact, N: Stage1Compile<T>>(mut self, nodes: &[N]) -> Result<Self, CompileError> {
        let (statement_id, statement_details) = self.add_new_statement::<T, &'static str, N>(&[], nodes)?;
        let statement_group = self.rule_data.current_group;
        self.rule_data.statement_groups.get_mut(&statement_group)
            .unwrap()
            .push(StatementGroupEntry::Exists(statement_id));

        self.rule_data.statement_data.insert(statement_id, statement_details);
        Ok(self)
    }

    fn when_absent<T: 'static + Fact, N: Stage1Compile<T>>(mut self, nodes: &[N]) -> Result<Self, CompileError> {
        let (statement_id, statement_details) = self.add_new_statement::<T, &'static str, N>(&[], nodes)?;
        let statement_group = self.rule_data.current_group;
        self.rule_data.statement_groups.get_mut(&statement_group)
            .unwrap()
            .push(StatementGroupEntry::Absent(statement_id));

        self.rule_data.statement_data.insert(statement_id, statement_details);
        Ok(self)
    }

    fn when_for_all<T: Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Result<Self, CompileError> {
        self.provides_when_for_all::<T, &'static str, N>(&[], nodes)
    }

    fn provides_when_for_all<T: Fact, S: AsRef<str>, N: Stage1Compile<T>>(mut self, provides: &[ProvidesNode<S, S>], nodes: &[N]) -> Result<Self, CompileError> {
        let (statement_id, statement_details) = self.add_new_statement(provides, nodes)?;
        self.rule_data.statement_data.insert(statement_id, statement_details);
        let parent_group = self.rule_data.current_group;
        self.add_new_group(StatementGroup::for_all(parent_group, statement_id));
        Ok(self)
    }

    fn all_group(mut self) -> Self {
        let parent_group = self.rule_data.current_group;
        self.add_new_group(StatementGroup::all(parent_group));
        self
    }

    fn any_group(mut self) -> Self {
        let parent_group = self.rule_data.current_group;
        self.add_new_group(StatementGroup::any(parent_group));
        self
    }

    fn end_group(mut self) -> Result<Self, CompileError> {
        let current_group_id = self.rule_data.current_group;
        let parent_id = self.rule_data.statement_groups.get(&current_group_id).unwrap().parent();
        if current_group_id != parent_id {
            // TODO - optimize this somehow
            let current_group = self.rule_data.statement_groups.remove(&current_group_id).unwrap();
            if current_group.can_single_entry_optimize() {
                let entry = current_group.extract_single_entry();
                self.rule_data.statement_groups.get_mut(&parent_id).unwrap().push(entry);
            } else {
                self.rule_data.statement_groups.insert(current_group_id, current_group);
            }
            self.rule_data.current_group = parent_id;
        }
        Ok(self)
    }

    fn then(mut self) -> Result<Self::CB, CompileError> {
        // TODO: Validate statement groups & requirements
        // TODO: How do we want to handle consequences?

        // The grouping algorithm allows some pretty bad combinations at this stage
        // AND(NOT(AND(NOT(
        // A state machine may make more sense, but I've spent too much time trying to logic my way out of this
        // https://youtu.be/x4E5hzC8Xvs?list=PL6EC7B047181AD013&t=536
        loop {
            let mut current_group_id = self.rule_data.current_group;
            let mut parent_id = self.rule_data.statement_groups.get(&current_group_id).unwrap().parent();
            if current_group_id == parent_id {
                break;
            }
            self = self.end_group()?;
        }
        Ok(ArrayConsequenceBuilder{
            rule_data: self.rule_data,
            consequence_data: ArrayConsequenceData {},
            base_builder: self.base_builder
        })
    }
}



struct ArrayConsequenceData {

}


pub struct ArrayConsequenceBuilder {
    rule_data: ArrayRuleData,
    consequence_data: ArrayConsequenceData,
    base_builder: ArrayBaseBuilder,
}

impl ConsequenceBuilder for ArrayConsequenceBuilder {
    type BB = ArrayBaseBuilder;

    fn end(self) -> Result<Self::BB, CompileError> {
        Ok(self.base_builder)
    }
}
