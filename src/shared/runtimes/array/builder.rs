use shared::fact::Fact;
use shared::nodes::alpha::{IsHashEq, AlphaNode};
use shared::nodes::beta::{CollectRequired, BetaNode};
use shared::compiler::prelude::Stage1Node;
use runtime::memory::StringCache;
use shared::compiler::prelude::{DrainWhere, DeclareNode};
use shared::compiler::id_generator::{IdGenerator, StatementGroupId, StatementId, ConditionId, ConditionGroupId, RuleId};
use runtime::memory::SymbolId;
use std::collections::HashSet;
use std::collections::HashMap;
use errors::CompileError;
use std;
use std::collections::BTreeMap;
use shared::fact::Getter;
use shared::nodes::alpha::HashEqField;
use anymap::any::{IntoBox, Any, UncheckedAnyExt};
use anymap::Map;
use shared::compiler::builder::BaseBuilder;
use shared::compiler::builder::RuleBuilder;
use shared::compiler::prelude::Stage1Compile;
use shared::compiler::builder::ConsequenceBuilder;
use shared::compiler::builder::KnowledgeBase;
use bimap::BiMap;

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
    Child(StatementGroupId),
}

#[derive(Clone, Eq, PartialEq, Debug)]
enum StatementGroup {
    All(StatementGroupId, Vec<StatementGroupEntry>),
    Any(StatementGroupId, Vec<StatementGroupEntry>),
    Exists(StatementGroupId, Vec<StatementGroupEntry>),
    NotAll(StatementGroupId, Vec<StatementGroupEntry>),
    ForAll(StatementGroupId, StatementId, Vec<StatementGroupEntry>),
}

impl StatementGroup {
    fn all(parent: StatementGroupId) -> StatementGroup {
        StatementGroup::All(parent, Vec::new())
    }

    fn any(parent: StatementGroupId) -> StatementGroup {
        StatementGroup::Any(parent, Vec::new())
    }

    fn exists(parent: StatementGroupId) -> StatementGroup {
        StatementGroup::Exists(parent, Vec::new())
    }

    fn not_all(parent: StatementGroupId) -> StatementGroup {
        StatementGroup::NotAll(parent, Vec::new())
    }

    fn for_all(parent: StatementGroupId, statement: StatementId) -> StatementGroup {
        StatementGroup::ForAll(parent, statement, Vec::new())
    }

    fn parent(&self) -> StatementGroupId {
        match *self {
            StatementGroup::All(parent, _) => parent,
            StatementGroup::Any(parent, _) => parent,
            StatementGroup::Exists(parent, _) => parent,
            StatementGroup::NotAll(parent, _) => parent,
            StatementGroup::ForAll(parent, ..) => parent,
        }
    }

    fn push(&mut self, entry: StatementGroupEntry) {
        match *self {
            StatementGroup::All(_, ref mut entries) => entries.push(entry),
            StatementGroup::Any(_, ref mut entries) => entries.push(entry),
            StatementGroup::Exists(_, ref mut entries) => entries.push(entry),
            StatementGroup::NotAll(_, ref mut entries) => entries.push(entry),
            StatementGroup::ForAll(_, _, ref mut entries) => entries.push(entry),
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

#[derive(Clone, Eq, PartialEq, Debug)]
struct StatementData {
    provides: HashSet<SymbolId>, // HashSet or vec?
    requires: HashSet<SymbolId>,
    condition_groups: HashMap<ConditionGroupId, ConditionGroupType>
}

// TODO: After we build up the groupings & requirements, cascade down the groupings to ensure that we're not screwing anything up

struct ArrayRuleData {
    id: RuleId,
    name: SymbolId,
    salience: i32,
    no_loop: bool,
    agenda: SymbolId,
    current_group: StatementGroupId,
    statement_groups: BTreeMap<StatementGroupId, StatementGroup>,
    statement_data: BTreeMap<StatementId, StatementData>,
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
    test_nodes: BiMap<BetaNode<T>, ConditionId>
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
    beta_graph: BetaGraph<T>
}

impl<T:'static + Fact> IntoBox<NetworkBuilder + 'static> for ArrayNetworkBuilder<T> {
    fn into_box(self) -> Box<NetworkBuilder> {
        Box::new(self)
    }
}


impl<T: Fact> ArrayNetworkBuilder<T> {
    fn new() -> ArrayNetworkBuilder<T> {
        ArrayNetworkBuilder{ alpha_graph: Default::default(), beta_graph: Default::default()}
    }
}

impl<T:Fact> Default for ArrayNetworkBuilder<T> {
    fn default() -> Self {
        ArrayNetworkBuilder::new()
    }
}

impl<T: 'static + Fact> NetworkBuilder for ArrayNetworkBuilder<T> {

}

pub struct ArrayBaseBuilder {
    id_generator: IdGenerator,
    cache: StringCache,
    network_builders: Map<NetworkBuilder>
}

impl ArrayBaseBuilder {
    fn insert_alpha<T: 'static + Fact>(&mut self, statement_id: StatementId, nodes: Vec<AlphaNode<T>>) {
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

    fn insert_beta<T: 'static + Fact>(&mut self, rule_id: RuleId, statement_id: StatementId, beta_node: Stage1Node<T>) -> HashMap<ConditionGroupId, ConditionGroupType> {
        let mut condition_groups = Default::default();
        let(beta_graph, id_generator) =
            (
                &mut self.network_builders.entry::<ArrayNetworkBuilder<T>>().or_insert_with(|| Default::default())
                    .beta_graph,
                &mut self.id_generator
                );

        use self::Stage1Node::*;
        let statement_root = match beta_node {
            Any(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::Any, beta_nodes, &mut condition_groups),
            NotAny(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::NotAny, beta_nodes, &mut condition_groups),
            All(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::All, beta_nodes, &mut condition_groups),
            NotAll(ref beta_nodes) => Self::insert_beta_group(beta_graph, id_generator, rule_id, statement_id, ConditionGroupType::NotAll, beta_nodes, &mut condition_groups),
            _ => unreachable!("Should not find a test at the topmost level")
        };
        beta_graph.statement_root.insert(statement_id, statement_root);
        condition_groups
    }

    fn insert_beta_group<T: 'static + Fact>(beta_graph: &mut BetaGraph<T>,
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

    fn insert_beta_child<T: 'static + Fact>(beta_graph: &mut BetaGraph<T>,
                                            id_generator: &mut IdGenerator,
                                            rule_id: RuleId,
                                            statement_id: StatementId,
                                            beta_node: &Stage1Node<T>,
                                            condition_groups: &mut HashMap<ConditionGroupId, ConditionGroupType>) -> ConditionGroupChild {
        use self::Stage1Node::*;
        unimplemented!()
    }
}

impl BaseBuilder for ArrayBaseBuilder {
    type RB = ArrayRuleBuilder;
    type KB = ArrayKnowledgeBase;

    fn rule<S: AsRef<str>>(mut self, name: S) -> Self::RB {
        let id = self.id_generator.rule_ids.next();
        let name_symbol = self.cache.get_or_intern(name.as_ref());
        let agenda_symbol = self.cache.get_or_intern("MAIN");
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
                agenda: agenda_symbol,
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
    fn add_new_group(&mut self, new_group: StatementGroup) {
        let new_group_id = self.base_builder.id_generator.statement_group_ids.next();
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

    fn when<T: 'static + Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Result<Self, CompileError> {
        self.declare_when::<T, &'static str, N>(&[], nodes)
    }

    fn declare_when<T: 'static + Fact, S: AsRef<str>, N: Stage1Compile<T>>(mut self, declare: &[DeclareNode<S, S>], nodes: &[N]) -> Result<Self, CompileError> {
        let rule_id = self.rule_data.id;
        let statement_id = self.base_builder.id_generator.statement_ids.next();

        // Retrieve the upfront declarations
        // TODO - is there a way to do this in one line?
        let declares_result: Result<Vec<DeclareNode<SymbolId, Getter<T>>>, CompileError>
        = declare.iter()
            .map(|d| d.compile(&mut self.base_builder.cache))
            .collect();
        let declares = declares_result?;

        let mut beta_nodes = Stage1Node::All(Stage1Compile::stage1_compile_slice(nodes, &mut self.base_builder.cache)?).clean();
        let  alpha_nodes = beta_nodes.collect_alpha();
        self.base_builder.insert_alpha(statement_id, alpha_nodes);

        let mut required_symbols = Default::default();
        beta_nodes.collect_required(&mut required_symbols);

        let condition_groups = self.base_builder.insert_beta(rule_id, statement_id, beta_nodes);

        // TODO: Do prep the node for layout
        unimplemented!()
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

    fn exists_group(mut self) -> Self {
        let parent_group = self.rule_data.current_group;
        self.add_new_group(StatementGroup::exists(parent_group));
        self
    }

    fn not_group(mut self) -> Self {
        let parent_group = self.rule_data.current_group;
        self.add_new_group(StatementGroup::not_all(parent_group));
        self
    }

    fn for_all_group<T: 'static + Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Result<Self, CompileError> {
        self.declare_for_all_group::<T, &'static str, N>(&[], nodes)
    }

    fn declare_for_all_group<T: 'static + Fact, S: AsRef<str>, N: Stage1Compile<T>>(mut self, declare: &[DeclareNode<S, S>], nodes: &[N]) -> Result<Self, CompileError> {
        let statement_id = self.base_builder.id_generator.statement_ids.next();
        let node = Stage1Node::All(Stage1Compile::stage1_compile_slice(nodes, &mut self.base_builder.cache)?);
        // TODO: Do prep the node for layout
        let parent_group = self.rule_data.current_group;
        self.add_new_group(StatementGroup::for_all(parent_group, statement_id));
        Ok(self)
    }

    fn end_group(mut self) -> Result<Self, CompileError> {
        let parent_id = self.rule_data.statement_groups.get(&self.rule_data.current_group).unwrap().parent();
        if parent_id == self.rule_data.current_group {
            unreachable!("end_group at root {:?}", parent_id);
        }
        self.rule_data.current_group = parent_id;
        Ok(self)
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
