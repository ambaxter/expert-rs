use std::hash::Hash;
use std::marker::PhantomData;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;
use itertools::Itertools;
use string_interner;
use string_interner::DefaultStringInterner;
use std::collections::{HashMap, HashSet};
use expert::serial::SerialGen;
use expert::introspection::ReteIntrospection;
use expert::builder::{AlphaTest, ConditionInfo, Rule, RuleId, StatementId, KnowledgeBuilder};

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct HashEqId{id: usize}

impl Debug for HashEqId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<HashEqId> for usize {
    fn into(self) -> HashEqId {
        HashEqId{id: self}
    }
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct AlphaId {id: usize}

impl Debug for AlphaId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<AlphaId> for usize {
    fn into(self) -> AlphaId {
        AlphaId {id: self}
    }
}


#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct BetaId {id: usize}

impl Debug for BetaId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<BetaId> for usize {
    fn into(self) -> BetaId {
        BetaId {id: self}
    }
}

pub struct LayoutIdGenerator {
    hash_eq_ids: SerialGen<usize, HashEqId>,
    alpha_ids: SerialGen<usize, AlphaId>,
    beta_ids: SerialGen<usize, BetaId>
}

impl LayoutIdGenerator {
    pub fn new() -> LayoutIdGenerator {
        LayoutIdGenerator{
            hash_eq_ids: Default::default(),
            alpha_ids: Default::default(),
            beta_ids: Default::default()
        }
    }

    pub fn next_hash_eq_id(&mut self) -> HashEqId {
        self.hash_eq_ids.next()
    }

    pub fn next_alpha_id(&mut self) -> AlphaId {
        self.alpha_ids.next()
    }

    pub fn next_beta_id(&mut self) -> BetaId {
        self.beta_ids.next()
    }
}

impl Default for LayoutIdGenerator {
    fn default() -> Self {
        LayoutIdGenerator::new()
    }
}

pub struct KnowledgeBase<T: ReteIntrospection> {
    t: PhantomData<T>
}

impl<T: ReteIntrospection> KnowledgeBase<T> {

    pub fn compile(builder: KnowledgeBuilder<T>) -> KnowledgeBase<T> {
        let (string_repo, rules, condition_map) = builder.explode();


        let (hash_eq_nodes, alpha_network, statement_memories) = Self::compile_alpha_network(condition_map);

        let mut statement_rule_map = HashMap::new();
        for (rule_id, rule) in rules {
            for statement_id in &rule.statement_ids {
                statement_rule_map.insert(*statement_id, rule_id);
            }
        }

        KnowledgeBase{t: PhantomData}
    }

    fn compile_alpha_network(condition_map: HashMap<T::HashEq, HashMap<AlphaTest<T>, ConditionInfo>>)
                             -> (HashMap<HashEqId, (T::HashEq, HashEqNode)>, Vec<AlphaNode<T>>, HashMap<StatementId, MemoryId>) {
        let mut conditions: Vec<_> = condition_map.into_iter().collect();
        // Order conditions ascending by dependent statement count, then test count.
        conditions.sort_by(|&(_, ref tests1), &(_, ref tests2)| {
            if let (Some(ref hash1), Some(ref hash2)) = (tests1.get(&AlphaTest::HashEq), tests2.get(&AlphaTest::HashEq)) {
                hash1.dependents.len().cmp(&hash2.dependents.len()).then(tests1.len().cmp(&tests2.len()))
            } else {
                unreachable!("Unexpected comparison. HashEq must be set");
            }
        });

        let mut node_id_gen = LayoutIdGenerator::new();

        let mut hash_eq_nodes = HashMap::new();

        let mut statement_memories: HashMap<StatementId, MemoryId> = HashMap::new();

        let mut alpha_network = Vec::new();

        // Pop off the most shared & complex tests first and lay them out at the front of the network.
        // That way they're more likely to be right next to each other
        while let Some((hash_val, mut test_map)) = conditions.pop() {

            let mut layout_map = HashMap::new();

            // Take the HashEq node (our entry point) and exhaustively assign destination nodes until no more statements are shared.
            let mut hash_eq_info = test_map.remove(&AlphaTest::HashEq).unwrap();
            let hash_eq_id = node_id_gen.next_hash_eq_id();
            let mut hash_eq_destinations: Vec<DestinationNode> = Vec::new();

            // Lay down the node for the most shared nodes before the others
            while let Some((max_info, max_intersection)) = test_map.iter()
                .map(|(_, info)| info)
                .map(|info| (info, &hash_eq_info.dependents & &info.dependents))
                .filter(|&(_, ref intersection)| !intersection.is_empty())
                .max_by_key(|&(_, ref intersection)| intersection.len()) {

                let destination_id = layout_map.entry(max_info.id)
                        .or_insert_with(|| NodeLayout{node_id: node_id_gen.next_alpha_id(), destinations: Default::default()})
                        .node_id;

                hash_eq_info.dependents.retain(|x| !max_intersection.contains(&x));
                hash_eq_destinations.push(destination_id.into());
            }

            // Add the HashEq node to the map && store any remaining statements for the beta network
            hash_eq_nodes.insert(hash_eq_id, (hash_val, HashEqNode{id: hash_eq_id, store: !hash_eq_info.dependents.is_empty(), destinations: hash_eq_destinations}));

            for statment_id in hash_eq_info.dependents {
                statement_memories.insert(statment_id, hash_eq_id.into());
            }

            let mut tests: Vec<_> = test_map.into_iter().collect();

            loop {
                // Sort the remaining tests by layed-out vs not.
                // TODO: sort by dependents.size, too. put that at the front
                tests.sort_by_key(|&(_, ref info)| !layout_map.contains_key(&info.id));
                println!("Layout: {:?}", layout_map);
                println!("Sorted: {:?}", tests);

                // Again, in order of most shared to least, lay down nodes
                // TODO: when closure is cloneable, fix this to use cartisian product
                let output = tests.iter().enumerate().tuple_combinations()
                    .filter(|&((_, &(_, ref info1)), (_, &(_, ref info2)))| !info1.dependents.is_empty() && layout_map.contains_key(&info1.id) && !layout_map.contains_key(&info2.id))
                    .map(|((pos1, &(_, ref info1)), (_, &(_, ref info2)))| (pos1, info1.id, info2.id, &info1.dependents & &info2.dependents))
                    .filter(|&(_, _, _, ref shared)| !shared.is_empty())
                    .max_by_key(|&(_, _, _, ref shared)| shared.len());

                if let Some((pos1, id1, id2, shared)) = output {
                    let alpha2_id = layout_map.entry(id2)
                        .or_insert_with(|| NodeLayout{node_id: node_id_gen.next_alpha_id(), destinations: Default::default()})
                        .node_id;
                    layout_map.get_mut(&id1).unwrap().destinations.push(alpha2_id.into());
                    tests.get_mut(pos1).unwrap().1.dependents.retain(|x| !shared.contains(&x));
                } else {
                    break;
                }
            }
            println!("Final layout: {:?}", &layout_map);
            // TODO: Assert layout numbers are correct
            // Do the actual layout into the alpha network
            tests.sort_by_key(|&(_, ref info)| layout_map.get(&info.id).unwrap().node_id);
            for (test, info) in tests.into_iter() {
                let alpha_layout = layout_map.remove(&info.id).unwrap();
                let id = alpha_layout.node_id;
                let dest = alpha_layout.destinations;
                let store = !info.dependents.is_empty();
                assert_eq!(alpha_network.len(), alpha_layout.node_id.id);
                alpha_network.push(AlphaNode{id, test, store, dest});

                for statment_id in info.dependents {
                    statement_memories.insert(statment_id, id.into());
                }
            }

        }
        println!("Conditions: {:?}", &conditions);
        println!("HashEqNode: {:?}", &hash_eq_nodes);
        println!("Memory map: {:?}", &statement_memories);
        println!("Alpha Network: size {:?}", alpha_network.len());
        (hash_eq_nodes, alpha_network, statement_memories)
    }

    fn compile_beta_network(statement_memories: &HashMap<StatementId, MemoryId>,
                            statement_rule_map: &HashMap<StatementId, RuleId>,
                            mut hash_eq_nodes: HashMap<HashEqId, (T::HashEq, HashEqNode)>,
                            mut alpha_network: Vec<AlphaNode<T>>) {
        let mut beta_ids: SerialGen<usize, BetaId> = Default::default();

        let mut memory_rule_map: HashMap<MemoryId, HashSet<RuleId>> = HashMap::new();

        for (statement_id, memory_id) in statement_memories {
            let rule_id = *statement_rule_map.get(statement_id).unwrap();
            memory_rule_map
                .entry(*memory_id)
                .or_insert_with(|| Default::default()).insert(rule_id);
        }

        let mut beta_network= Vec::new();

        let mut beta_stack = Vec::new();


        // I think I can keep this in the map
        // 1. Select the memory (m1) with the most rules
        // 2. Select the next memory (m2) with the most shared rules
        // 3a. Create a new AND beta node (b1) (in NodeLayout<BetaId>)
        // 3b. Remove shared rules from m1 & m2. If either have no more rules, remove from map.
        // 3c. Add b1's destination id to m1 and m2's destinations
        // 3d. Add b1 to beta stack.
        // 4. If an m2 can be found, go to 3a. Otherwise add rule to destination. pop b1 off beta stack
        // 5. If stack empty, select next m2 for m1. if no m2, add rule ids as destination nodes. if no more m1 rules, remove from map

    }

}
#[derive(Debug, Eq, Hash, Ord, PartialOrd, PartialEq)]
struct NodeLayout<T> {
    node_id: T,
    destinations: Vec<DestinationNode>
}

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum DestinationNode {
    Alpha(AlphaId),
    Beta(BetaId),
    Rule(RuleId)
}

impl Into<DestinationNode> for AlphaId {
    fn into(self) -> DestinationNode {
        DestinationNode::Alpha(self)
    }
}

impl Into<DestinationNode> for BetaId {
    fn into(self) -> DestinationNode {
        DestinationNode::Beta(self)
    }
}

impl Into<DestinationNode> for RuleId {
    fn into(self) -> DestinationNode {
        DestinationNode::Rule(self)
    }

}

#[derive(Debug)]
pub struct HashEqNode {
    id: HashEqId,
    store: bool,
    destinations: Vec<DestinationNode>
}

pub struct AlphaNode<T: ReteIntrospection> {
    id: AlphaId,
    test: AlphaTest<T>,
    store: bool,
    dest: Vec<DestinationNode>
}

pub trait AlphaMemoryId {}

#[derive(Debug, Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum MemoryId {
    HashEq(HashEqId),
    Alpha(AlphaId),
    Beta(BetaId)
}

impl Into<MemoryId> for HashEqId {
    fn into(self) -> MemoryId {
        MemoryId::HashEq(self)
    }
}

impl AlphaMemoryId for HashEqId {}

impl Into<MemoryId> for AlphaId {
    fn into(self) -> MemoryId {
        MemoryId::Alpha(self)
    }
}

impl AlphaMemoryId for AlphaId {}

impl Into<MemoryId> for BetaId {
    fn into(self) -> MemoryId {
        MemoryId::Beta(self)
    }
}

pub struct AlphaMemory<T: ReteIntrospection>  {
    mem: HashMap<MemoryId, HashSet<Rc<T>>>,
}

impl<T: ReteIntrospection> AlphaMemory<T> {
    pub fn insert<I: Into<MemoryId> + AlphaMemoryId>(&mut self, id: I, val: Rc<T>) {
        let mem_id = id.into();
        self.mem.entry(mem_id)
            .or_insert_with(Default::default)
            .insert(val);
    }
}

pub struct AlphaNetwork<T: ReteIntrospection> {
    hash_eq_node: HashMap<T::HashEq, HashEqNode>,
    alpha_network: Vec<AlphaNode<T>>
}

pub struct FactStore<T: ReteIntrospection> {
    store: HashSet<Rc<T>>
}

impl<T: ReteIntrospection> FactStore<T> {
    pub fn insert(&mut self, val: T) -> Rc<T> {
        let rc = Rc::new(val);
        if !self.store.insert(rc.clone()) {
            self.store.get(&rc).unwrap().clone()
        } else {
            rc
        }
    }
}

pub enum BetaNodeType {
    And(MemoryId, MemoryId)
}

pub struct BetaNode {
    id: BetaId,
    b_type: BetaNodeType,
    destinations: Vec<DestinationNode>
}

pub struct BetaNetwork {
    b_network: Vec<BetaNode>
}

pub struct BetaMemory {
    tripwire: Vec<bool>,
}
