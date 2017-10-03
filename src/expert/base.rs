use std::hash::Hash;
use std::marker::PhantomData;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

use string_interner;
use string_interner::DefaultStringInterner;
use std::collections::{HashMap, HashSet};
use expert::node::ExhaustiveAlphaNode;
use expert::serial::SerialGen;
use expert::introspection::ReteIntrospection;
use expert::builder::{ConditionTest, KnowledgeBuilder};

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

        KnowledgeBase{t: PhantomData}
    }

}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum DestinationNode {
    Alpha(AlphaId),
    Beta(BetaId),
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

pub struct HashEqNode {
    id: HashEqId,
    store: bool,
    destinations: Vec<DestinationNode>
}

pub struct AlphaNode<T: ReteIntrospection> {
    id: AlphaId,
    test: ConditionTest<T>,
    store: bool,
    dest: Vec<DestinationNode>
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub enum MemoryId {
    HashEq(HashEqId),
    Alpha(AlphaId)
}

impl Into<MemoryId> for HashEqId {
    fn into(self) -> MemoryId {
        MemoryId::HashEq(self)
    }
}

impl Into<MemoryId> for AlphaId {
    fn into(self) -> MemoryId {
        MemoryId::Alpha(self)
    }
}

pub struct Memory<T: ReteIntrospection>  {
    t: PhantomData<T>,
    mem: HashMap<MemoryId, HashSet<Rc<T>>>,
}

impl<T: ReteIntrospection> Memory<T> {
    pub fn insert<I: Into<MemoryId>>(&mut self, id: I, val: Rc<T>) {
        let mem_id = id.into();
        self.mem.entry(mem_id)
            .or_insert_with(Default::default)
            .insert(val);
    }
}

pub struct AlphaNetwork<T: ReteIntrospection> {
    e_map: HashMap<T::HashEq, HashEqNode>,
    a_network: Vec<AlphaNode<T>>
}