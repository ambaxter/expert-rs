use std::hash::Hash;
use std::marker::PhantomData;
use std::fmt;
use std::fmt::Debug;
use std::rc::Rc;

use string_interner;
use string_interner::DefaultStringInterner;
use std::collections::{HashMap, HashSet};
use expert::serial::SerialGen;
use expert::introspection::ReteIntrospection;
use expert::builder::{ConditionTest, KnowledgeBuilder};
use expert::builder::RuleId;

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

pub trait AlphaMemoryId {}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
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

pub struct Memory<T: ReteIntrospection>  {
    mem: HashMap<MemoryId, HashSet<Rc<T>>>,
}

impl<T: ReteIntrospection> Memory<T> {
    pub fn insert<I: Into<MemoryId> + AlphaMemoryId>(&mut self, id: I, val: Rc<T>) {
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

pub mod new {
    use expert::introspection::ReteIntrospection;
    use ordered_float::NotNaN;

    #[repr(u8)]
    #[derive(Clone, Hash, Eq, PartialEq)]
    pub enum ConditionLimits<T: Eq + Ord + Clone> {
        S(T),
        D(T, T)
    }

    #[derive(Clone)]
    pub enum ConditionType<T: ReteIntrospection>{
        I8(fn(&T) -> &i8, ConditionLimits<i8>),
        I16(fn(&T) -> &i16, ConditionLimits<i16>),
        I32(fn(&T) -> &i32, ConditionLimits<i32>),
        I64(fn(&T) -> &i64, ConditionLimits<i64>),
        U8(fn(&T) -> &u8, ConditionLimits<u8>),
        U16(fn(&T) -> &u16, ConditionLimits<u16>),
        U32(fn(&T) -> &u32, ConditionLimits<u32>),
        U64(fn(&T) -> &u64, ConditionLimits<u64>),
        ISIZE(fn(&T) -> &isize, ConditionLimits<isize>),
        USIZE(fn(&T) -> &usize, ConditionLimits<usize>),
        F32(fn(&T) -> &f32, ConditionLimits<NotNaN<f32>>),
        F64(fn(&T) -> &f64, ConditionLimits<NotNaN<f64>>),
        //STR(fn(&T) -> &String, ConditionLimits<String>),
    }


    #[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
    pub enum ConditionTest {
        HashEq, // TODO: Move one layer up - HashEq vs Ordinal
        Lt{closed: bool},
        Gt{closed: bool},
        Btwn{from_closed: bool, to_closed: bool}
    }

    impl ConditionTest {
        fn test<T: Eq + Ord + Clone>(&self, val: &T, limits: &ConditionLimits<T>) -> bool {
            use self::ConditionTest::*;
            use self::ConditionLimits::*;
            match (self, limits) {
                (&HashEq, _) => true,
                (&Lt{closed}, &S(ref to)) => {
                    if closed {
                        val <= to
                    } else {
                        val < to
                    }
                },
                (&Gt{closed}, &S(ref from)) => {
                    if closed {
                        val >= from
                    } else {
                        val > from
                    }
                },
                (&Btwn{from_closed, to_closed}, &D(ref from, ref to)) => {
                    match (from_closed, to_closed) {
                        (false, false) => {
                            val > from && val < to
                        },
                        (false, true) => {
                            val > from && val <= to
                        }
                        (true, false) => {
                            val >= from && val < to
                        },
                        (true, true) => {
                            val >= from && val <= to
                        }
                    }
                },
                _ => unreachable!("Unexpected condition test combination.")
            }
        }
    }

    pub fn do_test<T:ReteIntrospection>() -> bool {
        use std::mem;
        let test = ConditionTest::Gt{closed: true};
        println!("Type: {:?}", mem::size_of::<ConditionType<T>>());
        println!("Test: {:?}", mem::size_of::<ConditionTest>());
        println!("String: {:?}", mem::size_of::<String>());
        let s = "20170910".to_owned();
        let limits = ConditionLimits::S("20171010".to_owned());
        test.test(&s, &limits)
    }
}
