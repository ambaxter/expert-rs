use std::marker::PhantomData;
use crate::traits::ReteIntrospection;
use crate::base::KnowledgeBase;
use crate::runtime::memory::{SymbolId, StringCache};
use crate::builders::ids::*;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::fmt::Debug;

pub struct ReteIdGenerator {
    rule_ids: RuleIdGen,
    statement_ids: StatementIdGen,
    condition_ids: ConditionIdGen
}

impl ReteIdGenerator {
    pub fn new() -> ReteIdGenerator {
        ReteIdGenerator{
            rule_ids: Default::default(),
            statement_ids: Default::default(),
            condition_ids: Default::default()
        }
    }

    pub fn next_rule_id(&mut self) -> RuleId {
        self.rule_ids.next()
    }

    pub fn next_statement_id(&mut self) -> StatementId {
        self.statement_ids.next()
    }

    pub fn next_condition_id(&mut self) -> ConditionId {
        self.condition_ids.next()
    }
}

impl Default for ReteIdGenerator {
    fn default() -> Self {
        ReteIdGenerator::new()
    }
}

struct BuilderShared<T: ReteIntrospection> {
    string_repo: StringCache,
    id_generator: ReteIdGenerator,
    condition_map: HashMap<T::HashEq, HashMap<AlphaTest<T>, ConditionInfo>>
}

impl<T: ReteIntrospection> BuilderShared<T> {
    fn new() -> BuilderShared<T> {
        BuilderShared{
            string_repo: StringCache::new(),
            id_generator: Default::default(),
            condition_map: Default::default()
        }
    }
}

pub struct KnowledgeBuilder<T: ReteIntrospection> {
    dummy: PhantomData<T>,
    build_shared: BuilderShared<T>,
    rules: HashMap<RuleId, Rule>
}

impl<T: ReteIntrospection> Debug for KnowledgeBuilder<T>
    where T::HashEq: Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "KBuilder {{")?;
        writeln!(f, "  Rules: [")?;
        for rule in self.rules.values() {
            writeln!(f, "    {:?} - sids: {:?}", self.build_shared.string_repo.resolve(rule.name_sym).unwrap(), rule.statement_ids)?;
        }
        writeln!(f, "  ],")?;
        writeln!(f, "  Alpha Nodes: [")?;
        for (root_hash, dependents) in &self.build_shared.condition_map {
            let root_info = dependents.get(&AlphaTest::HashEq).unwrap();
            writeln!(f, "   -{:?} - sids: {:?}", root_hash, root_info.dependents)?;
            for (test, info) in dependents.iter().filter(|&(t, _)| !t.is_hash_eq()) {
                writeln!(f, "    +{:?}({:?}) - {:?} - sids: {:?}",
                         info.field_sym.and_then(|s| self.build_shared.string_repo.resolve(s)),
                         info.id,
                         test,
                         info.dependents
                )?;
            }
        }
        writeln!(f, "  ],")?;
        write!(f, "}}")
    }
}

impl<T: ReteIntrospection> KnowledgeBuilder<T> {
    pub fn new() -> KnowledgeBuilder<T> {
        KnowledgeBuilder {dummy: PhantomData, build_shared: BuilderShared::new(), rules: Default::default()}
    }

    pub fn rule<S: Into<String> + AsRef<str>>(self, name: S) -> RuleBuilder<T> {
        RuleBuilder::new(self, name)
    }

    pub fn compile(self) -> KnowledgeBase<T> {
        KnowledgeBase::compile(self)
    }

    pub(crate) fn explode(self) -> (StringCache, HashMap<RuleId, Rule>, HashMap<T::HashEq, HashMap<AlphaTest<T>, ConditionInfo>>) {
        (self.build_shared.string_repo, self.rules, self.build_shared.condition_map)
    }


    fn get_shared(&mut self) -> &mut BuilderShared<T> {
        &mut self.build_shared
    }
}

pub struct Rule {
    rule_id: RuleId,
    name_sym: SymbolId,
    pub(crate) statement_ids: HashSet<StatementId>
}

pub struct RuleBuilder<T: ReteIntrospection> {
    kbuilder: KnowledgeBuilder<T>,
    rule_id: RuleId,
    name_sym: SymbolId,
    statement_ids: HashSet<StatementId>
}

impl<T: ReteIntrospection> RuleBuilder<T> {
    fn new<S: Into<String> + AsRef<str>>(mut kbuilder: KnowledgeBuilder<T>, name: S) -> RuleBuilder<T> {
        let (rule_id, name_sym) = {
            (kbuilder.get_shared().id_generator.next_rule_id(), kbuilder.get_shared().string_repo.get_or_intern(name))
        };
        RuleBuilder{kbuilder, rule_id, name_sym, statement_ids: Default::default()}
    }

    pub fn when(self) -> StatementBuilder<T> {
        StatementBuilder::new(self)
    }

    pub fn end(self) -> KnowledgeBuilder<T> {
        // nop in terms of output for now
        let rule = Rule{rule_id: self.rule_id, name_sym: self.name_sym, statement_ids: self.statement_ids};
        let mut kbuilder = self.kbuilder;
        kbuilder.rules.insert(rule.rule_id, rule);
        kbuilder
    }

    fn get_shared(&mut self) -> &mut BuilderShared<T> {
        self.kbuilder.get_shared()
    }
}

pub struct StatementBuilder<T: ReteIntrospection> {
    rule_builder: RuleBuilder<T>,
    conditions: Vec<StatementCondition>
}

impl<T: ReteIntrospection> StatementBuilder<T> {
    fn new(mut rule_builder: RuleBuilder<T>) -> StatementBuilder<T> {
        StatementBuilder {rule_builder, conditions: Default::default()}
    }

    pub fn exists(mut self) -> RuleBuilder<T> {
        self.conditions.push(StatementCondition::Exists);
        self.rule_builder
    }

    pub fn eq<S: Into<String> + AsRef<str>>(mut self, field: S, to: u64) -> StatementBuilder<T> {
        let field_sym = self.rule_builder.get_shared().string_repo.get_or_intern(field);
        self.conditions.push(StatementCondition::Eq{field_sym, to});
        self
    }

    pub fn lt<S: Into<String> + AsRef<str>>(mut self, field: S, to: u64, closed: bool) -> StatementBuilder<T> {
        let field_sym = self.rule_builder.get_shared().string_repo.get_or_intern(field);
        self.conditions.push(StatementCondition::Lt{field_sym, to, closed});
        self
    }

    pub fn gt<S: Into<String> + AsRef<str>>(mut self, field: S, from: u64, closed: bool) -> StatementBuilder<T> {
        let field_sym = self.rule_builder.get_shared().string_repo.get_or_intern(field);
        self.conditions.push(StatementCondition::Gt{field_sym, from, closed});
        self
    }

    pub fn btwn<S: Into<String> + AsRef<str>>(mut self, field: S, from: u64, from_closed: bool, to: u64, to_closed: bool) -> StatementBuilder<T> {
        let field_sym = self.rule_builder.get_shared().string_repo.get_or_intern(field);
        self.conditions.push(StatementCondition::Btwn{field_sym, from, from_closed, to, to_closed});
        self
    }

    pub fn then(self) -> RuleBuilder<T> {
        self.collapse_builder()
    }

    fn collapse_builder(mut self) -> RuleBuilder<T> {
        let hash_eq = T::create_hash_eq(&self.conditions, &self.rule_builder.get_shared().string_repo);
        let (conditions, mut rule_builder) = (self.conditions, self.rule_builder);

        if !conditions.is_empty() {
            let statement_id = {
                //let rule_id = rule_builder.rule_id;
                let shared = rule_builder.get_shared();
                let (id_generator, string_repo, condition_map) = (&mut shared.id_generator, &shared.string_repo, &mut shared.condition_map);

                let statement_id = id_generator.next_statement_id();

                let entry_point = condition_map
                    .entry(hash_eq).or_insert_with(Default::default);

                for condition in conditions.into_iter().filter(|c| !c.is_hash_eq()) {
                    let field_sym = condition.field_sym();
                    let test = condition.convert(string_repo);
                    entry_point.entry(test).or_insert_with(|| ConditionInfo::new(id_generator.next_condition_id(), field_sym))
                        .dependents.insert(statement_id);
                }

                entry_point.entry(AlphaTest::HashEq).or_insert_with(|| ConditionInfo::new(id_generator.next_condition_id(), None))
                    .dependents.insert(statement_id);

                statement_id
            };
            rule_builder.statement_ids.insert(statement_id);
        }
        rule_builder
    }
}

#[derive(Debug, Copy, Clone)]
pub enum StatementCondition {
    Exists,
    Eq{field_sym: SymbolId, to: u64},
    Lt{field_sym: SymbolId, to: u64, closed: bool},
    Gt{field_sym: SymbolId, from: u64, closed: bool},
    Btwn{field_sym: SymbolId, from:u64, from_closed: bool, to: u64, to_closed: bool}
}

impl StatementCondition {
    fn is_hash_eq(&self) -> bool {
        use self::StatementCondition::*;

        match self {
            &Exists | &Eq{..} => true,
            _ => false
        }
    }

    fn convert<T: ReteIntrospection>(self, string_repo: &StringCache) -> AlphaTest<T> {
        use self::StatementCondition::*;
        use self::CLimits::*;
        match self {
            Lt{field_sym, to, closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                let test = if closed {
                    OrdTest::Le
                } else {
                    OrdTest::Lt
                };
                AlphaTest::Ord{data: CData::U64(accessor, S(to)), test}
            }
            Gt{field_sym, from, closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                let test = if closed {
                    OrdTest::Ge
                } else {
                    OrdTest::Gt
                };
                AlphaTest::Ord{data: CData::U64(accessor, S(from)), test}
            }
            Btwn{field_sym, from, from_closed, to, to_closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                let test = match (from_closed, to_closed) {
                    (false, false) => OrdTest::GtLt,
                    (false, true) => OrdTest::GtLe,
                    (true, false) => OrdTest::GeLt,
                    (true, true) => OrdTest::GeLe,

                };
                AlphaTest::Ord{data: CData::U64(accessor, D(from, to)), test}
            },
            _ => AlphaTest::HashEq
        }
    }

    fn field_sym(&self) -> Option<SymbolId> {
        use self::StatementCondition::*;
        match self {
            &Lt{field_sym, ..} => Some(field_sym),
            &Gt{field_sym, ..} => Some(field_sym),
            &Btwn{field_sym, ..} => Some(field_sym),
            _ => None
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConditionInfo {
    pub(crate) id: ConditionId,
    pub(crate) field_sym: Option<SymbolId>,
    pub(crate) dependents: HashSet<StatementId>
}

impl ConditionInfo {
    fn new(id: ConditionId, field_sym: Option<SymbolId>) -> ConditionInfo {
        ConditionInfo{id, field_sym, dependents: Default::default()}
    }
}

#[derive(Hash, Eq, PartialEq)]
pub enum AlphaTest<T: ReteIntrospection> {
    HashEq,
    Ord{data: CData<T>, test: OrdTest }
}

impl<T: ReteIntrospection> AlphaTest<T> {
    pub fn is_hash_eq(&self) -> bool {
        use self::AlphaTest::*;
        match self {
            &HashEq => true,
            _ => false
        }
    }
}

impl<T: ReteIntrospection> Debug for AlphaTest<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::AlphaTest::*;
        write!(f, "Test{{")?;
        match self {
            &HashEq => {
                write!(f, "HashEq")?;
            },
            &Ord{ref data, test} => {
                write!(f, "Ord")?;
            }
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum CLimits<T: Hash + Eq + Ord + Clone> {
    S(T),
    D(T, T)
}

#[derive(Clone)]
pub enum CData<T: ReteIntrospection>{
    I8(fn(&T) -> &i8, CLimits<i8>),
    I16(fn(&T) -> &i16, CLimits<i16>),
    I32(fn(&T) -> &i32, CLimits<i32>),
    I64(fn(&T) -> &i64, CLimits<i64>),
    U8(fn(&T) -> &u8, CLimits<u8>),
    U16(fn(&T) -> &u16, CLimits<u16>),
    U32(fn(&T) -> &u32, CLimits<u32>),
    U64(fn(&T) -> &u64, CLimits<u64>),
    ISIZE(fn(&T) -> &isize, CLimits<isize>),
    USIZE(fn(&T) -> &usize, CLimits<usize>),
    //F32(fn(&T) -> &f32, ConditionLimits<NotNaN<f32>>),
    //F64(fn(&T) -> &f64, ConditionLimits<NotNaN<f64>>),
    //STR(fn(&T) -> &str, ConditionLimits<SymbolId>),
}

impl<T: ReteIntrospection> CData<T> {
    fn hash_self<H: Hasher, L: Hash>(ord: usize, accessor: usize, limits: &L, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        limits.hash(state);
    }
}

impl<T: ReteIntrospection> Hash for CData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::CData::*;
        match self {
            &I8(accessor, ref limits) => {
                Self::hash_self(0, accessor as usize, limits, state);
            },
            &I16(accessor, ref limits) => {
                Self::hash_self(1, accessor as usize, limits, state);
            },
            &I32(accessor, ref limits) => {
                Self::hash_self(2, accessor as usize, limits, state);

            },
            &I64(accessor, ref limits) => {
                Self::hash_self(3, accessor as usize, limits, state);
            },
            &U8(accessor, ref limits) => {
                Self::hash_self(4, accessor as usize, limits, state);
            },
            &U16(accessor, ref limits) => {
                Self::hash_self(5, accessor as usize, limits, state);
            },
            &U32(accessor, ref limits) => {
                Self::hash_self(6, accessor as usize, limits, state);
            },
            &U64(accessor, ref limits) => {
                Self::hash_self(7, accessor as usize, limits, state);
            },
            &ISIZE(accessor, ref limits) => {
                Self::hash_self(8, accessor as usize, limits, state);
            },
            &USIZE(accessor, ref limits) => {
                Self::hash_self(9, accessor as usize, limits, state);
            },
/*            &F32(accessor, ref limits) => {
                Self::hash_self(10, accessor as usize, limits, state);
            },
            &F64(accessor, ref limits) => {
                Self::hash_self(11, accessor as usize, limits, state);
            },*/
            _ => {}
        }
    }
}

impl<T: ReteIntrospection> PartialEq for CData<T> {
    fn eq(&self, other: &Self) -> bool {
        use self::CData::*;
        match (self, other) {
            (&I8(accessor1, ref limits1), &I8(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&I16(accessor1, ref limits1), &I16(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&I32(accessor1, ref limits1), &I32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&I64(accessor1, ref limits1), &I64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U8(accessor1, ref limits1), &U8(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U16(accessor1, ref limits1), &U16(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U32(accessor1, ref limits1), &U32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&U64(accessor1, ref limits1), &U64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&ISIZE(accessor1, ref limits1), &ISIZE(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&USIZE(accessor1, ref limits1), &USIZE(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
/*            (&F32(accessor1, ref limits1), &F32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&F64(accessor1, ref limits1), &F64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },*/
            _ => false
        }
    }
}

impl<T: ReteIntrospection> Eq for CData<T> {}

#[derive(Debug, Copy, Clone, Eq, Hash, PartialEq)]
pub enum OrdTest {
    Lt,
    Le,
    Gt,
    Ge,
    GtLt,
    GeLt,
    GtLe,
    GeLe
}


