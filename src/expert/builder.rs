use string_interner;
use string_interner::DefaultStringInterner;
use expert::serial::SerialGen;
use std::marker::PhantomData;
use expert::introspection::ReteIntrospection;
use expert::base::KnowledgeBase;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::{HashMap, HashSet};
use ordered_float::NotNaN;
use std::rc::Rc;
use std::fmt;
use std::fmt::Debug;


#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct RuleId{id: usize}

impl Debug for RuleId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<RuleId> for usize {
    fn into(self) -> RuleId {
        RuleId{id: self}
    }
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct StatementId{id: usize}

impl Debug for StatementId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<StatementId> for usize {
    fn into(self) -> StatementId {
        StatementId{id: self}
    }
}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct ConditionId{id: usize}

impl Debug for ConditionId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl Into<ConditionId> for usize {
    fn into(self) -> ConditionId {
        ConditionId{id: self}
    }
}

pub struct ReteIdGenerator {
    rule_ids: SerialGen<usize, RuleId>,
    statement_ids: SerialGen<usize, StatementId>,
    condition_ids: SerialGen<usize, ConditionId>
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
    string_repo: DefaultStringInterner,
    id_generator: ReteIdGenerator,
    condition_map: HashMap<T::HashEq, HashMap<AlphaTest<T>, ConditionInfo>>
}

impl<T: ReteIntrospection> BuilderShared<T> {
    fn new() -> BuilderShared<T> {
        BuilderShared{
            string_repo: Default::default(),
            id_generator: Default::default(),
            condition_map: Default::default()
        }
    }
}

pub struct KnowledgeBuilder<T: ReteIntrospection> {
    dummy: PhantomData<T>,
    build_shared: BuilderShared<T>,
    rules: Vec<Rule>
}

impl<T: ReteIntrospection> Debug for KnowledgeBuilder<T>
    where T::HashEq: Debug {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "KBuilder {{");
        writeln!(f, "  Rules: [");
        for rule in &self.rules {
            writeln!(f, "    {:?} - sids: {:?}", self.build_shared.string_repo.resolve(rule.name_sym).unwrap(), rule.statement_ids);
        }
        writeln!(f, "  ],");
        writeln!(f, "  Alpha Nodes: [");
        for (root_hash, dependents) in &self.build_shared.condition_map {
            let root_info = dependents.get(&AlphaTest::HashEq).unwrap();
            writeln!(f, "   -{:?} - sids: {:?}", root_hash, root_info.dependents);
            for (test, info) in dependents.iter().filter(|&(t, _)| !t.is_hash_eq()) {
                writeln!(f, "    +{:?}({:?}) - {:?} - sids: {:?}",
                         info.field_sym.and_then(|s| self.build_shared.string_repo.resolve(s)),
                         info.id,
                         test,
                         info.dependents
                );
            }
        }
        writeln!(f, "  ],");
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

    pub(crate) fn explode(self) -> (DefaultStringInterner, Vec<Rule>, HashMap<T::HashEq, HashMap<AlphaTest<T>, ConditionInfo>>) {
        (self.build_shared.string_repo, self.rules, self.build_shared.condition_map)
    }


    fn get_shared(&mut self) -> &mut BuilderShared<T> {
        &mut self.build_shared
    }
}

pub struct Rule {
    rule_id: RuleId,
    name_sym: usize,
    statement_ids: HashSet<StatementId>
}

pub struct RuleBuilder<T: ReteIntrospection> {
    kbuilder: KnowledgeBuilder<T>,
    rule_id: RuleId,
    name_sym: usize,
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
        kbuilder.rules.push(rule);
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

    pub fn then(mut self) -> RuleBuilder<T> {
        self.collapse_builder()
    }

    fn collapse_builder(mut self) -> RuleBuilder<T> {
        let hash_eq = T::create_hash_eq(&self.conditions, &self.rule_builder.get_shared().string_repo);
        let (mut conditions, mut rule_builder) = (self.conditions, self.rule_builder);

        if !conditions.is_empty() {
            let statement_id = {
                let rule_id = rule_builder.rule_id;
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
    Eq{field_sym: usize, to: u64},
    Lt{field_sym: usize, to: u64, closed: bool},
    Gt{field_sym: usize, from: u64, closed: bool},
    Btwn{field_sym: usize, from:u64, from_closed: bool, to: u64, to_closed: bool}
}

impl StatementCondition {
    fn is_hash_eq(&self) -> bool {
        use self::StatementCondition::*;

        match self {
            &Exists | &Eq{..} => true,
            _ => false
        }
    }

    fn convert<T: ReteIntrospection>(self, string_repo: &DefaultStringInterner) -> AlphaTest<T> {
        use self::StatementCondition::*;
        use self::ConditionLimits::*;
        match self {
            Lt{field_sym, to, closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                let test = if closed {
                    ConditionTest::Lte
                } else {
                    ConditionTest::Lt
                };
                AlphaTest::Ord{data: ConditionData::U64(accessor, S(to)), test}
            }
            Gt{field_sym, from, closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                let test = if closed {
                    ConditionTest::Gte
                } else {
                    ConditionTest::Gt
                };
                AlphaTest::Ord{data: ConditionData::U64(accessor, S(from)), test}
            }
            Btwn{field_sym, from, from_closed, to, to_closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                let test = match (from_closed, to_closed) {
                    (false, false) => ConditionTest::GtLt,
                    (false, true) => ConditionTest::GtLte,
                    (true, false) => ConditionTest::GteLt,
                    (true, true) => ConditionTest::GteLte,

                };
                AlphaTest::Ord{data: ConditionData::U64(accessor, D(from, to)), test}
            },
            _ => AlphaTest::HashEq
        }
    }

    fn field_sym(&self) -> Option<usize> {
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
    pub(crate) field_sym: Option<usize>,
    pub(crate) dependents: HashSet<StatementId>
}

impl ConditionInfo {
    fn new(id: ConditionId, field_sym: Option<usize>) -> ConditionInfo {
        ConditionInfo{id, field_sym, dependents: Default::default()}
    }
}

#[derive(Hash, Eq, PartialEq)]
pub enum AlphaTest<T: ReteIntrospection> {
    HashEq,
    Ord{data: ConditionData<T>, test: ConditionTest}
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
        write!(f, "Test{{");
        match self {
            &HashEq => {
                write!(f, "HashEq");
            },
            &Ord{ref data, test} => {
                write!(f, "Ord");
            }
        }
        write!(f, "}}")
    }
}

#[derive(Clone, Hash, Eq, PartialEq)]
pub enum ConditionLimits<T: Hash + Eq + Ord + Clone> {
    S(T),
    D(T, T)
}

#[derive(Clone)]
pub enum ConditionData<T: ReteIntrospection>{
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

impl<T: ReteIntrospection> ConditionData<T> {
    fn hash_self<H: Hasher, L: Hash>(ord: usize, accessor: usize, limits: &L, state: &mut H) {
        ord.hash(state);
        accessor.hash(state);
        limits.hash(state);
    }
}

impl<T: ReteIntrospection> Hash for ConditionData<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::ConditionData::*;
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
            &F32(accessor, ref limits) => {
                Self::hash_self(10, accessor as usize, limits, state);
            },
            &F64(accessor, ref limits) => {
                Self::hash_self(11, accessor as usize, limits, state);
            },
        }
    }
}

impl<T: ReteIntrospection> PartialEq for ConditionData<T> {
    fn eq(&self, other: &Self) -> bool {
        use self::ConditionData::*;
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
            (&F32(accessor1, ref limits1), &F32(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            (&F64(accessor1, ref limits1), &F64(accessor2, ref limits2)) => {
                (accessor1 as usize) == (accessor2 as usize) && limits1 == limits2
            },
            _ => false
        }
    }
}

impl<T: ReteIntrospection> Eq for ConditionData<T> {}

#[derive(Copy, Clone, Eq, Hash, PartialEq)]
pub enum ConditionTest {
    HashEq, // TODO: Move one layer up - HashEq vs Ordinal?
    Lt,
    Lte,
    Gt,
    Gte,
    GtLt,
    GteLt,
    GtLte,
    GteLte
}

impl ConditionTest {

    fn test<T: Hash + Eq + Ord + Clone>(&self, val: &T, limits: &ConditionLimits<T>) -> bool {
        use self::ConditionTest::*;
        use self::ConditionLimits::*;
        match (self, limits) {
            (&Lt, &S(ref to)) => {
                val < to
            },
            (&Lte, &S(ref to)) => {
                val <= to
            },
            (&Gt, &S(ref to)) => {
                val > to
            },
            (&Gte, &S(ref to)) => {
                val >= to
            },
            (&GtLt, &D(ref from, ref to)) => {
                val > from && val < to
            },
            (&GteLt, &D(ref from, ref to)) => {
                val >= from && val < to
            },
            (&GtLte, &D(ref from, ref to)) => {
                val > from && val <= to
            },
            (&GteLte, &D(ref from, ref to)) => {
                val >= from && val <= to
            },
            _ => unreachable!("Unexpected condition test combination.")
        }
    }
}


/*

impl<T: ReteIntrospection> Debug for ConditionTest<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::ConditionTest::*;
        write!(f, "Test{{");
        match self {
            &HashEq => {
                write!(f, "HashEq");
            },
            &Lt{to, closed, ..} => {
                if closed {
                    write!(f, "[0,{:?}]", to);
                } else {
                    write!(f, "[0,{:?})", to);
                }
            },
            &Gt{from, closed, ..} => {
                if closed {
                    write!(f, "[{:?},∞]", from);
                } else {
                    write!(f, "({:?},∞]", from);
                }
            },
            &Btwn{from, from_closed, to, to_closed, ..} => {
                match (from_closed, to_closed) {
                    (false, false) => {
                        write!(f, "({:?},{:?})", from, to);
                    },
                    (false, true) => {
                        write!(f, "({:?},{:?}]", from, to);
                    }
                    (true, false) => {
                        write!(f, "[{:?},{:?})", from, to);
                    },
                    (true, true) => {
                        write!(f, "[{:?},{:?}]", from, to);
                    }
                }
            }
        }
        write!(f, "}}")
    }
}
*/