use string_interner;
use string_interner::DefaultStringInterner;
use expert::serial::SerialGen;
use std::marker::PhantomData;
use expert::introspection::ReteIntrospection;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::{HashMap, HashSet};
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
    condition_map: HashMap<T::HashEq, HashMap<ConditionTest<T>, ConditionInfo>>
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
            let root_info = dependents.get(&ConditionTest::HashEq).unwrap();
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

    pub fn compile(self) {

    }

    pub(crate) fn explode(self) -> (DefaultStringInterner, Vec<Rule>, HashMap<T::HashEq, HashMap<ConditionTest<T>, ConditionInfo>>) {
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

                entry_point.entry(ConditionTest::HashEq).or_insert_with(|| ConditionInfo::new(id_generator.next_condition_id(), None))
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

    fn convert<T: ReteIntrospection>(self, string_repo: &DefaultStringInterner) -> ConditionTest<T> {
        use self::StatementCondition::*;
        match self {
            Lt{field_sym, to, closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                ConditionTest::Lt{accessor, to, closed}
            }
            Gt{field_sym, from, closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                ConditionTest::Gt{accessor, from, closed}
            }
            Btwn{field_sym, from, from_closed, to, to_closed} => {
                let accessor= string_repo.resolve(field_sym)
                    .and_then(|s| T::getter(s)).unwrap();
                ConditionTest::Btwn{accessor, from, from_closed, to, to_closed}
            },
            _ => ConditionTest::HashEq
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


#[derive(Copy, Clone)]
pub enum ConditionTest<T: ReteIntrospection> {
    HashEq,
    Lt{accessor: fn(&T) -> u64, to: u64, closed: bool},
    Gt{accessor: fn(&T) -> u64, from: u64, closed: bool},
    Btwn{accessor: fn(&T) -> u64, from:u64, from_closed: bool, to: u64, to_closed: bool}
}

#[derive(Debug, Clone)]
pub struct ConditionInfo {
    id: ConditionId,
    field_sym: Option<usize>,
    dependents: HashSet<StatementId>
}

impl ConditionInfo {
    fn new(id: ConditionId, field_sym: Option<usize>) -> ConditionInfo {
        ConditionInfo{id, field_sym, dependents: Default::default()}
    }
}

impl<T: ReteIntrospection> Hash for ConditionTest<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use self::ConditionTest::*;
        match self {
            &HashEq => {
                0.hash(state);
            },
            &Lt{accessor, to, closed} => {
                1.hash(state);
                (accessor as usize).hash(state);
                to.hash(state);
                closed.hash(state);
            },
            &Gt{accessor, from, closed} => {
                2.hash(state);
                (accessor as usize).hash(state);
                from.hash(state);
                closed.hash(state);
            },
            &Btwn{accessor, from, from_closed, to, to_closed} => {
                3.hash(state);
                (accessor as usize).hash(state);
                from.hash(state);
                from_closed.hash(state);
                to.hash(state);
                to_closed.hash(state);
            },
        }
    }
}

impl<T: ReteIntrospection> PartialEq for ConditionTest<T> {
    fn eq(&self, other: &Self) -> bool {
        use self::ConditionTest::*;
        match (self, other) {
            (&HashEq, &HashEq) => true,
            (&Lt{accessor, to, closed}, &Lt{accessor: accessor_o, to: to_o, closed: closed_o}) => {
                (accessor as usize) == (accessor_o as usize) &&
                    to == to_o &&
                    closed == closed_o
            },
            (&Gt{accessor, from, closed}, &Gt{accessor: accessor_o, from: from_o, closed: closed_o}) => {
                (accessor as usize) == (accessor_o as usize) &&
                    from == from_o &&
                    closed == closed_o
            },
            (&Btwn{accessor, from, from_closed, to, to_closed}, &Btwn{accessor: accessor_o, from: from_o, from_closed: from_close_o, to: to_o, to_closed: to_closed_o} ) => {
                (accessor as usize) == (accessor_o as usize) &&
                    from == from_o &&
                    from_closed == from_close_o &&
                    to == to_o &&
                    to_closed == to_closed_o
            }
            _ => false
        }
    }
}

impl<T: ReteIntrospection> Eq for ConditionTest<T> {}

impl<T: ReteIntrospection> ConditionTest<T> {

    pub fn is_hash_eq(&self) -> bool {
        use self::ConditionTest::*;
        match self {
            &HashEq => true,
            _ => false
        }
    }

    pub fn test(&self, t: &T) -> bool {
        use self::ConditionTest::*;
        match self {
            &HashEq => true,
            &Lt{accessor, to, closed} => {
                let val = accessor(t);
                if closed {
                    val <= to
                } else {
                    val < to
                }
            },
            &Gt{accessor, from, closed} => {
                let val = accessor(t);
                if closed {
                    val >= from
                } else {
                    val > from
                }
            },
            &Btwn{accessor, from, from_closed, to, to_closed} => {
                let val = accessor(t);
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
            }
        }
    }
}

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


// KBase and friends


pub enum DestinationNode {
    Alpha(usize),
    Beta(usize),
}

pub struct AlphaNode<T: ReteIntrospection> {
    id: ConditionId,
    condition: ConditionTest<T>,
    destinations: Vec<DestinationNode>
}
