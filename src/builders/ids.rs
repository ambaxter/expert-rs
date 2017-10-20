use std::fmt;
use std::fmt::Debug;
use serial::SerialGen;

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct RuleId{index: usize}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct StatementId{index: usize}

#[derive(Copy, Clone, Eq, Hash, Ord, PartialOrd, PartialEq)]
pub struct ConditionId{index: usize}

index_id!(RuleId, StatementId, ConditionId);

pub type RuleIdGen = SerialGen<usize, RuleId>;
pub type StatementIdGen =  SerialGen<usize, StatementId>;
pub type ConditionIdGen = SerialGen<usize, ConditionId>;

pub struct BuilderIdGen {
    rule_ids: RuleIdGen,
    statement_ids: StatementIdGen,
    condition_ids: ConditionIdGen
}

impl BuilderIdGen {
    pub fn new() -> BuilderIdGen {
        BuilderIdGen{
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

impl Default for BuilderIdGen {
    fn default() -> Self {
        BuilderIdGen::new()
    }
}