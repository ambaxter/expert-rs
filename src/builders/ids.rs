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
