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