use std::collections::HashSet;
use traits::{Insert, FieldValue};
use std::any::TypeId;

use runtime::memory::SymbolId;
use builders::ids::{RuleId, StatementId};

#[derive(Clone, Debug, Hash, Eq, Ord, PartialOrd, PartialEq)]
pub enum RuleCmd {
    Print(String),
    Insert(TypeId, Vec<FieldValue>),
    Return(TypeId, Vec<FieldValue>)
}

pub struct Rule {
    rule_id: RuleId,
    name: SymbolId,
    statement_ids: HashSet<StatementId>,
    commands: Vec<RuleCmd>
}