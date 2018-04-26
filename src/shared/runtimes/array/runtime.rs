use shared::fact::Fact;
use shared::nodes::alpha::AlphaNode;
use shared::nodes::beta::BetaNode;

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct AlphaNodeData {
    parent: usize,
    children_index: usize,
    children_len: usize,
    is_memory: bool,
}

#[derive(Debug)]
pub struct AlphaTree<T: Fact> {
    tests: Vec<AlphaNode<T>>,
    data: Vec<AlphaNodeData>,
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub enum BetaGroupType {
    ALL,
    NOTALL,
    ANY,
    NOTANY
}

#[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
pub struct BetaNodeData {
    parent: usize,
    child_tests_index: usize,
    child_test_len: usize,
    child_data_index: usize,
    child_data_len: usize,
    group_type: BetaGroupType,
}

#[derive(Debug)]
pub struct BetaTree<T: Fact> {
    data: Vec<BetaNodeData>,
    tests: Vec<BetaNode<T>>,
}