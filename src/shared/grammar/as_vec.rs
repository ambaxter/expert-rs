use super::prelude::*;
use super::super::tests::TestRepr;


#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum VecNodes<S: AsRef<str>> {
    TEST(TestRepr<S>),
    ANY(Vec<VecNodes<S>>),
    ALL(Vec<VecNodes<S>>)
}