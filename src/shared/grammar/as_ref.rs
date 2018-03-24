use super::prelude::*;
use super::super::tests::TestRepr;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum RefNodes<'a, S: 'a + AsRef<str>> {
    T(TestRepr<S>),
    ANY(&'a [RefNodes<'a, S>]),
    ALL(&'a [RefNodes<'a, S>]),
}