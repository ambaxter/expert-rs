use super::prelude::*;
pub use super::prelude::dyn;
use super::super::nodes::tests::{ApplyNot, EqTest, OrdTest, BetweenTest, StrArrayTest};
use super::super::nodes::beta::TestRepr;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum RefNodes<'a, S: 'a + AsRef<str>> {
    T(TestRepr<S>),
    Any(&'a [RefNodes<'a, S>]),
    NotAny(&'a [RefNodes<'a, S>]),
    All(&'a [RefNodes<'a, S>]),
    NotAll(&'a [RefNodes<'a, S>])
}

pub fn eq<'a, S: AsRef<str>, T: IntoEqTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::T(to.into_eq_test(field, EqTest::Eq))
}

pub fn ne<'a, S: AsRef<str>, T: IntoEqTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::T(to.into_eq_test(field, EqTest::Ne))
}

pub fn lt<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::T(to.into_ord_test(field, OrdTest::Lt))
}

pub fn le<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::T(to.into_ord_test(field, OrdTest::Le))
}

pub fn gt<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::T(to.into_ord_test(field, OrdTest::Gt))
}

pub fn ge<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::T(to.into_ord_test(field, OrdTest::Ge))
}

pub fn gtlt<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::T((from, to).into_btwn_test(field, BetweenTest::GtLt))
}

pub fn gelt<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::T((from, to).into_btwn_test(field, BetweenTest::GeLt))
}

pub fn gtle<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::T((from, to).into_btwn_test(field, BetweenTest::GtLe))
}

pub fn gele<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::T((from, to).into_btwn_test(field, BetweenTest::GeLe))
}

pub fn contains<'a, S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> RefNodes<'a, S> {
    RefNodes::T(val.into_str_test(field, StrArrayTest::Contains))
}

pub fn starts_with<'a, S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> RefNodes<'a, S> {
    RefNodes::T(val.into_str_test(field, StrArrayTest::StartsWith))
}

pub fn ends_with<'a, S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> RefNodes<'a, S> {
    RefNodes::T(val.into_str_test(field, StrArrayTest::EndsWith))
}

pub fn not<'a, S: AsRef<str>>(node: RefNodes<'a, S>) -> RefNodes<'a, S> {
    use self::RefNodes::*;
    match node {
        T(mut t) => {
            t.apply_not();
            T(t)
        },
        Any(t) => NotAny(t),
        NotAny(t) => Any(t),
        All(t) => NotAll(t),
        NotAll(t) => All(t)
    }
}

pub fn any<'a, S: AsRef<str>>(nodes: &'a[RefNodes<'a, S>]) -> RefNodes<'a, S> {
    RefNodes::Any(nodes)
}

pub fn all<'a, S: AsRef<str>>(nodes: &'a[RefNodes<'a, S>]) -> RefNodes<'a, S> {
    RefNodes::All(nodes)
}
