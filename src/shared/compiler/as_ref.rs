use super::prelude::*;
pub use super::prelude::dyn;
use super::super::nodes::tests::{ApplyNot, EqTest, OrdTest, BetweenTest, StrArrayTest};
use super::super::nodes::beta::TestRepr;
use runtime::memory::StringCache;
use errors::CompileError;
use shared::fact::Fact;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum RefNodes<'a, S: 'a + AsRef<str>> {
    Test(TestRepr<S>),
    Any(&'a [RefNodes<'a, S>]),
    NotAny(&'a [RefNodes<'a, S>]),
    All(&'a [RefNodes<'a, S>]),
    NotAll(&'a [RefNodes<'a, S>])
}

pub fn eq<'a, S: AsRef<str>, T: IntoEqTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::Test(to.into_eq_test(field, EqTest::Eq))
}

pub fn ne<'a, S: AsRef<str>, T: IntoEqTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::Test(to.into_eq_test(field, EqTest::Ne))
}

pub fn lt<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::Test(to.into_ord_test(field, OrdTest::Lt))
}

pub fn le<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::Test(to.into_ord_test(field, OrdTest::Le))
}

pub fn gt<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::Test(to.into_ord_test(field, OrdTest::Gt))
}

pub fn ge<'a, S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> RefNodes<'a, S> {
    RefNodes::Test(to.into_ord_test(field, OrdTest::Ge))
}

pub fn gtlt<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::Test((from, to).into_btwn_test(field, BetweenTest::GtLt))
}

pub fn gelt<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::Test((from, to).into_btwn_test(field, BetweenTest::GeLt))
}

pub fn gtle<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::Test((from, to).into_btwn_test(field, BetweenTest::GtLe))
}

pub fn gele<'a, S: AsRef<str>, T>(field: S, from: T, to: T) -> RefNodes<'a, S>
    where (T, T): IntoBtwnTest<S>{
    RefNodes::Test((from, to).into_btwn_test(field, BetweenTest::GeLe))
}

pub fn contains<'a, S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> RefNodes<'a, S> {
    RefNodes::Test(val.into_str_test(field, StrArrayTest::Contains))
}

pub fn starts_with<'a, S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> RefNodes<'a, S> {
    RefNodes::Test(val.into_str_test(field, StrArrayTest::StartsWith))
}

pub fn ends_with<'a, S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> RefNodes<'a, S> {
    RefNodes::Test(val.into_str_test(field, StrArrayTest::EndsWith))
}

pub fn not<'a, S: AsRef<str>>(node: RefNodes<'a, S>) -> RefNodes<'a, S> {
    use self::RefNodes::*;
    match node {
        Test(mut t) => {
            t.apply_not();
            Test(t)
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

impl<'a, S: AsRef<str>, T: Fact> Stage1Compile<T> for RefNodes<'a, S> {
    fn stage1_compile(&self, cache: &mut StringCache) -> Result<Stage1Node<T>, CompileError> {
        use self::RefNodes::*;
        match *self {
            Test(ref t) => Ok(Stage1Node::Test(t.compile(cache)?)),
            Any(ref v) => Ok(Stage1Node::Any(Stage1Compile::stage1_compile_slice(v, cache)?)),
            NotAny(ref v) => Ok(Stage1Node::NotAny(Stage1Compile::stage1_compile_slice(v, cache)?)),
            All(ref v) => Ok(Stage1Node::All(Stage1Compile::stage1_compile_slice(v, cache)?)),
            NotAll(ref v) => Ok(Stage1Node::NotAny(Stage1Compile::stage1_compile_slice(v, cache)?)),
        }
    }
}




#[cfg(test)]
mod tests {
    use shared::fact::{Fact, Getter};
    use super::*;
    use shared::nodes::alpha::HashEqField;

    #[derive(Clone, Hash, Eq, PartialEq, Debug)]
    struct Dummy {
        d: u64
    }

    impl Dummy {
        fn get_d(&self) -> &u64 {
            &self.d
        }
    }

    impl Fact for Dummy {
        type HashEq = ();

        fn getter(field: &str) -> Option<Getter<Self>> {
            match field {
                "d" => Some(Getter::U64(Dummy::get_d)),
                _ => unimplemented!()
            }
        }

        fn exhaustive_hash(&self) -> Box<Iterator<Item=<Self as Fact>::HashEq>> {
            unimplemented!()
        }

        fn create_hash_eq(conditions: &Vec<HashEqField>, cache: &StringCache) -> Self::HashEq {
            unimplemented!()
        }
    }

    #[test]
    pub fn as_ref_test() {
        let mut cache = StringCache::new();
        let nodes: Stage1Node<Dummy> = all(
            &[not(any(&[eq("d", 6u64)])), all(&[le("d", 64u64), le("d", dyn("ab"))])]
        ).stage1_compile(&mut cache).unwrap()
            .clean();
        println!("{:?}", nodes);
    }
}