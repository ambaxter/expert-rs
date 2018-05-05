use super::prelude::*;
pub use super::prelude::dyn;
use super::super::nodes::tests::{ApplyNot, EqTest, OrdTest, BetweenTest, StrArrayTest};
use super::super::nodes::beta::TestRepr;
use runtime::memory::StringCache;
use shared::fact::Fact;
use errors::CompileError;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum VecNodes<S: AsRef<str>> {
    Test(TestRepr<S>),
    Any(Vec<VecNodes<S>>),
    NotAny(Vec<VecNodes<S>>),
    All(Vec<VecNodes<S>>),
    NotAll(Vec<VecNodes<S>>),
}

pub fn eq<S: AsRef<str>, T: IntoEqTest<S>>(field: S, to: T) -> VecNodes<S> {
    VecNodes::Test(to.into_eq_test(field, EqTest::Eq))
}

pub fn ne<S: AsRef<str>, T: IntoEqTest<S>>(field: S, to: T) -> VecNodes<S> {
    VecNodes::Test(to.into_eq_test(field, EqTest::Ne))
}

pub fn lt<S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> VecNodes<S> {
    VecNodes::Test(to.into_ord_test(field, OrdTest::Lt))
}

pub fn le<S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> VecNodes<S> {
    VecNodes::Test(to.into_ord_test(field, OrdTest::Le))
}

pub fn gt<S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> VecNodes<S> {
    VecNodes::Test(to.into_ord_test(field, OrdTest::Gt))
}

pub fn ge<S: AsRef<str>, T: IntoOrdTest<S>>(field: S, to: T) -> VecNodes<S> {
    VecNodes::Test(to.into_ord_test(field, OrdTest::Ge))
}

pub fn gtlt<S: AsRef<str>, T>(field: S, from: T, to: T) -> VecNodes<S>
    where (T, T): IntoBtwnTest<S>{
    VecNodes::Test((from, to).into_btwn_test(field, BetweenTest::GtLt))
}

pub fn gelt<S: AsRef<str>, T>(field: S, from: T, to: T) -> VecNodes<S>
    where (T, T): IntoBtwnTest<S>{
    VecNodes::Test((from, to).into_btwn_test(field, BetweenTest::GeLt))
}

pub fn gtle<S: AsRef<str>, T>(field: S, from: T, to: T) -> VecNodes<S>
    where (T, T): IntoBtwnTest<S>{
    VecNodes::Test((from, to).into_btwn_test(field, BetweenTest::GtLe))
}

pub fn gele<S: AsRef<str>, T>(field: S, from: T, to: T) -> VecNodes<S>
    where (T, T): IntoBtwnTest<S>{
    VecNodes::Test((from, to).into_btwn_test(field, BetweenTest::GeLe))
}

pub fn contains<S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> VecNodes<S> {
    VecNodes::Test(val.into_str_test(field, StrArrayTest::Contains))
}

pub fn starts_with<S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> VecNodes<S> {
    VecNodes::Test(val.into_str_test(field, StrArrayTest::StartsWith))
}

pub fn ends_with<S: AsRef<str>, T: IntoStrTest<S>>(field: S, val: T) -> VecNodes<S> {
    VecNodes::Test(val.into_str_test(field, StrArrayTest::EndsWith))
}

pub fn not<S: AsRef<str>>(node: VecNodes<S>) -> VecNodes<S> {
    use self::VecNodes::*;
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

pub fn any<S: AsRef<str>>(nodes: Vec<VecNodes<S>>) -> VecNodes<S> {
    VecNodes::Any(nodes)
}

pub fn all<S: AsRef<str>>(nodes: Vec<VecNodes<S>>) -> VecNodes<S> {
    VecNodes::All(nodes)
}


impl<S: AsRef<str>, T: Fact> Stage1Compile<T> for VecNodes<S> {
    fn stage1_compile(&self, cache: &mut StringCache) -> Result<Stage1Node<T>, CompileError> {
        use self::VecNodes::*;
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
    pub fn as_vec_test() {
        let mut cache = StringCache::new();
        let nodes: Stage1Node<Dummy> = all(
            vec![not(any(vec![eq("d", 6u64), any(vec![gt("d", 1024u64)])])), all(vec![le("d", 64u64), le("d", dyn("ab"))])]
        ).stage1_compile(&mut cache).unwrap()
            .clean();
        println!("{:?}", nodes);
    }
}