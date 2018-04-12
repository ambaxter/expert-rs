use super::prelude::Stage1Compile;
use super::super::nodes::beta::SDynLimit;
use shared::fact::Fact;
use shared::nodes::beta::BetaNode;
use shared::compiler::prelude::Stage1Node;
use runtime::memory::StringCache;

struct BuilderContext {

}

// Have a single KnowledgeBuilder trait
// Have a single RuleBuilder trait
// Have a single Struct that implements both traits
// it switches between the two traits via impl trait on the entrance and exit functions
// stores current rule_id (0 by default)
// fetches each rule during modification (set name, salience, other options)
//
//pub struct KnowledgeBaseBuilder {
//
//}
//
//pub struct RuleBuilder {
//    cache: StringCache,
//}
//
//impl RuleBuilder {
//
//    pub fn when<T:Fact, N: Stage1Compile<T>>(mut self, nodes: &[N]) -> RuleBuilder {
//        Stage1Node::All(N::stage1_compile_slice(nodes, &mut self.cache).unwrap()).clean();
//        self
//    }
//
//}

pub struct KnowledgeBuilder {

}

pub trait KnowledgeBase {

}

pub trait BaseBuilder {
    type RB: RuleBuilder;
    type KB: KnowledgeBase;

    fn rule<S: Into<String>>(self, name: S) -> Self::RB;
    fn end(self) -> Self::KB;
}

pub trait RuleBuilder {
    type CB: ConsequenceBuilder;

    fn salience(self, salience: i32) -> Self;
    fn agenda<S: Into<String>>(self, agenda: S) -> Self;
    fn no_loop(self, no_loop: bool) -> Self;
    fn when<T:Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Self;
    fn when_not<T:Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Self;
    // TODO - add grouping in a future version
    fn then(self) -> Self::CB;
}

pub trait ConsequenceBuilder {
    type BB: BaseBuilder;
    fn end(self) -> Self::BB;
}

#[cfg(test)]
mod tests {
    use shared::fact::{Fact, Getter};
    use super::*;
    use super::super::as_ref::*;
    use runtime::memory::StringCache;
    use shared::compiler::prelude::Stage1Node;

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
    }

    #[test]
    pub fn as_ref_test() {
//        let cache = StringCache::new();
//        let b = RuleBuilder{cache}
//            .when::<Dummy, _>(&all(
//                &[not(any(&[eq("d", 6u64)])), all(&[le("d", 64u64), le("d", dyn("ab"))])]
//            ));
    }
}