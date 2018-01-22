use runtime::memory::SymbolId;
use std::hash::Hash;

/**
 * rule("name")
 *  .when_bound<Type>([with("field0", "$local0")], [eq("field1", 4u64), lte("field3", local("$local0"))])
 *  .when_bound<Type2>([as("$id2")], [gte("afield", local("local0"))])
 * .then()
 *  .insert<Type>([("afield2", 3u32), ("afield3", local("$local0")])
 *  .modify<Type2>("$id2", &["afield4", false])
 * .end()
 * .rule("name2)"// ...
 *
 */


struct KnowledgeBuilder {

}

struct RuleBuilder {
}

impl RuleBuilder {
    pub fn when<'a>(binds: &[grammar::BindFact<'a>], tests: &[u8]) {

    }
}

pub mod grammar {
    use runtime::memory::SymbolId;
    use std::hash::Hash;

    #[derive(Copy, Clone, Hash, Eq, PartialEq, Debug)]
    pub enum BindFact<'a> {
        Var(&'a str),
        With(&'a str, &'a str)
    }

    pub fn var<'a>(var: &'a str) -> BindFact<'a> {
        BindFact::Var(var)
    }

    pub fn with<'a>(field: &'a str, bind: &'a str) -> BindFact<'a> {
        BindFact::With(field, bind)
    }

}
