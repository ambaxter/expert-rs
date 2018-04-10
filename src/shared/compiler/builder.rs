use super::prelude::Stage1Compile;
use super::super::nodes::beta::SDynLimit;
use shared::fact::Fact;
use shared::nodes::beta::BetaNode;


pub struct KnowledgeBaseBuilder {

}

pub struct RuleBuilder {

}

impl RuleBuilder {

    pub fn when<T:Fact, N: Stage1Compile<T>>(nodes: &N) {

    }

}
