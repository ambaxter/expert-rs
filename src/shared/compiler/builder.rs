use super::prelude::Stage1Compile;
use errors::CompileError;
use shared::compiler::prelude::{DrainWhere, ProvidesNode};
use std;
use shared::fact::Fact;

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

pub trait KnowledgeBase {

}

pub trait BaseBuilder {
    type RB: RuleBuilder;
    type KB: KnowledgeBase;

    fn rule<S: AsRef<str>>(self, name: S) -> Self::RB;
    fn rule_with_agenda<S: AsRef<str>, A: AsRef<str>>(mut self, name: S, agenda_group: A) -> Self::RB;
    fn end(self) -> Self::KB;
}

pub trait RuleBuilder {
    type CB: ConsequenceBuilder;

    fn salience(self, salience: i32) -> Self;
    fn no_loop(self, no_loop: bool) -> Self;
    fn when<T: 'static + Fact, N: Stage1Compile<T>>(self, nodes: &[N]) -> Result<Self, CompileError>
        where Self: std::marker::Sized;
    fn provides_when<T: 'static + Fact, S: AsRef<str>, N: Stage1Compile<T>>(self, provides: &[ProvidesNode<S, S>], nodes: &[N]) -> Result<Self, CompileError>
        where Self: std::marker::Sized;
    fn all_group(self) -> Self;
    fn any_group(self) -> Self;
    fn exists_group(self) -> Self;
    fn not_group(self) -> Self;
    fn for_all_group<T:'static + Fact, N: Stage1Compile<T>>(self, node: &[N]) -> Result<Self, CompileError>
        where Self: std::marker::Sized;
    fn provides_for_all_group<T:'static + Fact, S: AsRef<str>, N: Stage1Compile<T>>(self, provides: &[ProvidesNode<S, S>], nodes: &[N]) -> Result<Self, CompileError>
        where Self: std::marker::Sized;
    fn end_group(self) -> Result<Self, CompileError> where Self: std::marker::Sized;
    fn then(self) -> Self::CB;
}

pub trait ConsequenceBuilder {
    type BB: BaseBuilder;
    fn end(self) -> Self::BB;
}
