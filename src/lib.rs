#![feature(plugin)]
#![plugin(interpolate_idents)]

#[macro_use]
extern crate failure_derive;

#[macro_use]
extern crate failure;

extern crate num;

#[macro_use]
extern crate itertools;

extern crate string_interner;

extern crate ordered_float;

extern crate float_cmp;

#[macro_use]
mod macros;

pub mod base;
pub mod traits;
pub mod network;
pub mod runtime;
pub mod serial;
pub mod builder;
pub mod builders;
pub mod iter;
pub mod errors;
pub mod shared;

pub use failure::Error;

type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! expert {
    ($base:ident; [$($input_type:ident),+]) => {
        interpolate_idents!{

            trait [$base AddStatement] : expert::traits::Fact {
                fn add_statement(builder: &mut [$base RuleBuilder], hash_eq: Self::HashEq) -> (&mut expert::runtime::memory::StringCache, &mut expert::builders::ids::BuilderIdGen, &mut HashMap<expert::network::tests::AlphaTest<Self>, expert::builders::statement::ConditionDesc>);
            }

            pub struct [$base RuleBuilder] {
                base_builder: [$base Builder],
                name: String,
                $( [statements_ $input_type]: HashMap<<$input_type as expert::traits::Fact>::HashEq, HashMap<expert::network::tests::AlphaTest<$input_type>, expert::builders::statement::ConditionDesc>>, )*
            }

            //TODO: move collapse from Statement to [$base RuleBuilder] impl

            $(
            impl [$base AddStatement] for $input_type {
                fn add_statement(builder: &mut [$base RuleBuilder], hash_eq: Self::HashEq) -> (&mut expert::runtime::memory::StringCache, &mut expert::builders::ids::BuilderIdGen, &mut HashMap<expert::network::tests::AlphaTest<Self>, expert::builders::statement::ConditionDesc>) {
                    (&mut builder.base_builder.cache, &mut builder.base_builder.ids, builder.[statements_ $input_type].entry(hash_eq).or_insert(Default::default()))
                }
            }
            )*

            pub struct [$base Builder] {
                cache: expert::runtime::memory::StringCache,
                ids: expert::builders::ids::BuilderIdGen,
                errors: Vec<expert::Error>
            }

            pub struct [$base Rule] {

            }

            pub struct [$base Network] {

            }

            pub struct [$base Session] {
                test: Vec<expert::runtime::memory::InterMemoryId>
            }

        }
    }
}
