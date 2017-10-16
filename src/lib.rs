#![feature(plugin)]
#![plugin(interpolate_idents)]

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

#[macro_export]
macro_rules! create_type {
    ($base:ident; inserts => [$($input_type:ident),+]; returns => [$($return_type:ident),*]) => {
        interpolate_idents!{

            struct [$base RuleBuilder] {

            }

            struct [$base Builder] {

            }

            struct [$base Network] {

            }

            struct [$base Session] {
                test: Vec<expert::runtime::memory::InterMemoryId>
            }

        }
    }
}
