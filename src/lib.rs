#![feature(plugin)]
#![plugin(interpolate_idents)]

#![recursion_limit = "1024"]

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

use failure::Error;

type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! create_type {
    ($base:ident; [$($input_type:ident),+]) => {
        interpolate_idents!{

            struct [$base RuleBuilder] {

            }

            struct [$base Builder] {

            }

            struct [$base Rule] {

            }

            struct [$base Network] {

            }

            struct [$base Session] {
                test: Vec<expert::runtime::memory::InterMemoryId>
            }

        }
    }
}
