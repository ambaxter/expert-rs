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
extern crate decimal;
extern crate chrono;
extern crate ord_subset;

extern crate enum_index;
#[macro_use]
extern crate enum_index_derive;

#[macro_use]
extern crate anymap;

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
