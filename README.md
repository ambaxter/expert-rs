# expert-rs

A cache friendly and type safe expert/business rules system written in Rust.

# Usage

Don't. It's not even Alpha quality, yet. 

Eventually you'll be able to add a dependency ala

```toml
[dependencies]
expert = "0.10"
```

And then write code similar to

```rust
#[macro_use]
extern crate expert;


// A fact type
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Fact)]
pub struct Fact1 {
    id: usize,
    len: usize,
}


// Another fact type
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, Fact)]
pub struct Fact2 {
    id: usize,
    height: usize
}

mod example {
    // Creates a new expert system specifically for Fact1 and Fact2
    expert!(Example; [Fact1, Fact2]);
}

pub fn main() {
    use example::*;
    
    // Create a custom rule builder
    let mut builder = ExampleBuilder::new();
    builder.
        // Add a new rule named "rule"
        rule("rule")
            // When a Fact1 is inserted with self.id == 2 and self.len < 32
            .when<Fact1>().eq("id", 2usize).lt("len", 32usize)
            // And when a Fact2 is inserted with sef.id ==3 and self.height >= 64
            .when<Fact2>().eq("id", 17usize).gte("height", 64usize)
        .then()
            // Add a new Fact1{id: 3, len: 20} to be available for return
            .ret<Fact1>().field("id", 3usize).field("len", 20usize)
        .end();
        
   // Create a knowledge base to hold the read-only, in-memory representation of all of the rules
   let example_base = builder.build();
   
   // Create a knowledge session/rule runtime
   let mut example_session = example_base.new_session();
   
   let a_fact = Fact1{id: 2usize, len: 31usize};
   let another_fact = Fact2{id: 3usize, height: 60usize};
   
   // Insert the facts 
   example_session.insert(a_fact);
   example_session.insert(another_fact);
   
   // Resolve the rule consequences
   example_session.resolve();
   
   let fact1_returns: &[Fact1] = <Fact1 as ExmapleReturn>::get_returns(&example_session);
   assert(fact1_returns.len() == 1);
   let fact1_ret = fact1_returns.get(0).unwrap();
   assert(fact1_ret.id == 3);
   assert(fact1_ret.len == 20);
}

```

# Features

* The code compiles!
* Alpha (fact analysis) nodes! 
* Statement Builder!

# In-Progress

* Rule Builder!
* KnowledgeBase Builder!
* Alpha, Beta node compilation and layout!
  * Networks represented as a set of arrays!
  * Alpha network ordered from most depended on nodes to least!
  * Beta network ordered from least depended to most!
* Chaining!
  * Forwards!
  * Backwards! (w/ Iterators)
* Returns, Insert within rule resolution
* All of those sweet, sweet macros

# Not In-Progress (everything else!)

* Variables
  * Fact
  * Field
* Complex logic ((this AND this AND NOT that) OR other)
* DSL
* Fast rule compilation times
* Rule optimizations
* Rule sanity checks
* In-place fact modification

# Why?

* Expert/Business Rules Systems are still in use in a variety of industries
* I wanted to understand how the algorithms worked
* I wanted to push my software development expertise
* I wanted to see if I could write it without pointers, fat or otherwise 
* It's hard




# License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in Metis by you, as defined in the Apache-2.0 license, shall be
dual licensed as above, without any additional terms or conditions.