#[derive(Debug, Fail)]
pub enum CompileError {
    #[fail(display = "Cannot convert from a {} into a {}", from, to)]
    BadTypeConversion {
        from: String,
        to: String
    },
    #[fail(display = "No getter {}", getter)]
    MissingGetter {
        getter: String
    },
    #[fail(display = "No setter {}", setter)]
    MissingSetter {
        setter: String
    }
}