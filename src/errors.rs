#[derive(Debug, Fail)]
pub enum CompileError {
    #[fail(display = "Cannot convert {} from a {} into a {}", var, from, to)]
    BadCast {
        var: String,
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