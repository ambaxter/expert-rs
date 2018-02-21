#[derive(Debug, Fail)]
pub enum CompileError {
    #[fail(display = "Invalid getter {}. Expected a {}, found {}", getter, to, from)]
    IncorrectGetter {
        getter: String,
        to: String,
        from: String
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