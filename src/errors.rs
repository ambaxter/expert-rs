use crate::shared::compiler::id_generator::StatementId;

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
    },
    #[fail(display = "Rule {} declares duplicate variable on statement {:?}", rule, statement_id)]
    MultipleVariables {
        rule: String,
        statement_id: StatementId,
    },
    #[fail(display = "Rule {} declares duplicate field {}", rule, field)]
    DuplicateFields {
        rule: String,
        field: String
    }
}