mod program;
pub use program::Program;

mod statement;
pub use statement::Statement;

mod statement_var;
pub use statement_var::StatementVar;

mod statement_vim9;
pub use statement_vim9::StatementVim9;

mod expression;
pub use expression::Expression;

// Terminals
mod identifier;
pub use identifier::Identifier;

mod literal_number;
pub use literal_number::LiteralNumber;

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
    Plus,
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum InfixOperator {
    Equal,
    NotEqual,

    LessThan,
    GreatherThan,
    LessThanOrEqual,
    GreatherThanOrEqual,

    Add,
    Sub,

    Mul,
    Div,
}
