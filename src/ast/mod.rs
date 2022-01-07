#![allow(unused_macros)]
#![allow(unused_imports)]

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

mod expression_call;
pub use expression_call::FunctionCall;

mod type_declaration;
pub use type_declaration::TypeDeclaration;

// Terminals
mod identifier;
pub use identifier::Identifier;

mod literal_number;
pub use literal_number::LiteralNumber;

mod vim_variable;
pub use vim_variable::VimVariable;
pub use vim_variable::VimVariableScope;

#[derive(Debug, Clone, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
    Plus,
}

macro_rules! PreOp {
    [!] => { PrefixOperator::Bang };
    [-] => { PrefixOperator::Minus };
    [+] => { PrefixOperator::Plus };
}
pub(crate) use PreOp;

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

    Call,
}

impl CodeGen for InfixOperator {
    fn gen(&self, _: &mut GenDB) -> String {
        // TODO: We need to do this with Vim9Add and others

        match self {
            InfixOperator::Equal => "==",
            InfixOperator::NotEqual => "!=",
            InfixOperator::LessThan => "<",
            InfixOperator::GreatherThan => ">",
            InfixOperator::LessThanOrEqual => "<=",
            InfixOperator::GreatherThanOrEqual => ">=",
            InfixOperator::Add => "+",
            InfixOperator::Sub => "-",
            InfixOperator::Mul => "*",
            InfixOperator::Div => "/",
            InfixOperator::Call => unimplemented!(),
        }
        .to_string()
    }
}

macro_rules! InfOp {
    [=] => { InfixOperator::Equal };
    [!=] => { InfixOperator::NotEqual };

    [<] => { InfixOperator::LessThan };
    [>] => { InfixOperator::GreatherThan };
    [<=] => { InfixOperator::LessThanOrEqual };
    [>=] => { InfixOperator::GreatherThanOrEqual };

    [+] => { InfixOperator::Add };
    [-] => { InfixOperator::Sub };

    [*] => { InfixOperator::Mul };
    [/] => { InfixOperator::Div };
}

pub(crate) use InfOp;

use crate::gen::CodeGen;
use crate::gen::GenDB;
