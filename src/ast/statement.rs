use crate::ast;
use crate::ast::Identifier;
use crate::lexer::Token;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Vim9Script(ast::StatementVim9),
    Var(ast::StatementVar),

    Empty,
    Error { msg: String },
}

impl Parse for Statement {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        // todo!()
        match p.next_token() {
            Token::Vim9Script(_) => {
                // return Ok(p.parse_vim9script()),

                if !matches!(p.next_token(), Token::NewLine | Token::EOF) {
                    panic!("Not handled")
                } else {
                    Ok(ast::Statement::Vim9Script(ast::StatementVim9 {}))
                }
            }
            Token::CommandVar => Ok(ast::Statement::Var(p.parse()?)),
            Token::EOF => Ok(ast::Statement::Empty),
            unparsed => panic!("AHHHHHHH {:?}", unparsed),
        }
    }
}
