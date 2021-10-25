use crate::ast;
use crate::lexer::TokenKind;
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
        println!("Was start of file? {:?}", p.token());
        let token = p.next_token();
        println!(".. and now parsing: {:?}\n", p.token());

        match token.kind {
            TokenKind::Vim9Script => {
                let next_token = p.next_token();
                if !matches!(next_token.kind, TokenKind::NewLine | TokenKind::EOF) {
                    panic!("Not handled")
                } else {
                    Ok(ast::Statement::Vim9Script(ast::StatementVim9 {}))
                }
            }
            TokenKind::CommandVar => Ok(ast::Statement::Var(p.parse()?)),
            TokenKind::EOF => Ok(ast::Statement::Empty),
            unparsed => panic!("AHHHHHHH {:?}", unparsed),
        }
    }
}
