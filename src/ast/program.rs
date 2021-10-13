use crate::ast;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<ast::Statement>,
}

impl Parse for Program {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let mut statements: Vec<ast::Statement> = Vec::new();
        loop {
            match p.token().kind {
                TokenKind::EOF => break,
                _ => statements.push(p.parse()?),
            }
        }

        Ok(Program { statements })
    }
}
