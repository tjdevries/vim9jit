use crate::ast;
use crate::gen::CodeGen;
use crate::gen::GenDB;
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

impl CodeGen for Program {
    fn gen(&self, db: &mut GenDB) -> String {
        self.statements
            .iter()
            .map(|s| s.gen(db))
            .collect::<Vec<String>>()
            .join("\n")
            .to_string()
    }
}
