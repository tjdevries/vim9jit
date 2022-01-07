use crate::ast;
use crate::gen::CodeGen;
use crate::gen::GenDB;
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
        let token = p.next_token();

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

impl CodeGen for Statement {
    fn gen(&self, db: &mut GenDB) -> String {
        match self {
            Statement::Vim9Script(v) => v.gen(db),
            Statement::Var(v) => v.gen(db),
            Statement::Empty => "".to_string(),
            _ => unimplemented!(),
        }
        .to_string()
        // "print('statement');".to_string()
    }
}
