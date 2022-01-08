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
    Def(ast::StatementDef),
    For(ast::StatementFor),

    Empty,
    Error { msg: String },
}

impl Parse for Statement {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let mut token = p.next_token();
        while token.kind == TokenKind::NewLine {
            token = p.next_token();
        }

        match &token.kind {
            TokenKind::Vim9Script => {
                let next_token = p.next_token();
                if !matches!(next_token.kind, TokenKind::NewLine | TokenKind::EOF) {
                    panic!("Not handled")
                } else {
                    Ok(ast::Statement::Vim9Script(ast::StatementVim9 {}))
                }
            }
            TokenKind::CommandVar => Ok(Statement::Var(p.parse()?)),
            TokenKind::CommandDef => Ok(Statement::Def(p.parse()?)),
            TokenKind::CommandFor => Ok(Statement::For(p.parse()?)),
            TokenKind::EOF => Ok(Statement::Empty),
            _ => {
                dbg!(p);
                panic!("AHHHHHHH {:?}", token)
            }
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
