use crate::ast;
use crate::gen::CodeGen;
use crate::gen::GenDB;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseError;
use crate::parser::ParseErrorKind;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Vim9Script(ast::StatementVim9),
    Var(ast::StatementVar),
    Def(ast::StatementDef),
    For(ast::StatementFor),
    Expr(ast::StatementExpr),
    Return(ast::Expression),

    EndDef,
    Empty,
    Error { msg: String },
}

impl Parse for Statement {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        // Certain tokens don't have a keyword to consume.
        // match &p.token().kind {
        //     TokenKind::Identifier => {
        //         return Ok(Statement::Expr(p.parse()?));
        //     }
        //     _ => {}
        // }

        let token = p.next_token();

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
            TokenKind::CommandReturn => {
                let ret = Statement::Return(p.parse()?);
                dbg!(&ret);
                Ok(ret)
            }
            TokenKind::NewLine => Ok(Statement::Empty),

            TokenKind::Identifier => {
                return Ok(Statement::Expr(p.parse()?));
            }
            // TokenKind::CommandEndDef => Ok(Statement::EndDef),
            TokenKind::EOF => Ok(Statement::Empty),
            _ => {
                dbg!(p);
                Err(ParseError {
                    kind: ParseErrorKind::Expected {
                        actual: format!("{:?}", token),
                        expected: "A valid Statement".to_string(),
                    },
                })
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
            Statement::Def(def) => def.gen(db),
            Statement::For(for_) => for_.gen(db),
            Statement::Expr(expr) => expr.gen(db),
            Statement::Return(ret) => format!("return {}", ret.gen(db)),
            Statement::EndDef => todo!(),
            Statement::Error { msg } => todo!(),
        }
        .to_string()
        // "print('statement');".to_string()
    }
}
