use super::Identifier;
use crate::ast;
use crate::gen::CodeGen;
use crate::gen::GenDB;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseError;
use crate::parser::ParseErrorKind;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct StatementVar {
    pub identifier: ast::Identifier,
    pub type_decl: Option<ast::TypeDeclaration>,
    pub equal: Token,
    pub expr: ast::Expression,
    // equals: Token,
    // eol: Token,
}

impl Parse for StatementVar {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let identifier = p.parse()?;

        // let type_decl = if p.peek_token().kind == TokenKind::TypeDeclaration {
        //     Some(p.parse()?)
        // } else {
        //     None
        // };
        let type_decl = None;

        let equal = if matches!(p.next_token().kind, TokenKind::Equal) {
            p.token()
        } else {
            return Err(ParseError {
                kind: ParseErrorKind::Expected {
                    expected: "equal".to_string(),
                    actual: format!("{:?}", p.next_token().kind),
                },
            });
        };

        let expression = p.parse()?;

        // Consume the EOL, probably should check it.
        p.next_token();

        Ok(ast::StatementVar {
            identifier,
            type_decl,
            equal,
            expr: expression,
        })
    }
}

impl CodeGen for StatementVar {
    fn gen(&self, db: &mut GenDB) -> String {
        db.add_var(self.identifier.clone(), self.type_decl.clone(), self.expr.clone());
        format!("local {} = {}", self.identifier.gen(db), self.expr.gen(db))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatementExpr {
    pub identifier: ast::Identifier,
    pub equal: Token,
    pub expr: ast::Expression,
}

impl Parse for StatementExpr {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        Ok(Self {
            // TODO: I REALLY DO NOT LIKE THAT THIS DOESN'T ADVANCE
            identifier: (&p.token()).into(),
            equal: p.next_token(),
            expr: p.parse()?,
        })
    }
}

impl CodeGen for StatementExpr {
    fn gen(&self, db: &mut GenDB) -> String {
        format!("{} = {}", self.identifier.gen(db), self.expr.gen(db))
    }
}
