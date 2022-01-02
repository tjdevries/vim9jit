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
    pub expression: ast::Expression,
    // equals: Token,
    // eol: Token,
}

impl Parse for StatementVar {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let identifier = p.parse()?;

        let type_decl = if p.peek_token().kind == TokenKind::TypeDeclaration {
            Some(p.parse()?)
        } else {
            None
        };

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
            expression,
        })
    }
}

impl CodeGen for StatementVar {
    fn gen(&self, db: &mut GenDB) -> String {
        format!("local {} = {}", self.identifier.gen(db), self.expression.gen(db))
    }
}
