use super::Identifier;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct StatementDef {
    pub name: Identifier,
    // pub args: Vec<Identifier>,
}

impl Parse for StatementDef {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let name = p.parse()?;

        // Left Paren, Right Paren (TODO: Args)
        assert!(p.expect_peek(TokenKind::LeftParen));
        assert!(p.expect_peek(TokenKind::RightParen));

        // Return type :
        assert!(p.expect_peek(TokenKind::Colon));

        // Return type :
        assert!(p.expect_peek(TokenKind::Identifier));
        assert!(p.expect_peek(TokenKind::NewLine));

        Ok(StatementDef {
            name,
            // TODO:
            // args: p.parse()?,
        })
    }
}
