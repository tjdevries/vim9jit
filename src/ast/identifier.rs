use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

impl Parse for Identifier {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let token = p.next_token();
        match token.kind {
            TokenKind::Identifier => Ok(Self { name: token.text }),
            _ => panic!("NOPE"),
        }
    }
}
