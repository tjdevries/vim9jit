use crate::lexer::Token;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
}

impl Parse for Identifier {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        match p.next_token() {
            Token::Identifier(chars) => Ok(Self {
                name: chars.iter().collect(),
            }),
            _ => panic!("NOPE"),
        }
    }
}
