use num::ToPrimitive;

use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseError;
use crate::parser::ParseErrorKind;
use crate::parser::ParseResult;

#[derive(Debug, Clone, PartialEq)]
pub struct LiteralNumber {
    pub value: f64,
}

impl Parse for LiteralNumber {
    fn parse(p: &mut crate::parser::Parser) -> ParseResult<Self> {
        let token = p.next_token();
        match token.kind {
            TokenKind::Number => {
                // TODO: Actually error correctly
                let value: f64 = token.text.parse().unwrap();
                Ok(Self { value })
            }
            kind => Err(ParseError {
                kind: ParseErrorKind::Expected {
                    actual: "TokenKind::Number".to_string(),
                    expected: format!("{:?}", kind),
                },
            }),
        }
    }
}

impl<I> From<I> for LiteralNumber
where
    I: ToPrimitive,
{
    fn from(val: I) -> Self {
        Self {
            value: val.to_f64().unwrap(),
        }
    }
}
