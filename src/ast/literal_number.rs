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
        let token = p.token();
        match token.kind {
            TokenKind::Number => {
                // TODO: Actually error correctly
                let value: f64 = token.text.parse().unwrap();
                Ok(Self { value })
            }
            _ => Err(ParseError {
                kind: ParseErrorKind::Expected {
                    actual: "Somethign else",
                    expected: "token",
                },
            }),
        }
    }
}

impl From<i64> for LiteralNumber {
    fn from(val: i64) -> Self {
        Self { value: val as f64 }
    }
}
