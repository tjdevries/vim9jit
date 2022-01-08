use super::Expression;
use crate::gen::CodeGen;
use crate::gen::GenDB;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseError;
use crate::parser::ParseErrorKind;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub name: String,
}

impl Parse for Identifier {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let token = p.next_token();
        match token.kind {
            TokenKind::Identifier => Ok(Self { name: token.text }),
            _ => Err(ParseError {
                kind: ParseErrorKind::Expected {
                    actual: format!("{:?}", token),
                    expected: "A valid identifier".to_string(),
                },
            }),
        }
    }
}

impl Into<Expression> for Identifier {
    fn into(self) -> Expression {
        Expression::Identifier(self)
    }
}

impl From<&str> for Identifier {
    fn from(val: &str) -> Self {
        Identifier { name: val.to_owned() }
    }
}

impl From<&Token> for Identifier {
    fn from(val: &Token) -> Self {
        Identifier {
            name: val.text.to_owned(),
        }
    }
}

impl CodeGen for Identifier {
    fn gen(&self, _: &mut GenDB) -> String {
        self.name.clone()
    }
}
