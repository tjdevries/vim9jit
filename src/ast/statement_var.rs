use crate::ast;
use crate::lexer::Token;
use crate::parser::Parse;
use crate::parser::ParseError;
use crate::parser::ParseErrorKind;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct StatementVar {
    pub identifier: ast::Identifier,
    pub expression: ast::Expression,
    // equals: Token,
    // eol: Token,
}

impl Parse for StatementVar {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let identifier: ast::Identifier = p.parse()?;

        if !matches!(p.next_token(), Token::Equal) {
            return Err(ParseError {
                kind: ParseErrorKind::Expected {
                    expected: "equal",
                    actual: "something else...?",
                },
            });
        }

        let expression = p.parse()?;

        // Consume the EOL, probably should check it.
        p.next_token();

        Ok(ast::StatementVar { identifier, expression })
    }
}
