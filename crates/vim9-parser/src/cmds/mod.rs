use anyhow::Result;
use vim9_lexer::TokenKind;

use crate::{
    CallExpression, ExCommand, Expression, Parser, Precedence, TokenMeta,
};

pub mod cmd_auto;
pub mod cmd_if;
pub mod cmd_try;
pub mod cmd_user;

#[derive(Debug, PartialEq, Clone)]
pub struct DeferCommand {
    defer_: TokenMeta,
    pub call: CallExpression,
}

impl DeferCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Defer(DeferCommand {
            defer_: parser.expect_identifier_with_text("defer")?.into(),
            call: {
                // Parse up to the point it would be a call expr
                let base = Expression::parse(parser, Precedence::Call)
                    .expect("base")
                    .into();

                // Create the call expr from the first base expression
                let right =
                    CallExpression::parse(parser, base).expect("call").into();

                // Closing on right paren, DO NOT advance
                parser
                    .expect_token(TokenKind::RightParen)
                    .expect("rightparen");

                right
            },
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BreakCommand {
    pub br: TokenMeta,
    eol: TokenMeta,
}

impl BreakCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Break(BreakCommand {
            br: parser.expect_identifier_with_text("break")?.into(),
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ContinueCommand {
    pub cont: TokenMeta,
    eol: TokenMeta,
}

impl ContinueCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Continue(ContinueCommand {
            cont: parser.expect_identifier_with_text("continue")?.into(),
            eol: parser.expect_eol()?,
        }))
    }
}
