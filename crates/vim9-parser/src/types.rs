use anyhow::Result;
use vim9_lexer::{Token, TokenKind};

use crate::{Literal, Parser};

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    colon: Token,
    pub inner: InnerType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InnerType {
    Any,
    Bool,
    Number,
    Float,
    String,
    Blob,
    List {
        open: Token,
        inner: Box<InnerType>,
        close: Token,
    },
    Dict(Box<InnerType>),
    Job,
    Channel,
    Func(InnerFuncType),
    Void,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InnerFuncType {
    Naked,
}

impl Type {
    pub fn parse(parser: &mut Parser) -> Result<Type> {
        Ok(Type {
            colon: parser.expect_token(TokenKind::SpacedColon)?,
            inner: InnerType::parse(parser)?,
        })
    }
}

impl InnerType {
    pub fn parse(parser: &mut Parser) -> Result<InnerType> {
        match parser.current_token.kind {
            TokenKind::Identifier => {
                let literal: Literal = parser.pop().try_into()?;
                Ok(match literal.token.text.as_str() {
                    "any" => InnerType::Any,
                    "bool" => InnerType::Bool,
                    "number" => InnerType::Number,
                    "void" => InnerType::Void,
                    "string" => InnerType::String,
                    "list" => InnerType::List {
                        open: parser.expect_fn(
                            |k| {
                                matches!(
                                    k,
                                    TokenKind::LessThan | TokenKind::AngleLeft
                                )
                            },
                            true,
                        )?,
                        inner: InnerType::parse(parser)?.into(),
                        close: parser.expect_fn(
                            |k| {
                                matches!(
                                    k,
                                    TokenKind::GreaterThan
                                        | TokenKind::AngleRight
                                )
                            },
                            true,
                        )?,
                    },
                    "func" => InnerType::Func(InnerFuncType::Naked),
                    _ => todo!("{:?}", literal.token),
                })
            }
            _ => unreachable!("should probably return an error"),
        }
    }
}
