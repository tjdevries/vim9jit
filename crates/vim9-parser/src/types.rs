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
    Dict {
        open: Token,
        inner: Box<InnerType>,
        close: Token,
    },
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
    fn match_open(k: &TokenKind) -> bool {
        matches!(k, TokenKind::LessThan | TokenKind::AngleLeft)
    }

    fn match_close(k: &TokenKind) -> bool {
        matches!(k, TokenKind::GreaterThan | TokenKind::AngleRight)
    }

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
                    "float" => InnerType::Float,
                    "list" => InnerType::List {
                        open: parser.expect_fn(Self::match_open, true)?,
                        inner: InnerType::parse(parser)?.into(),
                        close: parser.expect_fn(Self::match_close, true)?,
                    },
                    "dict" => InnerType::Dict {
                        open: parser.expect_fn(Self::match_open, true)?,
                        inner: InnerType::parse(parser)?.into(),
                        close: parser.expect_fn(Self::match_close, true)?,
                    },
                    "func" => InnerType::Func(InnerFuncType::Naked),
                    _ => todo!("{:?}", literal.token),
                })
            }
            _ => unreachable!("should probably return an error"),
        }
    }
}
