use anyhow::Result;
use vim9_lexer::{Token, TokenKind};

use crate::{Literal, Parser, TokenMeta};

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Any,
    Bool,
    Number,
    Float,
    String,
    Blob,
    List {
        open: TokenMeta,
        inner: Box<Type>,
        close: TokenMeta,
    },
    Dict {
        open: TokenMeta,
        inner: Box<Type>,
        close: TokenMeta,
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
    fn open(k: &TokenKind) -> bool {
        matches!(k, TokenKind::LessThan | TokenKind::AngleLeft)
    }

    fn close(k: &TokenKind) -> bool {
        matches!(k, TokenKind::GreaterThan | TokenKind::AngleRight)
    }

    fn parse_inner(parser: &Parser, consume: bool) -> Result<Type> {
        match parser.front_kind() {
            TokenKind::Identifier => {
                let literal: Literal = match consume {
                    true => parser.pop(),
                    false => parser.front_owned(),
                }
                .try_into()?;

                Ok(match literal.token.text.as_str() {
                    "any" => Type::Any,
                    "bool" => Type::Bool,
                    "number" => Type::Number,
                    "void" => Type::Void,
                    "string" => Type::String,
                    "float" => Type::Float,
                    "list" => {
                        if !consume {
                            parser.pop();
                        }

                        Type::List {
                            open: parser.expect_fn(Self::open, true)?.into(),
                            inner: Type::parse_inner(parser, true)?.into(),
                            close: parser
                                .expect_fn(Self::close, consume)?
                                .into(),
                        }
                    }
                    "dict" => {
                        if !consume {
                            parser.pop();
                        }

                        Type::Dict {
                            open: parser.expect_fn(Self::open, true)?.into(),
                            inner: Type::parse_inner(parser, true)?.into(),
                            close: parser
                                .expect_fn(Self::close, consume)?
                                .into(),
                        }
                    }
                    "func" => Type::Func(InnerFuncType::Naked),
                    "job" => Type::Job,
                    "channel" => Type::Channel,
                    _ => todo!("{:?}", literal.token),
                })
            }
            _ => unreachable!("should probably return an error"),
        }
    }

    pub fn parse_in_expression(parser: &Parser) -> Result<Type> {
        parser.expect_token(TokenKind::SpacedColon)?;
        Self::parse_inner(parser, false)
    }

    pub fn parse(parser: &Parser) -> Result<Type> {
        parser.expect_token(TokenKind::SpacedColon)?;
        Self::parse_inner(parser, true)
    }
}
