use anyhow::Result;
use vim9_lexer::TokenKind;

use crate::{Literal, Parser, TokenMeta};

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    colon: TokenMeta,
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
        open: TokenMeta,
        inner: Box<InnerType>,
        close: TokenMeta,
    },
    Dict {
        open: TokenMeta,
        inner: Box<InnerType>,
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
    pub fn parse_in_expression(parser: &Parser) -> Result<Type> {
        Ok(Type {
            colon: parser.expect_token(TokenKind::SpacedColon)?.into(),
            inner: InnerType::parse(parser, false)?,
        })
    }

    pub fn parse(parser: &Parser) -> Result<Type> {
        let res = Self::parse_in_expression(parser)?;
        parser.next_token();

        Ok(res)
    }
}

impl InnerType {
    fn open(k: &TokenKind) -> bool {
        matches!(k, TokenKind::LessThan | TokenKind::AngleLeft)
    }

    fn close(k: &TokenKind) -> bool {
        matches!(k, TokenKind::GreaterThan | TokenKind::AngleRight)
    }

    fn parse(parser: &Parser, consume: bool) -> Result<InnerType> {
        match parser.front_kind() {
            TokenKind::Identifier => {
                let literal: Literal = match consume {
                    true => parser.pop(),
                    false => parser.front_owned(),
                }
                .try_into()?;

                Ok(match literal.token.text.as_str() {
                    "any" => InnerType::Any,
                    "bool" => InnerType::Bool,
                    "number" => InnerType::Number,
                    "void" => InnerType::Void,
                    "string" => InnerType::String,
                    "float" => InnerType::Float,
                    "list" => {
                        if !consume {
                            parser.pop();
                        }

                        InnerType::List {
                            open: parser.expect_fn(Self::open, true)?.into(),
                            inner: InnerType::parse(parser, true)?.into(),
                            close: parser
                                .expect_fn(Self::close, consume)?
                                .into(),
                        }
                    }
                    "dict" => {
                        if !consume {
                            parser.pop();
                        }

                        InnerType::Dict {
                            open: parser.expect_fn(Self::open, true)?.into(),
                            inner: InnerType::parse(parser, true)?.into(),
                            close: parser
                                .expect_fn(Self::close, consume)?
                                .into(),
                        }
                    }
                    "func" => InnerType::Func(InnerFuncType::Naked),
                    "job" => InnerType::Job,
                    "channel" => InnerType::Channel,
                    _ => todo!("{:?}", literal.token),
                })
            }
            _ => unreachable!("should probably return an error"),
        }
    }
}
