use anyhow::Result;
use vim9_lexer::TokenKind;

use crate::{Literal, Parser, TokenMeta};

pub struct TypeOpts {
    pub bool: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Any,

    // TODO: Decide what to do with these.
    //  It's possible we can just dismiss
    //  BoolOrNumber, but i'm not 100% sure.
    //
    //  The way we could do that would be to
    //  just "not not" everything that comes from
    //  vimland... but I feel like BoolOrNumber makes
    //  that better by only doing it when we see a BoolOrNumber
    Bool,
    BoolOrNumber,

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

    fn parse_inner(parser: &Parser, consume: bool, opts: &TypeOpts) -> Result<Type> {
        match parser.front_kind() {
            TokenKind::Identifier => {
                let literal: Literal = match consume {
                    true => parser.pop(),
                    false => parser.front_owned(),
                }
                .try_into()?;

                Ok(match literal.token.text.as_str() {
                    "any" => Type::Any,
                    // "bool" => Type::BoolOrNumber,
                    "bool" => opts.bool.clone(),
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
                            inner: Type::parse_inner(parser, true, opts)?.into(),
                            close: parser.expect_fn(Self::close, consume)?.into(),
                        }
                    }
                    "dict" => {
                        if !consume {
                            parser.pop();
                        }

                        Type::Dict {
                            open: parser.expect_fn(Self::open, true)?.into(),
                            inner: Type::parse_inner(parser, true, opts)?.into(),
                            close: parser.expect_fn(Self::close, consume)?.into(),
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

    pub fn parse_in_expression(parser: &Parser, opts: &TypeOpts) -> Result<Type> {
        parser.expect_token(TokenKind::SpacedColon)?;
        Self::parse_inner(parser, false, opts)
    }

    pub fn parse(parser: &Parser, opts: &TypeOpts) -> Result<Type> {
        parser.expect_token(TokenKind::SpacedColon)?;
        Self::parse_inner(parser, true, opts)
    }
}
