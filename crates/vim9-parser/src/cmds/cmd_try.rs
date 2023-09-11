use std::collections::HashSet;

use anyhow::Result;

use crate::{Body, ExCommand, Expression, Parser, TokenMeta};

#[derive(Debug, PartialEq, Clone)]
pub struct TryCommand {
    try_: TokenMeta,
    try_eol: TokenMeta,
    pub body: Body,
    pub catch: Option<CatchCommand>,
    pub finally: Option<FinallyCommand>,
    endtry_: TokenMeta,
    endtry_eol: TokenMeta,
}

impl TryCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let try_endings: HashSet<String> = HashSet::from_iter(
            vec![
                "catch".to_string(),
                "finally".to_string(),
                "endtry".to_string(),
            ],
        );

        Ok(ExCommand::Try(TryCommand {
            try_: parser.expect_identifier_with_text("try")?.into(),
            try_eol: parser.expect_eol()?,
            body: Body::parse_until_any(parser, &try_endings)?,
            catch: {
                if parser.front_text().eq("catch") {
                    Some(CatchCommand {
                        catch_: parser.pop().into(),
                        expr: None,
                        catch_eol: parser.expect_eol()?,
                        body: Body::parse_until_any(parser, &try_endings)?,
                    })
                } else {
                    None
                }
            },
            finally: None,
            endtry_: parser.expect_identifier_with_text("endtry")?.into(),
            endtry_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchCommand {
    catch_: TokenMeta,
    pub expr: Option<Expression>,
    catch_eol: TokenMeta,
    pub body: Body,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FinallyCommand {}
