use std::collections::HashSet;

use anyhow::Result;
use vim9_lexer::Token;

use crate::{Body, ExCommand, Expression, Parser};

#[derive(Debug, PartialEq, Clone)]
pub struct TryCommand {
    try_: Token,
    try_eol: Token,
    pub body: Body,
    pub catch: Option<CatchCommand>,
    pub finally: Option<FinallyCommand>,
    endtry_: Token,
    endtry_eol: Token,
}

impl TryCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let try_endings: HashSet<String> = HashSet::from_iter(
            vec![
                "catch".to_string(),
                "finally".to_string(),
                "endtry".to_string(),
            ]
            .into_iter(),
        );

        Ok(ExCommand::Try(TryCommand {
            try_: parser.expect_identifier_with_text("try")?,
            try_eol: parser.expect_eol()?,
            body: Body::parse_until_any(parser, &try_endings)?,
            catch: {
                if parser.current_token.text.as_str() == "catch" {
                    Some(CatchCommand {
                        catch_: parser.pop(),
                        expr: None,
                        catch_eol: parser.expect_eol()?,
                        body: Body::parse_until_any(parser, &try_endings)?,
                    })
                } else {
                    None
                }
            },
            finally: None,
            endtry_: parser.expect_identifier_with_text("endtry")?,
            endtry_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CatchCommand {
    catch_: Token,
    pub expr: Option<Expression>,
    catch_eol: Token,
    pub body: Body,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FinallyCommand {}
