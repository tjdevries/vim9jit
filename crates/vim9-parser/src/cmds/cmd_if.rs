use std::collections::HashSet;

use anyhow::Result;

use crate::{Body, ExCommand, Expression, Parser, Precedence, TokenMeta};

#[derive(Debug, PartialEq, Clone)]
pub struct IfCommand {
    if_tok: TokenMeta,
    pub condition: Expression,
    if_eol: TokenMeta,
    pub body: Body,
    pub elseifs: Vec<ElseIfCommand>,
    pub else_command: Option<ElseCommand>,
    endif_tok: TokenMeta,
    endif_eol: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseIfCommand {
    elseif_tok: TokenMeta,
    pub condition: Expression,
    elseif_eol: TokenMeta,
    pub body: Body,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseCommand {
    else_tok: TokenMeta,
    else_eol: TokenMeta,
    pub body: Body,
}

impl IfCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let if_endings: HashSet<String> = HashSet::from_iter(vec![
            "elseif".to_string(),
            "else".to_string(),
            "endif".to_string(),
        ]);

        Ok(ExCommand::If(IfCommand {
            if_tok: parser.expect_identifier_with_text("if")?.into(),
            condition: Expression::parse(parser, Precedence::Lowest)?,
            if_eol: parser.expect_eol()?,
            body: Body::parse_until_any(parser, &if_endings)?,
            elseifs: {
                let mut elseifs = Vec::new();
                while parser.front_ref().text.equals("elseif") {
                    elseifs.push(ElseIfCommand {
                        elseif_tok: parser.pop().into(),
                        condition: Expression::parse(parser, Precedence::Lowest)?,
                        elseif_eol: parser.expect_eol()?,
                        body: Body::parse_until_any(parser, &if_endings)?,
                    })
                }

                elseifs
            },
            else_command: {
                if parser.front_ref().text.equals("else") {
                    Some(ElseCommand {
                        else_tok: parser.pop().into(),
                        else_eol: parser.expect_eol()?,
                        body: Body::parse_until(parser, "endif")?,
                    })
                } else {
                    None
                }
            },
            endif_tok: parser.expect_identifier_with_text("endif")?.into(),
            endif_eol: parser.expect_eol()?,
        }))
    }
}
