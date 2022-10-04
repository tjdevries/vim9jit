use std::collections::HashSet;

use anyhow::Result;
use vim9_lexer::Token;

use crate::{Body, ExCommand, Expression, Parser, Precedence};

#[derive(Debug, PartialEq, Clone)]
pub struct IfCommand {
    if_tok: Token,
    pub condition: Expression,
    if_eol: Token,
    pub body: Body,
    pub elseifs: Vec<ElseIfCommand>,
    pub else_command: Option<ElseCommand>,
    endif_tok: Token,
    endif_eol: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseIfCommand {
    elseif_tok: Token,
    pub condition: Expression,
    elseif_eol: Token,
    pub body: Body,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ElseCommand {
    else_tok: Token,
    else_eol: Token,
    pub body: Body,
}

impl IfCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let if_endings: HashSet<String> = HashSet::from_iter(
            vec![
                "elseif".to_string(),
                "else".to_string(),
                "endif".to_string(),
            ]
            .into_iter(),
        );

        Ok(ExCommand::If(IfCommand {
            if_tok: parser.expect_identifier_with_text("if")?,
            condition: Expression::parse(parser, Precedence::Lowest)?,
            if_eol: parser.expect_eol()?,
            body: Body::parse_until_any(parser, &if_endings)?,
            elseifs: {
                let mut elseifs = Vec::new();
                while parser.current_token.text == "elseif" {
                    elseifs.push(ElseIfCommand {
                        elseif_tok: parser.pop(),
                        condition: Expression::parse(
                            parser,
                            Precedence::Lowest,
                        )?,
                        elseif_eol: parser.expect_eol()?,
                        body: Body::parse_until_any(parser, &if_endings)?,
                    })
                }

                elseifs
            },
            else_command: {
                if parser.current_token.text == "else" {
                    Some(ElseCommand {
                        else_tok: parser.pop(),
                        else_eol: parser.expect_eol()?,
                        body: Body::parse_until(parser, "endif")?,
                    })
                } else {
                    None
                }
            },
            endif_tok: parser.expect_identifier_with_text("endif")?,
            endif_eol: parser.expect_eol()?,
        }))
    }
}
