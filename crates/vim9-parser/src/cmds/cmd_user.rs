use anyhow::Result;
use vim9_lexer::{Token, TokenKind};

use crate::{ExCommand, Parser};

// TODO:
// - consider how script vars are supposed to work in this context

#[derive(Debug, PartialEq, Clone)]
pub struct UserCommand {
    tok: Token,
    pub bang: bool,
    pub command_bang: bool,
    pub command_bar: bool,
    pub command_keepscript: bool,
    pub command_register: Option<String>,
    pub command_nargs: Option<String>,
    pub command_compl: Option<String>,
    pub command_range: Option<String>,
    pub command_addr: Option<String>,
    pub command_complete: Option<String>,
    pub name: String,
    pub command: Box<ExCommand>,
}

impl UserCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let tok = parser.expect_identifier_with_text("command")?;
        let bang = parser.consume_if_kind(TokenKind::Bang).is_some();

        let mut command_bang = false;
        let mut command_bar = false;
        let mut command_nargs = None;
        let mut command_complete = None;
        while parser.current_token.kind == TokenKind::Minus {
            parser.next_token();
            parser.ensure_token(TokenKind::Identifier)?;

            match parser.pop().text.as_ref() {
                "bar" => {
                    command_bar = true;
                }
                "bang" => {
                    command_bang = true;
                }
                "nargs" => {
                    parser.expect_token(TokenKind::Equal)?;
                    command_nargs = Some(parser.pop().text);
                }
                "complete" => {
                    parser.expect_token(TokenKind::Equal)?;
                    command_complete = Some(parser.pop().text);
                }
                _ => panic!("OH NO"),
            }
        }

        Ok(ExCommand::UserCommand(UserCommand {
            tok,
            bang,
            command_bang,
            command_bar,
            command_nargs,
            command_complete,
            name: parser.expect_token(TokenKind::Identifier)?.text,
            command: parser.parse_command()?.into(),
            command_keepscript: false,
            command_register: None,
            command_compl: None,
            command_range: None,
            command_addr: None,
        }))
    }
}
