use anyhow::Result;
use vim9_lexer::{Token, TokenKind};

use crate::{Block, Body, ExCommand, Literal, Parser};

#[derive(Debug, PartialEq, Clone)]
pub struct AugroupCommand {
    augroup: Token,
    pub augroup_name: Literal,
    augroup_eol: Token,
    pub body: Body,
    augroup_end: Token,
    augroup_end_name: Token,
    augroup_end_eol: Token,
}

impl AugroupCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Augroup(AugroupCommand {
            augroup: parser.expect_identifier_with_text("augroup")?,
            augroup_name: parser
                .expect_token(TokenKind::Identifier)?
                .try_into()?,
            augroup_eol: parser.expect_eol()?,
            // TODO: This should be until augroup END, unless you can't have nested ones legally
            body: Body::parse_until(parser, "augroup")?,
            augroup_end: parser.expect_identifier_with_text("augroup")?,
            augroup_end_name: parser.expect_identifier_with_text("END")?,
            augroup_end_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AutocmdCommand {
    autocmd: Token,
    pub bang: bool,
    pub events: Vec<Literal>,
    pub pattern: AutocmdPattern,
    pub block: AutocmdBlock,
}

#[derive(Debug, PartialEq, Clone)]
pub enum AutocmdPattern {
    Pattern(Vec<String>),
    Buffer,
}

fn tokens_are_neighbors(left: &Token, right: &Token) -> bool {
    left.span.end_row == right.span.start_row
        && left.span.end_col == right.span.start_col
}

impl AutocmdCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Autocmd(AutocmdCommand {
            // TODO: Accept au! for example
            autocmd: parser.expect_identifier_with_text("autocmd")?,
            bang: if parser.current_token.kind == TokenKind::Bang {
                parser.next_token();
                true
            } else {
                false
            },
            events: {
                let mut events = vec![];

                loop {
                    events.push(parser.pop().try_into()?);
                    if parser.current_token.kind != TokenKind::Comma {
                        break;
                    }

                    parser.next_token();
                }

                events
            },
            pattern: AutocmdPattern::parse(parser)?,
            block: AutocmdBlock::parse(parser)?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AutocmdBlock {
    Command(Box<ExCommand>),
    Block(Block),
}

impl AutocmdBlock {
    pub fn parse(parser: &mut Parser) -> Result<AutocmdBlock> {
        Ok(match parser.current_token.kind {
            TokenKind::LeftBrace => AutocmdBlock::Block(Block::parse(parser)?),
            _ => AutocmdBlock::Command(parser.parse_command()?.into()),
        })
    }
}
impl AutocmdPattern {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(if parser.current_token.kind == TokenKind::AngleLeft {
            parser.read_until(|t| {
                matches!(t.kind, TokenKind::AngleRight | TokenKind::GreaterThan)
            });

            AutocmdPattern::Buffer
        } else {
            AutocmdPattern::Pattern({
                let mut pattern = vec![];
                let mut text = String::new();

                loop {
                    let tok = parser.pop();

                    // Commas split different patterns
                    if tok.kind == TokenKind::Comma {
                        pattern.push(std::mem::take(&mut text));
                        continue;
                    }

                    // Append the text of the token.
                    text += tok.text.as_str();

                    if !tokens_are_neighbors(&tok, &parser.current_token) {
                        pattern.push(std::mem::take(&mut text));
                        break pattern;
                    }
                }
            })
        })
    }
}
