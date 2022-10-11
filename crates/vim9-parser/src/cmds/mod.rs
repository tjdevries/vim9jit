use anyhow::Result;
use vim9_lexer::{Token, TokenKind};

use crate::{
    Body, CallExpression, ExCommand, Expression, Heredoc, Identifier, Parser,
    Precedence, Signature, Type,
};

pub mod cmd_auto;
pub mod cmd_if;
pub mod cmd_try;
pub mod cmd_user;

#[derive(Debug, PartialEq, Clone)]
pub struct DeferCommand {
    defer_: Token,
    pub call: CallExpression,
}

impl DeferCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Defer(DeferCommand {
            defer_: parser.expect_identifier_with_text("defer")?,
            call: {
                // Parse up to the point it would be a call expr
                let base = Expression::parse(parser, Precedence::Call)
                    .expect("base")
                    .into();

                // Create the call expr from the first base expression
                let right =
                    CallExpression::parse(parser, base).expect("call").into();

                // Closing on right paren, DO NOT advance
                parser
                    .expect_token(TokenKind::RightParen)
                    .expect("rightparen");

                right
            },
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ImportCommand {
    ImportImplicit {
        import: Token,
        autoload: bool,
        file: String,
        name: Option<Expression>,
    },
    ImportUnpacked {
        import: Token,
        names: Vec<Identifier>,
        from: Token,
        file: String,
    },
}

impl ImportCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let import = parser.expect_identifier_with_text("import")?;
        let command = match parser.current_token.kind {
            TokenKind::LeftBrace => ImportCommand::ImportUnpacked {
                import,
                names: {
                    let names =
                        parser.parse_identifier_list(TokenKind::RightBrace)?;
                    parser.expect_peek(TokenKind::RightBrace)?;
                    parser.next_token();
                    names
                },
                from: parser.expect_identifier_with_text("from")?,
                file: parser.pop().text,
            },
            TokenKind::Identifier => {
                let autoload = if parser.current_token.text == "autoload" {
                    parser.next_token();
                    true
                } else {
                    false
                };

                ImportCommand::ImportImplicit {
                    import,
                    autoload,
                    file: parser.pop().text,
                    name: None,
                }
            }
            _ => unreachable!("{:?}", parser),
        };

        Ok(ExCommand::ImportCommand(command))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExportCommand {
    export: Token,
    pub command: Box<ExCommand>,
}

impl ExportCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::ExportCommand(Self {
            export: parser.expect_identifier_with_text("export")?,
            command: parser.parse_command()?.into(),
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SharedCommand {
    pub contents: String,
    eol: Token,
}

impl SharedCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let mut contents = String::new();
        let mut prev_end = 0;
        while parser.current_token.kind != TokenKind::EndOfLine
            && parser.current_token.kind != TokenKind::EndOfFile
        {
            let tok = parser.pop();

            contents += " ".repeat(tok.span.start_col - prev_end).as_str();
            contents += tok.text.as_str();

            prev_end = tok.span.end_col;
        }

        Ok(ExCommand::SharedCommand(SharedCommand {
            contents,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BreakCommand {
    pub br: Token,
    eol: Token,
}

impl BreakCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Break(BreakCommand {
            br: parser.expect_identifier_with_text("break")?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ContinueCommand {
    pub cont: Token,
    eol: Token,
}

impl ContinueCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Continue(ContinueCommand {
            cont: parser.expect_identifier_with_text("continue")?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FinishCommand {
    pub finish: Token,
    eol: Token,
}

impl FinishCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Finish(FinishCommand {
            finish: parser.expect_identifier_with_text("finish")?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Vim9ScriptCommand {
    pub noclear: bool,
    eol: Token,
}

impl Vim9ScriptCommand {
    pub fn parse(parser: &mut Parser) -> Result<Self> {
        Ok(Self {
            noclear: if parser.current_token.kind == TokenKind::EndOfLine {
                false
            } else {
                parser.expect_identifier_with_text("noclear")?;
                true
            },
            eol: parser.expect_eol()?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclCommand {
    var: Token,
    pub name: Identifier,
    pub ty: Option<Type>,
    eol: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarCommand {
    var: Token,
    pub ty: Option<Type>,
    pub name: Identifier,
    equal: Token,
    pub expr: Expression,
    eol: Token,
}

impl VarCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let var = parser.expect_token(TokenKind::Identifier)?;
        anyhow::ensure!(matches!(var.text.as_str(), "var" | "const"));

        let name = Identifier::parse(parser)?;
        let ty = match parser.current_token.kind {
            TokenKind::Equal => None,
            TokenKind::HeredocOperator => None,
            TokenKind::EndOfLine => None,
            TokenKind::EndOfFile => None,
            TokenKind::SpacedColon => Some(Type::parse(parser)?),
            _ => {
                return Err(anyhow::anyhow!(
                    "invalid type and/or equal for var: {:?}",
                    parser
                ))
            }
        };

        match parser.current_token.kind {
            TokenKind::HeredocOperator => {
                let op = parser.expect_token(TokenKind::HeredocOperator)?;

                let mut trim = false;
                let mut eval = false;

                let open = {
                    let mut token =
                        parser.expect_token(TokenKind::Identifier)?;
                    while token.text == "trim" || token.text == "eval" {
                        if token.text == "trim" {
                            trim = true;
                            token =
                                parser.expect_token(TokenKind::Identifier)?;
                        }

                        if token.text == "eval" {
                            eval = true;
                            token =
                                parser.expect_token(TokenKind::Identifier)?;
                        }
                    }

                    token
                };

                parser.expect_eol()?;

                let mut contents = vec![];
                let close = loop {
                    let mut line: Vec<Token> = vec![];
                    while !matches!(
                        parser.current_token.kind,
                        TokenKind::EndOfLine | TokenKind::EndOfFile
                    ) {
                        line.push(parser.current_token.clone());
                        parser.next_token();
                    }

                    if parser.current_token.kind == TokenKind::EndOfFile {
                        panic!("Failed to do the stuffs... {:?}", contents);
                    }

                    parser.next_token();
                    if line.len() == 1 && line[0].text == open.text {
                        break line[0].clone();
                    }

                    // TODO: We might not want to just convert these back to strings?
                    //  we might want to store a list of tokens?
                    let mut line_contents = String::new();
                    let mut prev_end = 0;
                    for tok in line {
                        line_contents +=
                            " ".repeat(tok.span.start_col - prev_end).as_str();
                        line_contents += tok.text.as_str();

                        prev_end = tok.span.end_col;
                    }

                    contents.push(line_contents);
                };

                Ok(ExCommand::Heredoc(Heredoc {
                    var,
                    name,
                    ty,
                    op,
                    trim,
                    eval,
                    open,
                    contents,
                    close,
                }))
            }
            TokenKind::Equal => {
                Ok(ExCommand::Var(VarCommand {
                    var,
                    name,
                    ty,
                    // TODO: Type Hints
                    equal: parser.expect_token(TokenKind::Equal)?,
                    expr: Expression::parse(parser, Precedence::Lowest)?,
                    eol: parser.expect_eol()?,
                }))
            }
            TokenKind::EndOfLine | TokenKind::EndOfFile => {
                Ok(ExCommand::Decl(DeclCommand {
                    var,
                    name,
                    ty,
                    eol: parser.expect_eol()?,
                }))
            }
            _ => Err(anyhow::anyhow!("invalid next character: {:?}", parser)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct DefCommand {
    def: Token,
    pub name: Identifier,
    pub args: Signature,
    pub ret: Option<Type>,
    def_eol: Token,
    pub body: Body,
    enddef: Token,
    end_eol: Token,
}

impl DefCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Def(DefCommand {
            def: parser.expect_identifier_with_text("def")?,
            name: Identifier::parse(parser)?,
            args: Signature::parse(parser)?,
            ret: {
                if parser.current_token.kind == TokenKind::SpacedColon {
                    Some(Type::parse(parser)?)
                } else {
                    None
                }
            },
            def_eol: parser.expect_eol()?,
            body: Body::parse_until(parser, "enddef")?,
            enddef: parser.expect_identifier_with_text("enddef")?,
            end_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EvalCommand {
    eval: Option<Token>,
    pub expr: Expression,
    eol: Token,
}

impl EvalCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Eval(EvalCommand {
            eval: None,
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallCommand {
    call: Option<Token>,

    // TODO: Turn this into pub expr: Expression
    pub name: Identifier,
    open: Token,
    pub args: Vec<Expression>,
    close: Token,
    eol: Token,
}

impl CallCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Call(CallCommand {
            call: parser.expect_identifier_with_text("call").ok(),
            name: Identifier::parse(parser)?,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen, true)?,
            close: parser.expect_token(TokenKind::RightParen)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct EchoCommand {
    echo: Token,
    pub expr: Expression,
    eol: Token,
}

impl EchoCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Echo(EchoCommand {
            echo: parser.expect_identifier_with_text("echo")?,
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnCommand {
    ret: Token,
    pub expr: Option<Expression>,
    eol: Token,
}

impl ReturnCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Return(Self {
            ret: parser
                .expect_token_with_text(TokenKind::Identifier, "return")?,
            expr: match parser.current_token.kind {
                TokenKind::EndOfLine => None,
                _ => Some(Expression::parse(parser, Precedence::Lowest)?),
            },
            eol: parser.expect_eol()?,
        }))
    }

    pub fn fake(expr: Option<Expression>) -> Self {
        Self {
            ret: Token::fake(),
            expr,
            eol: Token::fake(),
        }
    }
}
