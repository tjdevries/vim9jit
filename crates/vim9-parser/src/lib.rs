#![allow(dead_code)]

use anyhow::Result;
use vim9_lexer::new_lexer;
use vim9_lexer::Lexer;
use vim9_lexer::Token;
use vim9_lexer::TokenKind;

#[derive(Debug)]
pub struct Program {
    commands: Vec<ExCommand>,
}

#[derive(Debug, PartialEq)]
pub enum ExCommand {
    Vim9Script(Vim9ScriptCommand),
    Var(VarCommand),
    Return(ReturnCommand),
    Def(DefCommand),
    Call(CallCommand),

    Empty,
    NoOp(Token),
}

#[derive(Debug, PartialEq)]
pub struct Vim9ScriptCommand {
    noclear: bool,
    eol: Token,
}

#[derive(Debug, PartialEq)]
pub struct VarCommand {
    var: Token,
    pub name: Name,
    equal: Token,
    pub expr: Expression,
    eol: Token,
}

impl VarCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let var = VarCommand {
            var: parser.expect_token_with_text(TokenKind::Identifier, "var")?,
            name: Name::parse(parser)?,
            // TODO: Type Hints
            equal: parser.expect_token(TokenKind::Equal)?,
            expr: Expression::parse(parser)?,
            eol: parser.expect_eol()?,
        };

        Ok(ExCommand::Var(var))
    }
}

#[derive(Debug, PartialEq)]
pub struct DefCommand {
    def: Token,
    pub name: Name,
    pub args: Signature,
    pub ret: Option<Type>,
    def_eol: Token,
    pub body: Vec<ExCommand>,
    enddef: Token,
    end_eol: Token,
}

impl DefCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Def(DefCommand {
            def: parser.expect_identifier_with_text("def")?,
            name: Name::parse(parser)?,
            args: Signature::parse(parser)?,
            ret: {
                if parser.current_token.kind == TokenKind::Colon {
                    Some(Type {
                        colon: parser.expect_token(TokenKind::Colon)?,
                        name: Name::parse(parser)?,
                    })
                } else {
                    None
                }
            },
            def_eol: parser.expect_eol()?,
            body: {
                let mut body = vec![];
                while parser.current_token.text != "enddef" {
                    body.push(parser.parse_command()?);
                }

                // dbg!(parser.next_token());
                body
            },
            enddef: parser.expect_identifier_with_text("enddef")?,
            end_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct CallCommand {
    call: Option<Token>,
    pub name: Name,
    pub args: Arguments,
    eol: Token,
}

impl CallCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Call(CallCommand {
            call: None,
            name: Name::parse(parser)?,
            args: Arguments {
                open: parser.expect_token(TokenKind::LeftParen)?,
                args: vec![],
                close: parser.expect_token(TokenKind::RightParen)?,
            },
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub struct Arguments {
    open: Token,
    pub args: Vec<Expression>,
    close: Token,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    colon: Token,
    pub name: Name,
}

#[derive(Debug, PartialEq)]
pub struct Signature {
    open: Token,
    close: Token,
}

impl Signature {
    fn parse(parser: &mut Parser) -> Result<Signature> {
        Ok(Self {
            open: parser.expect_token(TokenKind::LeftParen)?,
            close: parser.expect_token(TokenKind::RightParen)?,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Argument {
    pub name: Name,
    colon: Token,
    // pub typ: ,
}

#[derive(Debug, PartialEq)]
pub struct ReturnCommand {
    ret: Token,
    pub expr: Expression,
    eol: Token,
}

impl ReturnCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Return(Self {
            ret: parser.expect_token_with_text(TokenKind::Identifier, "return")?,
            expr: Expression::parse(parser)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub enum Name {
    Identifier(RawIdentifier),
    ScopedIdentifier,
}

impl Name {
    fn parse(parser: &mut Parser) -> Result<Name> {
        // Todo: Other names
        Ok(Name::Identifier(RawIdentifier {
            name: parser.expect_token(TokenKind::Identifier)?.text,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Name,
    Number(Number),
}

impl Expression {
    pub fn parse(parser: &mut Parser) -> Result<Expression> {
        println!("Expression Parsing {:?}", parser);
        Ok(match &parser.current_token.kind {
            TokenKind::Integer => Expression::Number(Number {
                value: parser.next_token().text,
            }),
            _ => todo!("expresson parse: {:?}", parser.current_token),
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct RawIdentifier {
    name: String,
}

#[derive(Debug, PartialEq)]
pub struct Number {
    // ??? should this be a number?
    value: String,
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
        }
    }

    pub fn expect_eol(&mut self) -> Result<Token> {
        self.expect_token(TokenKind::EndOfLine)
    }

    pub fn expect_token(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.current_token.clone();
        if token.kind != kind {
            return Err(anyhow::anyhow!("Got token: {:?}, Expected: {:?}", token, kind));
        }

        self.next_token();
        Ok(token)
    }

    pub fn expect_token_with_text(&mut self, kind: TokenKind, text: &str) -> Result<Token> {
        let token = self.current_token.clone();
        if token.kind != kind {
            panic!("Got token: {:?}, Expected kind: {:?}", token, kind);
            return Err(anyhow::anyhow!("Got token: {:?}, Expected kind: {:?}", token, kind));
        }

        if token.text != text {
            panic!("Got token: {:?}, Expected text: {:?}", token, text);
            return Err(anyhow::anyhow!("Got token: {:?}, Expected text: {:?}", token, text));
        }

        self.next_token();
        Ok(token)
    }

    pub fn peek_identifier_with_text(&self, text: &str) -> bool {
        self.peek_token.kind == TokenKind::Identifier && self.peek_token.text == text
    }

    pub fn expect_identifier_with_text(&mut self, text: &str) -> Result<Token> {
        self.expect_token_with_text(TokenKind::Identifier, text)
    }

    pub fn next_token(&mut self) -> Token {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.current_token.clone()
    }

    fn command_match(&self, full: &str) -> bool {
        self.current_token.text == full
    }

    pub fn parse_command(&mut self) -> Result<ExCommand> {
        println!("PARSING COMMAND: {:#?}", self);
        Ok(match &self.current_token.kind {
            TokenKind::EndOfFile => panic!("EndOfFile!"),
            TokenKind::EndOfLine => {
                self.next_token();
                ExCommand::Empty
            }
            TokenKind::Identifier => {
                if self.command_match("vim9script") {
                    self.next_token();
                    ExCommand::Vim9Script(Vim9ScriptCommand {
                        noclear: if self.current_token.kind == TokenKind::EndOfLine {
                            false
                        } else {
                            self.expect_identifier_with_text("noclear")?;
                            true
                        },
                        eol: self.expect_eol()?,
                    })
                } else if self.command_match("var") {
                    VarCommand::parse(self)?
                } else if self.command_match("def") {
                    DefCommand::parse(self)?
                } else if self.command_match("return") {
                    let ret = ReturnCommand::parse(self)?;
                    // panic!("OH NO RETURN TIME: {:?}", ret);
                    ret
                } else {
                    if self.peek_token.kind == TokenKind::LeftParen {
                        CallCommand::parse(self)?
                    } else {
                        ExCommand::NoOp(self.next_token())
                    }
                }
            }
            _ => ExCommand::NoOp(self.next_token()),
        })
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { commands: vec![] };

        while self.current_token.kind != TokenKind::EndOfFile {
            println!("Parsing Command: {:#?}", self);
            println!("  Commands: {:#?}", program.commands);

            let command = self.parse_command().unwrap();
            if command != ExCommand::Empty {
                program.commands.push(command);
            }

            self.next_token();
        }

        program
    }
}

fn snapshot_parsing(input: &str) -> String {
    let lexer = new_lexer(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    format!("{:#?}", program.commands)
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.bind(|| {
                    insta::assert_snapshot!(snapshot_parsing(contents));
                });
            }
        };
    }

    snapshot!(test_var, "../testdata/snapshots/simple_var.vim");
    snapshot!(test_ret, "../testdata/snapshots/simple_ret.vim");

    // TODO: Slowly but surely, we can work towards this
    // snapshot!(test_matchparen, "../testdata/snapshots/matchparen.vim");
}
