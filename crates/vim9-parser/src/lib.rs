#![allow(unused_variables)]
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
    Echo(EchoCommand),
    Return(ReturnCommand),
    Def(DefCommand),
    If(IfCommand),
    Call(CallCommand),
    Finish(Token),

    Skip,
    EndOfFile,
    Comment(Token),
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
    pub name: Identifier,
    equal: Token,
    pub expr: Expression,
    eol: Token,
}

impl VarCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        let var = VarCommand {
            var: parser.expect_token_with_text(TokenKind::Identifier, "var")?,
            name: Identifier::parse(parser)?,
            // TODO: Type Hints
            equal: parser.expect_token(TokenKind::Equal)?,
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        };

        Ok(ExCommand::Var(var))
    }
}

#[derive(Debug, PartialEq)]
pub struct Body {
    pub commands: Vec<ExCommand>,
}

impl Body {
    pub fn parse_until(parser: &mut Parser, identifier: &str) -> Result<Body> {
        let mut commands = vec![];
        while parser.current_token.text != identifier {
            commands.push(parser.parse_command()?);
        }

        Ok(Body { commands })
    }
}

#[derive(Debug, PartialEq)]
pub struct IfCommand {
    if_tok: Token,
    pub condition: Expression,
    if_eol: Token,
    pub body: Body,
    endif_tok: Token,
    endif_eol: Token,
}

impl IfCommand {
    pub fn parser(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::If(IfCommand {
            if_tok: parser.expect_identifier_with_text("if")?,
            condition: Expression::parse(parser, Precedence::Lowest)?,
            if_eol: parser.expect_eol()?,
            body: Body::parse_until(parser, "endif")?,
            endif_tok: parser.expect_identifier_with_text("endif")?,
            endif_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq)]
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
                if parser.current_token.kind == TokenKind::Colon {
                    Some(Type {
                        colon: parser.expect_token(TokenKind::Colon)?,
                        name: Identifier::parse(parser)?,
                    })
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

#[derive(Debug, PartialEq)]
pub struct CallCommand {
    call: Option<Token>,

    // TODO: Turn this into pub expr: Expression
    pub name: Identifier,
    pub args: Arguments,
    eol: Token,
}

impl CallCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Call(CallCommand {
            call: None,
            name: Identifier::parse(parser)?,
            args: Arguments::parse(parser)?,
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

impl Arguments {
    pub fn parse(parser: &mut Parser) -> Result<Arguments> {
        Ok(Arguments {
            open: parser.expect_token(TokenKind::LeftParen)?,
            args: {
                let mut args = vec![];
                while parser.current_token.kind != TokenKind::RightParen {
                    args.push(Expression::parse(parser, Precedence::Lowest)?);

                    // TODO: Consume ','?
                }

                args
            },
            close: parser.expect_token(TokenKind::RightParen)?,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Type {
    colon: Token,
    pub name: Identifier,
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
pub struct Parameter {
    pub name: Identifier,
    colon: Token,
    // pub typ: ,
}

#[derive(Debug, PartialEq)]
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
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub enum Identifier {
    Raw(RawIdentifier),
    ScopedIdentifier,
}

impl Identifier {
    fn parse(parser: &mut Parser) -> Result<Identifier> {
        // Todo: Other names
        Ok(Identifier::Raw(RawIdentifier {
            name: parser.expect_token(TokenKind::Identifier)?.text,
        }))
    }
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Empty,
    Identifier(Identifier),
    Number(VimNumber),
    String(VimString),
    Boolean(VimBoolean),
    Grouped(GroupedExpression),
    Call(CallExpression),

    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub expr: Box<Expression>,
    open: Token,
    pub args: Vec<Expression>,
    close: Token,
}

#[derive(Debug, PartialEq)]
pub struct GroupedExpression {
    open: Token,
    pub expr: Box<Expression>,
    close: Token,
}

#[derive(Debug, PartialEq)]
pub struct VimBoolean {
    token: Token,
    pub value: bool,
}

type PrefixFn = Box<dyn Fn(&mut Parser) -> Result<Expression>>;

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    token: Token,
    pub operator: Operator,
    pub right: Box<Expression>,
}

type InfixFn = Box<dyn Fn(&mut Parser, Box<Expression>) -> Result<Expression>>;

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    token: Token,
    pub operator: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct VimNumber {
    value: String,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Bang,
}

#[derive(Debug, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

// The parseIdentifier method doesn’t do a lot. It only returns a *ast.Identifier with the current
// token in the Token field and the literal value of the token in Value. It doesn’t advance the
// tokens, it doesn’t call nextToken. That’s important. All of our parsing functions, prefixParseFn
// or infixParseFn, are going to follow this protocol: start with curToken being the type of token
// you’re associated with and return with curToken being the last token that’s part of your
// expression type. Never advance the tokens too far.

impl Expression {
    pub fn parse(parser: &mut Parser, prec: Precedence) -> Result<Expression> {
        let expr = parser.parse_expression(prec);
        parser.next_token();

        expr
    }
}

#[derive(Debug, PartialEq)]
pub enum VimString {
    SingleQuote(String),
    DoubleQuote(String),
}

#[derive(Debug, PartialEq)]
pub struct RawIdentifier {
    name: String,
}

impl Into<Expression> for RawIdentifier {
    fn into(self) -> Expression {
        Expression::Identifier(Identifier::Raw(self))
    }
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
}

mod prefix_expr {
    use super::*;

    pub fn parse_vim_number(parser: &mut Parser) -> Result<Expression> {
        anyhow::ensure!(parser.current_token.kind == TokenKind::Integer);

        Ok(Expression::Number(VimNumber {
            value: parser.current_token.text.clone(),
        }))
    }

    pub fn parse_identifier(parser: &mut Parser) -> Result<Expression> {
        anyhow::ensure!(parser.current_token.kind == TokenKind::Identifier);

        Ok(RawIdentifier {
            name: parser.current_token.text.clone(),
        }
        .into())
    }

    pub fn parse_bool(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::Boolean(VimBoolean {
            token: parser.current_token.clone(),
            value: match &parser.current_token.kind {
                TokenKind::True => true,
                TokenKind::False => false,
                _ => unreachable!("parse_bool"),
            },
        }))
    }
    pub fn parse_single_string(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::SingleQuote(
            parser.current_token.text.clone(),
        )))
    }

    pub fn parse_double_string(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::DoubleQuote(
            parser.current_token.text.clone(),
        )))
    }

    pub fn parse_prefix_operator(parser: &mut Parser) -> Result<Expression> {
        let token = parser.pop();
        let operator = match &token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            _ => unreachable!("Not a valid prefix operator: {:?}", token),
        };

        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right: Box::new(parser.parse_expression(Precedence::Prefix)?),
        }))
    }

    pub fn parse_grouped_expr(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::Grouped(GroupedExpression {
            open: parser.expect_token(TokenKind::LeftParen)?,
            expr: Box::new(parser.parse_expression(Precedence::Lowest)?),
            close: {
                println!("Running close now...");
                parser.expect_peek(TokenKind::RightParen)?
            },
        }))
    }
}

mod infix_expr {
    use super::*;

    pub fn parse_infix_operator(parser: &mut Parser, left: Box<Expression>) -> Result<Expression> {
        let prec = parser.current_precedence();
        println!("Prec: {:?}", prec);

        let token = parser.pop();
        let operator = match token.kind {
            TokenKind::Plus => Operator::Plus,
            _ => unreachable!("Not a valid infix operator: {:?}", token),
        };

        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left,
            right: Box::new(parser.parse_expression(prec)?),
        }))
    }

    pub fn parser_call_expr(parser: &mut Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::Call(CallExpression {
            expr: left,
            // open: parser.expect_peek(TokenKind::LeftParen)?,
            open: parser.current_token.clone(),
            args: {
                if parser.peek_token.kind == TokenKind::RightParen {
                    parser.next_token();
                    vec![]
                } else {
                    // TODO: Comma
                    parser.next_token();
                    vec![parser.parse_expression(Precedence::Lowest)?]
                }
            },
            close: parser.expect_peek(TokenKind::RightParen)?,
        }))
    }
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            lexer,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        self.get_precedence(&self.peek_token.kind).unwrap_or_default()
    }

    fn current_precedence(&self) -> Precedence {
        self.get_precedence(&self.current_token.kind).unwrap_or_default()
    }

    fn get_precedence(&self, kind: &TokenKind) -> Option<Precedence> {
        Some(match kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Div | TokenKind::Mul => Precedence::Product,
            TokenKind::LeftParen => Precedence::Call,
            TokenKind::RightParen => Precedence::Lowest,
            _ => panic!("Unexpected precendence kind: {:?}", kind)
            // TokenKind::LessThan => todo!(),
            // TokenKind::LessThanOrEqual => todo!(),
            // TokenKind::GreaterThan => todo!(),
            // TokenKind::GreaterThanOrEqual => todo!(),
            // TokenKind::EqualTo => todo!(),
            // TokenKind::NotEqualTo => todo!(),
            // TokenKind::Or => todo!(),
            // TokenKind::And => todo!(),
            // TokenKind::Comma => todo!(),
            // TokenKind::Colon => todo!(),
            // TokenKind::SingleQuoteString => todo!(),
            // TokenKind::DoubleQuoteString => todo!(),
            // TokenKind::LeftParen => todo!(),
            // TokenKind::RightParen => todo!(),
            // TokenKind::LeftBrace => todo!(),
            // TokenKind::RightBrace => todo!(),
            // TokenKind::LeftBracket => todo!(),
            // TokenKind::RightBracket => todo!(),
        })
    }

    fn get_prefix_fn(&self) -> Option<PrefixFn> {
        Some(Box::new(match self.current_token.kind {
            TokenKind::Integer => prefix_expr::parse_vim_number,
            TokenKind::Identifier => prefix_expr::parse_identifier,
            TokenKind::DoubleQuoteString => prefix_expr::parse_double_string,
            TokenKind::SingleQuoteString => prefix_expr::parse_single_string,
            TokenKind::LeftParen => prefix_expr::parse_grouped_expr,
            TokenKind::True | TokenKind::False => prefix_expr::parse_bool,
            TokenKind::Plus | TokenKind::Minus => prefix_expr::parse_prefix_operator,
            _ => return None,
        }))
    }

    fn get_infix_fn(&self) -> Option<InfixFn> {
        Some(Box::new(match self.peek_token.kind {
            TokenKind::Plus => infix_expr::parse_infix_operator,
            TokenKind::LeftParen => infix_expr::parser_call_expr,
            TokenKind::Identifier => return None,
            _ => unimplemented!("get_infix_fn: {:#?}", self),
        }))
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression> {
        let prefix = self.get_prefix_fn();

        let mut left = match prefix {
            Some(prefix) => prefix(self)?,
            None => return Err(anyhow::anyhow!("Failed to find prefix function for {:#?}", self)),
        };

        while self.peek_token.kind != TokenKind::EndOfLine && prec < self.peek_precedence() {
            let infix = match self.get_infix_fn() {
                Some(infix) => infix,
                None => return Ok(left),
            };

            self.next_token();
            left = infix(self, Box::new(left))?;
        }

        Ok(left)
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

    /// Consumes the current
    pub fn expect_peek(&mut self, kind: TokenKind) -> Result<Token> {
        if self.peek_token.kind != kind {
            return Err(anyhow::anyhow!(
                "Got token: {:?}, Expected: {:?}",
                self.peek_token,
                kind
            ));
        }

        Ok(self.next_token())
    }

    pub fn expect_token_with_text(&mut self, kind: TokenKind, text: &str) -> Result<Token> {
        let token = self.current_token.clone();
        if token.kind != kind {
            panic!("Got token: {:?}, Expected kind: {:?}", token, kind);
            // return Err(anyhow::anyhow!("Got token: {:?}, Expected kind: {:?}", token, kind));
        }

        if token.text != text {
            panic!("Got token: {:?}, Expected text: {:?}", token, text);
            // return Err(anyhow::anyhow!("Got token: {:?}, Expected text: {:?}", token, text));
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

    pub fn pop(&mut self) -> Token {
        let tok = self.current_token.clone();
        self.next_token();

        tok
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
        // For the following branches, you need to return early if it completely consumes
        // the last character and advances past.
        //
        // This is the desired behavior for `parse` which will consume until the end of line
        // generally speaking.
        let ex = match &self.current_token.kind {
            TokenKind::EndOfFile => panic!("EndOfFile!"),
            TokenKind::Comment => ExCommand::Comment(self.current_token.clone()),
            TokenKind::EndOfLine => ExCommand::NoOp(self.current_token.clone()),
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
                    return Ok(VarCommand::parse(self)?);
                } else if self.command_match("echo") {
                    return Ok(EchoCommand::parse(self)?);
                } else if self.command_match("def") {
                    return Ok(DefCommand::parse(self)?);
                } else if self.command_match("return") {
                    return Ok(ReturnCommand::parse(self)?);
                } else if self.command_match("if") {
                    return Ok(IfCommand::parser(self)?);
                } else if self.command_match("finish") {
                    ExCommand::Finish(self.current_token.clone())
                } else {
                    if self.peek_token.kind == TokenKind::LeftParen {
                        return Ok(CallCommand::parse(self)?);
                    } else {
                        ExCommand::NoOp(self.current_token.clone())
                    }
                }
            }
            _ => ExCommand::NoOp(self.current_token.clone()),
        };

        self.next_token();
        Ok(ex)
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { commands: vec![] };

        while self.current_token.kind != TokenKind::EndOfFile {
            let command = self.parse_command().unwrap();
            if command != ExCommand::Skip {
                program.commands.push(command);
            }
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
    snapshot!(test_comment, "../testdata/snapshots/comment.vim");
    snapshot!(test_header, "../testdata/snapshots/header.vim");
    snapshot!(test_expr, "../testdata/snapshots/expr.vim");
    snapshot!(test_echo, "../testdata/snapshots/echo.vim");

    // TODO: Slowly but surely, we can work towards this
    // snapshot!(test_matchparen, "../testdata/snapshots/matchparen.vim");
}
