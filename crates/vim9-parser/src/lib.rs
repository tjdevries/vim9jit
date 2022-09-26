#![allow(unused_variables)]
#![allow(dead_code)]

use std::collections::VecDeque;
use std::fmt::Debug;

use anyhow::Result;
use once_cell::sync::OnceCell;
use tracing::info;
use tracing::trace;
use tracing_subscriber::util::SubscriberInitExt;
use vim9_lexer::new_lexer;
use vim9_lexer::Lexer;
use vim9_lexer::Token;
use vim9_lexer::TokenKind;

#[derive(Debug)]
pub struct Program {
    pub commands: Vec<ExCommand>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExCommand {
    Vim9Script(Vim9ScriptCommand),
    Var(VarCommand),
    Heredoc(Heredoc),
    Decl(DeclCommand),
    Echo(EchoCommand),
    Return(ReturnCommand),
    Def(DefCommand),
    If(IfCommand),
    Call(CallCommand),
    Finish(FinishCommand),
    Augroup(AugroupCommand),
    Autocmd(AutocmdCommand),
    Statement(StatementCommand),

    Skip,
    EndOfFile,
    Comment(Token),
    NoOp(Token),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Heredoc {
    var: Token,
    pub ty: Option<Type>,
    pub name: Identifier,
    op: Token,
    pub trim: bool,
    pub eval: bool,
    open: Token,
    pub contents: Vec<String>,
    close: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FinishCommand {
    pub finish: Token,
    eol: Token,
}

impl FinishCommand {
    fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Finish(FinishCommand {
            finish: parser.expect_identifier_with_text("finish")?,
            eol: parser.expect_eol()?,
        }))
    }
}

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
    fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Augroup(AugroupCommand {
            augroup: parser.expect_identifier_with_text("augroup")?,
            augroup_name: parser.expect_token(TokenKind::Identifier)?.try_into()?,
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
    pub pattern: Vec<Literal>,
    pub block: AutocmdBlock,
}

impl AutocmdCommand {
    fn parse(parser: &mut Parser) -> Result<ExCommand> {
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
            pattern: {
                let mut pattern = vec![];
                loop {
                    pattern.push(parser.pop().try_into()?);
                    if parser.current_token.kind != TokenKind::Comma {
                        break;
                    }

                    parser.next_token();
                }

                pattern
            },
            block: AutocmdBlock::parse(parser)?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub token: Token,
}

impl TryFrom<Token> for Literal {
    type Error = anyhow::Error;

    fn try_from(token: Token) -> Result<Self> {
        Ok(match token.kind {
            TokenKind::Identifier => Self { token },
            TokenKind::Mul => Self { token },
            _ => unimplemented!("Not valid"),
        })
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

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    open: Token,
    pub body: Body,
    close: Token,
    eol: Token,
}

impl Block {
    pub fn parse(parser: &mut Parser) -> Result<Block> {
        Ok(Self {
            open: parser.expect_token(TokenKind::LeftBrace)?,
            body: Body::parse_until(parser, "}")?,
            close: parser.expect_token(TokenKind::RightBrace)?,
            eol: parser.expect_eol()?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementCommand {
    Assign(AssignStatement),
}

impl StatementCommand {
    pub fn matches(parser: &mut Parser) -> bool {
        // x = expr
        parser.peek_token.kind == TokenKind::Equal
            // g:something = expr
            || parser.peek_token.kind == TokenKind::Colon
    }

    #[tracing::instrument]
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        info!("{:?}", parser);
        let identifier = Identifier::parse(parser)?;
        if parser.current_token.kind == TokenKind::Equal {
            return Ok(ExCommand::Statement(StatementCommand::Assign(AssignStatement {
                left: identifier,
                equals: parser.expect_token(TokenKind::Equal)?,
                right: {
                    let right = parser.parse_expression(Precedence::Lowest)?;
                    parser.next_token();
                    right
                },
                eol: parser.expect_eol()?,
            })));
        }

        todo!("expr command: {:?}, {:#?}", identifier, parser)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignStatement {
    pub left: Identifier,
    equals: Token,
    pub right: Expression,
    eol: Token,
}

// impl AssignStatement {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         // Ok(ExCommand::Assign
//     }
// }

#[derive(Debug, PartialEq, Clone)]
pub struct Vim9ScriptCommand {
    pub noclear: bool,
    eol: Token,
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
        let var = parser.expect_token_with_text(TokenKind::Identifier, "var")?;
        info!(var=?var, cur=?parser.current_token);
        let name = Identifier::parse(parser)?;
        info!(name=?name, cur=?parser.current_token);
        let ty = match parser.current_token.kind {
            TokenKind::Equal => None,
            TokenKind::HeredocOperator => None,
            TokenKind::EndOfLine => None,
            TokenKind::EndOfFile => None,
            TokenKind::SpacedColon => Some(Type::parse(parser)?),
            _ => return Err(anyhow::anyhow!("invalid type and/or equal for var: {:?}", parser)),
        };

        match parser.current_token.kind {
            TokenKind::HeredocOperator => {
                // todo!("HEREDOX")
                let op = parser.expect_token(TokenKind::HeredocOperator)?;

                // TODO: Parse these out
                let mut trim = false;
                let mut eval = false;

                let open = {
                    let mut token = parser.expect_token(TokenKind::Identifier)?;
                    while token.text == "trim" || token.text == "eval" {
                        if token.text == "trim" {
                            trim = true;
                            token = parser.expect_token(TokenKind::Identifier)?;
                        }

                        if token.text == "eval" {
                            eval = true;
                            token = parser.expect_token(TokenKind::Identifier)?;
                        }
                    }

                    token
                };

                parser.expect_eol()?;

                let mut contents = vec![];
                let close = loop {
                    let mut line: Vec<Token> = vec![];
                    while !matches!(parser.current_token.kind, TokenKind::EndOfLine | TokenKind::EndOfFile) {
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
                        line_contents += " ".repeat(tok.span.start_col - prev_end).as_str();
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
            TokenKind::EndOfLine | TokenKind::EndOfFile => Ok(ExCommand::Decl(DeclCommand {
                var,
                name,
                ty,
                eol: parser.expect_eol()?,
            })),
            _ => Err(anyhow::anyhow!("invalid next character: {:?}", parser)),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct IfCommand {
    if_tok: Token,
    pub condition: Expression,
    if_eol: Token,
    pub body: Body,
    endif_tok: Token,
    endif_eol: Token,
}

impl IfCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
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
pub struct CallCommand {
    call: Option<Token>,

    // TODO: Turn this into pub expr: Expression
    pub name: Identifier,
    open: Token,
    pub args: Vec<Expression>,
    close: Token,
    eol: Token,
}

impl Into<CallExpression> for &CallCommand {
    fn into(self) -> CallExpression {
        CallExpression {
            expr: Box::new(Expression::Identifier(self.name.clone())),
            open: self.open.clone(),
            args: self.args.clone(),
            close: self.close.clone(),
        }
    }
}

impl CallCommand {
    pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
        Ok(ExCommand::Call(CallCommand {
            call: None,
            name: Identifier::parse(parser)?,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen, true)?,
            close: parser.expect_token(TokenKind::RightParen)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
pub struct Type {
    colon: Token,
    pub inner: InnerType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum InnerType {
    Any,
    Bool,
    Number,
    Float,
    String,
    Blob,
    List {
        open: Token,
        inner: Box<InnerType>,
        close: Token,
    },
    Dict(Box<InnerType>),
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
    fn parse(parser: &mut Parser) -> Result<Type> {
        Ok(Type {
            colon: parser.expect_token(TokenKind::SpacedColon)?,
            inner: InnerType::parse(parser)?,
        })
    }
}

impl InnerType {
    fn parse(parser: &mut Parser) -> Result<InnerType> {
        match parser.current_token.kind {
            TokenKind::Identifier => {
                let literal: Literal = parser.pop().try_into()?;
                Ok(match literal.token.text.as_str() {
                    "any" => InnerType::Any,
                    "bool" => InnerType::Bool,
                    "number" => InnerType::Number,
                    "void" => InnerType::Void,
                    "string" => InnerType::String,
                    "list" => InnerType::List {
                        open: parser.expect_token(TokenKind::LessThan)?,
                        inner: InnerType::parse(parser)?.into(),
                        close: parser.expect_token(TokenKind::GreaterThan)?,
                    },
                    "func" => InnerType::Func(InnerFuncType::Naked),
                    _ => todo!("{:?}", literal.token),
                })
            }
            _ => unreachable!("should probably return an error"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    open: Token,
    pub params: Vec<Parameter>,
    close: Token,
}

impl Signature {
    fn parse(parser: &mut Parser) -> Result<Signature> {
        Ok(Self {
            open: parser.expect_token(TokenKind::LeftParen)?,
            params: {
                let mut params = Vec::new();
                while parser.current_token.kind != TokenKind::RightParen {
                    info!(parser=?parser);
                    params.push(Parameter::parse(parser)?);
                    if parser.current_token.kind == TokenKind::Comma {
                        parser.next_token();
                    }
                }

                params
            },
            close: parser.expect_token(TokenKind::RightParen)?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
/// Parameter definitions
///
/// {arguments} is a sequence of zero or more argument
/// declarations.  There are three forms:
///      {name}: {type}
///      {name} = {value}
///      {name}: {type} = {value}
pub struct Parameter {
    pub name: Identifier,
    pub ty: Option<Type>,
    equal: Option<Token>,
    pub default_val: Option<Expression>,
}

impl Parameter {
    fn parse(parser: &mut Parser) -> Result<Parameter> {
        let name = Identifier::parse(parser)?;

        info!(parser=?parser, "current parser state");
        let ty = if parser.current_token.kind == TokenKind::SpacedColon {
            Some(Type::parse(parser)?)
        } else {
            None
        };

        let (equal, default_val) = if parser.current_token.kind == TokenKind::Equal {
            (
                Some(parser.expect_token(TokenKind::Equal)?),
                Some(Expression::parse(parser, Precedence::Lowest)?),
            )
        } else {
            (None, None)
        };

        Ok(Parameter {
            name,
            ty,
            equal,
            default_val,
        })
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
            ret: parser.expect_token_with_text(TokenKind::Identifier, "return")?,
            expr: match parser.current_token.kind {
                TokenKind::EndOfLine => None,
                _ => Some(Expression::parse(parser, Precedence::Lowest)?),
            },
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(PartialEq, Clone)]
pub enum Identifier {
    Raw(RawIdentifier),
    Scope(ScopedIdentifier),
    Unpacked(UnpackIdentifier),
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Raw(raw) => write!(f, "Raw({})", raw.name),
            Identifier::Scope(scope) => write!(f, "Scope({:?})", scope),
            Identifier::Unpacked(unpack) => write!(f, "Unpack({:?})", unpack),
        }
    }
}

impl TryInto<VimScope> for Identifier {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<VimScope, Self::Error> {
        match self {
            Identifier::Raw(raw) => Ok(match raw.name.as_str() {
                "g" => VimScope::Global,
                "v" => VimScope::VimVar,
                "t" => VimScope::Tab,
                "w" => VimScope::Window,
                "b" => VimScope::Buffer,
                "s" => VimScope::Script,
                "l" => VimScope::Local,
                _ => return Err(anyhow::anyhow!("invalid scope: {:?}", raw)),
            }),
            _ => Err(anyhow::anyhow!("must be a raw identifier")),
        }
    }
}

impl Identifier {
    fn parse_in_expression(parser: &mut Parser) -> Result<Identifier> {
        if parser.current_token.kind == TokenKind::LeftBracket {
            return Ok(Identifier::Unpacked(UnpackIdentifier {
                open: parser.ensure_token(TokenKind::LeftBracket)?,
                identifiers: parser.parse_identifier_list(TokenKind::RightBracket)?,
                close: parser.expect_peek(TokenKind::RightBracket)?,
            }));
        }

        Ok(if parser.peek_token.kind == TokenKind::Colon {
            Identifier::Scope(ScopedIdentifier {
                scope: {
                    // TODO: get the right scope
                    parser.next_token();
                    VimScope::Global
                },
                colon: parser.expect_token(TokenKind::Colon)?,
                accessor: Identifier::parse_in_expression(parser)?.into(),
            })
        } else {
            // Todo: Other names
            Identifier::Raw(RawIdentifier {
                name: {
                    let current = parser.current_token.clone();
                    anyhow::ensure!(matches!(
                        current.kind,
                        TokenKind::Identifier | TokenKind::True | TokenKind::False | TokenKind::Null
                    ));

                    current.text
                },
            })
        })
    }

    fn parse(parser: &mut Parser) -> Result<Identifier> {
        let ret = Self::parse_in_expression(parser)?;
        parser.next_token();

        Ok(ret)
    }
}

#[derive(PartialEq, Clone)]
pub struct UnpackIdentifier {
    open: Token,
    identifiers: Vec<Identifier>,
    close: Token,
}

impl Debug for UnpackIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}]", self.identifiers)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ScopedIdentifier {
    pub scope: VimScope,
    colon: Token,
    // TODO: This is a lie, we need to handle g:["StringAccess"]
    pub accessor: Box<Identifier>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum VimScope {
    Global,
    Tab,
    Window,
    Buffer,
    Script,
    Local,
    VimVar,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Empty,
    Identifier(Identifier),
    Number(VimNumber),
    String(VimString),
    Boolean(VimBoolean),
    Grouped(GroupedExpression),
    Call(CallExpression),
    Index(IndexExpression),
    Slice(VimSlice),
    Array(ArrayLiteral),
    Dict(DictLiteral),
    VimOption(VimOption),

    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub container: Box<Expression>,
    open: Token,
    pub index: Box<IndexType>,
    close: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IndexType {
    Item(Expression),
    Slice(VimSlice),
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimSlice {
    pub start: Option<Box<Expression>>,
    colon: Token,
    pub finish: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimOption {
    ampersand: Token,
    pub option: Literal,
}

#[derive(Debug, PartialEq, Clone)]
pub struct KeyValue {
    pub key: VimKey,
    colon: Token,
    pub value: Expression,
}

impl KeyValue {
    pub fn parse(parser: &mut Parser) -> Result<KeyValue> {
        Ok(Self {
            key: match parser.current_token.kind {
                TokenKind::Identifier => VimKey::Literal(parser.pop().try_into()?),
                _ => unimplemented!("{:?}", parser),
            },
            colon: parser.expect_token(TokenKind::SpacedColon)?,
            value: parser.parse_expression(Precedence::Lowest)?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VimKey {
    Literal(Literal),
}

#[derive(Debug, PartialEq, Clone)]
pub struct DictLiteral {
    open: Token,
    pub elements: Vec<KeyValue>,
    close: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    open: Token,
    pub elements: Vec<Expression>,
    close: Token,
}

#[derive(PartialEq, Clone)]
pub struct CallExpression {
    pub expr: Box<Expression>,
    open: Token,
    pub args: Vec<Expression>,
    close: Token,
}

impl Debug for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "f: {:?} arg: {:#?}", self.expr, self.args)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupedExpression {
    open: Token,
    pub expr: Box<Expression>,
    close: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimBoolean {
    token: Token,
    pub value: bool,
}

type PrefixFn = Box<dyn Fn(&mut Parser) -> Result<Expression>>;

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    token: Token,
    pub operator: Operator,
    pub right: Box<Expression>,
}

type InfixFn = Box<dyn Fn(&mut Parser, Box<Expression>) -> Result<Expression>>;

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    token: Token,
    pub operator: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimNumber {
    pub value: String,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Bang,
    Modulo,
    Or,
    And,
    EqualTo,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    StringConcat,
}

#[derive(Debug, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Equals,
    LessGreater,
    StringConcat,
    Sum,
    Product,
    Modulo,
    Colon,
    Prefix,
    Call,
    Index,
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

#[derive(Debug, PartialEq, Clone)]
pub enum VimString {
    SingleQuote(String),
    DoubleQuote(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RawIdentifier {
    pub name: String,
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
    token_buffer: VecDeque<Token>,
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
            TokenKind::Bang => Operator::Bang,
            TokenKind::Percent => Operator::Modulo,
            _ => unreachable!("Not a valid prefix operator: {:?}", token),
        };

        Ok(Expression::Prefix(PrefixExpression {
            token,
            operator,
            right: parser.parse_expression(Precedence::Prefix)?.into(),
        }))
    }

    pub fn parse_grouped_expr(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::Grouped(GroupedExpression {
            open: parser.expect_token(TokenKind::LeftParen)?,
            expr: parser.parse_expression(Precedence::Lowest)?.into(),
            close: parser.expect_peek(TokenKind::RightParen)?,
        }))
    }

    pub fn parse_array_literal(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::Array(ArrayLiteral {
            open: parser.ensure_token(TokenKind::LeftBracket)?,
            elements: parser.parse_expression_list(TokenKind::RightBracket, false)?,
            close: parser.expect_peek(TokenKind::RightBracket)?,
        }))
    }

    pub fn parse_dict_literal(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::Dict(DictLiteral {
            open: parser.ensure_token(TokenKind::LeftBrace)?,
            elements: parser.parse_keyvalue_list(TokenKind::RightBrace)?,
            close: parser.expect_peek(TokenKind::RightBrace)?,
        }))
    }

    pub fn parse_vim_option(parser: &mut Parser) -> Result<Expression> {
        Ok(Expression::VimOption(VimOption {
            ampersand: parser.expect_token(TokenKind::Ampersand)?,
            option: parser.ensure_token(TokenKind::Identifier)?.try_into()?,
        }))
    }

    pub fn parse_prefix_colon(parser: &mut Parser) -> Result<Expression> {
        todo!("[Colon] I don't think we ever get this in reality?");
    }

    pub fn parse_prefix_spaced_colon(parser: &mut Parser) -> Result<Expression> {
        todo!("[SpacedColon] I don't think we ever get this in reality? ");
    }
}

mod infix_expr {
    use super::*;

    pub fn parse_infix_operator(parser: &mut Parser, left: Box<Expression>) -> Result<Expression> {
        parser.skip_whitespace();

        let prec = parser.current_precedence();
        let token = parser.pop();
        let operator = match token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Or => Operator::Or,
            TokenKind::EqualTo => Operator::EqualTo,
            TokenKind::And => Operator::And,
            TokenKind::Percent => Operator::Modulo,
            TokenKind::LessThan => Operator::LessThan,
            TokenKind::GreaterThan => Operator::GreaterThan,
            TokenKind::LessThanOrEqual => Operator::LessThanOrEqual,
            TokenKind::GreaterThanOrEqual => Operator::GreaterThanOrEqual,
            TokenKind::StringConcat => Operator::StringConcat,
            _ => unreachable!("Not a valid infix operator: {:?}", token),
        };

        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left,
            right: parser.parse_expression(prec)?.into(),
        }))
    }

    #[tracing::instrument]
    pub fn parse_colon(parser: &mut Parser, left: Box<Expression>) -> Result<Expression> {
        // White space is required in a sublist (list slice) around the ":", except at
        // the start and end: >
        //  otherlist = mylist[v : count]   # v:count has a different meaning
        //  otherlist = mylist[:]       # make a copy of the List
        //  otherlist = mylist[v :]
        //  otherlist = mylist[: v]

        // Options:
        //  1. scoped variable -> g:var, s:var, w:var, ...
        //  2. two sided slice, no space -> 0:5, var:other
        //  3. two side slice, space -> 0 : 5, 0: 5, 0 :5, g : var
        //  5. right open -> 0: , 0 : , g :var

        // This is an example of a prefix operator, so we shouldn't handle this here.
        //  4. left open -> :5, : 5,            (illegal? g: var)

        // Attempt to determine if current val is => g:var, w:something, etc.
        // TODO: Can i get rid of this clone?
        if let Expression::Identifier(ident) = (*left).clone() {
            let valid_scope: Result<VimScope> = ident.try_into();
            if let Ok(scope) = valid_scope {
                if parser.current_token.kind == TokenKind::Colon {
                    return Ok(Expression::Identifier(Identifier::Scope(ScopedIdentifier {
                        scope,
                        colon: parser.expect_token(TokenKind::Colon)?,
                        accessor: Identifier::parse_in_expression(parser)?.into(),
                    })));
                }
            }
        }

        let colon = parser.pop();
        anyhow::ensure!(
            matches!(colon.kind, TokenKind::Colon),
            "token: {:?}, parser: {:?}",
            colon,
            parser
        );

        if parser.current_token.kind == TokenKind::RightBracket {
            return Ok(Expression::Slice(
                VimSlice {
                    start: Some(left),
                    colon,
                    finish: None,
                }
                .into(),
            ));
        }

        todo!("I don't think this should be possible anymore: {:?}", parser)
    }

    #[tracing::instrument(skip(parser, left))]
    pub fn parser_call_expr(parser: &mut Parser, left: Box<Expression>) -> Result<Expression> {
        trace!("peek: {:?} left: {:?}", parser.peek_token, left);

        Ok(Expression::Call(CallExpression {
            expr: left,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen, false)?,
            close: parser.expect_peek(TokenKind::RightParen)?,
        }))
    }

    pub fn parser_index_expr(parser: &mut Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::Index(IndexExpression {
            container: left,
            open: parser.expect_token(TokenKind::LeftBracket)?,
            index: IndexType::parse(parser)?.into(),
            close: parser.ensure_token(TokenKind::RightBracket)?,
        }))
    }
}

impl IndexType {
    fn parse(parser: &mut Parser) -> Result<IndexType> {
        let (colon, left) = match parser.current_token.kind {
            TokenKind::Colon | TokenKind::SpacedColon => (parser.pop(), None),
            _ => {
                let left = parser.parse_expression(Precedence::Colon)?;
                if let Expression::Slice(slice) = left {
                    return Ok(IndexType::Slice(slice));
                }

                if parser.peek_token.kind == TokenKind::RightBracket {
                    parser.next_token();
                    return Ok(IndexType::Item(left));
                }

                // Slice { -1, ... }
                // Negative { Slice ... }
                info!(parser=?parser, left=?left);
                let colon = parser.next_token();
                anyhow::ensure!(
                    matches!(colon.kind, TokenKind::Colon | TokenKind::SpacedColon),
                    "[IndexType] token: {:#?}, parser: {:#?}, slice: {:#?}",
                    colon,
                    parser,
                    left
                );

                // Move past the colon, so that we're on the expression to the right
                parser.next_token();

                (colon, Some(left.into()))
            }
        };

        if parser.current_token.kind == TokenKind::RightBracket {
            return Ok(IndexType::Slice(VimSlice {
                start: left,
                colon,
                finish: None,
            }));
        }

        let right = Expression::parse(parser, Precedence::Lowest)?;
        return Ok(IndexType::Slice(VimSlice {
            start: left,
            colon,
            finish: Some(right.into()),
        }));
    }
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        Self {
            current_token: lexer.next_token(),
            peek_token: lexer.next_token(),
            token_buffer: VecDeque::new(),
            lexer,
        }
    }

    fn peek_non_whitespace(&mut self) -> Token {
        let mut peek_index = 1;
        let mut peek_token = self.peek_token.clone();
        while peek_token.kind == TokenKind::EndOfLine {
            peek_index += 1;
            peek_token = self.peek_n(peek_index);
            info!(peek_token=?peek_token);
        }

        peek_token
    }

    fn peek_precedence(&mut self) -> Precedence {
        let kind = self.peek_non_whitespace().kind;
        self.get_precedence(&kind).unwrap_or_default()
    }

    fn current_precedence(&self) -> Precedence {
        self.get_precedence(&self.current_token.kind).unwrap_or_default()
    }

    fn get_precedence(&self, kind: &TokenKind) -> Option<Precedence> {
        Some(match kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Div | TokenKind::Mul => Precedence::Product,
            TokenKind::Percent => Precedence::Modulo,
            TokenKind::StringConcat => Precedence::StringConcat,
            TokenKind::LeftParen => Precedence::Call,
            TokenKind::LeftBracket => Precedence::Index,
            TokenKind::Colon => Precedence::Colon,
            TokenKind::Comma => Precedence::Lowest,
            TokenKind::SpacedColon => Precedence::Lowest,
            TokenKind::Or | TokenKind::And => Precedence::Equals,
            TokenKind::EqualTo
                | TokenKind::LessThan
                | TokenKind::GreaterThan
                | TokenKind::LessThanOrEqual
                | TokenKind::GreaterThanOrEqual => Precedence::LessGreater,
            TokenKind::RightBracket
                | TokenKind::RightBrace
                | TokenKind::RightParen
                => Precedence::Lowest,
            TokenKind::EndOfLine | TokenKind::EndOfFile => Precedence::Lowest,
            // We have to check new lines to see if we need to handle anything there.
            //  I'm not sure this is 100% great, but we'll leave it this way for now.
            TokenKind::Identifier | TokenKind::Comment=> Precedence::Lowest,

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

    #[tracing::instrument]
    fn get_prefix_fn(&self) -> Option<PrefixFn> {
        Some(Box::new(match self.current_token.kind {
            TokenKind::Integer => prefix_expr::parse_vim_number,
            TokenKind::Identifier => prefix_expr::parse_identifier,
            TokenKind::DoubleQuoteString => prefix_expr::parse_double_string,
            TokenKind::SingleQuoteString => prefix_expr::parse_single_string,
            TokenKind::LeftParen => prefix_expr::parse_grouped_expr,
            TokenKind::LeftBracket => prefix_expr::parse_array_literal,
            TokenKind::LeftBrace => prefix_expr::parse_dict_literal,
            TokenKind::Ampersand => prefix_expr::parse_vim_option,
            TokenKind::Colon => prefix_expr::parse_prefix_colon,
            TokenKind::SpacedColon => prefix_expr::parse_prefix_spaced_colon,
            TokenKind::True | TokenKind::False => prefix_expr::parse_bool,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Bang | TokenKind::Percent => {
                prefix_expr::parse_prefix_operator
            }
            _ => return None,
        }))
    }

    fn get_infix_fn(&mut self) -> Option<InfixFn> {
        let peek_token = self.peek_non_whitespace();

        Some(Box::new(match peek_token.kind {
            TokenKind::Plus | TokenKind::Minus | TokenKind::Percent | TokenKind::StringConcat => {
                infix_expr::parse_infix_operator
            }
            TokenKind::Or | TokenKind::And => infix_expr::parse_infix_operator,
            TokenKind::LeftParen => infix_expr::parser_call_expr,
            TokenKind::LeftBracket => infix_expr::parser_index_expr,
            TokenKind::Colon => infix_expr::parse_colon,
            TokenKind::EqualTo
            | TokenKind::LessThan
            | TokenKind::GreaterThan
            | TokenKind::LessThanOrEqual
            | TokenKind::GreaterThanOrEqual => infix_expr::parse_infix_operator,
            TokenKind::Identifier => return None,
            // TokenKind::SpacedColon => infix_expr::parser_index_type,
            _ => unimplemented!("get_infix_fn: {:#?}", self),
        }))
    }

    #[tracing::instrument(skip_all, fields(tok=?self.current_token))]
    fn parse_expression(&mut self, prec: Precedence) -> Result<Expression> {
        self.skip_whitespace();

        trace!("current_token: {:?}", self.current_token);
        let prefix = self.get_prefix_fn();

        let mut left = match prefix {
            Some(prefix) => prefix(self)?,
            None => return Err(anyhow::anyhow!("Failed to find prefix function for {:#?}", self)),
        };

        // TODO: Some things may not allow newlines, but for now, let's just skip them
        // while self.current_token.kind == TokenKind::EndOfLine {
        //     self.next_token();
        // }

        info!(left = ?left);

        // while self.current_token.kind != TokenKind::EndOfLine && prec < self.peek_precedence() {
        while prec < self.peek_precedence() {
            let infix = match self.get_infix_fn() {
                Some(infix) => infix,
                None => return Ok(left),
            };

            self.next_token();
            left = infix(self, left.into())?;
        }

        Ok(left)
    }

    pub fn expect_eol(&mut self) -> Result<Token> {
        let curkind = &self.current_token.kind;
        if curkind == &TokenKind::EndOfLine || curkind == &TokenKind::EndOfFile {
            Ok(self.pop())
        } else {
            Err(anyhow::anyhow!("expected eol or eof, got: {:?}", self.current_token))
        }
    }

    pub fn ensure_token(&self, kind: TokenKind) -> Result<Token> {
        let token = self.current_token.clone();
        if token.kind != kind {
            return Err(anyhow::anyhow!("Got token: {:?}, Expected: {:?}", token, kind));
        }

        Ok(token)
    }

    pub fn ensure_peek(&self, kind: TokenKind) -> Result<Token> {
        let token = self.peek_token.clone();
        if token.kind != kind {
            return Err(anyhow::anyhow!("Got token: {:?}, Expected: {:?}", token, kind));
        }

        Ok(token)
    }

    pub fn expect_token(&mut self, kind: TokenKind) -> Result<Token> {
        let token = self.current_token.clone();
        if token.kind != kind {
            return Err(anyhow::anyhow!(
                "\n[expect_token] Got token: {:?}, Expected: {:?}\n{:#?}",
                token,
                kind,
                self
            ));
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
        self.peek_token = self.token_buffer.pop_front().unwrap_or_else(|| self.lexer.next_token());

        self.current_token.clone()
    }

    pub fn peek_n(&mut self, n: usize) -> Token {
        if n == 0 {
            return self.current_token.clone();
        }

        if n == 1 {
            self.peek_token.clone()
        } else {
            while self.token_buffer.len() < n - 1 {
                self.token_buffer.push_back(self.lexer.next_token());
            }

            self.token_buffer.back().unwrap().clone()
        }
    }

    fn command_match(&self, full: &str) -> bool {
        self.current_token.text == full
    }

    #[tracing::instrument(skip_all, fields(tok=?self.current_token))]
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
                    return Ok(IfCommand::parse(self)?);
                } else if self.command_match("augroup") {
                    return Ok(AugroupCommand::parse(self)?);
                } else if self.command_match("autocmd") {
                    return Ok(AutocmdCommand::parse(self)?);
                } else if self.command_match("finish") {
                    return Ok(FinishCommand::parse(self)?);
                } else {
                    if self.peek_token.kind == TokenKind::LeftParen {
                        return Ok(CallCommand::parse(self)?);
                    } else if StatementCommand::matches(self) {
                        return Ok(StatementCommand::parse(self)?);
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
            // let command = match self.parse_command() {
            //     Ok(command) => command,
            //     Err(err) => panic!(
            //         "\nFailed to parse command.\nCurrent Commands: {:#?}. Error: {}",
            //         program, err
            //     ),
            // };

            let command = self.parse_command().unwrap();
            if command != ExCommand::Skip {
                program.commands.push(command);
            }
        }

        program
    }

    fn parse_identifier_list(&mut self, close_kind: TokenKind) -> Result<Vec<Identifier>> {
        let mut results = vec![];
        if self.peek_token.kind == close_kind {
            return Ok(results);
        }

        self.next_token();
        results.push(Identifier::parse_in_expression(self)?);

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();
            self.next_token();

            results.push(Identifier::parse_in_expression(self)?);
        }

        self.ensure_peek(close_kind)?;
        Ok(results)
    }

    fn parse_expression_list(&mut self, close_kind: TokenKind, consume_close: bool) -> Result<Vec<Expression>> {
        let mut results = vec![];
        if self.peek_token.kind != close_kind {
            self.next_token();
            results.push(self.parse_expression(Precedence::Lowest)?);

            while self.peek_token.kind == TokenKind::Comma {
                // Consume end of expression
                self.next_token();

                // Consume comma
                self.next_token();
                results.push(self.parse_expression(Precedence::Lowest)?);
            }
        }

        self.ensure_peek(close_kind)?;
        if consume_close {
            self.next_token();
        }

        Ok(results)
    }

    fn parse_keyvalue_list(&mut self, close_kind: TokenKind) -> Result<Vec<KeyValue>> {
        let mut elements: Vec<KeyValue> = vec![];
        if self.peek_token.kind == TokenKind::RightBrace {
            self.next_token();
            return Ok(elements);
        }

        self.next_token();
        elements.push(KeyValue::parse(self)?);

        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();
            self.next_token();
            elements.push(KeyValue::parse(self)?);
        }

        self.ensure_peek(close_kind)?;
        Ok(elements)
    }

    fn skip_whitespace(&mut self) {
        while self.current_token.kind == TokenKind::EndOfLine {
            self.next_token();
        }
    }
}

fn snapshot_parsing(input: &str) -> String {
    let lexer = new_lexer(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    format!("{:#?}", program.commands)
}

pub fn new_parser(lexer: Lexer) -> Parser {
    Parser::new(lexer)
}

fn setup_trace() {
    static INSTANCE: OnceCell<()> = OnceCell::new();
    INSTANCE.get_or_init(|| {
        tracing_subscriber::fmt()
            .with_max_level(tracing::Level::TRACE)
            .without_time()
            .with_target(false)
            .finish()
            .init();
    });
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                setup_trace();

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
    snapshot!(test_scopes, "../testdata/snapshots/scopes.vim");
    snapshot!(test_autocmd, "../testdata/snapshots/autocmd.vim");
    snapshot!(test_array, "../testdata/snapshots/array.vim");
    snapshot!(test_dict, "../testdata/snapshots/dict.vim");
    snapshot!(test_if, "../testdata/snapshots/if.vim");
    snapshot!(test_call, "../testdata/snapshots/call.vim");
    snapshot!(test_concat, "../testdata/snapshots/concat.vim");
    snapshot!(test_unpack, "../testdata/snapshots/unpack.vim");
    snapshot!(test_assign, "../testdata/snapshots/assign.vim");
    snapshot!(test_vimvar, "../testdata/snapshots/vimvar.vim");
    snapshot!(test_busted, "../testdata/snapshots/busted.vim");
    snapshot!(test_heredoc, "../testdata/snapshots/heredoc.vim");
    snapshot!(test_typed_params, "../testdata/snapshots/typed_params.vim");
    snapshot!(test_index, "../testdata/snapshots/index.vim");
    snapshot!(test_advanced_index, "../testdata/snapshots/advanced_index.vim");
    snapshot!(test_multiline, "../testdata/snapshots/multiline.vim");

    #[test]
    fn test_peek_n() {
        let input = "vim9script\nvar x = true\n";
        let lexer = new_lexer(input);
        let mut parser = Parser::new(lexer);
        assert_eq!(parser.peek_token.kind, TokenKind::EndOfLine);
        assert_eq!(parser.peek_n(0).kind, TokenKind::Identifier);
        assert_eq!(parser.peek_n(1).kind, TokenKind::EndOfLine);
        assert_eq!(parser.peek_n(2).kind, TokenKind::Identifier);
        assert_eq!(parser.peek_n(3).kind, TokenKind::Identifier);
    }

    // TODO: Slowly but surely, we can work towards this
    // snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");
}
