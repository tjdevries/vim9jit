#![allow(unused_variables)]
#![allow(dead_code)]

use std::{
    cell::{Ref, RefCell},
    collections::{HashSet, VecDeque},
    fmt::Debug,
};

use anyhow::Result;
use once_cell::sync::OnceCell;
use tracing_subscriber::util::SubscriberInitExt;
use vim9_lexer::{Lexer, Span, Token, TokenKind};

mod cmds;
pub use cmds::cmd_if::IfCommand;
// pub use cmds::{
//     cmd_auto::{AugroupCommand, AutocmdBlock, AutocmdCommand},
//     cmd_if::{ElseCommand, ElseIfCommand, IfCommand},
//     cmd_try::TryCommand,
//     cmd_user::UserCommand,
//     BreakCommand, CallCommand, ContinueCommand, DeclCommand, DefCommand,
//     DeferCommand, EchoCommand, EvalCommand, ExportCommand, FinishCommand,
//     ImportCommand, ReturnCommand, SharedCommand, VarCommand, Vim9ScriptCommand,
// };

mod types;
pub use types::{InnerType, Type};

#[derive(PartialEq, Clone)]
pub struct TokenMeta {
    pub kind: TokenKind,
    pub span: Span,
}

impl Debug for TokenMeta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token({:?}, {:?})", self.kind, self.span)
    }
}

impl<'a> From<Token<'a>> for TokenMeta {
    fn from(t: Token) -> Self {
        Self {
            kind: t.kind,
            span: t.span,
        }
    }
}

impl<'a> From<&Token<'a>> for TokenMeta {
    fn from(t: &Token) -> Self {
        Self {
            kind: t.kind.clone(),
            span: t.span.clone(),
        }
    }
}

impl<'a> From<Ref<'_, Token<'_>>> for TokenMeta {
    fn from(t: Ref<'_, Token<'_>>) -> Self {
        Self {
            kind: t.kind.clone(),
            span: t.span.clone(),
        }
    }
}

impl<'a> From<TokenOwned> for TokenMeta {
    fn from(t: TokenOwned) -> Self {
        Self {
            kind: t.kind,
            span: t.span,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct TokenOwned {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

impl Debug for TokenOwned {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token({:?}, {:?}, {:?})",
            self.kind, self.text, self.span
        )
    }
}

impl<'a> From<Token<'a>> for TokenOwned {
    fn from(t: Token<'a>) -> Self {
        Self {
            kind: t.kind,
            text: t.text.to_string(),
            span: t.span,
        }
    }
}

impl<'a> From<&Token<'a>> for TokenOwned {
    fn from(t: &Token<'a>) -> Self {
        Self {
            kind: t.kind.clone(),
            text: t.text.to_string(),
            span: t.span.clone(),
        }
    }
}

impl<'a> From<Ref<'_, Token<'_>>> for TokenOwned {
    fn from(t: Ref<'_, Token<'_>>) -> Self {
        Self {
            kind: t.kind.clone(),
            text: t.text.to_string(),
            span: t.span.clone(),
        }
    }
}

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
    Execute(ExecuteCommand),
    Return(ReturnCommand),
    Def(DefCommand),
    If(IfCommand),
    // For(ForCommand),
    // While(WhileCommand),
    // Try(TryCommand),
    Call(CallCommand),
    // Defer(DeferCommand),
    // Eval(EvalCommand),
    Finish(FinishCommand),
    // Break(BreakCommand),
    // Continue(ContinueCommand),
    // Augroup(AugroupCommand),
    // Autocmd(AutocmdCommand),
    Statement(StatementCommand),
    // UserCommand(UserCommand),
    SharedCommand(SharedCommand),
    // ExportCommand(ExportCommand),
    // ImportCommand(ImportCommand),
    Skip,
    EndOfFile,

    Comment(TokenOwned),
    NoOp(TokenOwned),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Vim9ScriptCommand {
    pub noclear: bool,
    eol: TokenMeta,
}

impl Vim9ScriptCommand {
    pub fn parse(parser: &Parser) -> Result<Self> {
        Ok(Self {
            noclear: if parser.cur_kind() == TokenKind::EndOfLine {
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
pub struct DefCommand {
    def: TokenMeta,
    pub name: Identifier,
    pub args: Signature,
    pub ret: Option<Type>,
    def_eol: TokenMeta,
    pub body: Body,
    enddef: TokenMeta,
    end_eol: TokenMeta,
}

impl DefCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Def(DefCommand {
            def: parser.expect_identifier_with_text("def")?.into(),
            name: Identifier::parse(parser)?,
            args: Signature::parse(parser)?,
            ret: {
                if parser.cur_kind() == TokenKind::SpacedColon {
                    Some(Type::parse(parser)?)
                } else {
                    None
                }
            },
            def_eol: parser.expect_eol()?,
            body: Body::parse_until(parser, "enddef")?,
            enddef: parser.expect_identifier_with_text("enddef")?.into(),
            end_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ReturnCommand {
    ret: TokenMeta,
    pub expr: Option<Expression>,
    eol: TokenMeta,
}

impl ReturnCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Return(Self {
            ret: parser
                .expect_token_with_text(TokenKind::Identifier, "return")?
                .into(),
            expr: match parser.cur_kind() {
                TokenKind::EndOfLine => None,
                _ => Some(Expression::parse(parser, Precedence::Lowest)?),
            },
            eol: parser.expect_eol()?,
        }))
    }

    pub fn fake(expr: Option<Expression>) -> Self {
        Self {
            ret: Token::fake().into(),
            expr,
            eol: Token::fake().into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExecuteCommand {
    execute: TokenMeta,
    pub expr: Expression,
    eol: TokenMeta,
}

impl ExecuteCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Execute(ExecuteCommand {
            execute: parser.expect_identifier_with_text("execute")?.into(),
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FinishCommand {
    pub finish: TokenMeta,
    eol: TokenMeta,
}

impl FinishCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Finish(FinishCommand {
            finish: parser.expect_identifier_with_text("finish")?.into(),
            eol: parser.expect_eol()?,
        }))
    }
}

#[derive(PartialEq, Clone)]
pub struct CallExpression {
    pub expr: Box<Expression>,
    open: TokenMeta,
    pub args: Vec<Expression>,
    close: TokenMeta,
}

impl CallExpression {
    pub fn name(&self) -> Option<&Identifier> {
        match self.expr.as_ref() {
            Expression::Identifier(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn parse(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<CallExpression> {
        Ok(CallExpression {
            expr: left,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen)?,
            close: parser
                .ensure_token(TokenKind::RightParen)
                .expect("[CallExpression::parse]"),
        })
    }
}

impl Debug for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "f: {:?} arg: {:#?}", self.expr, self.args)
    }
}

impl Into<CallExpression> for &CallCommand {
    fn into(self) -> CallExpression {
        CallExpression {
            expr: self.expr.clone().into(),
            open: Token::fake().into(),
            args: self.args.clone(),
            close: Token::fake().into(),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SharedCommand {
    pub contents: String,
    eol: TokenMeta,
}

impl SharedCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let mut contents = String::new();
        let mut prev_end = 0;
        while parser.cur_kind() != TokenKind::EndOfLine
            && parser.cur_kind() != TokenKind::EndOfFile
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
pub struct EchoCommand {
    echo: TokenMeta,
    pub expr: Expression,
    eol: TokenMeta,
}

impl EchoCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let echo: TokenMeta = parser
            .expect_fn_token(
                |t| {
                    matches!(
                        t.text.to_string().as_str(),
                        "echo" | "echon" | "echomsg"
                    )
                },
                true,
            )?
            .into();

        Ok(ExCommand::Echo(EchoCommand {
            echo,
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        }))
    }
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct ForCommand {
//     for_: Token,
//     pub for_identifier: Identifier,
//     in_: Token,
//     pub for_expr: Expression,
//     eol: Token,
//     pub body: Body,
//     endfor_: Token,
//     endfor_eol: Token,
// }
//
// impl ForCommand {
//     pub fn parse(parser: &Parser) -> Result<ExCommand> {
//         Ok(ExCommand::For(ForCommand {
//             for_: parser.expect_identifier_with_text("for")?,
//             for_identifier: Identifier::parse(parser)?,
//             in_: parser.expect_identifier_with_text("in")?,
//             for_expr: Expression::parse(parser, Precedence::Lowest)?,
//             eol: parser.expect_eol()?,
//             body: Body::parse_until(parser, "endfor")?,
//             endfor_: parser.expect_identifier_with_text("endfor")?,
//             endfor_eol: parser.expect_eol()?,
//         }))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct WhileCommand {
//     while_: Token,
//     pub condition: Expression,
//     while_eol: Token,
//     pub body: Body,
//     endwhile_: Token,
//     endwhile_eol: Token,
// }
//
// impl WhileCommand {
//     pub fn parse(parser: &Parser) -> Result<ExCommand> {
//         Ok(ExCommand::While(WhileCommand {
//             while_: parser.expect_identifier_with_text("while")?,
//             condition: Expression::parse(parser, Precedence::Lowest)?,
//             while_eol: parser.expect_eol()?,
//             body: Body::parse_until(parser, "endwhile")?,
//             endwhile_: parser.expect_identifier_with_text("endwhile")?,
//             endwhile_eol: parser.expect_eol()?,
//         }))
//     }
// }
//
#[derive(Debug, PartialEq, Clone)]
pub struct Heredoc {
    var: TokenMeta,
    pub ty: Option<Type>,
    pub name: Identifier,
    op: TokenMeta,
    pub trim: bool,
    pub eval: bool,
    open: TokenMeta,
    pub contents: Vec<String>,
    close: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclCommand {
    var: TokenMeta,
    pub name: Identifier,
    pub ty: Option<Type>,
    eol: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub token: TokenOwned,
}

impl<'a> TryFrom<Token<'a>> for Literal {
    type Error = anyhow::Error;

    fn try_from(token: Token) -> Result<Self> {
        let token: TokenOwned = token.into();
        Ok(match token.kind {
            TokenKind::Identifier => Self { token },
            TokenKind::Mul => Self { token },
            _ => unimplemented!("Not valid: {:#?}", token),
        })
    }
}

impl<'a> TryFrom<TokenOwned> for Literal {
    type Error = anyhow::Error;

    fn try_from(token: TokenOwned) -> Result<Self> {
        let token: TokenOwned = token.into();
        Ok(match token.kind {
            TokenKind::Identifier => Self { token },
            TokenKind::Mul => Self { token },
            _ => unimplemented!("Not valid: {:#?}", token),
        })
    }
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct Block {
//     open: Token,
//     pub body: Body,
//     close: Token,
//     eol: Token,
// }
//
// impl Block {
//     pub fn parse(parser: &Parser) -> Result<Block> {
//         Ok(Self {
//             open: parser.expect_token(TokenKind::LeftBrace)?,
//             body: Body::parse_until(parser, "}")?,
//             close: parser.expect_token(TokenKind::RightBrace)?,
//             eol: parser.expect_eol()?,
//         })
//     }
// }
//
#[derive(Debug, PartialEq, Clone)]
pub enum StatementCommand {
    Assign(AssignStatement),
    Mutate(MutationStatement),
}

impl StatementCommand {
    pub fn matches(parser: &Parser) -> bool {
        parser.cur_kind() == TokenKind::Colon
            || parser.line_matches(|t| {
                matches!(
                    t.kind,
                    TokenKind::Equal
                        | TokenKind::PlusEquals
                        | TokenKind::MinusEquals
                        | TokenKind::MulEquals
                        | TokenKind::DivEquals
                        | TokenKind::StringConcatEquals
                )
            })
    }

    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let expr = Expression::parse(parser, Precedence::Lowest)?;
        if parser.cur_kind() == TokenKind::Equal {
            return Ok(ExCommand::Statement(StatementCommand::Assign(
                AssignStatement {
                    left: expr,
                    equals: parser.expect_token(TokenKind::Equal)?.into(),
                    right: {
                        let right =
                            parser.parse_expression(Precedence::Lowest)?;
                        parser.next_token();
                        right
                    },
                    eol: parser.expect_eol()?,
                },
            )));
        }

        // todo!("expr command: {:?}, {:#?}", expr, parser)
        Ok(ExCommand::Statement(StatementCommand::Mutate(
            MutationStatement {
                left: expr,
                modifier: parser.pop(),
                right: {
                    let right = parser.parse_expression(Precedence::Lowest)?;
                    parser.next_token();
                    right
                },
                eol: parser.expect_eol()?,
            },
        )))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct MutationStatement {
    pub left: Expression,
    pub modifier: TokenOwned,
    pub right: Expression,
    eol: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AssignStatement {
    pub left: Expression,
    equals: TokenMeta,
    pub right: Expression,
    eol: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body {
    pub commands: Vec<ExCommand>,
}

impl Body {
    pub fn parse_until_any(
        parser: &Parser,
        identifiers: &HashSet<String>,
    ) -> Result<Body> {
        let mut commands = vec![];
        while !identifiers.contains(parser.front_text().as_str()) {
            commands.push(parser.parse_command()?);
        }

        Ok(Body { commands })
    }

    pub fn parse_until(parser: &Parser, identifier: &str) -> Result<Body> {
        let mut commands = vec![];
        while parser.front_text().as_str() != identifier {
            commands.push(parser.parse_command()?);
        }

        Ok(Body { commands })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Signature {
    open: TokenMeta,
    pub params: Vec<Parameter>,
    close: TokenMeta,
}

impl Signature {
    fn parse(parser: &Parser) -> Result<Signature> {
        Ok(Self {
            open: parser.expect_token(TokenKind::LeftParen)?.into(),
            params: {
                let mut params = Vec::new();
                while parser.cur_kind() != TokenKind::RightParen {
                    params.push(Parameter::parse(parser)?);
                    parser.skip_whitespace();
                    if parser.cur_kind() == TokenKind::Comma {
                        parser.next_token();
                        parser.skip_whitespace();
                    }
                }

                params
            },
            close: parser.expect_token(TokenKind::RightParen)?.into(),
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
    equal: Option<TokenMeta>,
    pub default_val: Option<Expression>,
}

impl Parameter {
    fn parse(parser: &Parser) -> Result<Parameter> {
        let name = Identifier::parse(parser)?;

        let ty = if parser.cur_kind() == TokenKind::SpacedColon {
            Some(Type::parse(parser)?)
        } else {
            None
        };

        let (equal, default_val) = if parser.cur_kind() == TokenKind::Equal {
            (
                Some(parser.expect_token(TokenKind::Equal)?.into()),
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

#[derive(PartialEq, Clone)]
pub enum Identifier {
    Raw(RawIdentifier),
    Scope(ScopedIdentifier),
    Unpacked(UnpackIdentifier),
    Ellipsis,
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Identifier::Raw(raw) => write!(f, "Raw({})", raw.name),
            Identifier::Scope(scope) => write!(f, "Scope({:?})", scope),
            Identifier::Unpacked(unpack) => write!(f, "Unpack({:?})", unpack),
            Identifier::Ellipsis => write!(f, "<Ellipsis>"),
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
    pub fn is_valid_local(&self) -> bool {
        match self {
            Identifier::Raw(_) => true,
            Identifier::Scope(_) => false,
            Identifier::Unpacked(_) => todo!(),
            Identifier::Ellipsis => false,
        }
    }

    fn parse_in_expression(parser: &Parser) -> Result<Identifier> {
        if parser.cur_kind() == TokenKind::LeftBracket {
            return Ok(Identifier::Unpacked(UnpackIdentifier {
                open: parser.ensure_token(TokenKind::LeftBracket)?,
                identifiers: parser
                    .parse_identifier_list(TokenKind::RightBracket)?,
                close: parser.expect_peek(TokenKind::RightBracket)?.into(),
            }));
        }

        Ok(match parser.peek_kind() {
            TokenKind::Colon => {
                Identifier::Scope(ScopedIdentifier {
                    scope: {
                        // TODO: get the right scope
                        parser.next_token();
                        VimScope::Global
                    },
                    colon: parser.expect_token(TokenKind::Colon)?.into(),
                    accessor: Identifier::parse_in_expression(parser)?.into(),
                })
            }
            TokenKind::Ellipsis => Identifier::Ellipsis,
            _ => Identifier::Raw(RawIdentifier {
                name: {
                    let current = parser.front_owned();
                    anyhow::ensure!(
                        matches!(
                            current.kind,
                            TokenKind::Identifier
                                | TokenKind::True
                                | TokenKind::False
                                | TokenKind::Null
                        ),
                        "{:#?}",
                        current
                    );

                    current.text.to_string()
                },
            }),
        })
    }

    fn parse(parser: &Parser) -> Result<Identifier> {
        let ret = Self::parse_in_expression(parser)?;
        parser.next_token();

        Ok(ret)
    }
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

#[derive(PartialEq, Clone)]
pub struct UnpackIdentifier {
    open: TokenMeta,
    pub identifiers: Vec<Identifier>,
    close: TokenMeta,
}

impl Debug for UnpackIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:?}]", self.identifiers)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ScopedIdentifier {
    pub scope: VimScope,
    colon: TokenMeta,
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
    DictAccess(DictAccess),
    VimOption(VimOption),
    Register(Register),
    // Lambda(Lambda),
    // Expandable(Expandable),
    MethodCall(MethodCall),
    // Ternary(Ternary),
    //
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

#[derive(PartialEq, Clone)]
pub struct VimNumber {
    pub value: String,
}

impl Debug for VimNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Number({})", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VimString {
    SingleQuote(String),
    DoubleQuote(String),
    Interpolated(String),
    InterpolatedLit(String),
    EnvironmentVariable(String),
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct Ternary {
//     pub cond: Box<Expression>,
//     question: Token,
//     pub if_true: Box<Expression>,
//     colon: Token,
//     pub if_false: Box<Expression>,
// }

#[derive(Debug, PartialEq, Clone)]
pub struct MethodCall {
    pub left: Box<Expression>,
    tok: TokenMeta,
    pub right: Box<CallExpression>,
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct Expandable {
//     left: Token,
//     pub ident: Identifier,
//     right: Token,
// }

#[derive(Debug, PartialEq, Clone)]
pub struct DictAccess {
    pub container: Box<Expression>,
    dot: TokenMeta,
    pub index: RawIdentifier,
}

// #[derive(Debug, PartialEq, Clone)]
// pub struct Lambda {
//     pub args: Signature,
//     pub ret: Option<Type>,
//     arrow: Token,
//     pub body: Body,
// }
//
// impl Lambda {
//     pub fn parse(parser: &Parser) -> Result<Lambda> {
//         Ok(Lambda {
//             args: Signature::parse(parser)?,
//             ret: {
//                 if parser.cur_kind() == TokenKind::SpacedColon {
//                     Some(Type::parse(parser)?)
//                 } else {
//                     None
//                 }
//             },
//             arrow: parser.expect_token(TokenKind::Arrow)?,
//             body: {
//                 if parser.cur_kind() == TokenKind::LeftBrace {
//                     todo!("parse blocks correctly");
//                 } else {
//                     Body {
//                         commands: {
//                             let mut v = Vec::new();
//                             v.push(ExCommand::Return(ReturnCommand::fake(
//                                 Some(
//                                     parser
//                                         .parse_expression(Precedence::Lowest)?,
//                                 ),
//                             )));
//                             v
//                         },
//                     }
//                 }
//             },
//         })
//     }
// }
//
#[derive(Debug, PartialEq, Clone)]
pub struct Register {
    pub register: String,
}

#[derive(Debug, PartialEq, Clone)]
pub struct IndexExpression {
    pub container: Box<Expression>,
    open: TokenMeta,
    pub index: Box<IndexType>,
    close: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub enum IndexType {
    Item(Expression),
    Slice(VimSlice),
}

impl IndexType {
    fn parse(parser: &Parser) -> Result<IndexType> {
        let (colon, left) = match parser.cur_kind() {
            TokenKind::Colon | TokenKind::SpacedColon => (parser.pop(), None),
            _ => {
                let left = parser.parse_expression(Precedence::Lowest)?;
                if let Expression::Slice(slice) = left {
                    return Ok(IndexType::Slice(slice));
                }

                if parser.peek_kind() == TokenKind::RightBracket {
                    parser.next_token();
                    return Ok(IndexType::Item(left));
                }

                // Slice { -1, ... }
                // Negative { Slice ... }
                let colon = parser.pop();
                anyhow::ensure!(
                    colon.kind.is_colon(),
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

        if parser.cur_kind() == TokenKind::RightBracket {
            return Ok(IndexType::Slice(VimSlice {
                start: left,
                colon: colon.into(),
                finish: None,
            }));
        }

        let right = Expression::parse(parser, Precedence::Lowest)?;
        return Ok(IndexType::Slice(VimSlice {
            start: left,
            colon: colon.into(),
            finish: Some(right.into()),
        }));
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimSlice {
    pub start: Option<Box<Expression>>,
    colon: TokenMeta,
    pub finish: Option<Box<Expression>>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimOption {
    ampersand: TokenMeta,
    pub option: Literal,
}

#[derive(Debug, PartialEq, Clone)]
pub struct KeyValue {
    pub key: VimKey,
    colon: TokenMeta,
    pub value: Expression,
}

impl KeyValue {
    pub fn parse(parser: &Parser) -> Result<KeyValue> {
        parser.skip_whitespace();

        Ok(Self {
            key: match parser.cur_kind() {
                TokenKind::Identifier => {
                    VimKey::Literal(parser.pop().try_into()?)
                }
                TokenKind::SingleQuoteString => VimKey::Literal(Literal {
                    token: parser.pop(),
                }),
                TokenKind::DoubleQuoteString => VimKey::Literal(Literal {
                    token: parser.pop(),
                }),
                TokenKind::LeftBracket => {
                    // Consume left bracket, we do not want this to parser
                    // as an array literal.
                    parser.next_token();

                    let expr = VimKey::Expression(Expression::parse(
                        parser,
                        Precedence::Lowest,
                    )?);

                    // Consume right token, to do the matching right bracket
                    parser.expect_token(TokenKind::RightBracket)?;

                    expr
                }
                _ => unimplemented!("{:?}", parser),
            },
            colon: parser.expect_token(TokenKind::SpacedColon)?.into(),
            value: parser.parse_expression(Precedence::Lowest)?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum VimKey {
    Literal(Literal),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub struct DictLiteral {
    open: TokenMeta,
    pub elements: Vec<KeyValue>,
    close: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    open: TokenMeta,
    pub elements: Vec<Expression>,
    close: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct GroupedExpression {
    open: TokenMeta,
    pub expr: Box<Expression>,
    close: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct VimBoolean {
    pub value: bool,
}

type PrefixFn = Box<dyn Fn(&Parser) -> Result<Expression>>;

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    token: TokenMeta,
    pub operator: Operator,
    pub right: Box<Expression>,
}

type InfixFn = Box<dyn Fn(&Parser, Box<Expression>) -> Result<Expression>>;

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    token: TokenOwned,
    pub operator: Operator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl InfixExpression {
    pub fn new(
        operator: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    ) -> Self {
        Self {
            token: Token::fake().into(),
            operator,
            left,
            right,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Bang,
    Modulo,
    Or,
    And,
    StringConcat,
    Divide,
    Multiply,

    // Comparisons
    EqualTo,
    EqualToIns,
    NotEqualTo,
    NotEqualToIns,
    LessThan,
    LessThanIns,
    LessThanOrEqual,
    LessThanOrEqualIns,
    GreaterThan,
    GreaterThanIns,
    GreaterThanOrEqual,
    GreaterThanOrEqualIns,
    RegexpMatches,
    RegexpMatchesIns,
    NotRegexpMatches,
    NotRegexpMatchesIns,
    Is,
    IsInsensitive,
    IsNot,
    IsNotInsensitive,
}

// const (
// _ int = iota
// LOWEST
// EQUALS
// // ==
// LESSGREATER // > or <
// SUM// +
// PRODUCT// *
// PREFIX// -X or !X
// CALL// myFunction(X)
// )

// -g:func()
// -((g:func)())
//
// x[1]()
// ((x)[1])()

#[derive(Debug, PartialEq, PartialOrd, Default)]
pub enum Precedence {
    #[default]
    Lowest,
    Equals,
    Or,
    And,
    Ternary,
    LessGreater,
    StringConcat,
    Sum,
    Product,
    Modulo,
    MethodCall,
    Prefix,
    Call,
    Index,
    Dot,

    // g:something, x[1 : 2], x[1 :]
    Colon,
}

// The parseIdentifier method doesn’t do a lot. It only returns a *ast.Identifier with the current
// token in the Token field and the literal value of the token in Value. It doesn’t advance the
// tokens, it doesn’t call nextToken. That’s important. All of our parsing functions, prefixParseFn
// or infixParseFn, are going to follow this protocol: start with curToken being the type of token
// you’re associated with and return with curToken being the last token that’s part of your
// expression type. Never advance the tokens too far.

impl Expression {
    pub fn parse(parser: &Parser, prec: Precedence) -> Result<Expression> {
        let expr = parser.parse_expression(prec);
        parser.next_token();

        expr
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VarCommand {
    var: TokenMeta,
    pub ty: Option<Type>,
    pub name: Identifier,
    equal: TokenMeta,
    pub expr: Expression,
    eol: TokenMeta,
}

impl VarCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let var = parser.expect_token(TokenKind::Identifier)?;
        anyhow::ensure!(matches!(
            var.text.to_string().as_str(),
            "var" | "const"
        ));

        let var: TokenMeta = var.into();

        let name = Identifier::parse(parser)?;
        let ty = match parser.cur_kind() {
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

        match parser.cur_kind() {
            TokenKind::HeredocOperator => {
                let op =
                    parser.expect_token(TokenKind::HeredocOperator)?.into();

                let mut trim = false;
                let mut eval = false;

                let open: TokenOwned = {
                    let mut token: TokenOwned =
                        parser.expect_token(TokenKind::Identifier)?.into();

                    while token.text.as_str() == "trim"
                        || token.text.as_str() == "eval"
                    {
                        if token.text.as_str() == "trim" {
                            trim = true;
                            token = parser
                                .expect_token(TokenKind::Identifier)?
                                .into();
                        }

                        if token.text.as_str() == "eval" {
                            eval = true;
                            token = parser
                                .expect_token(TokenKind::Identifier)?
                                .into();
                        }
                    }

                    token
                };

                parser.expect_eol()?;

                let mut contents = vec![];
                let close = loop {
                    let mut line: Vec<TokenOwned> = vec![];
                    while !matches!(
                        parser.cur_kind(),
                        TokenKind::EndOfLine | TokenKind::EndOfFile
                    ) {
                        line.push(parser.front_owned());
                        parser.next_token();
                    }

                    if parser.cur_kind() == TokenKind::EndOfFile {
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
                    open: open.into(),
                    contents,
                    close: close.into(),
                }))
            }
            TokenKind::Equal => Ok(ExCommand::Var(VarCommand {
                var,
                name,
                ty,
                equal: parser.expect_token(TokenKind::Equal)?.into(),
                expr: Expression::parse(parser, Precedence::Lowest)?,
                eol: parser.expect_eol()?,
            })),
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
pub struct CallCommand {
    call: Option<TokenMeta>,
    pub expr: Expression,
    open: TokenMeta,
    pub args: Vec<Expression>,
    close: TokenMeta,
    eol: TokenMeta,
}

impl CallCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Call(CallCommand {
            call: parser
                .expect_identifier_with_text("call")
                .ok()
                .map(|t| t.into()),
            expr: Expression::parse(parser, Precedence::Call)?,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen)?,
            close: parser.expect_token(TokenKind::RightParen)?.into(),
            eol: parser.expect_eol()?,
        }))
    }

    pub fn matches(parser: &Parser) -> bool {
        parser.line_contains_kind(TokenKind::LeftParen)
            // && parser.line_contains_kind(TokenKind::RightParen)
            && !parser.line_contains_any(|t| {
                t.kind.is_assignment() || t.kind == TokenKind::MethodArrow
            })
    }
}

mod prefix_expr {
    use super::*;

    pub fn parse_number(parser: &Parser) -> Result<Expression> {
        anyhow::ensure!(matches!(
            parser.cur_kind(),
            TokenKind::Integer | TokenKind::Float
        ));

        Ok(Expression::Number(VimNumber {
            value: parser.front_text(),
        }))
    }

    pub fn parse_identifier(parser: &Parser) -> Result<Expression> {
        anyhow::ensure!(parser.cur_kind() == TokenKind::Identifier);

        Ok(RawIdentifier {
            name: parser.front_text(),
        }
        .into())
    }

    pub fn parse_register(parser: &Parser) -> Result<Expression> {
        anyhow::ensure!(parser.cur_kind() == TokenKind::Register);

        Ok(Expression::Register(Register {
            register: parser.front_text(),
        }))
    }

    pub fn parse_bool(parser: &Parser) -> Result<Expression> {
        Ok(Expression::Boolean(VimBoolean {
            value: match &parser.cur_kind() {
                TokenKind::True => true,
                TokenKind::False => false,
                _ => unreachable!("parse_bool"),
            },
        }))
    }

    pub fn parse_single_string(parser: &Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::SingleQuote(
            parser.front_text(),
        )))
    }

    pub fn parse_double_string(parser: &Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::DoubleQuote(
            parser.front_text(),
        )))
    }

    pub fn parse_interpolated(parser: &Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::InterpolatedLit(
            parser.front_text(),
        )))
    }

    pub fn parse_interpolated_lit(parser: &Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::Interpolated(
            parser.front_text(),
        )))
    }

    pub fn parse_env_var(parser: &Parser) -> Result<Expression> {
        Ok(Expression::String(VimString::EnvironmentVariable(
            parser.front_text(),
        )))
    }

    pub fn parse_prefix_operator(parser: &Parser) -> Result<Expression> {
        let token = parser.pop();
        let operator = match &token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Bang => Operator::Bang,
            TokenKind::Percent => Operator::Modulo,
            _ => unreachable!("Not a valid prefix operator: {:?}", token),
        };

        Ok(Expression::Prefix(PrefixExpression {
            token: token.into(),
            operator,
            right: parser.parse_expression(Precedence::Prefix)?.into(),
        }))
    }

    pub fn parse_grouped_expr(parser: &Parser) -> Result<Expression> {
        if parser.line_contains_kind(TokenKind::Arrow) {
            todo!()
            // Ok(Expression::Lambda(Lambda::parse(parser)?))
        } else {
            Ok(Expression::Grouped(GroupedExpression {
                open: parser.expect_token(TokenKind::LeftParen)?.into(),
                expr: parser.parse_expression(Precedence::Lowest)?.into(),
                close: parser.expect_peek(TokenKind::RightParen)?.into(),
            }))
        }
    }

    pub fn parse_array_literal(parser: &Parser) -> Result<Expression> {
        Ok(Expression::Array(ArrayLiteral {
            open: parser.ensure_token(TokenKind::LeftBracket)?,
            elements: parser.parse_expression_list(TokenKind::RightBracket)?,
            close: parser.ensure_token(TokenKind::RightBracket)?,
        }))
    }

    pub fn parse_dict_literal(parser: &Parser) -> Result<Expression> {
        Ok(Expression::Dict(DictLiteral {
            open: parser.ensure_token(TokenKind::LeftBrace)?,
            elements: parser.parse_keyvalue_list(TokenKind::RightBrace)?,
            close: parser
                .ensure_token(TokenKind::RightBrace)
                .expect("parse_dict_literal"),
        }))
    }

    pub fn parse_vim_option(parser: &Parser) -> Result<Expression> {
        Ok(Expression::VimOption(VimOption {
            ampersand: parser.expect_token(TokenKind::Ampersand)?.into(),
            option: parser.front_owned().try_into()?,
        }))
    }

    //     pub fn parse_prefix_colon(parser: &Parser) -> Result<Expression> {
    //         todo!(
    //             "[Colon] I don't think we ever get this in reality? {:#?}",
    //             parser
    //         );
    //     }
    //
    //     pub fn parse_prefix_spaced_colon(
    //         parser: &Parser,
    //     ) -> Result<Expression> {
    //         todo!("[SpacedColon] I don't think we ever get this in reality? ");
    //     }
    //
    //     pub fn parse_expandable_sequence(
    //         parser: &Parser,
    //     ) -> Result<Expression> {
    //         Ok(Expression::Expandable(Expandable {
    //             left: parser.expect_token(TokenKind::AngleLeft)?,
    //             ident: {
    //                 // read until greater than or angle,
    //                 // smoosh into raw identifier
    //                 let mut name = String::new();
    //                 while !matches!(
    //                     parser.cur_kind(),
    //                     TokenKind::GreaterThan | TokenKind::AngleRight
    //                 ) {
    //                     name += parser.pop().text.as_str();
    //                 }
    //
    //                 Identifier::Raw(RawIdentifier { name })
    //             },
    //             right: parser.expect_fn(
    //                 |k| matches!(k, TokenKind::GreaterThan | TokenKind::AngleRight),
    //                 false,
    //             )?,
    //         }))
    //     }
}

mod infix_expr {
    use super::*;

    pub fn parse_infix_operator(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<Expression> {
        parser.skip_whitespace();

        let prec = parser.current_precedence();
        let token = parser.pop();
        let operator = match token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Div => Operator::Divide,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Or => Operator::Or,
            TokenKind::And => Operator::And,
            TokenKind::Percent => Operator::Modulo,
            TokenKind::StringConcat => Operator::StringConcat,
            // Comparisons {{{
            TokenKind::EqualTo => Operator::EqualTo,
            TokenKind::EqualToIns => Operator::EqualToIns,
            TokenKind::NotEqualTo => Operator::NotEqualTo,
            TokenKind::NotEqualToIns => Operator::NotEqualToIns,
            TokenKind::LessThan => Operator::LessThan,
            TokenKind::LessThanIns => Operator::LessThanIns,
            TokenKind::LessThanOrEqual => Operator::LessThanOrEqual,
            TokenKind::LessThanOrEqualIns => Operator::LessThanOrEqualIns,
            TokenKind::GreaterThan => Operator::GreaterThan,
            TokenKind::GreaterThanIns => Operator::GreaterThanIns,
            TokenKind::GreaterThanOrEqual => Operator::GreaterThanOrEqual,
            TokenKind::GreaterThanOrEqualIns => Operator::GreaterThanOrEqualIns,
            TokenKind::RegexpMatches => Operator::RegexpMatches,
            TokenKind::RegexpMatchesIns => Operator::RegexpMatchesIns,
            TokenKind::NotRegexpMatches => Operator::NotRegexpMatches,
            TokenKind::NotRegexpMatchesIns => Operator::NotRegexpMatchesIns,
            TokenKind::Is => Operator::Is,
            TokenKind::IsInsensitive => Operator::IsInsensitive,
            TokenKind::IsNot => Operator::IsNot,
            TokenKind::IsNotInsensitive => Operator::IsNotInsensitive,
            // }}}
            _ => unreachable!("Not a valid infix operator: {:?}", token),
        };

        Ok(Expression::Infix(InfixExpression {
            token,
            operator,
            left,
            right: parser.parse_expression(prec)?.into(),
        }))
    }

    pub fn parse_dot_operator(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<Expression> {
        Ok(Expression::DictAccess(DictAccess {
            container: left,
            dot: parser.expect_token(TokenKind::Dot)?.into(),
            index: RawIdentifier {
                name: parser.front_owned().text.to_string(),
            },
        }))
    }

    pub fn parse_method_call(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<Expression> {
        Ok(Expression::MethodCall(MethodCall {
            left,
            tok: {
                parser.skip_whitespace();
                parser.expect_token(TokenKind::MethodArrow)?.into()
            },
            right: {
                // Parse up to the point it would be a call expr
                let base = Expression::parse(parser, Precedence::Call)
                    .expect("base")
                    .into();

                // Create the call expr from the first base expression
                let right =
                    CallExpression::parse(parser, base).expect("call").into();

                // Closing on right paren, DO NOT advance
                parser
                    .ensure_token(TokenKind::RightParen)
                    .expect("rightparen");

                right
            },
        }))
    }

    pub fn parse_colon(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<Expression> {
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
                if parser.cur_kind() == TokenKind::Colon {
                    return Ok(Expression::Identifier(Identifier::Scope(
                        ScopedIdentifier {
                            scope,
                            colon: parser
                                .expect_token(TokenKind::Colon)?
                                .into(),
                            accessor: Identifier::parse_in_expression(parser)?
                                .into(),
                        },
                    )));
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

        if parser.cur_kind() == TokenKind::RightBracket {
            return Ok(Expression::Slice(
                VimSlice {
                    start: Some(left),
                    colon: colon.into(),
                    finish: None,
                }
                .into(),
            ));
        }

        todo!(
            "I don't think this should be possible anymore: {:?}",
            parser
        )
    }

    pub fn parser_call_expr(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<Expression> {
        Ok(Expression::Call(CallExpression::parse(parser, left)?))
    }

    pub fn parser_index_expr(
        parser: &Parser,
        left: Box<Expression>,
    ) -> Result<Expression> {
        Ok(Expression::Index(IndexExpression {
            container: left,
            open: parser.expect_token(TokenKind::LeftBracket)?.into(),
            index: IndexType::parse(parser)?.into(),
            close: parser.ensure_token(TokenKind::RightBracket)?.into(),
        }))
    }

    //     pub fn parse_ternary_expr(
    //         parser: &Parser,
    //         left: Box<Expression>,
    //     ) -> Result<Expression> {
    //         Ok(Expression::Ternary(Ternary {
    //             cond: left,
    //             question: parser.expect_token(TokenKind::QuestionMark)?,
    //             if_true: Expression::parse(parser, Precedence::Lowest)?.into(),
    //             colon: parser.expect_token(TokenKind::SpacedColon)?,
    //             if_false: parser.parse_expression(Precedence::Lowest)?.into(),
    //         }))
    //     }
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a Lexer,
    token_buffer: RefCell<VecDeque<Token<'a>>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a Lexer) -> Self {
        let mut tokens = VecDeque::new();
        tokens.push_back(lexer.next_token().unwrap());
        tokens.push_back(lexer.next_token().unwrap());

        Self {
            token_buffer: RefCell::new(tokens),
            lexer,
        }
    }

    pub fn pop(&self) -> TokenOwned {
        let mut tokens = self.token_buffer.borrow_mut();
        if tokens.len() <= 2 {
            tokens.push_back(self.lexer.next_token().unwrap());
        }

        tokens.pop_front().unwrap().into()
    }

    fn front_ref(&self) -> Ref<'_, Token> {
        Ref::map(self.token_buffer.borrow(), |mi| &mi[0])
    }

    fn front_owned(&self) -> TokenOwned {
        (&self.token_buffer.borrow()[0]).into()
    }

    fn front_text(&self) -> String {
        self.token_buffer.borrow()[0].text.to_string()
    }

    fn cur_kind(&self) -> TokenKind {
        self.token_buffer.borrow()[0].kind.clone()
    }

    fn peek_text(&self) -> &str {
        self.peek_ntext(1)
    }

    fn peek_ntext(&self, n: usize) -> &str {
        todo!("peek_text")
    }

    fn peek_kind(&self) -> TokenKind {
        self.peek_nkind(1)
    }

    fn peek_nkind(&self, n: usize) -> TokenKind {
        self.peek_n(n).kind
    }

    fn peek_real_kind(&self) -> TokenKind {
        self.peek_non_whitespace().0.kind
    }

    fn current_precedence(&self) -> Precedence {
        self.get_precedence(&self.cur_kind()).unwrap_or_default()
    }

    fn line_matches<F>(&self, f: F) -> bool
    where
        F: Fn(&TokenOwned) -> bool,
    {
        let mut peek_index = 0;
        loop {
            let tok = self.peek_n(peek_index);
            if tok.kind == TokenKind::EndOfLine
                || tok.kind == TokenKind::EndOfFile
            {
                return false;
            } else if f(&tok) {
                return true;
            }

            peek_index += 1
        }
    }

    fn line_contains_kind(&self, kind: TokenKind) -> bool {
        let mut peek_index = 0;
        loop {
            let tok = self.peek_n(peek_index);
            if tok.kind == TokenKind::EndOfLine
                || tok.kind == TokenKind::EndOfFile
            {
                return false;
            } else if tok.kind == kind {
                return true;
            }

            peek_index += 1
        }
    }

    fn line_contains_any<F>(&self, f: F) -> bool
    where
        F: Fn(&TokenOwned) -> bool,
    {
        let mut peek_index = 0;
        loop {
            let tok = self.peek_n(peek_index);
            if tok.kind == TokenKind::EndOfLine
                || tok.kind == TokenKind::EndOfFile
            {
                return false;
            } else if f(&tok) {
                return true;
            }

            peek_index += 1
        }
    }

    fn peek_non_whitespace(&self) -> (TokenOwned, bool) {
        // let mut peek_index = 1;
        // let mut peek_token = self.peek_token.clone();
        let mut peek_index = 1;
        loop {
            let kind = self.peek_nkind(peek_index);
            if kind.is_eof() {
                break;
            }

            if !kind.is_whitespace() {
                break;
            }

            peek_index += 1;
        }

        (self.peek_n(peek_index), peek_index > 1)
    }

    fn peek_precedence(&self) -> Precedence {
        let kind = self.peek_non_whitespace().0.kind;
        self.get_precedence(&kind).unwrap_or_default()
    }

    fn get_prefix_fn(&self) -> Option<PrefixFn> {
        use prefix_expr::*;
        use TokenKind::*;

        Some(Box::new(match self.cur_kind() {
            Integer | Float => parse_number,
            Identifier => parse_identifier,
            Register => parse_register,
            Ampersand => parse_vim_option,
            DoubleQuoteString => parse_double_string,
            SingleQuoteString => parse_single_string,
            InterpolatedString => parse_interpolated,
            InterpolatedLiteralString => parse_interpolated_lit,
            // EnvironmentVariable => parse_env_var,
            LeftParen => parse_grouped_expr,
            LeftBracket => parse_array_literal,
            LeftBrace => parse_dict_literal,
            // Colon => parse_prefix_colon,
            // SpacedColon => parse_prefix_spaced_colon,
            // AngleLeft => parse_expandable_sequence,
            True | False => parse_bool,
            Plus | Minus | Bang | Percent => parse_prefix_operator,
            _ => return None,
        }))
    }

    fn get_precedence(&self, kind: &TokenKind) -> Option<Precedence> {
        Some(match kind {
            TokenKind::Plus | TokenKind::Minus => Precedence::Sum,
            TokenKind::Div | TokenKind::Mul => Precedence::Product,
            TokenKind::Or => Precedence::Or,
            TokenKind::And => Precedence::And,
            TokenKind::Dot => Precedence::Dot,
            TokenKind::Percent => Precedence::Modulo,
            TokenKind::StringConcat => Precedence::StringConcat,
            TokenKind::LeftParen => Precedence::Call,
            TokenKind::LeftBracket => Precedence::Index,
            TokenKind::Colon => Precedence::Colon,
            TokenKind::Comma => Precedence::Lowest,
            TokenKind::MethodArrow => Precedence::MethodCall,
            t if t.is_comparison() => Precedence::LessGreater,
            TokenKind::RightBracket
            | TokenKind::RightBrace
            | TokenKind::RightParen => Precedence::Lowest,
            TokenKind::EndOfLine | TokenKind::EndOfFile => Precedence::Lowest,

            // We have to check new lines to see if we need to handle anything there.
            //  I'm not sure this is 100% great, but we'll leave it this way for now.
            TokenKind::Identifier | TokenKind::Comment => Precedence::Lowest,

            // TODO: Not confident that this is the right level
            TokenKind::AngleLeft => Precedence::Lowest,
            TokenKind::Equal
            | TokenKind::PlusEquals
            | TokenKind::MinusEquals
            | TokenKind::MulEquals
            | TokenKind::DivEquals
            | TokenKind::StringConcatEquals => Precedence::Lowest,
            TokenKind::QuestionMark => Precedence::Ternary,

            TokenKind::SpacedColon => {
                // panic!("Should not received spaced colon here: {:#?}", self)
                Precedence::Lowest
            }

            _ => {
                panic!("Unexpected precendence kind: {:?} // {:#?}", kind, self)
            }
        })
    }

    fn get_infix_fn(&self) -> Option<InfixFn> {
        let (peek_token, skipped) = self.peek_non_whitespace();

        Some(Box::new(match peek_token.kind {
            // Mathemtical operations
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Div
            | TokenKind::Percent
            | TokenKind::StringConcat => infix_expr::parse_infix_operator,
            // Logical comparisons
            TokenKind::Or | TokenKind::And => infix_expr::parse_infix_operator,
            TokenKind::LeftParen => infix_expr::parser_call_expr,
            TokenKind::LeftBracket => infix_expr::parser_index_expr,
            TokenKind::Colon => {
                if skipped {
                    return None;
                } else {
                    infix_expr::parse_colon
                }
            }
            TokenKind::MethodArrow => infix_expr::parse_method_call,
            TokenKind::EqualTo
            | TokenKind::EqualToIns
            | TokenKind::NotEqualTo
            | TokenKind::NotEqualToIns
            | TokenKind::LessThan
            | TokenKind::LessThanIns
            | TokenKind::LessThanOrEqual
            | TokenKind::LessThanOrEqualIns
            | TokenKind::GreaterThan
            | TokenKind::GreaterThanIns
            | TokenKind::GreaterThanOrEqual
            | TokenKind::GreaterThanOrEqualIns
            | TokenKind::RegexpMatches
            | TokenKind::RegexpMatchesIns
            | TokenKind::NotRegexpMatches
            | TokenKind::NotRegexpMatchesIns
            | TokenKind::Is
            | TokenKind::IsInsensitive
            | TokenKind::IsNot
            | TokenKind::IsNotInsensitive => infix_expr::parse_infix_operator,
            TokenKind::Dot => infix_expr::parse_dot_operator,
            // TokenKind::QuestionMark => infix_expr::parse_ternary_expr,
            // TokenKind::SpacedColon => infix_expr::parser_index_type,
            TokenKind::Identifier => return None,
            _ => unimplemented!("get_infix_fn: {:#?}", self),
        }))
    }

    fn parse_expression(&self, prec: Precedence) -> Result<Expression> {
        // info!("parseing expr: {:#?}", prec);

        self.skip_whitespace();

        let prefix = self.get_prefix_fn();

        #[allow(unused_mut)]
        let mut left = match prefix {
            Some(prefix) => prefix(self)?,
            None => {
                return Err(anyhow::anyhow!(
                    "Failed to find prefix function for {:#?}",
                    self
                ));
            }
        };

        // TODO: Some things may not allow newlines, but for now, let's just skip them
        // while self.cur_kind() == TokenKind::EndOfLine {
        //     self.next_token();
        // }

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

    pub fn expect_eol(&self) -> Result<TokenMeta> {
        let curkind = self.cur_kind();
        if curkind == TokenKind::EndOfLine
            || curkind == TokenKind::EndOfFile
                // We kind of cheat to say end of line comments are fine...
                // even tho sometimes they are not
            || curkind == TokenKind::Comment
        {
            Ok(self.pop().into())
        } else {
            Err(anyhow::anyhow!("expected eol or eof, got: {:?}", self))
        }
    }

    pub fn ensure_token(&self, kind: TokenKind) -> Result<TokenMeta> {
        let token = self.front_ref();
        if token.kind != kind {
            return Err(anyhow::anyhow!(
                "[ensure_token] Got token: {:?}, Expected: {:?}",
                token,
                kind
            ));
        }

        Ok(token.into())
    }

    pub fn ensure_peek(&self, kind: TokenKind) -> Result<TokenMeta> {
        let tok = self.peek_n(1);
        if tok.kind != kind {
            return Err(anyhow::anyhow!(
                "[ensure_peek] Got token: {:?}, Expected: {:?}",
                tok,
                kind
            ));
        }

        Ok(self.peek_n(1).into())
    }

    pub fn expect_token(&self, kind: TokenKind) -> Result<TokenOwned> {
        let token = self.front_owned();
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
    pub fn expect_peek(&self, kind: TokenKind) -> Result<TokenOwned> {
        let tok = self.peek();
        if tok.kind != kind {
            return Err(anyhow::anyhow!(
                "Got token: {:?}, Expected: {:?}",
                tok,
                kind
            ));
        }

        self.next_token();
        Ok(self.front_owned())
    }

    pub fn expect_fn<F>(&self, f: F, consume: bool) -> Result<TokenOwned>
    where
        F: Fn(&TokenKind) -> bool,
    {
        self.expect_fn_token(|t| f(&t.kind), consume)
    }

    pub fn expect_fn_token<F>(&self, f: F, consume: bool) -> Result<TokenOwned>
    where
        F: Fn(&Token) -> bool,
    {
        let token = self.front_ref();
        if !f(&token) {
            return Err(anyhow::anyhow!("[expect_fn] Got token: {:?}", token));
        }

        let token = token.into();
        if consume {
            self.next_token();
        }

        Ok(token)
    }

    pub fn expect_token_with_text(
        &self,
        kind: TokenKind,
        text: &str,
    ) -> Result<TokenOwned> {
        let token = self.front_owned();
        if token.kind != kind {
            return Err(anyhow::anyhow!(
                "Got token: {:?}, Expected kind: {:?}",
                token,
                kind
            ));
        }

        if !token.text.eq(text) {
            return Err(anyhow::anyhow!(
                "Got token: {:?}, Expected text: {:?}",
                token,
                text
            ));
        }

        self.next_token();
        Ok(token)
    }

    pub fn peek_identifier_with_text(&self, text: &str) -> bool {
        self.peek_nkind(1) == TokenKind::Identifier && self.peek_text().eq(text)
    }

    pub fn expect_identifier_with_text(
        &self,
        text: &str,
    ) -> Result<TokenOwned> {
        self.expect_token_with_text(TokenKind::Identifier, text)
    }

    pub fn next_token(&self) {
        self.pop();
    }

    pub fn next_real_token(&self) {
        self.skip_whitespace();
        self.next_token();
    }

    pub fn peek(&self) -> TokenOwned {
        self.peek_n(1)
    }

    pub fn peek_n(&self, n: usize) -> TokenOwned {
        let mut tokens = self.token_buffer.borrow_mut();
        match tokens.len() {
            0 | 1 => panic!("Should always have a least 2 entries"),
            len if len - 1 >= n => (&tokens[n]).into(),
            len => {
                for _ in 0..(n + 1 - len) {
                    tokens.push_back(self.lexer.next_token().unwrap())
                }

                let res = (&tokens[n]).into();
                res
            }
        }
    }

    fn command_match(&self, full: &str) -> bool {
        self.front_ref().text.eq(full)
    }

    pub fn parse_command(&self) -> Result<ExCommand> {
        // If the line starts with a colon, then just skip over it.
        if self.cur_kind() == TokenKind::Colon {
            self.next_token();
        }

        // TODO: Handle modifiers.
        // Perhaps return as separate item/note for ExCommand, or return as tuple.
        //  Could additionally be added as ExCommand parent type or something.
        //  Will just be an annoying amount of writing :)
        if self.front_ref().text.eq("silent") {
            self.next_token();
            if self.cur_kind() == TokenKind::Bang {
                self.next_token();
            }
        }

        // For the following branches, you need to return early if it completely consumes
        // the last character and advances past.
        //
        // This is the desired behavior for `parse` which will consume until the end of line
        // generally speaking.
        Ok(match &self.cur_kind() {
            TokenKind::EndOfFile => ExCommand::NoOp(self.pop().into()),
            TokenKind::Comment => ExCommand::Comment(self.pop().into()),
            TokenKind::EndOfLine => ExCommand::NoOp(self.pop().into()),
            TokenKind::Identifier => {
                if self.command_match("vim9script") {
                    self.next_token();
                    ExCommand::Vim9Script(Vim9ScriptCommand::parse(self)?)
                } else if self.command_match("execute") {
                    ExecuteCommand::parse(self)?
                } else if self.command_match("var")
                    || self.command_match("const")
                {
                    VarCommand::parse(self)?
                } else if self.command_match("echo")
                    || self.command_match("echon")
                    || self.command_match("echomsg")
                {
                    EchoCommand::parse(self)?
                } else if self.command_match("call") {
                    CallCommand::parse(self)?
                } else if self.command_match("def") {
                    DefCommand::parse(self)?
                } else if self.command_match("if") {
                    return Ok(IfCommand::parse(self)?);
                } else if self.command_match("return") {
                    ReturnCommand::parse(self)?
                } else if self.command_match("finish") {
                    FinishCommand::parse(self)?
                } else if CallCommand::matches(self) {
                    CallCommand::parse(self)?
                } else if StatementCommand::matches(self) {
                    StatementCommand::parse(self)?
                } else {
                    SharedCommand::parse(self)?
                }

                // if self.command_match("vim9script") {
                //     self.next_token();
                //     ExCommand::Vim9Script(Vim9ScriptCommand::parse(self)?)
                // } else {
                //     } else if self.command_match("unlet") {
                //         println!("TODO: UNLET");
                //         return Ok(SharedCommand::parse(self)?);
                //     } else if self.command_match("export") {
                //         return Ok(ExportCommand::parse(self)?);
                //     } else if self.command_match("for") {
                //         return Ok(ForCommand::parse(self)?);
                //     } else if self.command_match("while") {
                //         return Ok(WhileCommand::parse(self)?);
                //     } else if self.command_match("try") {
                //         return Ok(TryCommand::parse(self)?);
                //     } else if self.command_match("import") {
                //         return Ok(ImportCommand::parse(self)?);
                //     } else if self.command_match("echo")
                //         || self.command_match("echon")
                //         || self.command_match("echomsg")
                //     {
                //         return Ok(EchoCommand::parse(self)?);
                //     } else if self.command_match("defer") {
                //         return Ok(DeferCommand::parse(self)?);
                //     } else if self.command_match("augroup") {
                //         return Ok(AugroupCommand::parse(self)?);
                //     } else if self.command_match("autocmd") {
                //         return Ok(AutocmdCommand::parse(self)?);
                //     } else if self.command_match("break") {
                //         return Ok(BreakCommand::parse(self)?);
                //     } else if self.command_match("continue") {
                //         return Ok(ContinueCommand::parse(self)?);
                //     } else if self.command_match("command") {
                //         return Ok(UserCommand::parse(self)?);
                //     } else if self.command_match("set")
                //         || self.command_match("setlocal")
                //     {
                //         return Ok(SharedCommand::parse(self)?);
                //     } else if self.command_match("nnoremap")
                //         || self.command_match("inoremap")
                //         || self.command_match("anoremenu")
                //         || self.command_match("normal")
                //     {
                //         // TODO: Make mapping command
                //         return Ok(SharedCommand::parse(self)?);
                //     } else {
                //         if CallCommand::matches(self) {
                //             return Ok(CallCommand::parse(self)?);
                //         } else if self
                //             .line_contains_kind(TokenKind::MethodArrow)
                //         {
                //             return Ok(EvalCommand::parse(self)?);
                //         } else {
                //             // ExCommand::NoOp(self.front().clone())
                //             return Ok(SharedCommand::parse(self)?);
                //         }
                //     }
                // }
            }
            // TokenKind::LeftBracket => return Ok(EvalCommand::parse(self)?),
            _ => ExCommand::NoOp(self.pop().into()),
        })
    }

    pub fn parse_program(&self) -> Program {
        let mut program = Program { commands: vec![] };

        while self.cur_kind() != TokenKind::EndOfFile {
            // let command = match self.parse_command() {
            //     Ok(command) => command,
            //     Err(err) => panic!(
            //         "\nFailed to parse command.\nCurrent Commands:{:#?}.\nError: {}",
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

    // Ends with the parser pointing to the close token as the current token
    fn list_parser<T, F>(&self, close: TokenKind, parse: F) -> Result<Vec<T>>
    where
        T: Debug,
        F: Fn(&Self) -> Result<T>,
    {
        let mut results = vec![];
        if self.peek_real_kind() == close {
            self.next_real_token();
            return Ok(results);
        }

        // Consume opening token
        self.next_real_token();

        // Consume first T
        results.push(parse(self)?);

        while self.peek_real_kind() != close {
            // Consume last token of T
            self.next_real_token();

            if self.cur_kind() == close {
                return Ok(results);
            } else if self.peek_kind() == close {
                break;
            }

            // Must have comma
            self.expect_token(TokenKind::Comma)
                .expect("list_parser: comma");

            // Trailing commas are generally speaking accepted.
            //  I don't actually care that sometimes they aren't.
            //  I'll let bramvim handle that
            self.skip_whitespace();
            if self.cur_kind() == close {
                return Ok(results);
            }

            // Next T
            results.push(parse(self)?);

            self.skip_whitespace();
            if self.cur_kind() == close {
                return Ok(results);
            } else if self.peek_kind() == close {
                break;
            }

            if self.cur_kind().is_eof() {
                panic!("EOF");
            }
        }

        if self.peek_kind() != close {
            self.skip_peeked_whitespace()
        }

        self.expect_peek(close)?;
        Ok(results)
    }

    fn parse_identifier_list(&self, k: TokenKind) -> Result<Vec<Identifier>> {
        self.list_parser(k, |p| Identifier::parse_in_expression(p))
    }

    fn parse_expression_list(&self, k: TokenKind) -> Result<Vec<Expression>> {
        self.list_parser(k, |p| p.parse_expression(Precedence::Lowest))
    }

    fn parse_keyvalue_list(&self, k: TokenKind) -> Result<Vec<KeyValue>> {
        self.list_parser(k, |p| KeyValue::parse(p))
    }

    #[tracing::instrument]
    fn skip_peeked_whitespace(&self) {
        if self.peek_kind() == TokenKind::EndOfFile {
            return;
        }

        while self.peek_kind().is_whitespace() {
            if self.peek_kind() == TokenKind::EndOfFile {
                break;
            }

            self.next_token();
        }
    }

    fn skip_whitespace(&self) {
        while self.cur_kind().is_whitespace()
            || self.cur_kind() == TokenKind::Comment
        {
            if self.cur_kind() == TokenKind::EndOfFile {
                break;
            }

            self.next_token();
        }
    }

    fn consume_if_kind(&self, kind: TokenKind) -> Option<TokenOwned> {
        if self.cur_kind() == kind {
            Some(self.pop())
        } else {
            None
        }
    }

    pub fn read_until<F, T>(&self, f: F)
    where
        F: Fn(&Token) -> bool,
    {
        while !f(&self.front_ref()) && !self.cur_kind().is_eof() {
            self.pop();
        }
    }
}

fn snapshot_parsing(input: &str) -> String {
    let lexer = Lexer::new(input);
    let parser = Parser::new(&lexer);
    let program = parser.parse_program();

    format!("{:#?}", program.commands)
}

pub fn new_parser<'a>(lexer: &'a Lexer) -> Parser<'a> {
    Parser::new(&lexer)
}

pub fn setup_trace() {
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
    use crate::*;

    macro_rules! snap {
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

    snap!(test_var, "../testdata/snapshots/simple_var.vim");
    snap!(test_ret, "../testdata/snapshots/simple_ret.vim");
    snap!(test_shared, "../testdata/snapshots/shared.vim");
    snap!(test_comment, "../testdata/snapshots/comment.vim");
    snap!(test_header, "../testdata/snapshots/header.vim");
    snap!(test_expr, "../testdata/snapshots/expr.vim");
    snap!(test_echo, "../testdata/snapshots/echo.vim");
    snap!(test_execute, "../testdata/snapshots/execute.vim");
    snap!(test_scopes, "../testdata/snapshots/scopes.vim");
    snap!(test_array, "../testdata/snapshots/array.vim");
    snap!(test_dict, "../testdata/snapshots/dict.vim");
    snap!(test_if, "../testdata/snapshots/if.vim");
    snap!(test_call, "../testdata/snapshots/call.vim");
    snap!(test_concat, "../testdata/snapshots/concat.vim");
    snap!(test_assign, "../testdata/snapshots/assign.vim");
    snap!(test_vimvar, "../testdata/snapshots/vimvar.vim");
    snap!(test_busted, "../testdata/snapshots/busted.vim");
    snap!(test_heredoc, "../testdata/snapshots/heredoc.vim");
    snap!(test_typed_params, "../testdata/snapshots/typed_params.vim");
    // snap!(test_index, "../testdata/snapshots/index.vim");
    // snap!(test_adv_index, "../testdata/snapshots/adv_index.vim");
    // snap!(test_multiline, "../testdata/snapshots/multiline.vim");
    // snap!(test_cfilter, "../testdata/snapshots/cfilter.vim");
    // snap!(test_lambda, "../testdata/snapshots/lambda.vim");
    // snap!(test_comparisons, "../testdata/snapshots/comparisons.vim");
    // snap!(test_methods, "../testdata/snapshots/methods.vim");
    // snap!(test_eval, "../testdata/snapshots/eval.vim");
    // snap!(test_export, "../testdata/snapshots/export.vim");
    // snap!(test_import, "../testdata/snapshots/import.vim");
    // snap!(test_autocmd, "../testdata/snapshots/autocmd.vim");
    // snap!(test_unpack, "../testdata/snapshots/unpack.vim");

    // https://github.com/yegappan/lsp test suite
    // snap!(test_handlers, "../../shared/snapshots/lsp_handlers.vim");
    // snap!(test_selection, "../../shared/snapshots/lsp_selection.vim");
    // snap!(test_fileselect, "../../shared/snapshots/lsp_fileselect.vim");

    #[test]
    fn test_peek_n() {
        let input = "vim9script\nvar x = true\n";
        let lexer = Lexer::new(input);
        let parser = Parser::new(&lexer);
        assert_eq!(parser.peek_kind(), TokenKind::EndOfLine);
        assert_eq!(parser.peek_n(0).kind, TokenKind::Identifier);
        assert_eq!(parser.peek_n(1).kind, TokenKind::EndOfLine);
        assert_eq!(parser.peek_n(2).kind, TokenKind::Identifier);
        assert_eq!(parser.peek_n(3).kind, TokenKind::Identifier);
        assert_eq!(parser.peek_n(4).kind, TokenKind::Equal);
        assert_eq!(parser.peek_n(1).kind, TokenKind::EndOfLine);
    }

    // TODO: Slowly but surely, we can work towards this
    // snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");
}
