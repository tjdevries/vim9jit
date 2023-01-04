#![allow(unused_variables)]
#![allow(dead_code)]

use std::{
    cell::{Ref, RefCell},
    collections::{HashSet, VecDeque},
    fmt::Debug,
};

use anyhow::Result;
use macros::parse_context;
use once_cell::sync::OnceCell;
use tracing_subscriber::util::SubscriberInitExt;
use types::TypeOpts;
use vim9_lexer::{Lexer, Span, Token, TokenKind};
mod cmds;
pub use cmds::{
    cmd_auto::{AugroupCommand, AutocmdBlock, AutocmdCommand},
    cmd_if::{ElseCommand, ElseIfCommand, IfCommand},
    cmd_try::TryCommand,
    cmd_user::UserCommand,
    BreakCommand, ContinueCommand, DeferCommand,
};

mod types;
pub use types::Type;

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
    For(ForCommand),
    While(WhileCommand),
    Try(TryCommand),
    Call(CallCommand),
    Defer(DeferCommand),
    Eval(EvalCommand),
    Finish(FinishCommand),
    Break(BreakCommand),
    Continue(ContinueCommand),
    Augroup(AugroupCommand),
    Autocmd(AutocmdCommand),
    Statement(StatementCommand),
    UserCommand(UserCommand),
    SharedCommand(SharedCommand),
    ExportCommand(ExportCommand),
    ImportCommand(ImportCommand),
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
        parser.next_token();

        Ok(Self {
            noclear: if parser.front_kind() == TokenKind::EndOfLine {
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

#[parse_context]
impl DefCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Def(DefCommand {
            def: parser.expect_identifier_with_text("def")?.into(),
            name: Identifier::parse(parser)?,
            args: Signature::parse(parser)?,
            ret: {
                if parser.front_kind() == TokenKind::SpacedColon {
                    Some(Type::parse(parser, &TypeOpts { bool: Type::Bool })?)
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
            expr: match parser.front_kind() {
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

#[parse_context]
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

#[parse_context]
impl CallExpression {
    pub fn name(&self) -> Option<&Identifier> {
        match self.expr.as_ref() {
            Expression::Identifier(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn parse(parser: &Parser, left: Box<Expression>) -> Result<CallExpression> {
        Ok(CallExpression {
            expr: left,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen)?,
            close: parser.ensure_token(TokenKind::RightParen)?,
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
pub struct ExportCommand {
    export: TokenMeta,
    pub command: Box<ExCommand>,
}

impl ExportCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::ExportCommand(Self {
            export: parser.expect_identifier_with_text("export")?.into(),
            command: parser.parse_command()?.into(),
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ImportCommand {
    ImportImplicit {
        import: TokenMeta,
        autoload: bool,
        file: String,
        // Optional `as` qualifier to rename the import locally
        name: Option<Expression>,
        eol: TokenMeta,
    },
    ImportUnpacked {
        import: TokenMeta,
        names: Vec<Identifier>,
        from: TokenMeta,
        file: String,
        eol: TokenMeta,
    },
}

impl ImportCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let import = parser.expect_identifier_with_text("import")?.into();
        let command = match parser.front_kind() {
            TokenKind::LeftBrace => ImportCommand::ImportUnpacked {
                import,
                names: {
                    let names = parser.parse_identifier_list(TokenKind::RightBrace)?;
                    parser.ensure_token(TokenKind::RightBrace)?;
                    parser.next_token();
                    names
                },
                from: parser.expect_identifier_with_text("from")?.into(),
                file: parser.pop().text.to_string(),
                eol: parser.expect_eol()?,
            },
            TokenKind::Identifier | TokenKind::SingleQuoteString | TokenKind::DoubleQuoteString => {
                let autoload = if parser.front_ref().text.eq("autoload") {
                    parser.next_token();
                    true
                } else {
                    false
                };

                ImportCommand::ImportImplicit {
                    import,
                    autoload,
                    file: parser.pop().text.to_string(),
                    name: {
                        if parser.front_ref().text.eq("as") {
                            // pop as
                            parser.pop();

                            Some(Expression::parse(parser, Precedence::Lowest)?)
                        } else {
                            None
                        }
                    },
                    eol: parser.expect_eol()?,
                }
            }
            _ => unreachable!("{:?}", parser),
        };

        Ok(ExCommand::ImportCommand(command))
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
        while !parser.front_kind().is_whitespace() {
            let tok = parser.pop();

            if prev_end > tok.span.start_col {
                panic!("failed to make shared command: {:#?}", parser);
            }

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
                |t| matches!(t.text.to_string().as_str(), "echo" | "echon" | "echomsg"),
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

#[derive(Debug, PartialEq, Clone)]
pub struct ForCommand {
    for_: TokenMeta,
    pub for_identifier: Identifier,
    pub for_type: Option<Type>,
    in_: TokenMeta,
    pub for_expr: Expression,
    eol: TokenMeta,
    pub body: Body,
    endfor_: TokenMeta,
    endfor_eol: TokenMeta,
}

impl ForCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::For(ForCommand {
            for_: parser.expect_identifier_with_text("for")?.into(),
            for_identifier: Identifier::parse(parser)?,
            for_type: if parser.front_kind() == TokenKind::SpacedColon {
                Some(Type::parse(
                    parser,
                    &TypeOpts {
                        bool: Type::BoolOrNumber,
                    },
                )?)
            } else {
                None
            },
            in_: parser.expect_identifier_with_text("in")?.into(),
            for_expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
            body: Body::parse_until(parser, "endfor")?,
            endfor_: parser.expect_identifier_with_text("endfor")?.into(),
            endfor_eol: parser.expect_eol()?,
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct WhileCommand {
    while_: TokenMeta,
    pub condition: Expression,
    while_eol: TokenMeta,
    pub body: Body,
    endwhile_: TokenMeta,
    endwhile_eol: TokenMeta,
}

impl WhileCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::While(WhileCommand {
            while_: parser.expect_identifier_with_text("while")?.into(),
            condition: Expression::parse(parser, Precedence::Lowest)?,
            while_eol: parser.expect_eol()?,
            body: Body::parse_until(parser, "endwhile")?,
            endwhile_: parser.expect_identifier_with_text("endwhile")?.into(),
            endwhile_eol: parser.expect_eol()?,
        }))
    }
}

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

#[derive(Debug, PartialEq, Clone)]
pub struct Block {
    open: TokenMeta,
    pub body: Body,
    close: TokenMeta,
    eol: TokenMeta,
}

impl Block {
    pub fn parse(parser: &Parser) -> Result<Block> {
        Ok(Self {
            open: parser.expect_token(TokenKind::LeftBrace)?.into(),
            body: Body::parse_until(parser, "}")?,
            close: parser.expect_token(TokenKind::RightBrace)?.into(),
            eol: parser.expect_eol()?,
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementCommand {
    Assign(AssignStatement),
    Mutate(MutationStatement),
}

impl StatementCommand {
    pub fn matches(parser: &Parser) -> bool {
        parser.front_kind() == TokenKind::Colon
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
        if parser.front_kind() == TokenKind::Equal {
            return Ok(ExCommand::Statement(StatementCommand::Assign(
                AssignStatement {
                    left: expr,
                    equals: parser.expect_token(TokenKind::Equal)?.into(),
                    right: {
                        let right = parser.parse_expression(Precedence::Lowest)?;
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

#[parse_context]
impl Body {
    pub fn parse_until_any(parser: &Parser, identifiers: &HashSet<String>) -> Result<Body> {
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

#[parse_context]
impl Signature {
    fn parse(parser: &Parser) -> Result<Signature> {
        parser.skip_whitespace();

        Ok(Self {
            open: parser.ensure_token(TokenKind::LeftParen)?.into(),
            params: parser.parse_paramter_list()?,
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

#[parse_context]
impl Parameter {
    fn parse(parser: &Parser) -> Result<Parameter> {
        parser.skip_whitespace();

        let name = Identifier::parse_in_expression(parser)?;

        let ty = if parser.peek_real_kind() == TokenKind::SpacedColon {
            parser.next_real_token();
            Some(Type::parse_in_expression(
                parser,
                &TypeOpts {
                    bool: Type::BoolOrNumber,
                },
            )?)
        } else {
            None
        };

        let (equal, default_val) = if parser.peek_real_kind() == TokenKind::Equal {
            parser.next_real_token();
            (
                Some(parser.expect_token(TokenKind::Equal)?.into()),
                Some(parser.parse_expression(Precedence::Lowest)?),
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
            Identifier::Ellipsis => false,
            Identifier::Unpacked(unpacked) => unpacked
                .identifiers
                .iter()
                .all(|ident| ident.is_valid_local()),
        }
    }

    fn parse_in_expression(parser: &Parser) -> Result<Identifier> {
        if parser.front_kind() == TokenKind::LeftBracket {
            return Ok(Identifier::Unpacked(UnpackIdentifier {
                open: parser.ensure_token(TokenKind::LeftBracket)?,
                identifiers: parser.parse_identifier_list(TokenKind::RightBracket)?,
                close: parser.ensure_token(TokenKind::RightBracket)?,
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
    Lambda(Lambda),
    Expandable(Expandable),
    MethodCall(MethodCall),
    Ternary(Ternary),

    // Composite types
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

#[derive(Debug, PartialEq, Clone)]
pub struct Ternary {
    pub cond: Box<Expression>,
    question: TokenMeta,
    pub if_true: Box<Expression>,
    colon: TokenMeta,
    pub if_false: Box<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct MethodCall {
    pub left: Box<Expression>,
    tok: TokenMeta,
    pub right: Box<CallExpression>,
}

#[parse_context]
impl MethodCall {
    pub fn parse(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::MethodCall(MethodCall {
            left,
            tok: {
                parser.skip_whitespace();
                parser.expect_token(TokenKind::MethodArrow)?.into()
            },
            right: {
                // Parse up to the point it would be a call expr
                let base = Expression::parse(parser, Precedence::Call)?.into();

                parser.ensure_token(TokenKind::LeftParen)?;

                // Create the call expr from the first base expression
                let right = CallExpression::parse(parser, base)?.into();

                // Closing on right paren, DO NOT advance
                parser.ensure_token(TokenKind::RightParen)?;

                right
            },
        }))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expandable {
    left: TokenMeta,
    pub ident: Identifier,
    right: TokenMeta,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DictAccess {
    pub container: Box<Expression>,
    dot: TokenMeta,
    pub index: RawIdentifier,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    pub args: Signature,
    pub ret: Option<Type>,
    arrow: TokenMeta,
    pub body: Body,
}

impl Lambda {
    pub fn parse(parser: &Parser) -> Result<Lambda> {
        Ok(Lambda {
            args: Signature::parse(parser)?,
            ret: {
                if parser.front_kind() == TokenKind::SpacedColon {
                    Some(Type::parse(
                        parser,
                        &TypeOpts {
                            bool: Type::BoolOrNumber,
                        },
                    )?)
                } else {
                    None
                }
            },
            arrow: parser.expect_token(TokenKind::Arrow)?.into(),
            body: {
                if parser.front_kind() == TokenKind::LeftBrace {
                    todo!("parse blocks correctly");
                } else {
                    Body {
                        commands: {
                            let mut v = Vec::new();
                            v.push(ExCommand::Return(ReturnCommand::fake(Some(
                                parser.parse_expression(Precedence::Lowest)?,
                            ))));
                            v
                        },
                    }
                }
            },
        })
    }
}

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
        let (colon, left) = match parser.front_kind() {
            TokenKind::Colon | TokenKind::SpacedColon => (parser.pop().into(), None),
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
                parser.pop();
                let colon = parser.front_owned();
                anyhow::ensure!(
                    colon.kind.is_colon(),
                    "[IndexType] token: {:#?}, parser: {:#?}, slice: {:#?}",
                    colon,
                    parser,
                    left
                );

                // Move past the colon, so that we're on the expression to the right
                parser.next_token();

                (colon.into(), Some(left.into()))
            }
        };

        if parser.front_kind() == TokenKind::RightBracket {
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
            key: match parser.front_kind() {
                TokenKind::Identifier => VimKey::Literal(parser.pop().try_into()?),
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

                    let expr = VimKey::Expression(Expression::parse(parser, Precedence::Lowest)?);

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
    pub fn new(operator: Operator, left: Box<Expression>, right: Box<Expression>) -> Self {
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
    Increment,
    Decrement,

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

impl Operator {
    pub fn is_comparison(&self) -> bool {
        use Operator::*;

        matches!(
            self,
            EqualTo
                | EqualToIns
                | NotEqualTo
                | NotEqualToIns
                | LessThan
                | LessThanIns
                | LessThanOrEqual
                | LessThanOrEqualIns
                | GreaterThan
                | GreaterThanIns
                | GreaterThanOrEqual
                | GreaterThanOrEqualIns
                | RegexpMatches
                | RegexpMatchesIns
                | NotRegexpMatches
                | NotRegexpMatchesIns
                | Is
                | IsInsensitive
                | IsNot
                | IsNotInsensitive
        )
    }

    pub fn is_math(&self) -> bool {
        use Operator::*;
        matches!(self, Plus | Minus | Multiply | Divide)
    }

    pub fn literal(&self) -> Option<&'static str> {
        Some(match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Bang => todo!(),
            Operator::Modulo => "%",
            Operator::Or => "or",
            Operator::And => "and",
            Operator::StringConcat => "..",
            Operator::Divide => "/",
            Operator::Multiply => "*",
            Operator::EqualTo => "==",
            Operator::NotEqualTo => "~=",
            Operator::LessThan => "<",
            Operator::LessThanOrEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanOrEqual => ">=",
            Operator::Is => todo!(),
            Operator::IsInsensitive => todo!(),
            Operator::IsNot => todo!(),
            Operator::IsNotInsensitive => todo!(),
            _ => return None,
        })
    }
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

    /// PrefixExpr7 has a separate precendence:
    ///
    /// When using -> the |expr7| operators will be applied first, thus:  
    ///  -1.234->string()
    /// Is equivalent to:  
    ///  (-1.234)->string()
    /// And NOT:  
    ///  -(1.234->string())
    PrefixExpr7,

    // <expr>-><funcname>()
    MethodCall,

    // +, -
    //  This is different than PrefixExpr7.
    //  See that enum for more details
    Prefix,

    Call,
    Index,
    Dot,

    // g:something, x[1 : 2], x[1 :]
    Colon,
    // --, ++
    // PrefixMutator,
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

#[parse_context]
impl VarCommand {
    pub fn matches(parser: &Parser) -> bool {
        parser.command_match("var")
            || parser.command_match("const")
            || parser.command_match("final")
    }
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        let var = parser.expect_token(TokenKind::Identifier)?;
        anyhow::ensure!(matches!(
            var.text.to_string().as_str(),
            "var" | "const" | "final"
        ));

        let var: TokenMeta = var.into();

        let name = Identifier::parse(parser)?;
        let ty = match parser.front_kind() {
            TokenKind::Equal => None,
            TokenKind::HeredocOperator => None,
            TokenKind::EndOfLine => None,
            TokenKind::EndOfFile => None,
            TokenKind::SpacedColon => Some(Type::parse(parser, &TypeOpts { bool: Type::Bool })?),
            _ => {
                return Err(anyhow::anyhow!(
                    "invalid type and/or equal for var: {:?}",
                    parser
                ))
            }
        };

        match parser.front_kind() {
            TokenKind::HeredocOperator => {
                let op = parser.expect_token(TokenKind::HeredocOperator)?.into();

                let mut trim = false;
                let mut eval = false;

                let open: TokenOwned = {
                    let mut token: TokenOwned = parser.expect_token(TokenKind::Identifier)?.into();

                    while token.text.as_str() == "trim" || token.text.as_str() == "eval" {
                        if token.text.as_str() == "trim" {
                            trim = true;
                            token = parser.expect_token(TokenKind::Identifier)?.into();
                        }

                        if token.text.as_str() == "eval" {
                            eval = true;
                            token = parser.expect_token(TokenKind::Identifier)?.into();
                        }
                    }

                    token
                };

                parser.expect_eol()?;

                let mut contents = vec![];
                let close = loop {
                    let mut line: Vec<TokenOwned> = vec![];
                    while !matches!(
                        parser.front_kind(),
                        TokenKind::EndOfLine | TokenKind::EndOfFile
                    ) {
                        line.push(parser.front_owned());
                        parser.next_token();
                    }

                    if parser.front_kind() == TokenKind::EndOfFile {
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
pub struct EvalCommand {
    eval: Option<TokenMeta>,
    pub expr: Expression,
    eol: TokenMeta,
}

impl EvalCommand {
    pub fn parse(parser: &Parser) -> Result<ExCommand> {
        Ok(ExCommand::Eval(EvalCommand {
            eval: None,
            expr: Expression::parse(parser, Precedence::Lowest)?,
            eol: parser.expect_eol()?,
        }))
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
            parser.front_kind(),
            TokenKind::Integer | TokenKind::Float
        ));

        Ok(Expression::Number(VimNumber {
            value: parser.front_text(),
        }))
    }

    pub fn parse_identifier(parser: &Parser) -> Result<Expression> {
        anyhow::ensure!(parser.front_kind() == TokenKind::Identifier);

        Ok(RawIdentifier {
            name: parser.front_text(),
        }
        .into())
    }

    pub fn parse_register(parser: &Parser) -> Result<Expression> {
        anyhow::ensure!(parser.front_kind() == TokenKind::Register);

        Ok(Expression::Register(Register {
            register: parser.front_text(),
        }))
    }

    pub fn parse_bool(parser: &Parser) -> Result<Expression> {
        Ok(Expression::Boolean(VimBoolean {
            value: match &parser.front_kind() {
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
        let (operator, prec) = match &token.kind {
            TokenKind::Plus => (Operator::Plus, Precedence::Prefix),
            TokenKind::Minus => (Operator::Minus, Precedence::Prefix),
            TokenKind::Percent => (Operator::Modulo, Precedence::Prefix),
            TokenKind::Bang => (Operator::Bang, Precedence::PrefixExpr7),
            TokenKind::Increment => (Operator::Increment, Precedence::Prefix),
            TokenKind::Decrement => (Operator::Decrement, Precedence::Prefix),
            _ => unreachable!("Not a valid prefix operator: {:?}", token),
        };

        Ok(Expression::Prefix(PrefixExpression {
            token: token.into(),
            operator,
            right: parser.parse_expression(prec)?.into(),
        }))
    }

    pub fn parse_grouped_expr(parser: &Parser) -> Result<Expression> {
        if parser.line_contains_kind(TokenKind::Arrow) {
            Ok(Expression::Lambda(Lambda::parse(parser)?))
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

    pub fn parse_prefix_colon(parser: &Parser) -> Result<Expression> {
        todo!(
            "[Colon] I don't think we ever get this in reality? {:#?}",
            parser
        );
    }

    pub fn parse_prefix_spaced_colon(parser: &Parser) -> Result<Expression> {
        unreachable!("[SpacedColon] should not happen {:#?}", parser);
    }

    pub fn parse_expandable_sequence(parser: &Parser) -> Result<Expression> {
        Ok(Expression::Expandable(Expandable {
            left: parser.expect_token(TokenKind::AngleLeft)?.into(),
            ident: {
                // read until greater than or angle,
                // smoosh into raw identifier
                let mut name = String::new();
                while !matches!(
                    parser.front_kind(),
                    TokenKind::GreaterThan | TokenKind::AngleRight
                ) {
                    name += parser.pop().text.as_str();
                }

                Identifier::Raw(RawIdentifier { name })
            },
            right: parser
                .expect_fn(
                    |k| matches!(k, TokenKind::GreaterThan | TokenKind::AngleRight),
                    false,
                )?
                .into(),
        }))
    }
}

mod infix_expr {
    use super::*;

    pub fn parse_infix_operator(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        parser.skip_whitespace();

        let prec = parser.current_precedence()?;
        let token = parser.pop();
        let operator = match token.kind {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Div => Operator::Divide,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Mul => Operator::Multiply,
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

    pub fn parse_dot_operator(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::DictAccess(DictAccess {
            container: left,
            dot: parser.expect_token(TokenKind::Dot)?.into(),
            index: RawIdentifier {
                name: parser.front_owned().text.to_string(),
            },
        }))
    }

    pub fn parse_method_call(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        MethodCall::parse(&parser, left)
    }

    pub fn parse_colon(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
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
                if parser.front_kind() == TokenKind::Colon {
                    return Ok(Expression::Identifier(Identifier::Scope(
                        ScopedIdentifier {
                            scope,
                            colon: parser.expect_token(TokenKind::Colon)?.into(),
                            accessor: Identifier::parse_in_expression(parser)?.into(),
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

        if parser.front_kind() == TokenKind::RightBracket {
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

    pub fn parse_call_expr(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::Call(CallExpression::parse(parser, left)?))
    }

    pub fn parse_index_expr(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::Index(IndexExpression {
            container: left,
            open: parser.expect_token(TokenKind::LeftBracket)?.into(),
            index: IndexType::parse(parser)?.into(),
            close: parser.ensure_token(TokenKind::RightBracket)?.into(),
        }))
    }

    pub fn parse_ternary_expr(parser: &Parser, left: Box<Expression>) -> Result<Expression> {
        Ok(Expression::Ternary(Ternary {
            cond: left,
            question: {
                parser.skip_whitespace();
                parser.expect_token(TokenKind::QuestionMark)?.into()
            },
            if_true: Expression::parse(parser, Precedence::Lowest)?.into(),
            colon: {
                parser.skip_whitespace();
                parser.expect_token(TokenKind::SpacedColon)?.into()
            },
            if_false: parser.parse_expression(Precedence::Lowest)?.into(),
        }))
    }
}

#[derive(Debug)]
pub struct PeekInfo {
    next: TokenOwned,
    post_whitespace: TokenOwned,
    post_comment: TokenOwned,
}

impl PeekInfo {
    fn relevant_kind(&self) -> &TokenKind {
        if is_multiline_kind(&self.post_comment.kind) {
            &self.post_comment.kind
        } else {
            &self.post_whitespace.kind
        }
    }

    fn infix_token(&self) -> &TokenOwned {
        if is_multiline_kind(&self.post_comment.kind) {
            &self.post_comment
        } else {
            &self.post_whitespace
        }
    }
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
        if self.token_buffer.borrow().len() <= 2 {
            self.fill_buffer(2);
        }

        self.token_buffer.borrow_mut().pop_front().unwrap().into()
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

    fn front_kind(&self) -> TokenKind {
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
        self.peek_info().post_whitespace.kind
    }

    fn current_precedence(&self) -> Result<Precedence> {
        Ok(self.get_precedence(&self.front_kind())?.unwrap_or_default())
    }

    fn line_matches<F>(&self, f: F) -> bool
    where
        F: Fn(&TokenOwned) -> bool,
    {
        let mut peek_index = 0;
        loop {
            let tok = self.peek_n(peek_index);
            if tok.kind == TokenKind::EndOfLine || tok.kind == TokenKind::EndOfFile {
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
            if tok.kind == TokenKind::EndOfLine || tok.kind == TokenKind::EndOfFile {
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
            if tok.kind == TokenKind::EndOfLine || tok.kind == TokenKind::EndOfFile {
                return false;
            } else if f(&tok) {
                return true;
            }

            peek_index += 1
        }
    }

    fn peek_info(&self) -> PeekInfo {
        let next = self.peek();
        let mut post_whitespace = None;
        let mut post_comment = None;

        let mut peek_index = 1;
        loop {
            let kind = self.peek_nkind(peek_index);
            if kind.is_eof() {
                break;
            }

            if post_whitespace.is_none() {
                if !kind.is_whitespace() {
                    post_whitespace = Some(self.peek_n(peek_index));
                }
            }

            if post_comment.is_none() {
                if !kind.is_whitespace() && kind != TokenKind::Comment {
                    post_comment = Some(self.peek_n(peek_index));
                    break;
                }
            }

            peek_index += 1;
        }

        PeekInfo {
            next,
            post_whitespace: post_whitespace.unwrap_or_else(|| self.peek_n(peek_index)),
            post_comment: post_comment.unwrap_or_else(|| self.peek_n(peek_index)),
        }
    }

    fn get_prefix_fn(&self) -> Option<PrefixFn> {
        use prefix_expr::*;
        use TokenKind::*;

        Some(Box::new(match self.front_kind() {
            Integer | Float => parse_number,
            Identifier => parse_identifier,
            Register => parse_register,
            Ampersand => parse_vim_option,
            DoubleQuoteString => parse_double_string,
            SingleQuoteString => parse_single_string,
            InterpolatedString => parse_interpolated,
            InterpolatedLiteralString => parse_interpolated_lit,
            EnvironmentVariable => parse_env_var,
            LeftParen => parse_grouped_expr,
            LeftBracket => parse_array_literal,
            LeftBrace => parse_dict_literal,
            Colon => parse_prefix_colon,
            SpacedColon => parse_prefix_spaced_colon,
            AngleLeft => parse_expandable_sequence,
            True | False => parse_bool,
            Plus | Minus | Bang | Percent | Increment | Decrement => parse_prefix_operator,
            _ => return None,
        }))
    }

    fn get_precedence(&self, kind: &TokenKind) -> Result<Option<Precedence>> {
        Ok(Some(match kind {
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
            TokenKind::RightBracket | TokenKind::RightBrace | TokenKind::RightParen => {
                Precedence::Lowest
            }
            TokenKind::EndOfLine | TokenKind::EndOfFile => Precedence::Lowest,

            // We have to check new lines to see if we need to handle anything there.
            //  I'm not sure this is 100% great, but we'll leave it this way for now.
            TokenKind::Identifier
            | TokenKind::Integer
            | TokenKind::Float
            | TokenKind::Comment
            | TokenKind::Increment
            | TokenKind::Decrement => Precedence::Lowest,

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
                return Err(anyhow::anyhow!(
                    "Unexpected precendence kind: {:?} // {:#?}",
                    kind,
                    self
                ));
            }
        }))
    }

    fn get_infix_fn(&self) -> Option<InfixFn> {
        let peek_info = self.peek_info();
        let peek_token = peek_info.infix_token();
        let skipped = &peek_info.next != peek_token;

        Some(Box::new(match peek_token.kind {
            // Mathemtical operations
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Div
            | TokenKind::Percent
            | TokenKind::Mul
            | TokenKind::StringConcat => infix_expr::parse_infix_operator,
            TokenKind::MethodArrow => infix_expr::parse_method_call,
            TokenKind::LeftParen => infix_expr::parse_call_expr,
            TokenKind::LeftBracket => infix_expr::parse_index_expr,
            TokenKind::Dot => infix_expr::parse_dot_operator,
            TokenKind::QuestionMark => infix_expr::parse_ternary_expr,
            TokenKind::Colon => {
                if skipped {
                    return None;
                } else {
                    infix_expr::parse_colon
                }
            }
            // Logical comparisons
            TokenKind::Or | TokenKind::And => infix_expr::parse_infix_operator,
            // Type comparisons
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
            // TokenKind::SpacedColon => infix_expr::parser_index_type,
            TokenKind::Identifier => return None,
            _ => unimplemented!("get_infix_fn: {:#?}", self),
        }))
    }

    fn parse_expression(&self, prec: Precedence) -> Result<Expression> {
        // info!("parseing expr: {:#?}", prec);

        self.skip_whitespace();

        let prefix = self.get_prefix_fn();

        let mut left = match prefix {
            Some(prefix) => prefix(self)?,
            None => {
                return Err(anyhow::anyhow!("No prefix function: {:#?}", self));
            }
        };

        loop {
            let peeked = self.peek_info();
            if prec >= self.get_precedence(peeked.relevant_kind())?.unwrap() {
                break;
            }

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
        let curkind = self.front_kind();
        if curkind == TokenKind::EndOfLine
            || curkind == TokenKind::EndOfFile
                // We kind of cheat to say end of line comments are fine...
                // even tho sometimes they are not
            || curkind == TokenKind::Comment
        {
            Ok(self.pop().into())
        } else {
            Err(anyhow::anyhow!("expected eol or eof, got: {:#?}", self))
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

    // TODO: Could possibly (if we care about the perf) write a copy of this function
    // that, instead of returning an owned, directly returns the metadata only
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

    pub fn expect_token_with_text(&self, kind: TokenKind, text: &str) -> Result<TokenOwned> {
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

    pub fn expect_identifier_with_text(&self, text: &str) -> Result<TokenOwned> {
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
        let tokens = self.token_buffer.borrow();
        match tokens.len() {
            0 | 1 => panic!("Should always have a least 2 entries"),
            len if len - 1 >= n => (&tokens[n]).into(),
            len => {
                drop(tokens);
                self.fill_buffer(n);

                let tokens = self.token_buffer.borrow();
                (&tokens[n]).into()
            }
        }
    }

    fn fill_buffer(&self, n: usize) {
        let mut tokens = self.token_buffer.borrow_mut();

        let len = tokens.len();
        for _ in 0..(n + 1 - len) {
            let tok = match self.lexer.next_token() {
                Ok(tok) => tok,
                Err(err) => Token {
                    kind: TokenKind::EndOfFile,
                    text: vim9_lexer::TokenText::Empty,
                    span: Span::empty(),
                },
            };

            tokens.push_back(tok)
        }
    }

    fn command_match(&self, full: &str) -> bool {
        self.front_ref().text.eq(full)
    }

    pub fn parse_command(&self) -> Result<ExCommand> {
        use TokenKind::*;

        // If the line starts with a colon, then just skip over it.
        if self.front_kind() == Colon {
            self.next_token();
        }

        // TODO: Handle modifiers.
        // Perhaps return as separate item/note for ExCommand, or return as tuple.
        //  Could additionally be added as ExCommand parent type or something.
        //  Will just be an annoying amount of writing :)
        if self.front_ref().text.eq("silent") {
            self.next_token();
            if self.front_kind() == Bang {
                self.next_token();
            }
        }

        if let Some(res) = self.is_multiline_expr() {
            return res;
        }

        // For the following branches, you need to return early if it completely consumes
        // the last character and advances past.
        //
        // This is the desired behavior for `parse` which will consume until the end of line
        // generally speaking.
        Ok(match &self.front_kind() {
            // Whitespace
            EndOfFile => ExCommand::NoOp(self.pop().into()),
            EndOfLine => ExCommand::NoOp(self.pop().into()),

            // Comments
            Comment => ExCommand::Comment(self.pop().into()),

            Identifier => {
                if self.command_match("vim9script") {
                    ExCommand::Vim9Script(Vim9ScriptCommand::parse(self)?)
                } else if self.command_match("execute") {
                    ExecuteCommand::parse(self)?
                } else if VarCommand::matches(self) {
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
                } else if self.command_match("export") {
                    ExportCommand::parse(self)?
                } else if self.command_match("import") {
                    ImportCommand::parse(self)?
                } else if self.command_match("augroup") {
                    AugroupCommand::parse(self)?
                } else if self.command_match("autocmd") {
                    AutocmdCommand::parse(self)?
                } else if self.command_match("command") {
                    UserCommand::parse(self)?
                } else if self.command_match("set") || self.command_match("setlocal") {
                    SharedCommand::parse(self)?
                } else if self.command_match("nnoremap")
                    || self.command_match("inoremap")
                    || self.command_match("anoremenu")
                    || self.command_match("normal")
                {
                    SharedCommand::parse(self)?
                } else if self.command_match("unlet") {
                    println!("TODO: UNLET");
                    return Ok(SharedCommand::parse(self)?);
                } else if self.command_match("for") {
                    ForCommand::parse(self)?
                } else if self.command_match("while") {
                    return Ok(WhileCommand::parse(self)?);
                } else if self.command_match("try") {
                    TryCommand::parse(self)?
                } else if self.command_match("defer") {
                    DeferCommand::parse(self)?
                } else if self.command_match("break") {
                    BreakCommand::parse(self)?
                } else if self.command_match("continue") {
                    ContinueCommand::parse(self)?
                }
                // The following commands must remain at the bottom end of the list
                //
                // This is true of a few of the other kind of "dynamic" style commands
                // that are detected/guessed by what the rest of the line looks like.
                else if CallCommand::matches(self) {
                    CallCommand::parse(self)?
                } else if StatementCommand::matches(self) {
                    // TODO: There are some kind of assignments that aren't legal if there is a
                    // colon. I'm not sure what to do about that... it seems a bit weird.
                    //
                    // But I can't find the exact reasoning behind this.
                    //
                    // It's illegal to do:
                    //
                    // var sum = 1
                    // :sum = sum + 1
                    StatementCommand::parse(self)?
                } else if self.line_contains_kind(MethodArrow) {
                    EvalCommand::parse(self)?
                } else {
                    SharedCommand::parse(self)?
                }
            }
            LeftBracket => EvalCommand::parse(self)?,

            // Method calls can start with a number/float (and probably others)
            // It looks something like:
            //
            // 3->setwinvar(id, '&conceallevel')
            Integer | Float if self.line_contains_kind(MethodArrow) => EvalCommand::parse(self)?,

            // As of writing this, vim9script only allows increment / decrement
            // operators at the beginning of a line. They can't be used in expressions
            // at this time.
            Decrement | Increment => EvalCommand::parse(self)?,

            _ => return Err(anyhow::anyhow!("TODO: Parser kind: {:#?}", self)),
        })
    }

    pub fn parse_program(&self) -> Program {
        let mut program = Program { commands: vec![] };

        while self.front_kind() != TokenKind::EndOfFile {
            let command = match self.parse_command() {
                Ok(command) => command,
                Err(err) => panic!(
                    "\nFailed to parse command.\nCurrent Commands:{:#?}.\nError: {}",
                    program, err
                ),
            };
            // let command = self.parse_command().unwrap();

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

        // Empty list, () or [], for example
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

            if self.front_kind() == close {
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
            if self.front_kind() == close {
                return Ok(results);
            }

            // Next T
            results.push(parse(self)?);

            // If the next token is close, then it's time to be done.
            // We've consume everything that we need.
            self.skip_whitespace();
            if self.peek_kind() == close {
                break;
            }

            if self.front_kind().is_eof() {
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

    fn parse_paramter_list(&self) -> Result<Vec<Parameter>> {
        self.list_parser(TokenKind::RightParen, |p| Parameter::parse(self))

        // let mut params = Vec::new();
        // while self.front_kind() != TokenKind::RightParen {
        //     params.push(Parameter::parse(self)?);
        //     self.skip_whitespace();
        //     if self.front_kind() == TokenKind::Comma {
        //         self.next_token();
        //         self.skip_whitespace();
        //     }
        // }
        //
        // Ok(params)
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
        while self.front_kind().is_whitespace() || self.front_kind() == TokenKind::Comment {
            if self.front_kind() == TokenKind::EndOfFile {
                break;
            }

            self.next_token();
        }
    }

    fn consume_if_kind(&self, kind: TokenKind) -> Option<TokenOwned> {
        if self.front_kind() == kind {
            Some(self.pop())
        } else {
            None
        }
    }

    pub fn read_until<F>(&self, f: F)
    where
        F: Fn(&Token) -> bool,
    {
        while !f(&self.front_ref()) && !self.front_kind().is_eof() {
            self.pop();
        }
    }

    pub fn is_multiline_expr(&self) -> Option<Result<ExCommand>> {
        // This (partially) handles the case of doing:
        // > foo
        // >    ->something()
        //
        // Honestly, this is not a good solution :/
        let peek_info = self.peek_info();

        if self.front_kind() == TokenKind::Identifier
            && peek_info.next.kind == TokenKind::EndOfLine
            && peek_info.post_comment.kind == TokenKind::MethodArrow
        {
            return Some(EvalCommand::parse(self));
        }

        None
    }
}

fn is_multiline_kind(kind: &TokenKind) -> bool {
    matches!(kind, TokenKind::MethodArrow)
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
    snap!(test_index, "../testdata/snapshots/index.vim");
    snap!(test_adv_index, "../testdata/snapshots/adv_index.vim");
    snap!(test_multiline, "../testdata/snapshots/multiline.vim");
    snap!(test_cfilter, "../testdata/snapshots/cfilter.vim");
    snap!(test_lambda, "../testdata/snapshots/lambda.vim");
    snap!(test_comparisons, "../testdata/snapshots/comparisons.vim");
    snap!(test_eval, "../testdata/snapshots/eval.vim");
    snap!(test_export, "../testdata/snapshots/export.vim");
    snap!(test_import, "../testdata/snapshots/import.vim");
    snap!(test_autocmd, "../testdata/snapshots/autocmd.vim");
    snap!(test_unpack, "../testdata/snapshots/unpack.vim");
    snap!(test_methods, "../testdata/snapshots/methods.vim");

    // https://github.com/yegappan/lsp test suite
    snap!(test_handlers, "../../shared/snapshots/lsp_handlers.vim");
    snap!(test_selection, "../../shared/snapshots/lsp_selection.vim");
    snap!(test_fileselect, "../../shared/snapshots/lsp_fileselect.vim");
    snap!(test_startup, "../../shared/snapshots/startup9.vim");

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
    // snap!(test_matchparen, "../../shared/snapshots/matchparen.vim");
}
