use log::debug;
use num::ToPrimitive;

use super::FunctionCall;
use crate::ast;
use crate::ast::VimVariable;
use crate::ast::VimVariableScope;
use crate::gen::CodeGen;
use crate::gen::GenDB;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::parser;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;
use crate::parser::Precedence;

#[derive(Debug, Clone, PartialEq)]
// TODO: Other primitive types
pub enum Expression {
    Empty,

    Number(ast::LiteralNumber),

    Identifier(ast::Identifier),
    VimVariable(ast::VimVariable),

    // TODO: Other call types
    Call(ast::FunctionCall),

    Prefix {
        operator: ast::PrefixOperator,
        right: Box<Expression>,
    },

    Infix {
        left: Box<Expression>,
        operator: ast::InfixOperator,
        right: Box<Expression>,
    },
}

impl<I> From<I> for Expression
where
    I: ToPrimitive,
{
    fn from(val: I) -> Self {
        Expression::Number(val.into())
    }
}

// impl<bool> From<bool>for Expression {
//     fn from(val: bool) -> Self {
//         Expression::Bool(val.into())
//     }
// }

// TODO: I really don't like that I have tokenkind as another argument.
// How can I pass a function back that captures a variable but actually have it work w/ types.
fn get_prefix_fn(token: Token) -> fn(&mut Parser, TokenKind) -> ParseResult<Expression> {
    match &token.kind {
        TokenKind::Number => |p, _| Ok(Expression::Number(p.parse()?)),
        TokenKind::Identifier => |p, _| Ok(Expression::Identifier(p.parse()?)),

        // vim variable scopes
        TokenKind::GlobalScope
        | TokenKind::TabScope
        | TokenKind::WindowScope
        | TokenKind::BufferScope
        | TokenKind::ScriptScope
        | TokenKind::LocalScope => |p, kind| {
            // consume <scope>:
            p.next_token();

            Ok(Expression::VimVariable(VimVariable {
                scope: match kind {
                    TokenKind::GlobalScope => VimVariableScope::Global,
                    TokenKind::TabScope => VimVariableScope::Tab,
                    TokenKind::WindowScope => VimVariableScope::Window,
                    TokenKind::BufferScope => VimVariableScope::Buffer,
                    TokenKind::ScriptScope => VimVariableScope::Script,
                    TokenKind::LocalScope => VimVariableScope::Local,
                    _ => unreachable!("cannot have any other token kinds here"),
                },
                identifier: p.parse()?,
            }))
        },

        TokenKind::Plus | TokenKind::Minus => |p, kind| {
            // Consume the operator
            p.next_token();

            Ok(Expression::Prefix {
                operator: match kind {
                    TokenKind::Plus => ast::PrefixOperator::Plus,
                    TokenKind::Minus => ast::PrefixOperator::Minus,
                    _ => unreachable!(),
                },
                right: Box::new(parse_expresion(p, Precedence::Prefix)?),
            })
        },

        TokenKind::Star => todo!("Multiply"),
        TokenKind::Slash => todo!("Divide"),

        TokenKind::Comma => todo!("Comma "),
        TokenKind::Equal => todo!("Equal"),
        TokenKind::LeftBracket => todo!("LeftBracket"),
        TokenKind::RightBracket => todo!("RightBracket"),

        // These probably should never happen???
        _ => todo!("Unhandled prefix kind: {:?}", token.clone()),
    }
}

fn get_infix_fn<'a>(token: Token) -> Option<Box<dyn Fn(&'a mut Parser, Expression) -> ParseResult<Expression>>> {
    match token.kind {
        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
            let operator = match token.kind {
                TokenKind::Plus => ast::InfixOperator::Add,
                TokenKind::Minus => ast::InfixOperator::Sub,
                TokenKind::Star => ast::InfixOperator::Mul,
                TokenKind::Slash => ast::InfixOperator::Div,
                _ => unreachable!("These are the only operators in the match statement"),
            };

            debug!("Generating infix function for: {:?}", operator);

            Some(Box::new(move |p: &'a mut Parser, left| {
                Ok(Expression::Infix {
                    left: Box::new(left),
                    operator,
                    right: Box::new(parse_expresion(p, p.precedence())?),
                })
            }))
        }
        TokenKind::LeftParen => Some(Box::new(|p: &'a mut Parser, left| {
            let call = Expression::Call(FunctionCall {
                function: left.into(),
                args: parse_expression_list(p, TokenKind::Comma, TokenKind::RightParen)?,
            });

            // assert!(p.token().kind != TokenKind::RightParen);

            Ok(call)
        })),
        _ => None,
    }
}

fn parse_expresion(p: &mut Parser, precedence: Precedence) -> ParseResult<Expression> {
    debug!("ParseExpression: start {:?}", p.peek_token());

    let prefix = get_prefix_fn(p.peek_token());
    let mut left = prefix(p, p.peek_token().kind)?;

    while !matches!(p.peek_token().kind, TokenKind::NewLine | TokenKind::EOF) && precedence < p.peek_precedence() {
        let infix = match get_infix_fn(p.next_token()) {
            Some(infix) => infix,
            None => break,
        };

        left = infix(p, left)?;
    }

    Ok(left)
}

// NOTE: Consumes the `right` token.
fn parse_expression_list(p: &mut Parser, separator: TokenKind, right: TokenKind) -> ParseResult<Vec<Expression>> {
    let mut list = Vec::new();

    Ok(if p.peek_token().kind == right {
        p.expect(&right)?;
        list
    } else {
        list.push(parse_expresion(p, Precedence::Lowest)?);

        while p.peek_token().kind == separator {
            dbg!(p.next_token());
            list.push(dbg!(parse_expresion(p, Precedence::Lowest)?));
        }

        dbg!(p.position);
        p.expect(&right)?;
        dbg!(p.position);
        list
    })
}

impl Parse for Expression {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        parse_expresion(p, parser::Precedence::Lowest)
    }
}

impl CodeGen for Expression {
    fn gen(&self, db: &mut GenDB) -> String {
        match self {
            Expression::Empty => "".to_string(),
            Expression::Number(num) => num.value.to_string(),
            Expression::Identifier(identifier) => identifier.gen(db),
            Expression::VimVariable(_) => todo!(),
            Expression::Prefix { .. } => todo!(),
            Expression::Infix { left, operator, right } => {
                match (
                    db.has_shared_behavior(&None, left),
                    db.has_shared_behavior(&None, right),
                ) {
                    (true, true) => {
                        format!("({} {} {})", left.gen(db), operator.gen(db), right.gen(db))
                    }
                    _ => {
                        format!(r#"Vim9__{:?}({}, {})"#, operator, left.gen(db), right.gen(db))
                    }
                }
            }
            Expression::Call(function_call) => function_call.gen(db),
        }
    }
}
