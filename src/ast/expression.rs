use num::ToPrimitive;

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
pub enum Expression {
    Number(ast::LiteralNumber),

    Identifier(ast::Identifier),

    VimVariable(ast::VimVariable),

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
    match token.kind {
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
        kind => todo!("Unhandled prefix kind: {:?}", kind),
    }
}

fn get_infix_fn<'a>(token: Token) -> Option<impl Fn(&'a mut Parser, Expression) -> ParseResult<Expression>> {
    match token.kind {
        TokenKind::Plus | TokenKind::Minus | TokenKind::Star | TokenKind::Slash => {
            let operator = match token.kind {
                TokenKind::Plus => ast::InfixOperator::Add,
                TokenKind::Minus => ast::InfixOperator::Sub,
                TokenKind::Star => ast::InfixOperator::Mul,
                TokenKind::Slash => ast::InfixOperator::Div,
                _ => unreachable!("These are the only operators in the match statement"),
            };

            println!("Generating infix function for: {:?}", operator);

            Some(move |p: &'a mut Parser, left| {
                println!("Current Token: {:?}", p.token());
                println!("LEFT: {:?}", left);

                Ok(Expression::Infix {
                    left: Box::new(left),
                    operator,
                    right: Box::new(parse_expresion(p, p.precedence())?),
                })
            })
        }
        _ => None,
    }
}

fn parse_expresion(p: &mut Parser, precedence: Precedence) -> ParseResult<Expression> {
    println!("We are attempting to parse {:?}", p.peek_token());

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

impl Parse for Expression {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        parse_expresion(p, parser::Precedence::Lowest)
    }
}

impl CodeGen for Expression {
    fn gen(&self, db: &mut GenDB) -> String {
        match self {
            Expression::Number(num) => num.value.to_string(),
            Expression::Identifier(identifier) => identifier.gen(db),
            Expression::VimVariable(_) => todo!(),
            Expression::Prefix { .. } => todo!(),
            Expression::Infix { left, operator, right } => {
                // TODO: We need some way to track the current state of things.
                // since I want to be able to look up the type of the left & right guys.
                format!("{} {} {}", left.gen(db), operator.gen(db), right.gen(db))
            }
        }
    }
}
