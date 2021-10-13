use crate::ast;
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

// fn terminal(tok: Token) -> ParseResult<Expression> {
//     Ok(match tok {
//         Token::Number(num) => {
//             // TODO: This is terrible parsing. It's like you're writing C++...
//             let num: String = num.iter().collect();
//             let parsed_num: i64 = num.parse().unwrap();
//             Expression::Number(parsed_num)
//         }
//         _ => panic!("I thought rust was safe...??? explain this."),
//     })
// }

fn get_prefix_fn(token: Token) -> fn(&mut Parser) -> ParseResult<Expression> {
    match token.kind {
        TokenKind::Number => |p| Ok(Expression::Number(p.parse()?)),
        TokenKind::Identifier => |p| Ok(Expression::Identifier(p.parse()?)),
        TokenKind::CommandVar => todo!("CommandVar "),
        TokenKind::NewLine => todo!("NewLine "),
        TokenKind::Plus => |p| {
            Ok(Expression::Prefix {
                operator: ast::PrefixOperator::Plus,
                right: Box::new(p.parse()?),
            })
        },
        TokenKind::Minus => |p| {
            Ok(Expression::Prefix {
                operator: ast::PrefixOperator::Minus,
                right: Box::new(p.parse()?),
            })
        },
        TokenKind::Comma => todo!("Comma "),
        TokenKind::Star => todo!("Multiply "),
        TokenKind::Slash => todo!("Divide "),
        TokenKind::Equal => todo!("Equal"),
        TokenKind::LeftBracket => todo!("LeftBracket "),
        TokenKind::RightBracket => todo!("RightBracket "),

        // These probably should never happen???
        TokenKind::Ignore => todo!("Ignore "),
        TokenKind::EOF => todo!("EOF "),
        TokenKind::Comment => todo!("Comment"),
        TokenKind::ParseError => todo!("ParseError"),
        TokenKind::StartOfFile => todo!("StartOfFile"),
        TokenKind::Vim9Script => todo!("Vim9Script"),
        _ => todo!(),
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

            Some(move |p: &'a mut Parser, left| {
                let precedence = p.precedence();
                p.next_token();

                Ok(Expression::Infix {
                    left: Box::new(left),
                    operator,
                    right: Box::new(parse_expresion(p, precedence)?),
                })
            })
        }
        _ => None,
    }
}

fn parse_expresion(p: &mut Parser, precedence: Precedence) -> ParseResult<Expression> {
    println!("We are attempting to parse {:?}", p.token());

    let prefix = get_prefix_fn(p.token());
    let mut left = prefix(p)?;

    while !matches!(p.peek_token().kind, TokenKind::NewLine | TokenKind::EOF) && precedence < p.peek_precedence() {
        let token = p.next_token();

        let infix = match get_infix_fn(token) {
            Some(infix) => infix,
            None => break,
        };

        left = infix(p, left)?;
    }

    Ok(left)
}

impl Parse for Expression {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        p.next_token();
        parse_expresion(p, parser::Precedence::Lowest)
    }
}
