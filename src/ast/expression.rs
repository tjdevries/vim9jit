use crate::ast;
use crate::lexer::Token;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Number(i64),

    Identifier(ast::Identifier),

    Prefix {
        operator: ast::PrefixOperator,
        right: Box<Expression>,
    },

    Infix {
        operator: ast::InfixOperator,
        right: Box<Expression>,
        left: Box<Expression>,
    },
}

fn terminal(tok: Token) -> ParseResult<Expression> {
    Ok(match tok {
        Token::Number(num) => {
            // TODO: This is terrible parsing. It's like you're writing C++...
            let num: String = num.iter().collect();
            let parsed_num: i64 = num.parse().unwrap();
            Expression::Number(parsed_num)
        }
        _ => panic!("I thought rust was safe...??? explain this."),
    })
}

impl Parse for Expression {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let tok = p.next_token();
        let peeked = p.peek_token();

        // This is the last token in an expression.
        if matches!(peeked, Token::EOF | Token::NewLine) {
            p.next_token();

            terminal(tok)
        } else {
            match peeked {
                // 5 + 2 * 10
                // 5 * 2 + 10
                Token::Plus => {
                    // Advance
                    p.next_token();

                    Ok(Expression::Infix {
                        operator: ast::InfixOperator::Add,
                        left: Box::new(terminal(tok)?),
                        right: Box::new(p.parse()?),
                    })
                }

                Token::Multiply => {
                    p.next_token();

                    let next = p.next_token();

                    Ok(ast::Expression::Infix {
                        operator: ast::InfixOperator::Mul,
                        left: Box::new(terminal(tok)?),
                        right: Box::new(terminal(next)?),
                    })
                }
                peeked => panic!("TJ please write some more code {:?}\n{:?}", peeked, p.tokens),
            }
        }
    }
}
