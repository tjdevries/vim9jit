use thiserror::Error;

use crate::ast;
use crate::lexer::Token;

#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum ParseErrorKind {
    #[error("Unexpected end of file")]
    Eof,

    #[error("Failed to parse: {message}")]
    Message { message: &'static str },

    #[error("Expected eof")]
    ExpectedEof { actual: &'static str },

    #[error("Unknown token")]
    Unknown,

    #[error("expected {expected}, but got `{actual}`")]
    Expected {
        actual: &'static str,
        expected: &'static str,
    },

    #[error("Invalid number literal: {0}")]
    InvalidLitNum(#[from] std::num::ParseFloatError),
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
}

pub type ParseResult<T, E = ParseError> = Result<T, E>;

pub trait Parse
where
    Self: Sized,
{
    fn parse(p: &mut Parser) -> ParseResult<Self>;
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Debug)]
enum Precedence {
    Lowest,
    Equality,
    LesserGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

fn get_precedence(operator: ast::InfixOperator) -> Precedence {
    // TODO: The rest of em
    match operator {
        ast::InfixOperator::Add | ast::InfixOperator::Sub => Precedence::Sum,
        ast::InfixOperator::Mul | ast::InfixOperator::Div => Precedence::Product,
        _ => Precedence::Lowest,
    }
}
pub struct Parser {
    pub tokens: Vec<Token>,

    position: usize,
    read_position: usize,
}

impl Parser {
    pub fn parse<T>(&mut self) -> ParseResult<T>
    where
        T: Parse,
    {
        T::parse(self)
    }

    pub fn next_token(&mut self) -> Token {
        self.position = self.read_position;
        self.read_position = self.read_position + 1;

        self.token()
    }

    pub fn peek_token(&mut self) -> Token {
        self.get_token_at(self.read_position)
    }

    pub fn token(&self) -> Token {
        self.get_token_at(self.position)
    }

    fn get_token_at(&self, n: usize) -> Token {
        if n >= self.tokens.len() {
            Token::EOF
        } else {
            // TODO: Can I avoid cloning this?
            self.tokens[n].clone()
        }
    }

    fn parse_expression(&mut self) -> Option<ast::Expression> {
        let tok = self.next_token();
        let peeked = self.peek_token();

        // This is the last token in an expression.
        if matches!(peeked, Token::EOF | Token::NewLine) {
            Some(convert_single_token_to_expression(tok))
        } else {
            match peeked {
                // 5 + 2 * 10
                // 5 * 2 + 10
                Token::Plus => {
                    // Advance
                    self.next_token();

                    Some(ast::Expression::Infix {
                        operator: ast::InfixOperator::Add,
                        left: Box::new(convert_single_token_to_expression(tok)),
                        right: Box::new(self.parse_expression()?),
                    })
                }

                Token::Multiply => {
                    self.next_token();

                    let next = self.next_token();

                    Some(ast::Expression::Infix {
                        operator: ast::InfixOperator::Mul,
                        left: Box::new(convert_single_token_to_expression(tok)),
                        right: Box::new(convert_single_token_to_expression(next)),
                    })
                }

                peeked => panic!("TJ please write some more code {:?}\n{:?}", peeked, self.tokens),
            }
        }
    }
}

fn convert_single_token_to_expression(tok: Token) -> ast::Expression {
    match tok {
        Token::Number(num) => {
            // TODO: This is terrible parsing. It's like you're writing C++...
            let num: String = num.iter().collect();
            let parsed_num: i64 = num.parse().unwrap();
            ast::Expression::Number(parsed_num)
        }
        _ => panic!("I thought rust was safe...??? explain this."),
    }
}

fn parse_var(p: &mut Parser) -> Option<ast::Statement> {
    let identifier = match parse_identifier(p) {
        Some(identifier) => identifier,
        None => {
            return Some(ast::Statement::Error {
                msg: format!("Was expecting identifier, got nothing {:?}", p.token()),
            })
        }
    };

    let tok = p.next_token();
    if !matches!(tok, Token::Equal) {
        return Some(ast::Statement::Error {
            msg: format!("Was expecting token equals, got {:?}", tok),
        });
    }

    // TODO: Actually read the expression...
    let expression = match p.parse_expression() {
        Some(expression) => expression,
        None => {
            return Some(ast::Statement::Error {
                msg: format!("Was expecting expression, got GARBAGE {:?}", p.token()),
            })
        }
    };

    Some(ast::Statement::Var(ast::StatementVar { identifier, expression }))
}

fn parse_identifier(p: &mut Parser) -> Option<ast::Identifier> {
    let tok = p.next_token();
    let name = match tok {
        Token::Identifier(chars) => chars.iter().collect(),
        _ => return None,
    };

    Some(ast::Identifier { name })
}

fn parse(tokens: Vec<Token>) -> ParseResult<ast::Program> {
    let mut parser = Parser {
        tokens,
        position: 0,
        read_position: 0,
    };

    parser.parse()

    // let mut statements = Vec::new();
    // while let Some(statement) = parser.next_statement() {
    //     statements.push(statement)
    // }

    //     for tok in tokens {
    //         statements.push(match tok {
    //             Token::Vim9Script(_) => Statement::Vim9Script,
    //             Token::Var => Statement::Var {
    //                 identifier: Identifier { name: "x".into() },
    //                 expression: Expression {},
    //             },
    //             tok => println!("Have not yet parsed: {:?}", tok),
    //         });
    //     }

    // ast::Program { statements }
}

// vim9script
// let x = 5
// def asdf()
//  stuff inside
//  echo [1, 2, 3]
// enddef

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::*;
    use crate::lexer::tokenize_file;

    fn get_tokens(input: &str) -> Vec<Token> {
        tokenize_file(format!("vim9script\n{}", input).chars().collect()).unwrap()
    }

    #[test]
    fn parses_vim9script() -> ParseResult<()> {
        let tokens = tokenize_file("vim9script".into()).unwrap();

        assert_eq!(
            Program {
                statements: vec![Statement::Vim9Script(StatementVim9 {})]
            },
            parse(tokens)?
        );

        Ok(())
    }

    #[test]
    fn parses_a_var_statement() -> ParseResult<()> {
        let tokens = get_tokens("var x = 5");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script(StatementVim9 {}),
                    Statement::Var(StatementVar {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Number(5),
                    })
                ]
            },
            parse(tokens)?
        );

        Ok(())
    }

    #[test]
    fn parses_a_var_statement_with_addition() -> ParseResult<()> {
        let tokens = get_tokens("var x = 5 + 6");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script(StatementVim9 {}),
                    Statement::Var(StatementVar {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Infix {
                            operator: InfixOperator::Add,
                            left: Box::new(Expression::Number(5)),
                            right: Box::new(Expression::Number(6)),
                        },
                    })
                ]
            },
            parse(tokens)?
        );

        Ok(())
    }

    #[test]
    fn parses_a_var_statement_operator_precedence_1() -> ParseResult<()> {
        let tokens = get_tokens("var x = 5 + 6 * 2");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script(StatementVim9 {}),
                    Statement::Var(StatementVar {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Infix {
                            operator: InfixOperator::Add,
                            left: Box::new(Expression::Number(5)),
                            right: Box::new(Expression::Infix {
                                operator: InfixOperator::Mul,
                                left: Box::new(Expression::Number(6)),
                                right: Box::new(Expression::Number(2)),
                            }),
                        },
                    })
                ]
            },
            parse(tokens)?
        );

        Ok(())
    }

    #[test]
    fn parses_a_var_statement_operator_precedence_2() -> ParseResult<()> {
        let tokens = get_tokens("var x = 5 * 6 + 2");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script(StatementVim9 {}),
                    Statement::Var(StatementVar {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Infix {
                            operator: InfixOperator::Add,
                            left: Box::new(Expression::Infix {
                                operator: InfixOperator::Mul,
                                left: Box::new(Expression::Number(5)),
                                right: Box::new(Expression::Number(6)),
                            }),
                            right: Box::new(Expression::Number(2)),
                        },
                    })
                ]
            },
            parse(tokens)?
        );

        Ok(())
    }

    #[test]
    fn errors_when_a_var_statement_has_no_equal() {
        let tokens = get_tokens("var x 5");
        assert!(parse(tokens).is_err());
    }
}
