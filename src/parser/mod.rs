use thiserror::Error;

use crate::ast;
use crate::lexer::Token;
use crate::lexer::TokenKind;

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
pub enum Precedence {
    Lowest,
    Equality,
    LesserGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

pub struct Parser {
    pub tokens: Vec<Token>,

    position: usize,
    read_position: usize,
}

fn get_precedence(token: Token) -> Precedence {
    use TokenKind::*;

    match token.kind {
        Plus | Minus => Precedence::Sum,
        Star | Slash => Precedence::Product,
        _ => Precedence::Lowest,
    }
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

    pub fn peek_token(&self) -> Token {
        self.get_token_at(self.read_position)
    }

    pub fn peek_precedence(&self) -> Precedence {
        get_precedence(self.peek_token())
    }

    pub fn token(&self) -> Token {
        self.get_token_at(self.position)
    }

    pub fn precedence(&self) -> Precedence {
        get_precedence(self.token())
    }

    fn get_token_at(&self, n: usize) -> Token {
        if n >= self.tokens.len() {
            // TODO: Probably should be string with EOF in it??
            Token {
                kind: TokenKind::EOF,
                text: String::new(),
            }
        } else {
            // TODO: Can I avoid cloning this?
            self.tokens[n].clone()
        }
    }
}

fn parse_identifier(p: &mut Parser) -> Option<ast::Identifier> {
    let tok = p.next_token();
    let name = match tok.kind {
        TokenKind::Identifier => tok.text,
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
                        expression: Expression::Number(5.into()),
                    })
                ]
            },
            parse(tokens)?
        );

        Ok(())
    }

    #[test]
    fn parses_a_prefix_expression() -> ParseResult<()> {
        let tokens = get_tokens("var x = -5");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script(StatementVim9 {}),
                    Statement::Var(StatementVar {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Prefix {
                            operator: PrefixOperator::Minus,
                            right: Box::new(Expression::Number(5.into())),
                        }
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
                            left: Box::new(Expression::Number(5.into())),
                            right: Box::new(Expression::Number(6.into())),
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
                            left: Box::new(Expression::Number(5.into())),
                            right: Box::new(Expression::Infix {
                                operator: InfixOperator::Mul,
                                left: Box::new(Expression::Number(6.into())),
                                right: Box::new(Expression::Number(2.into())),
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
        // -> var x = ((5 * 6) + 2)

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
                                left: Box::new(Expression::Number(5.into())),
                                right: Box::new(Expression::Number(6.into())),
                            }),
                            right: Box::new(Expression::Number(2.into())),
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
