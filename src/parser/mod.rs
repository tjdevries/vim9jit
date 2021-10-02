use crate::lexer::tokenize_file;
use crate::lexer::Token;

#[derive(Debug, PartialEq)]
struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
enum Statement {
    Vim9Script,
    Var {
        identifier: Identifier,
        expression: Expression,
    },

    Error {
        msg: String,
    },
}

#[derive(Debug, PartialEq)]
struct Identifier {
    name: String,
}

#[derive(Debug, PartialEq)]
enum Expression {
    Number(i64),

    Infix {
        operator: Operator,
        left: Box<Expression>,
        right: Box<Expression>,
    },
}

#[derive(Debug, PartialEq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

struct Parser {
    tokens: Vec<Token>,

    position: usize,
    read_position: usize,
}

impl Parser {
    pub fn next_token(&mut self) -> Token {
        self.position = self.read_position;
        self.read_position = self.read_position + 1;

        self.token()
    }

    fn get_token_at(&self, n: usize) -> Token {
        if n >= self.tokens.len() {
            Token::EOF
        } else {
            // TODO: Can I avoid cloning this?
            self.tokens[n].clone()
        }
    }

    pub fn peek_token(&mut self) -> Token {
        self.get_token_at(self.read_position)
    }

    pub fn token(&self) -> Token {
        self.get_token_at(self.position)
    }

    fn next_statement(&mut self) -> Option<Statement> {
        let mut tok = Token::StartOfFile;
        while tok != Token::EOF {
            tok = self.next_token();

            match &tok {
                Token::Vim9Script(_) => return self.parse_vim9script(),
                Token::Var => return parse_var(self),
                // {
                //     identifier: Identifier { name: "x".into() },
                //     expression: Expression {},
                // },
                thing => {
                    println!("Have not yet parsed: {:?}", thing);
                }
            }
        }

        // Silently skip this
        None
    }

    fn parse_vim9script(&mut self) -> Option<Statement> {
        if !matches!(self.next_token(), Token::NewLine | Token::EOF) {
            None
        } else {
            Some(Statement::Vim9Script)
        }
    }

    fn parse_expression(&mut self) -> Option<Expression> {
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

                    Some(Expression::Infix {
                        operator: Operator::Add,
                        left: Box::new(convert_single_token_to_expression(tok)),
                        right: Box::new(self.parse_expression()?),
                    })
                }

                Token::Multiply => {
                    self.next_token();

                    let next = self.next_token();

                    Some(Expression::Infix {
                        operator: Operator::Mul,
                        left: Box::new(convert_single_token_to_expression(tok)),
                        right: Box::new(convert_single_token_to_expression(next)),
                    })
                }

                peeked => panic!("TJ please write some more code {:?}\n{:?}", peeked, self.tokens),
            }
        }
    }
}

fn convert_single_token_to_expression(tok: Token) -> Expression {
    match tok {
        Token::Number(num) => {
            // TODO: This is terrible parsing. It's like you're writing C++...
            let num: String = num.iter().collect();
            let parsed_num: i64 = num.parse().unwrap();
            Expression::Number(parsed_num)
        }
        _ => panic!("I thought rust was safe...??? explain this."),
    }
}

fn parse_var(p: &mut Parser) -> Option<Statement> {
    let identifier = match parse_identifier(p) {
        Some(identifier) => identifier,
        None => {
            return Some(Statement::Error {
                msg: format!("Was expecting identifier, got nothing {:?}", p.token()),
            })
        }
    };

    let tok = p.next_token();
    if !matches!(tok, Token::Equal) {
        return Some(Statement::Error {
            msg: format!("Was expecting token equals, got {:?}", tok),
        });
    }

    // TODO: Actually read the expression...
    let expression = match p.parse_expression() {
        Some(expression) => expression,
        None => {
            return Some(Statement::Error {
                msg: format!("Was expecting expression, got GARBAGE {:?}", p.token()),
            })
        }
    };

    Some(Statement::Var { identifier, expression })
}

fn parse_identifier(p: &mut Parser) -> Option<Identifier> {
    let tok = p.next_token();
    let name = match tok {
        Token::Identifier(chars) => chars.iter().collect(),
        _ => return None,
    };

    Some(Identifier { name })
}

fn parse(tokens: Vec<Token>) -> Program {
    let mut parser = Parser {
        tokens,
        position: 0,
        read_position: 0,
    };

    let mut statements = Vec::new();
    while let Some(statement) = parser.next_statement() {
        statements.push(statement)
    }

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

    Program { statements }
}

// vim9script
// let x = 5
// def asdf()
//  stuff inside
//  echo [1, 2, 3]
// enddef

#[derive(Debug)]
pub struct ParseError {
    tok: Token,
}
type Result<T> = std::result::Result<T, ParseError>;

#[cfg(test)]
mod test {
    use super::*;

    fn get_tokens(input: &str) -> Vec<Token> {
        tokenize_file(format!("vim9script\n{}", input).chars().collect()).unwrap()
    }

    #[test]
    fn parses_vim9script() {
        let tokens = tokenize_file("vim9script".into()).unwrap();

        assert_eq!(
            Program {
                statements: vec![Statement::Vim9Script]
            },
            parse(tokens)
        );
    }

    #[test]
    fn parses_a_var_statement() {
        let tokens = get_tokens("var x = 5");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script,
                    Statement::Var {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Number(5),
                    }
                ]
            },
            parse(tokens)
        );
    }

    #[test]
    fn parses_a_var_statement_with_addition() {
        let tokens = get_tokens("var x = 5 + 6");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script,
                    Statement::Var {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Infix {
                            operator: Operator::Add,
                            left: Box::new(Expression::Number(5)),
                            right: Box::new(Expression::Number(6)),
                        },
                    }
                ]
            },
            parse(tokens)
        );
    }

    #[test]
    fn parses_a_var_statement_operator_precedence_1() {
        let tokens = get_tokens("var x = 5 + 6 * 2");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script,
                    Statement::Var {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Infix {
                            operator: Operator::Add,
                            left: Box::new(Expression::Number(5)),
                            right: Box::new(Expression::Infix {
                                operator: Operator::Mul,
                                left: Box::new(Expression::Number(6)),
                                right: Box::new(Expression::Number(2)),
                            }),
                        },
                    }
                ]
            },
            parse(tokens)
        );
    }

    #[test]
    fn parses_a_var_statement_operator_precedence_2() {
        let tokens = get_tokens("var x = 5 * 6 + 2");

        assert_eq!(
            Program {
                statements: vec![
                    Statement::Vim9Script,
                    Statement::Var {
                        identifier: Identifier { name: "x".into() },
                        expression: Expression::Infix {
                            operator: Operator::Add,
                            left: Box::new(Expression::Infix {
                                operator: Operator::Mul,
                                left: Box::new(Expression::Number(5)),
                                right: Box::new(Expression::Number(6)),
                            }),
                            right: Box::new(Expression::Number(2)),
                        },
                    }
                ]
            },
            parse(tokens)
        );
    }

    #[test]
    fn parses_a_var_statement_with_no_equal() {
        let tokens = get_tokens("var x 5");
        let parsed = parse(tokens);

        // TODO: This is not that pretty
        assert!(matches!(parsed.statements[1], Statement::Error { .. }));
    }
}
