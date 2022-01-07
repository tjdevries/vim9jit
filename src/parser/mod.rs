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
    Expected { actual: String, expected: String },

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
        LeftParen => Precedence::Call,

        // Everything else, but I want this to error if we miss one
        StartOfFile | Vim9Script | Comment | Number | Identifier | TypeDeclaration | CommandVar | GlobalScope
        | TabScope | WindowScope | BufferScope | ScriptScope | LocalScope | NewLine | Equal | RightParen
        | LeftBracket | RightBracket | Colon | Comma | Ignore | EOF | ParseError => Precedence::Lowest,
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

    pub fn expect_peek(&mut self, expect: TokenKind) -> bool {
        self.next_token().kind == expect
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

pub fn parse(tokens: Vec<Token>) -> ParseResult<ast::Program> {
    let mut parser = Parser {
        tokens,
        position: 0,
        read_position: 0,
    };

    parser.parse()
}

// vim9script
// let x = 5
// def asdf()
//  stuff inside
//  echo [1, 2, 3]
// enddef

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::ast::InfOp;
    use crate::ast::PreOp;
    use crate::ast::TypeDeclaration;
    use crate::ast::*;
    use crate::lexer::tokenize_file;
    use crate::lexer::T;

    macro_rules! prog {
        [$( $x:expr ),*] => {
            Program {
                statements: vec![
                    vim9!(),
                    $(
                        $x,
                    )*
                ]
            }
        }
    }

    macro_rules! vim9 {
        () => {
            Statement::Vim9Script(StatementVim9 {})
        };
    }

    macro_rules! var {
        ($id: ident = $expression: expr) => {
             Statement::Var(StatementVar {
                identifier: id!($id),
                type_decl: None,
                equal: T![=],
                expr: $expression.into(),
            })
        };

        ($id: ident: $type_decl: ident = $expression: expr) => {
             Statement::Var(StatementVar {
                identifier: id!($id),
                type_decl: Some(TypeDeclaration::$type_decl),
                equal: T![=],
                expr: $expression.into(),
            })
        };
    }

    macro_rules! inf {
        ($op:tt, $left:expr, $right:expr) => {
            Expression::Infix {
                left: Box::new($left.into()),
                operator: InfOp!($op),
                right: Box::new($right.into()),
            }
        };
    }

    macro_rules! pre {
        ($op:tt, $expression:expr) => {
            Expression::Prefix {
                operator: PreOp!($op),
                right: Box::new($expression.into()),
            }
        };
    }

    macro_rules! id {
        ($name:ident) => {
            Identifier {
                name: stringify!($name).to_string(),
            }
        };
    }

    macro_rules! call {
        ($name:ident,[$($x:expr),*]) => {
            Expression::Call(FunctionCall {
                function: Box::new(Expression::Identifier(id!($name))),
                args: vec![
                    $(
                        $x,
                    )*
                ],

                rparen: Token::new(TokenKind::RightParen, ")"),
            })
        };
    }

    fn get_tokens(input: &str) -> Vec<Token> {
        tokenize_file(format!("vim9script\n{}", input).chars().collect()).unwrap()
    }

    #[test]
    fn parses_vim9script() -> ParseResult<()> {
        let tokens = tokenize_file("vim9script".into()).unwrap();

        assert_eq!(prog![], parse(tokens)?);

        Ok(())
    }

    macro_rules! test_prog {
        ($name:ident, $program:literal, $statement:expr) => {
            #[test]
            fn $name() -> ParseResult<()> {
                let tokens = get_tokens($program);
                dbg!(&tokens);

                assert_eq!(prog![$statement], parse(tokens)?);

                Ok(())
            }
        }; // TODO: Make list of statements
           // ($name:ident, $program:literal,[$statement:expr]) => {
           //     #[test]
           //     fn $name() -> ParseResult<()> {
           //         let tokens = get_tokens($program);

           //         assert_eq!(prog![$statement], parse(tokens)?);

           //         Ok(())
           //     }
           // };
    }

    test_prog!(
        parses_a_simple_var,
        "var foobarbaz = 12341234",
        var! { foobarbaz = 12341234 }
    );

    test_prog!(
        parses_a_prefix,
        "var x = -5 * foo",
        var! { x = inf!(*, pre!(-, 5), id!(foo)) }
    );

    test_prog!(
        parses_a_var_statement_with_addition,
        "var x = 5 + 6",
        var! { x = inf!(+, 5, 6) }
    );

    test_prog!(
        parses_a_var_statement_operator_precedence_1,
        "var x = 5 + 6 * 2",
        var! { x = inf!(+, 5, inf!(*, 6, 2)) }
    );

    test_prog!(
        parses_a_var_statement_operator_precedence_2,
        "var x = 5 * 6 + foo",
        var! { x = inf!(+, inf!(*, 5, 6), id!(foo)) }
    );

    test_prog!(
        parses_a_var_with_a_global_variable,
        "var x = g:foo",
        var! {
            x = Expression::VimVariable(
                ast::VimVariable{
                    scope: ast::VimVariableScope::Global,
                    identifier: "foo".into()
                }
            )
        }
    );

    test_prog!(
        parses_a_var_with_a_type_definition_number,
        "var xyz: number = 5",
        var! {
            xyz: Number = 5
        }
    );

    test_prog!(
        parses_a_var_with_a_type_definition_bool,
        "var xyz: bool = hello",
        var! {
            xyz: Bool = id!(hello)
        }
    );

    test_prog!(
        parses_a_fuction_call,
        "var x = abs(1)",
        var! { x = call!(abs, [1.into()]) }
    );

    #[test]
    fn errors_when_a_var_statement_has_no_equal() {
        let tokens = get_tokens("var x 5");
        assert!(parse(tokens).is_err());
    }
}
