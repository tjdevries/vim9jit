#![allow(dead_code)]

use std::collections::VecDeque;
use std::fmt::Debug;

#[derive(Clone, PartialEq)]
pub struct Span {
    start_row: usize,
    start_col: usize,
    end_row: usize,
    end_col: usize,
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({},{})->({},{})",
            self.start_row, self.start_col, self.end_row, self.end_col
        )
    }
}

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub span: Span,
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Token({:?}, {:?}, {:?})", self.kind, self.text, self.span)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal,
    EndOfFile,

    // TODO: Is this crazy for this lang??
    EndOfLine,
    Comment,

    // Identifiers and literals
    Identifier,
    Integer,

    // Reserved Tokens
    True,
    False,
    Null,

    // Operators
    Equal,
    Plus,
    Minus,
    Mul,
    Div,
    PlusEquals,
    MinusEquals,
    MulEquals,
    DivEquals,

    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    EqualTo,
    NotEqualTo,
    Or,
    And,

    // Delimiters
    Comma,
    Colon,

    SingleQuoteString,
    DoubleQuoteString,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
}

pub struct Lexer {
    pub input: String,
    pub position: usize,
    pub read_position: usize,
    pub ch: Option<char>,

    chars: Vec<char>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer {{ position: {}, read_position: {}, ch: {:?} }}",
            self.position, self.read_position, self.ch
        )
    }
}

impl Lexer {
    fn new(input: &str) -> Self {
        let input = input.to_string();
        let chars = input.clone().chars().collect();
        Self {
            input,
            position: 0,
            read_position: 0,
            ch: None,
            chars,
        }
    }

    fn make_span(&self, start: usize, end: usize) -> Span {
        // TODO: This is broken for doing stuff with empty strings... :'(
        assert!(
            start <= end,
            "start must be less than end. {} {}\n{:#?}",
            start,
            end,
            self
        );

        let mut span = Span {
            start_row: 0,
            start_col: 0,
            end_row: 0,
            end_col: 0,
        };

        let mut row = 0;
        let mut row_start = 0;
        for (idx, ch) in self.input.chars().enumerate() {
            if idx == start {
                span.start_row = row;
                span.start_col = idx - row_start;
            }

            if idx == end {
                span.end_row = row;
                span.end_col = idx - row_start;
                return span;
            }

            if ch == '\n' {
                row += 1;
                row_start = idx + 1;
            }
        }

        unreachable!(
            "Should always fine the start and end for a span:\n{} {} {}",
            start,
            end,
            self.input.chars().count()
        )
    }

    pub fn read_char(&mut self) {
        match self.chars.get(self.read_position) {
            Some(&c) => {
                self.ch = Some(c);
                self.position = self.read_position;
                self.read_position += 1;
            }
            None => {
                self.ch = None;
            }
        }
    }

    fn read_number(&mut self) -> Token {
        let position = self.position;
        while let Some(ch) = self.ch && ch.is_numeric(){
            self.read_char();
        }

        Token {
            kind: TokenKind::Integer,
            text: self.chars[position..self.position].iter().collect(),
            span: self.make_span(position, self.position),
        }
    }

    fn read_until(&mut self, until: char, kind: TokenKind) -> Token {
        self.read_char();
        if let Some(ch) = self.ch && ch == until {
            return Token {
                kind,
                text: self.chars[self.position..self.position].iter().collect(),
                span: self.make_span(self.position, self.position ),
            };
        }

        let position = self.position;

        while let Some(ch) = self.ch && ch != until {
            self.read_char();
        }

        Token {
            kind,
            text: self.chars[position..self.position].iter().collect(),
            span: self.make_span(position, self.position - 1),
        }
    }

    fn read_comment(&mut self) -> Token {
        let position = self.position;
        while let Some(ch) = self.ch && ch != '\n' {
            self.read_char();
        }

        Token {
            kind: TokenKind::Comment,
            text: self.chars[position..self.position].iter().collect(),
            span: self.make_span(position, self.position),
        }
    }

    fn read_identifier(&mut self) -> Token {
        let position = self.position;
        while let Some(ch) = self.ch && is_identifier(ch) {
            self.read_char();
        }

        let text: String = self.chars[position..self.position].iter().collect();
        let kind = match text.as_str() {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            _ => TokenKind::Identifier,
        };

        Token {
            kind,
            text,
            span: self.make_span(position, self.position),
        }
    }

    fn skip_whitespace(&mut self) {
        if self.ch == Some('\n') {
            return;
        }

        while let Some(ch) = self.ch && ch.is_ascii_whitespace() {
            if ch == '\n' {
                return
            }

            self.read_char();
        }
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.get(self.position + 1)
    }

    fn if_peek(&mut self, peeked: char, no: TokenKind, yes: TokenKind) -> Token {
        if let Some(ch) = self.peek_char() {
            if *ch == peeked {
                let position = self.position;
                self.read_char();

                Token {
                    kind: yes,
                    text: self.chars[position..=self.position].iter().collect(),
                    span: self.make_span(position, self.position),
                }
            } else {
                Token {
                    kind: no,
                    text: self.ch.unwrap().to_string(),
                    span: self.make_span(self.position, self.position),
                }
            }
        } else {
            Token {
                kind: TokenKind::EndOfFile,
                text: "".to_string(),
                span: self.make_span(self.position, self.position),
            }
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            Some(ch) => {
                macro_rules! literal {
                    ($kind:tt) => {
                        Token {
                            kind: TokenKind::$kind,
                            text: ch.to_string(),
                            span: self.make_span(self.position, self.position),
                        }
                    };
                }

                match ch {
                    // Operators that can optionally have an additional equals
                    '=' => self.if_peek('=', TokenKind::Equal, TokenKind::EqualTo),
                    '+' => self.if_peek('=', TokenKind::Plus, TokenKind::PlusEquals),
                    '-' => self.if_peek('=', TokenKind::Minus, TokenKind::MinusEquals),
                    '*' => self.if_peek('=', TokenKind::Mul, TokenKind::MulEquals),
                    '/' => self.if_peek('=', TokenKind::Div, TokenKind::DivEquals),
                    '>' => self.if_peek('=', TokenKind::GreaterThan, TokenKind::GreaterThanOrEqual),
                    '<' => self.if_peek('=', TokenKind::LessThan, TokenKind::LessThanOrEqual),
                    '|' => self.if_peek('|', TokenKind::Illegal, TokenKind::Or),
                    '&' => self.if_peek('&', TokenKind::Illegal, TokenKind::And),

                    ':' => literal!(Colon),
                    '(' => literal!(LeftParen),
                    ')' => literal!(RightParen),
                    '[' => literal!(LeftBracket),
                    ']' => literal!(RightBracket),
                    '{' => literal!(LeftBrace),
                    '}' => literal!(RightBrace),
                    '\n' => literal!(EndOfLine),
                    '#' => self.read_comment(),

                    // TODO: Handle escaped strings.
                    '\'' => self.read_until('\'', TokenKind::SingleQuoteString),
                    '"' => self.read_until('"', TokenKind::DoubleQuoteString),

                    _ => {
                        // Token
                        if is_letter(ch) {
                            return self.read_identifier();
                        } else if ch.is_ascii_digit() {
                            return self.read_number();
                        } else {
                            Token {
                                kind: TokenKind::Illegal,
                                text: ch.to_string(),
                                span: self.make_span(self.position, self.position),
                            }
                        }
                    }
                }
            }
            None => Token {
                kind: TokenKind::EndOfFile,
                text: "".to_string(),
                span: self.make_span(self.position, self.position),
            },
        };

        self.read_char();
        tok
    }
}

fn is_identifier(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

pub fn new_lexer(input: &str) -> Lexer {
    let mut l = Lexer::new(input);
    l.read_char();
    l
}

pub fn snapshot_lexing(input: &str) -> String {
    let mut lexer = new_lexer(input);

    let mut tokens = VecDeque::new();
    loop {
        let tok = lexer.next_token();
        if tok.kind == TokenKind::EndOfFile {
            break;
        }

        tokens.push_back(tok);
    }

    let mut output = String::new();
    for (row, line) in input.lines().enumerate() {
        output += line;
        output += "\n";

        while let Some(tok) = tokens.pop_front() {
            if tok.span.start_row != tok.span.end_row {
                panic!("We haven't handled this yet");
            }

            if tok.span.start_row != row {
                tokens.push_front(tok);
                break;
            }

            output += &" ".repeat(tok.span.start_col);
            output += &"^".repeat(tok.span.end_col + 1 - tok.span.start_col);
            output += &format!(" {:?}", tok);
            output += "\n"
        }
    }

    output
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.bind(|| {
                    insta::assert_snapshot!(snapshot_lexing(contents));
                });
            }
        };
    }

    snapshot!(test_lexer_1, "../testdata/snapshots/lexer_1.vim");
    snapshot!(test_comparisons, "../testdata/snapshots/comparisons.vim");
    snapshot!(test_string, "../testdata/snapshots/string.vim");

    // TODO: Check more thoroughly
    snapshot!(test_matchparen, "../testdata/snapshots/matchparen.vim");
}
