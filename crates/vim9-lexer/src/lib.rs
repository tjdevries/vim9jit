#![feature(let_chains)]
#![allow(dead_code)]

use std::{collections::VecDeque, fmt::Debug};

#[derive(Clone, PartialEq)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
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

impl Token {
    pub fn fake() -> Token {
        Token {
            kind: TokenKind::Virtual,
            text: "".to_string(),
            span: Span {
                start_row: 0,
                start_col: 0,
                end_row: 0,
                end_col: 0,
            },
        }
    }
}

impl Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token({:?}, {:?}, {:?})",
            self.kind, self.text, self.span
        )
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    Illegal,
    EndOfFile,
    Virtual,

    // TODO: Is this crazy for this lang??
    EndOfLine,
    Comment,

    // Identifiers and literals
    Identifier,
    Integer,
    Float,
    EnvironmentVariable,

    // Reserved Tokens
    True,
    False,
    Null,
    Ellipsis,

    // Operators
    Equal,
    Plus,
    Minus,
    Mul,
    Div,
    Percent,
    PlusEquals,
    MinusEquals,
    MulEquals,
    DivEquals,
    PercentEquals,
    Ampersand,
    Dot,
    StringConcat,
    StringConcatEquals,

    //                  use 'ignorecase'    match case      ignore case
    // equal                   ==              ==#             ==?
    // not equal               !=              !=#             !=?
    // greater than            >               >#              >?
    // greater than or equal   >=              >=#             >=?
    // smaller than            <               <#              <?
    // smaller than or equal   <=              <=#             <=?
    // regexp matches          =~              =~#             =~?
    // regexp doesn't match    !~              !~#             !~?
    // same instance           is              is#             is?
    // different instance      isnot           isnot#          isnot?
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

    // Bools
    Or,
    And,
    Bang,
    QuestionMark,
    HeredocOperator,
    Register,

    // Delimiters
    Comma,
    Colon,
    SpacedColon,
    Caret,
    Escaped,
    Arrow,
    MethodArrow,
    AngleLeft,
    AngleRight,

    SingleQuote,
    SingleQuoteString,
    DoubleQuoteString,
    InterpolatedString,
    InterpolatedLiteralString,

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
}

impl TokenKind {
    pub fn is_colon(&self) -> bool {
        *self == Self::Colon || *self == Self::SpacedColon
    }

    pub fn is_whitespace(&self) -> bool {
        match self {
            Self::EndOfLine => true,
            Self::EndOfFile => true,
            _ => false,
        }
    }

    pub fn is_eof(&self) -> bool {
        *self == Self::EndOfFile
    }

    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
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
                | TokenKind::IsNotInsensitive
        )
    }

    pub fn is_assignment(&self) -> bool {
        use TokenKind::*;
        matches!(
            self,
            Equal
                | PlusEquals
                | MinusEquals
                | MulEquals
                | DivEquals
                | StringConcatEquals
        )
    }
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
        while let Some(ch) = self.ch && (ch.is_numeric() || ch == '\'') {
            self.read_char();
        }

        if self.ch == Some('.') {
            // consume the .
            self.read_char();

            // read the rest of the number
            while let Some(ch) = self.ch && ch.is_numeric() {
                self.read_char();
            }

            Token {
                kind: TokenKind::Float,
                text: self.chars[position..self.position].iter().collect(),
                span: self.make_span(position, self.position),
            }
        } else {
            Token {
                kind: TokenKind::Integer,
                text: self.chars[position..self.position]
                    .iter()
                    .filter(|c| **c != '\'')
                    .collect(),
                span: self.make_span(position, self.position),
            }
        }
    }

    fn read_until_or<F>(
        &mut self,
        until: char,
        fail: F,
        passed: TokenKind,
        failed: TokenKind,
    ) -> Token
    where
        F: Fn(&char) -> bool,
    {
        self.read_char();
        if let Some(ch) = self.ch && ch == until {
            return Token {
                kind: passed,
                text: self.chars[self.position..self.position].iter().collect(),
                span: self.make_span(self.position, self.position),
            };
        }

        let position = self.position;

        while let Some(ch) = self.ch && ch != until {
            if fail(&ch) {
                return Token {
                    kind: failed,
                    text: self.chars[self.position..self.position].iter().collect(),
                    span: self.make_span(self.position, self.position),
                };
            }

            self.read_char();
        }

        Token {
            kind: passed,
            text: self.chars[position..self.position].iter().collect(),
            span: self.make_span(position, self.position - 1),
        }
    }

    fn read_until<F>(
        &mut self,
        until: char,
        kind: TokenKind,
        fail: F,
    ) -> Option<Token>
    where
        F: Fn(&char) -> bool,
    {
        self.read_char();
        if let Some(ch) = self.ch && ch == until {
            return Some(Token {
                kind,
                text: self.chars[self.position..self.position].iter().collect(),
                span: self.make_span(self.position, self.position ),
            });
        }

        let position = self.position;

        while let Some(ch) = self.ch && ch != until {
            if fail(&ch) {
                return None;
            }

            self.read_char();
        }

        Some(Token {
            kind,
            text: self.chars[position..self.position].iter().collect(),
            span: self.make_span(position, self.position - 1),
        })
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
            "is" => match self.ch {
                Some('#') => {
                    self.read_char();
                    TokenKind::Is
                }
                Some('?') => {
                    self.read_char();
                    TokenKind::IsInsensitive
                }
                _ => TokenKind::Is,
            },
            "isnot" => match self.ch {
                Some('#') => {
                    self.read_char();
                    TokenKind::IsNot
                }
                Some('?') => {
                    self.read_char();
                    TokenKind::IsNotInsensitive
                }
                _ => TokenKind::IsNot,
            },
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

    fn peek_n(&self, n: usize) -> Option<&char> {
        self.chars.get(self.position + n)
    }

    fn if_peek(
        &mut self,
        peeked: char,
        no: TokenKind,
        yes: TokenKind,
    ) -> Token {
        if let Some(ch) = self.peek_char() {
            if *ch == peeked {
                let position = self.position;
                self.read_char();

                Token {
                    kind: yes,
                    text: self.chars[position..=self.position].iter().collect(),
                    span: self.make_span(position, self.position + 1),
                }
            } else {
                Token {
                    kind: no,
                    text: self.ch.unwrap().to_string(),
                    span: self.make_span(self.position, self.position + 1),
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

    // pub fn next_token(&mut self) -> Token {
    //     match &self.mode {
    //         LexerMode::Standard => self.next_standard_token(),
    //         LexerMode::HeredocStart => {
    //             // Looking for trim or eval or MARKER
    //             self.skip_whitespace();
    //
    //             let ch = self.ch.unwrap();
    //             if !ch.is_alphabetic() {
    //                 return Token {
    //                     kind: TokenKind::Illegal,
    //                     text: ch.to_string(),
    //                     span: self.make_span(self.position, self.position + 1),
    //                 };
    //             }
    //
    //             let identifier = self.read_identifier();
    //             match identifier.text.as_str() {
    //                 "trim" => identifier,
    //                 "eval" => identifier,
    //                 _ => {
    //                     // TODO: Should make sure that it's uppercase and
    //                     // all this other garbage, but for now, just assume it's fine
    //                     self.mode = LexerMode::Heredoc {
    //                         marker: identifier.text.clone(),
    //                     };
    //                     Token {
    //                         kind: TokenKind::HeredocMarker,
    //                         text: identifier.text,
    //                         span: identifier.span,
    //                     }
    //                 }
    //             }
    //         }
    //         LexerMode::Heredoc { marker } => {
    //             // Keep consuming ALL the text, until we see ^MARKER$
    //             todo!()
    //         }
    //     }
    // }

    fn literal(&mut self, ch: char, kind: TokenKind) -> Token {
        Token {
            kind,
            text: ch.to_string(),
            span: self.make_span(self.position, self.position + 1),
        }
    }

    fn empty_literal(&mut self, ch: char, kind: TokenKind) -> Token {
        Token {
            kind,
            text: ch.to_string(),
            span: self.make_span(self.position, self.position),
        }
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;

        self.skip_whitespace();

        let tok = match self.ch {
            Some(ch) => {
                match ch {
                    // Operators that can optionally have an additional equals
                    '=' => self.handle_equal(),
                    '!' => self.handle_bang(),
                    '>' => self.handle_gt(),
                    '<' => self.handle_lt(),
                    '-' => self.handle_dash(),
                    '$' => self.handle_dollar(),
                    '.' => self.handle_dot(),
                    '+' => self.if_peek('=', Plus, PlusEquals),
                    '*' => self.if_peek('=', Mul, MulEquals),
                    '/' => self.if_peek('=', Div, DivEquals),
                    '|' => self.if_peek('|', Illegal, Or),
                    '&' => self.if_peek('&', Ampersand, And),
                    '%' => self.if_peek('=', Percent, PercentEquals),
                    '\\' => {
                        self.read_char();
                        Token {
                            kind: Escaped,
                            text: self.ch.unwrap().to_string(),
                            span: self
                                .make_span(self.position - 1, self.position),
                        }
                    }
                    '@' => {
                        self.read_char();
                        Token {
                            kind: Register,
                            text: self.ch.unwrap().to_string(),
                            span: self
                                .make_span(self.position - 1, self.position),
                        }
                    }

                    ':' => self.handle_colon(),
                    '?' => self.literal(ch, QuestionMark),
                    '^' => self.literal(ch, Caret),
                    '(' => self.literal(ch, LeftParen),
                    ')' => self.literal(ch, RightParen),
                    '[' => self.literal(ch, LeftBracket),
                    ']' => self.literal(ch, RightBracket),
                    '{' => self.literal(ch, LeftBrace),
                    '}' => self.literal(ch, RightBrace),
                    ',' => self.literal(ch, Comma),
                    '\n' => self.empty_literal(ch, EndOfLine),
                    '#' => self.read_comment(),

                    // TODO: Handle escaped strings.
                    '\'' => self.handle_single_quote(),
                    '"' => self.read_until_or(
                        '"',
                        |ch| *ch == '\n',
                        DoubleQuoteString,
                        Comment,
                    ),

                    _ => {
                        // Token
                        if is_letter(ch) || ch == '_' {
                            return self.read_identifier();
                        } else if ch.is_ascii_digit() {
                            return self.read_number();
                        } else {
                            Token {
                                kind: Illegal,
                                text: ch.to_string(),
                                span: self
                                    .make_span(self.position, self.position),
                            }
                        }
                    }
                }
            }
            None => Token {
                kind: EndOfFile,
                text: "".to_string(),
                span: self.make_span(self.position, self.position),
            },
        };

        self.read_char();
        tok
    }

    fn read_one(&mut self, kind: TokenKind) -> Token {
        Token {
            kind,
            text: self.ch.unwrap().to_string(),
            span: self.make_span(self.position, self.position + 1),
        }
    }

    fn read_two(&mut self, kind: TokenKind) -> Token {
        let position = self.position;
        self.read_char();

        Token {
            kind,
            text: self.chars[position..=self.position].iter().collect(),
            span: self.make_span(position, self.position + 1),
        }
    }

    fn read_three(&mut self, kind: TokenKind) -> Token {
        let position = self.position;
        self.read_char();
        self.read_char();

        Token {
            kind,
            text: self.chars[position..=self.position].iter().collect(),
            span: self.make_span(position, self.position + 1),
        }
    }

    fn handle_equal(&mut self) -> Token {
        let peeked = (
            self.peek_n(1).unwrap().to_owned(),
            self.peek_n(2).unwrap().to_owned(),
        );

        match peeked {
            ('=', '#') => self.read_three(TokenKind::EqualTo),
            ('=', '?') => self.read_three(TokenKind::EqualToIns),
            ('=', _) => self.read_two(TokenKind::EqualTo),
            ('<', '<') => self.read_three(TokenKind::HeredocOperator),
            ('<', _) => self.read_two(TokenKind::Illegal),
            ('~', '#') => self.read_three(TokenKind::RegexpMatches),
            ('~', '?') => self.read_three(TokenKind::RegexpMatchesIns),
            ('~', _) => self.read_two(TokenKind::RegexpMatches),
            ('>', _) => self.read_two(TokenKind::Arrow),
            (_, _) => self.read_one(TokenKind::Equal),
        }
    }

    fn handle_bang(&mut self) -> Token {
        let peeked = (
            self.peek_n(1).unwrap().to_owned(),
            self.peek_n(2).unwrap().to_owned(),
        );

        match peeked {
            ('=', '#') => self.read_three(TokenKind::NotEqualTo),
            ('=', '?') => self.read_three(TokenKind::NotEqualToIns),
            ('=', _) => self.read_two(TokenKind::NotEqualTo),
            ('~', '#') => self.read_three(TokenKind::NotRegexpMatches),
            ('~', '?') => self.read_three(TokenKind::NotRegexpMatchesIns),
            ('~', _) => self.read_two(TokenKind::NotRegexpMatches),
            (_, _) => self.read_one(TokenKind::Bang),
        }
    }

    fn handle_gt(&mut self) -> Token {
        let peeked = (
            self.peek_n(1).unwrap().to_owned(),
            self.peek_n(2).unwrap().to_owned(),
        );

        match peeked {
            ('=', '#') => self.read_three(TokenKind::GreaterThanOrEqual),
            ('=', '?') => self.read_three(TokenKind::GreaterThanOrEqualIns),
            ('=', _) => self.read_two(TokenKind::GreaterThanOrEqual),
            ('#', _) => self.read_two(TokenKind::GreaterThan),
            ('?', _) => self.read_two(TokenKind::GreaterThanIns),
            (c, _) if c.is_whitespace() => {
                self.read_one(TokenKind::GreaterThan)
            }
            (_, _) => self.read_one(TokenKind::AngleRight),
        }
    }

    fn handle_lt(&mut self) -> Token {
        let peeked = (
            self.peek_n(1).unwrap().to_owned(),
            self.peek_n(2).unwrap().to_owned(),
        );

        match peeked {
            ('=', '#') => self.read_three(TokenKind::LessThanOrEqual),
            ('=', '?') => self.read_three(TokenKind::LessThanOrEqualIns),
            ('=', _) => self.read_two(TokenKind::LessThanOrEqual),
            ('#', _) => self.read_two(TokenKind::LessThan),
            ('?', _) => self.read_two(TokenKind::LessThanIns),
            (c, _) if c.is_whitespace() => self.read_one(TokenKind::LessThan),
            (_, _) => self.read_one(TokenKind::AngleLeft),
        }
    }

    fn handle_dash(&mut self) -> Token {
        match self.peek_char().unwrap() {
            '=' => self.read_two(TokenKind::MinusEquals),
            '>' => self.read_two(TokenKind::MethodArrow),
            _ => self.read_one(TokenKind::Minus),
        }
    }

    pub fn handle_dollar(&mut self) -> Token {
        match self.peek_char().unwrap() {
            '\'' => {
                self.read_char();
                self.read_until(
                    '\'',
                    TokenKind::InterpolatedLiteralString,
                    |ch| *ch == '\n',
                )
                .unwrap()
            }
            '"' => {
                self.read_char();
                self.read_until('\"', TokenKind::InterpolatedString, |ch| {
                    *ch == '\n'
                })
                .expect(&format!("{:?}", self))
            }
            c if is_identifier(*c) => {
                self.read_char();

                let position = self.position;
                while let Some(ch) = self.ch && is_identifier(ch) {
                    self.read_char();
                }

                Token {
                    kind: TokenKind::EnvironmentVariable,
                    text: self.chars[position..self.position].iter().collect(),
                    span: self.make_span(position, self.position),
                }
            }
            _ => Token {
                kind: TokenKind::Illegal,
                text: self.ch.unwrap().to_string(),
                span: self.make_span(self.position, self.position),
            },
        }
    }

    fn handle_dot(&mut self) -> Token {
        let peeked = (
            self.peek_n(1).unwrap().to_owned(),
            self.peek_n(2).unwrap().to_owned(),
        );

        match peeked {
            ('.', '=') => self.read_three(TokenKind::StringConcatEquals),
            ('.', '.') => self.read_three(TokenKind::Ellipsis),
            ('.', _) => self.read_two(TokenKind::StringConcat),
            (_, _) => self.read_one(TokenKind::Dot),
        }
    }

    fn handle_colon(&mut self) -> Token {
        match self.peek_char().unwrap() {
            ' ' => self.read_two(TokenKind::SpacedColon),
            ']' => self.read_one(TokenKind::SpacedColon),
            _ => self.read_one(TokenKind::Colon),
        }
    }

    fn peek_in_line<F>(&mut self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let mut n = 1;
        while let Some(peeked) = self.peek_n(n) && *peeked != '\n' {
            if f(*peeked) {
                return true
            }

            n += 1;
        }

        false
    }

    fn handle_single_quote(&mut self) -> Token {
        if self.peek_in_line(|ch| ch == '\'') {
            return self
                .read_until('\'', TokenKind::SingleQuoteString, |ch| {
                    *ch == '\n'
                })
                .expect(&format!("{:#?}", self));
        }

        self.read_one(TokenKind::SingleQuote)
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

        if tok.kind == TokenKind::Illegal {
            panic!("failure: {:#?}", lexer);
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
            output += &"^".repeat(tok.span.end_col - tok.span.start_col);
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
    snapshot!(test_scopes, "../testdata/snapshots/scopes.vim");
    snapshot!(test_autocmd, "../testdata/snapshots/autocmd.vim");
    snapshot!(test_heredoc, "../testdata/snapshots/heredoc.vim");
    snapshot!(test_register, "../testdata/snapshots/register.vim");
    snapshot!(test_lambda, "../testdata/snapshots/lambda.vim");
    snapshot!(test_types, "../testdata/snapshots/types.vim");
    snapshot!(test_methods, "../testdata/snapshots/methods.vim");
    snapshot!(test_execute, "../testdata/snapshots/execute.vim");

    // snapshot!(test_cfilter, "../testdata/snapshots/cfilter.vim");

    // TODO: Check more thoroughly
    snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");
    snapshot!(test_handlers, "../../shared/snapshots/handlers.vim");
}
