#![allow(unreachable_code)]

use std::{
    cell::{Cell, RefCell},
    collections::VecDeque,
    fmt::{Debug, Display},
};

use anyhow::{Context, Result};

#[derive(Clone, PartialEq)]
pub struct Span {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

impl Span {
    pub fn empty() -> Self {
        Self {
            start_row: 0,
            start_col: 0,
            end_row: 0,
            end_col: 0,
        }
    }
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

#[derive(Clone, PartialEq, Default)]
pub enum TokenText<'a> {
    Slice(&'a [char]),
    Owned(String),
    #[default]
    Empty,
}

impl<'a> From<TokenText<'a>> for String {
    fn from(val: TokenText<'a>) -> Self {
        match val {
            TokenText::Slice(s) => s.iter().collect(),
            TokenText::Owned(s) => s,
            TokenText::Empty => "".to_string(),
        }
    }
}

impl<'a> From<&TokenText<'a>> for String {
    fn from(val: &TokenText<'a>) -> Self {
        match val {
            TokenText::Slice(s) => s.iter().collect(),
            TokenText::Owned(s) => s.clone(),
            TokenText::Empty => "".to_string(),
        }
    }
}

impl<'a> Debug for TokenText<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenText::Slice(s) => {
                write!(f, "{:?}", s.iter().cloned().collect::<String>())
            }
            TokenText::Owned(o) => write!(f, "{o:?}"),
            TokenText::Empty => write!(f, "<Empty>"),
        }
    }
}

impl<'a> Display for TokenText<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenText::Slice(s) => s.iter().collect::<String>(),
                TokenText::Owned(s) => s.clone(),
                TokenText::Empty => "".to_string(),
            }
        )
    }
}

impl<'a> TokenText<'a> {
    pub fn as_str(&'a self) -> &'a str {
        match self {
            TokenText::Slice(_) => todo!(),
            TokenText::Owned(s) => s,
            TokenText::Empty => "",
        }
    }

    pub fn equals(&self, val: &str) -> bool {
        match self {
            TokenText::Slice(s) => {
                // This seems like I shouldn't have to do this... oh well
                val.chars().collect::<Vec<char>>() == *s
            }
            TokenText::Owned(s) => s.as_str() == val,
            TokenText::Empty => false,
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub text: TokenText<'a>,
    pub span: Span,
}

impl Token<'_> {
    pub fn owned(from: Token<'_>) -> Token<'static> {
        Token {
            text: TokenText::Owned(from.text.to_string()),
            ..from
        }
    }

    pub fn fake() -> Token<'static> {
        Token {
            kind: TokenKind::Virtual,
            text: TokenText::Empty,
            span: Span::empty(),
        }
    }
}

impl Debug for Token<'_> {
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
    Literal,

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

    // Prepended operators
    Decrement,
    Increment,

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
        matches!(self, Self::EndOfLine | Self::EndOfFile)
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
            Equal | PlusEquals | MinusEquals | MulEquals | DivEquals | StringConcatEquals
        )
    }
}

trait SubLexer {
    fn next_token(&self, lexer: &Lexer) -> Result<(Token<'static>, Option<Box<dyn SubLexer>>)>;
}

type TokenAndLexer = (Token<'static>, Option<Box<dyn SubLexer>>);

pub struct LexerState {
    position: usize,
    read_position: usize,
}

pub struct Lexer {
    state: RefCell<LexerState>,

    sublexer: Cell<Option<Box<dyn SubLexer>>>,

    /// Vec containing all the chars,
    /// this allows easy accessing (and accounts for unicode chars and what not)
    chars: Vec<char>,

    /// Helper vec for quick line lookup
    lines: Vec<usize>,
}

impl Debug for Lexer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Lexer {{ position: {}, read_position: {}, ch: {:?} }}",
            self.state.borrow().position,
            self.state.borrow().read_position,
            self.ch()
        )
    }
}

impl Lexer {
    fn ch(&self) -> Option<&char> {
        self.chars.get(self.state.borrow().position)
    }

    pub fn new(input: &str) -> Self {
        let chars = input.chars().collect();

        let mut lines = vec![0];
        lines.extend(
            input
                .chars()
                .enumerate()
                .filter_map(|(idx, ch)| (ch == '\n').then_some(idx + 1)),
        );

        Self {
            state: RefCell::new(LexerState {
                position: 0,
                read_position: 1,
            }),
            sublexer: Cell::new(None),
            chars,
            lines,
        }
    }

    fn make_span(&self, start: usize, end: usize) -> Result<Span> {
        anyhow::ensure!(start <= end, "start must be less than end");

        let start_row = self
            .lines
            .iter()
            .enumerate()
            .find_map(|(line, &ch)| (ch > start).then_some(line))
            .ok_or_else(|| anyhow::anyhow!("input: {} {}", start, end))
            .context("start_row")?
            - 1;

        let end_row = self
            .lines
            .iter()
            .enumerate()
            .skip(start_row)
            .find_map(|(line, &ch)| (ch > end).then_some(line))
            .context("end_row")?
            - 1;

        Ok(Span {
            start_row,
            start_col: start - self.lines[start_row],
            end_row,
            end_col: end - self.lines[end_row],
        })
    }

    pub fn read_char(&self) {
        // println!("read_char: {:?}", self);

        let pos = self.state.borrow().position;
        if self.chars.get(pos).is_some() {
            self.state.borrow_mut().position = pos + 1;
            self.state.borrow_mut().read_position += 1;
        }
    }

    fn read_while<F>(&self, cond: F) -> Result<(usize, usize)>
    where
        F: Fn(char) -> bool,
    {
        let position = self.state.borrow().position;
        loop {
            if !cond(*self.ch().unwrap()) {
                break;
            }

            self.read_char();
        }

        Ok((position, self.state.borrow().position))
    }

    fn position(&self) -> usize {
        self.state.borrow().position
    }

    fn read_number(&self) -> Result<Token> {
        let (pos, _) = self.read_while(|ch| ch.is_numeric() || ch == '\'')?;

        if self.ch() == Some(&'.') {
            // consume the .
            self.read_char();

            // read the rest of the number
            while self.ch().is_some_and(|ch| ch.is_numeric()) {
                self.read_char();
            }

            Ok(Token {
                kind: TokenKind::Float,
                text: TokenText::Slice(self.chars[pos..self.position()].into()),
                span: self.make_span(pos, self.position())?,
            })
        } else {
            Ok(Token {
                kind: TokenKind::Integer,
                text: TokenText::Owned(
                    self.chars[pos..self.position()]
                        .iter()
                        .filter(|c| **c != '\'')
                        .collect(),
                ),
                span: self.make_span(pos, self.position())?,
            })
        }
    }

    fn read_until_or<F>(
        &self,
        until: char,
        fail: F,
        passed: TokenKind,
        failed: TokenKind,
    ) -> Result<Token>
    where
        F: Fn(&char) -> bool,
    {
        self.read_char();
        if self.ch().is_some_and(|&ch| ch == until) {
            return Ok(Token {
                kind: passed,
                text: TokenText::Slice(self.chars[self.position()..self.position()].into()),
                span: self.make_span(self.position(), self.position())?,
            });
        }

        let position = self.position();

        while let Some(ch) = self.ch().filter(|&&ch| ch != until) {
            if fail(ch) {
                return Ok(Token {
                    kind: failed,
                    text: TokenText::Slice(self.chars[self.position()..self.position()].into()),
                    span: self.make_span(self.position(), self.position())?,
                });
            }

            self.read_char();
        }

        Ok(Token {
            kind: passed,
            text: TokenText::Slice(self.chars[position..self.position()].into()),
            span: self.make_span(position, self.position() - 1)?,
        })
    }

    fn read_line(&self) -> Result<Token<'static>> {
        // TODO: Could probably do this without static and just peek until we see
        // a newline, then return that slice, but I wrote it this way the first time
        // so I think it's OK
        //
        // Also, we're only using this right now for NormalModeParser, so we need
        // to figure that out later
        let position = self.position();

        let mut line = String::new();
        loop {
            match self.ch() {
                Some(&ch) => {
                    line += &ch.to_string();

                    if let Some(&peek) = self.peek_char() {
                        if peek == '\n' {
                            break;
                        }
                    }

                    self.read_char();
                }
                None => panic!("OH NO"),
            }
        }

        Ok(Token {
            kind: TokenKind::Literal,
            text: TokenText::Owned(line),
            span: self.make_span(position, self.position())?,
        })
    }

    fn read_until<F>(&self, until: char, kind: TokenKind, fail: F) -> Result<Option<Token>>
    where
        F: Fn(&char) -> bool,
    {
        self.read_char();
        if self.ch().is_some_and(|&ch| ch == until) {
            return Ok(Some(Token {
                kind,
                text: TokenText::Slice(self.chars[self.position()..self.position()].into()),
                span: self.make_span(self.position(), self.position())?,
            }));
        }

        let position = self.position();

        while let Some(ch) = self.ch().filter(|&&ch| ch != until) {
            if fail(ch) {
                return Ok(None);
            }

            self.read_char();
        }

        Ok(Some(Token {
            kind,
            text: TokenText::Slice(self.chars[position..self.position()].into()),
            span: self.make_span(position, self.position() - 1)?,
        }))
    }

    fn read_comment(&self) -> Result<Token> {
        let (pos, _) = self.read_while(|ch| ch != '\n')?;

        Ok(Token {
            kind: TokenKind::Comment,
            text: TokenText::Slice(self.chars[pos..self.position()].into()),
            span: self.make_span(pos, self.position())?,
        })
    }

    fn read_identifier(&self) -> Result<Token> {
        let position = self.position();
        while self.ch().is_some_and(|&ch| is_identifier(ch)) {
            self.read_char();
        }

        // TODO(clone)
        let text_str = self.chars[position..self.position()]
            .iter()
            .collect::<String>();

        let text = TokenText::Slice(self.chars[position..self.position()].into());

        let kind = match text_str.as_str() {
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "null" => TokenKind::Null,
            "is" => match self.ch() {
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
            "isnot" => match self.ch() {
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
            "normal" => {
                self.sublexer.set(Some(Box::new(NormalModeParser {})));

                TokenKind::Identifier
            }
            _ => TokenKind::Identifier,
        };

        Ok(Token {
            kind,
            text,
            span: self.make_span(position, self.position())?,
        })
    }

    fn skip_whitespace(&self) {
        if self.ch() == Some(&'\n') {
            return;
        }

        while let Some(&ch) = self.ch().filter(|&&ch| ch.is_ascii_whitespace()) {
            if ch == '\n' {
                return;
            }

            self.read_char();
        }
    }

    fn peek_char(&self) -> Option<&char> {
        self.chars.get(self.position() + 1)
    }

    fn peek_n(&self, n: usize) -> Option<&char> {
        self.chars.get(self.position() + n)
    }

    fn if_peek(&self, peeked: char, no: TokenKind, yes: TokenKind) -> Result<Token> {
        if let Some(ch) = self.peek_char() {
            if *ch == peeked {
                let position = self.position();
                self.read_char();

                Ok(Token {
                    kind: yes,
                    text: TokenText::Slice(self.chars[position..=self.position()].into()),
                    span: self.make_span(position, self.position() + 1)?,
                })
            } else {
                Ok(Token {
                    kind: no,
                    // text: self.ch.unwrap().to_string(),
                    text: TokenText::Slice(self.chars[self.position()..=self.position()].into()),
                    span: self.make_span(self.position(), self.position() + 1)?,
                })
            }
        } else {
            Ok(Token {
                kind: TokenKind::EndOfFile,
                text: TokenText::Owned("".to_string()),
                span: Span::empty(),
            })
        }
    }

    fn literal(&self, ch: char, kind: TokenKind) -> Result<Token> {
        Ok(Token {
            kind,
            text: TokenText::Owned(ch.to_string()),
            span: self.make_span(self.position(), self.position() + 1)?,
        })
    }

    fn empty_literal(&self, ch: char, kind: TokenKind) -> Result<Token> {
        Ok(Token {
            kind,
            text: TokenText::Owned(ch.to_string()),
            span: self.make_span(self.position(), self.position())?,
        })
    }

    pub fn next_token(&self) -> Result<Token> {
        // Handle sublexers...
        //  there is some goofiness with all this stuff
        if let Some(sublexer) = self.sublexer.take() {
            let (tok, next_lexer) = sublexer.next_token(self)?;
            self.sublexer.set(next_lexer);
            self.read_char();
            return Ok(tok);
        }

        use TokenKind::*;

        self.skip_whitespace();

        let tok = match self.ch() {
            Some(&ch) => {
                match ch {
                    // Operators that can optionally have an additional equals
                    '=' => self.handle_equal(),
                    '!' => self.handle_bang(),
                    '>' => self.handle_gt(),
                    '<' => self.handle_lt(),
                    '-' => self.handle_dash(),
                    '$' => self.handle_dollar(),
                    '.' => self.handle_dot(),
                    '+' => self.handle_plus(),
                    '*' => self.if_peek('=', Mul, MulEquals),
                    '/' => self.if_peek('=', Div, DivEquals),
                    '|' => self.if_peek('|', Illegal, Or),
                    '&' => self.if_peek('&', Ampersand, And),
                    '%' => self.if_peek('=', Percent, PercentEquals),
                    '\\' => {
                        self.read_char();
                        Ok(Token {
                            kind: Escaped,
                            text: TokenText::Owned(self.ch().unwrap().to_string()),
                            span: self.make_span(self.position() - 1, self.position())?,
                        })
                    }
                    '@' => {
                        self.read_char();
                        Ok(Token {
                            kind: Register,
                            text: TokenText::Owned(self.ch().unwrap().to_string()),
                            span: self.make_span(self.position() - 1, self.position())?,
                        })
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
                    '"' => self.handle_double_quote(),

                    _ => {
                        // Token
                        if ch.is_alphabetic() || ch == '_' {
                            return self.read_identifier();
                        } else if ch.is_ascii_digit() {
                            return self.read_number();
                        } else {
                            Ok(Token {
                                kind: Illegal,
                                // text: TokenText::Ch(ch),
                                text: todo!(),
                                span: self.make_span(self.position(), self.position())?,
                            })
                        }
                    }
                }
            }
            None => Ok(Token {
                kind: EndOfFile,
                text: TokenText::Owned("".to_string()),
                span: Span::empty(),
            }),
        }?;

        self.read_char();
        Ok(tok)
    }

    fn read_one(&self, kind: TokenKind) -> Result<Token> {
        Ok(Token {
            kind,
            text: TokenText::Owned(self.ch().unwrap().to_string()),
            span: self.make_span(self.position(), self.position() + 1)?,
        })
    }

    fn read_two(&self, kind: TokenKind) -> Result<Token> {
        let position = self.position();
        self.read_char();

        Ok(Token {
            kind,
            text: TokenText::Slice(self.chars[position..=self.position()].into()),
            span: self.make_span(position, self.position() + 1)?,
        })
    }

    fn read_three(&self, kind: TokenKind) -> Result<Token> {
        let position = self.position();
        self.read_char();
        self.read_char();

        Ok(Token {
            kind,
            text: TokenText::Slice(self.chars[position..=self.position()].into()),
            span: self.make_span(position, self.position() + 1)?,
        })
    }

    fn handle_equal(&self) -> Result<Token> {
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

    fn handle_bang(&self) -> Result<Token> {
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

    fn handle_gt(&self) -> Result<Token> {
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
            (c, _) if c.is_whitespace() => self.read_one(TokenKind::GreaterThan),
            (_, _) => self.read_one(TokenKind::AngleRight),
        }
    }

    fn handle_lt(&self) -> Result<Token> {
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

    fn handle_dash(&self) -> Result<Token> {
        match self.peek_char().unwrap() {
            '=' => self.read_two(TokenKind::MinusEquals),
            '>' => self.read_two(TokenKind::MethodArrow),
            '-' => self.read_two(TokenKind::Decrement),
            _ => self.read_one(TokenKind::Minus),
        }
    }

    pub fn handle_dollar(&self) -> Result<Token> {
        Ok(match self.peek_char().unwrap() {
            '\'' => {
                self.read_char();
                self.read_until('\'', TokenKind::InterpolatedLiteralString, |ch| *ch == '\n')?
                    .unwrap()
            }
            '"' => {
                self.read_char();
                self.read_until('\"', TokenKind::InterpolatedString, |ch| *ch == '\n')?
                    .unwrap()
            }
            c if is_identifier(*c) => {
                self.read_char();

                let position = self.position();
                while self.ch().is_some_and(|&ch| is_identifier(ch)) {
                    self.read_char();
                }

                Token {
                    kind: TokenKind::EnvironmentVariable,
                    text: TokenText::Slice(self.chars[position..self.position()].into()),
                    span: self.make_span(position, self.position())?,
                }
            }
            _ => Token {
                kind: TokenKind::Illegal,
                // text: TokenText::Ch(self.ch.unwrap()),
                text: todo!(),
                span: self.make_span(self.position(), self.position())?,
            },
        })
    }

    fn handle_dot(&self) -> Result<Token> {
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

    fn handle_plus(&self) -> Result<Token> {
        match self.peek_n(1).unwrap() {
            '=' => self.read_two(TokenKind::PlusEquals),
            '+' => self.read_two(TokenKind::Increment),
            _ => self.read_one(TokenKind::Plus),
        }
    }

    fn handle_colon(&self) -> Result<Token> {
        match self.peek_char().unwrap() {
            ' ' => self.read_two(TokenKind::SpacedColon),
            ']' => self.read_one(TokenKind::SpacedColon),
            _ => self.read_one(TokenKind::Colon),
        }
    }

    fn peek_in_line<F>(&self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        let mut n = 1;
        while let Some(&peeked) = self.peek_n(n).filter(|&&peeked| peeked != '\n') {
            if f(peeked) {
                return true;
            }

            n += 1;
        }

        false
    }

    fn handle_single_quote(&self) -> Result<Token> {
        if self.peek_in_line(|ch| ch == '\'') {
            return Ok(self
                .read_until('\'', TokenKind::SingleQuoteString, is_newline)?
                .expect("single quote"));
        }

        self.read_one(TokenKind::SingleQuote)
    }

    fn handle_double_quote(&self) -> Result<Token> {
        if self.peek_in_line(|ch| ch == '"') {
            return Ok(self
                .read_until('"', TokenKind::DoubleQuoteString, is_newline)?
                .unwrap());
        }

        self.read_until_or(
            '"',
            |ch| *ch == '\n',
            TokenKind::DoubleQuoteString,
            TokenKind::Comment,
        )
    }
}

/// NormalModeParser is used to parse normal mode commands.
///
/// It will just read the rest of the line after normal mode,
/// consume all the text as one literal, and then continue on afterwards
struct NormalModeParser {}

impl SubLexer for NormalModeParser {
    fn next_token(&self, lexer: &Lexer) -> Result<TokenAndLexer> {
        if let Some(&ch) = lexer.ch() {
            if ch == ' ' {
                lexer.read_char();
            }
        }

        match lexer.ch() {
            Some(&ch) => match ch {
                '!' => Ok((
                    Token::owned(lexer.handle_bang()?),
                    Some(Box::new(NormalModeParser {})),
                )),
                _ => Ok((Token::owned(lexer.read_line()?), None)),
            },
            None => unreachable!("don't think this should happen..."),
        }
    }
}

fn is_newline(ch: &char) -> bool {
    *ch == '\n' || *ch == '\0'
}

fn is_identifier(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

pub fn snapshot_lexing(input: &str) -> String {
    let lexer = Lexer::new(input);

    let mut tokens = VecDeque::new();
    while let Ok(tok) = lexer.next_token() {
        if tok.kind == TokenKind::EndOfFile {
            break;
        }

        if tok.kind == TokenKind::Illegal {
            panic!("failure: {input:#?}");
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
            output += &format!(" {tok:?}");
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

    snapshot!(test_lexer, "../testdata/snapshots/lexer.vim");
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
    snapshot!(test_normal, "../testdata/snapshots/normal.vim");

    // snapshot!(test_cfilter, "../testdata/snapshots/cfilter.vim");

    // TODO: Check more thoroughly
    snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");
    snapshot!(test_handlers, "../../shared/snapshots/lsp_handlers.vim");
    snapshot!(test_selection, "../../shared/snapshots/lsp_selection.vim");
}
