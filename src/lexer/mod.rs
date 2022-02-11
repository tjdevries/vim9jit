fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_alphanumeric(ch: char) -> bool {
    is_letter(ch) || is_digit(ch)
}

fn is_identifier(ch: char) -> bool {
    is_alphanumeric(ch) || ch == '_'
}

fn is_whitespace(ch: char) -> bool {
    ch.is_whitespace() || ch == EOF
}

const EOF: char = 0 as char;
const EOL: char = '\n';

#[derive(Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub line: usize,
    pub col: usize,
    pub text: String,
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token<{:3},{:13}> = '{}'",
            self.line,
            format!("{:?}", self.kind),
            self.text.replace("\n", "\\n")
        )
    }
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        // TODO: At this point, I don't care about line/col for equals.
        // This lets me not worry about constructing them correctly
        self.kind == other.kind && self.text == other.text
    }
}

impl Token {
    pub fn dummy<A: AsRef<str>>(kind: TokenKind, text: A) -> Self {
        Self::new(kind, 0, text.as_ref())
    }

    pub fn new(kind: TokenKind, line: usize, text: &str) -> Self {
        match kind {
            TokenKind::Identifier => {
                assert!(text != "", "Text must not be empty");
            }
            _ => {}
        };

        Self {
            kind,
            line,
            col: 0,
            text: text.into(),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum TokenKind {
    StartOfFile,

    Vim9Script,
    Comment,

    Number,
    Identifier,

    // Commands
    CommandVar,
    CommandDef,
    CommandEndDef,
    CommandFor,
    CommandEndFor,
    CommandReturn,
    CommandEcho,

    // Scopes
    GlobalScope,
    TabScope,
    WindowScope,
    BufferScope,
    ScriptScope, // Does this one still exist??
    LocalScope,  // Does this one still exist??

    // Whitespace
    NewLine,

    // Operations
    Plus,
    Minus,
    Star,
    Slash,

    Equal,

    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Colon,

    Comma,

    Ignore,
    EOF,

    ParseError,
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,         // Source code
    pub position: usize,      // Reading position
    pub read_position: usize, // Current moving reading position
    pub ch: char,             // Current read character

    line: usize,
    offset: usize,
}

impl Lexer {
    fn new(input: Vec<char>) -> Lexer {
        Lexer {
            input,
            position: 0,
            read_position: 0,
            line: 0,
            offset: 0,
            ch: EOF,
        }
    }

    /// Read next char, update positions
    pub fn read_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            self.ch = EOF;
        } else {
            self.ch = self.input[self.read_position];
        }

        if self.ch == '\n' {
            self.line += 1;
        }

        self.position = self.read_position;
        self.read_position = self.read_position + 1;

        self.ch
    }

    pub fn peek_char(&self) -> char {
        self.peek_n(1).unwrap_or(EOF)
    }

    pub fn peek_n(&self, n: usize) -> Option<char> {
        let pos = self.read_position + n - 1;

        if pos >= self.input.len() {
            None
        } else {
            Some(self.input[pos])
        }
    }

    pub fn peek_until(&self, n: usize) -> String {
        let mut chars = Vec::with_capacity(n);
        for i in 1..=n {
            chars.push(self.peek_n(i).unwrap_or(EOF))
        }

        chars.iter().collect()
    }

    pub fn is_start(&self) -> bool {
        self.position == 0
    }
}

pub struct State {
    tokens: Vec<Token>,
    next: fn(&mut Lexer) -> Option<State>,
}

#[macro_export]
macro_rules! tok {
    ($kind:ident, $l:expr) => {
        Token::new(TokenKind::$kind, $l.line, &$l.ch.to_string())
    };

    ($kind:ident, $l:expr, $text:expr) => {
        Token::new(TokenKind::$kind, $l.line, &$text)
    };
}

macro_rules! T {
    [=] => { Token::dummy(TokenKind::Equal, "=") };
    [,] => { Token::dummy(TokenKind::Comma, ",") };
    [:] => { Token::dummy(TokenKind::Colon, ":") };

    [$l:expr; =] => { Token::new(TokenKind::Equal, $l.line, "=") };
    [$l:expr; ,] => { Token::new(TokenKind::Comma, $l.line, ",") };
    [$l:expr; :] => { Token::new(TokenKind::Colon, $l.line, ":") };
}
pub(crate) use T;

// all combinators have (lexer, tok)
mod combinator {
    use super::*;

    pub fn scoped_var(lexer: &mut Lexer, c: char) -> Token {
        lexer.read_char();

        match c {
            'g' => tok!(GlobalScope, lexer, "g:"),
            't' => tok!(TabScope, lexer, "t:"),
            'w' => tok!(WindowScope, lexer, "w:"),
            'b' => tok!(BufferScope, lexer, "b:"),
            's' => tok!(ScriptScope, lexer, "s:"),
            'l' => tok!(LocalScope, lexer, "l:"),
            _ => tok!(ParseError, lexer, c.to_string()),
        }
    }
}

mod tokenizer {
    use log::trace;

    use super::*;

    macro_rules! tok_error {
        () => {
            Some(State {
                tokens: vec![Token::dummy(TokenKind::ParseError, String::new())],
                next: panic_next,
            })
        };
    }

    fn shared(lexer: &mut Lexer) -> Option<Token> {
        let kind = match lexer.ch {
            '#' => return Some(Token::dummy(TokenKind::Comment, read_until_eol(lexer))),
            ' ' => return Some(Token::dummy(TokenKind::Ignore, read_while(lexer, is_whitespace))),

            EOF => TokenKind::EOF,
            EOL => TokenKind::NewLine,
            '-' => TokenKind::Minus,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            ':' => TokenKind::Colon,
            '\n' => TokenKind::NewLine,

            _ => return None,
        };

        Some(Token::new(kind, lexer.line, &lexer.ch.to_string()))
    }

    fn panic_next(_: &mut Lexer) -> Option<State> {
        panic!("PANIC TOKENIZER")
    }

    pub fn new_file(lexer: &mut Lexer) -> Option<State> {
        trace!("Newfile token: {:?}", lexer.ch);

        if !lexer.is_start() {
            return tok_error!();
        };

        if lexer.ch.is_whitespace() {
            read_while(lexer, is_whitespace);
            lexer.read_char();
        }

        if "im9script" != lexer.peek_until("im9script".len()) {
            return tok_error!();
        }

        Some(State {
            tokens: vec![tok!(Vim9Script, lexer, &read_until_eol(lexer))],
            next: new_statement,
        })
    }

    pub fn new_statement(lexer: &mut Lexer) -> Option<State> {
        trace!("Current token: {:?}", lexer.ch);

        let tok = match shared(lexer) {
            Some(t) => t,
            None => match lexer.ch {
                scope if lexer.peek_char() == ':' => combinator::scoped_var(lexer, scope),

                val => {
                    let text = read_while(lexer, |ch| !is_whitespace(ch));

                    match text.as_str() {
                        // TODO: Need to handle all the different commands here
                        "var" => tok!(CommandVar, lexer, text),
                        "for" => tok!(CommandFor, lexer, text),
                        "endfor" => tok!(CommandEndFor, lexer, text),
                        "def" => tok!(CommandDef, lexer, text),
                        "enddef" => tok!(CommandEndDef, lexer, text),
                        "echo" => tok!(CommandEcho, lexer, text),
                        "return" => tok!(CommandReturn, lexer, text),

                        // non commands
                        _ => {
                            if is_digit(val) {
                                tok!(Number, lexer, text)
                            } else if text.chars().all(|c| is_identifier(c)) {
                                tok!(Identifier, lexer, text)
                            } else {
                                panic!("Not yet parseable: {}, {:?}", val, lexer)
                            }
                        }
                    }
                }
            },
        };

        if tok.kind == TokenKind::EOF {
            return None;
        }

        Some(State {
            next: if tok.kind == TokenKind::NewLine || tok.kind == TokenKind::Ignore {
                new_statement
            } else {
                generic
            },
            tokens: vec![tok],
        })
    }

    pub fn generic(lexer: &mut Lexer) -> Option<State> {
        trace!("generic:: {:?}", lexer.ch);

        let tok = match shared(lexer) {
            Some(t) => t,
            None => match lexer.ch {
                EOF => tok!(EOF, lexer),
                '\n' => tok!(NewLine, lexer),

                '+' => tok!(Plus, lexer),
                '*' => tok!(Star, lexer),
                '=' => tok!(Equal, lexer),
                '[' => tok!(LeftBracket, lexer),
                ']' => tok!(RightBracket, lexer),
                ',' => T![lexer; ,],
                ':' => T![lexer; :],

                c if c.is_whitespace() => tok!(Ignore, lexer),

                val => {
                    if lexer.peek_char() == ':' {
                        combinator::scoped_var(lexer, val)
                    } else {
                        let text = read_while(lexer, |ch| is_identifier(ch));

                        if is_digit(val) {
                            tok!(Number, lexer, text)
                        } else if !text.is_empty() && text.chars().all(|c| is_identifier(c)) {
                            tok!(Identifier, lexer, text)
                        } else {
                            panic!("OH NO! {}, {:?}", val, lexer)
                        }
                    }
                }
            },
        };

        Some(State {
            next: if tok.kind == TokenKind::NewLine {
                new_statement
            } else {
                generic
            },

            tokens: vec![tok],
        })
    }
}

#[derive(Debug)]
pub struct LexerError {
    tok: Token,
}

type Result<T> = std::result::Result<T, LexerError>;

fn read_until_eol(l: &mut Lexer) -> String {
    read_while(l, |ch| ch != EOF && ch != EOL)
}

fn read_while(l: &mut Lexer, predicate: fn(ch: char) -> bool) -> String {
    let mut result = String::new();

    // TODO: Return an empty range?
    if !predicate(l.ch) {
        return result;
    }

    result.push(l.ch);
    while predicate(l.peek_char()) {
        l.read_char();
        result.push(l.ch)
    }

    result
}

pub fn tokenize_file(input: String) -> Result<Vec<Token>> {
    tokenize(input, tokenizer::new_file)
}

fn tokenize(input: String, state: fn(&mut Lexer) -> Option<State>) -> Result<Vec<Token>> {
    let mut lexer = Lexer::new(input.chars().collect());
    lexer.read_char();

    let mut state_fn = Some(State {
        tokens: vec![],
        next: state,
    });

    let mut count = 0;
    let mut result = Vec::new();
    'outer: while let Some(State { tokens, next }) = state_fn {
        count += 1;
        if count > 1000 {
            panic!("Infinite loop?")
        }

        for tok in tokens {
            if tok.kind == TokenKind::ParseError {
                return Err(LexerError { tok });
            }

            if tok.kind == TokenKind::EOF {
                break 'outer;
            }

            if tok.kind == TokenKind::Ignore {
                continue;
            }

            result.push(tok);
        }

        state_fn = next(&mut lexer);
        lexer.read_char();
    }

    Ok(result)
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;
    use TokenKind::*;

    use super::*;
    use crate::constants;

    macro_rules! v9 {
        () => {
            Token::dummy(Vim9Script, "vim9script")
        };

        ($l:expr) => {
            Token::dummy(Vim9Script, format!("vim9script{}", $l))
        };
    }

    macro_rules! test_tokens {
        ($name: ident, $syntax: literal, [$( $x:expr ),*]) => {
            #[test]
            fn $name() -> Result<()> {
                let mut text = "vim9script\n".to_string();
                text.push_str($syntax);

                let mut expected = Vec::new();
                expected.push(v9!());
                expected.push(Token::dummy(TokenKind::NewLine, "\n".to_string()));
                $(
                    expected.push($x);
                )*

                let parsed = tokenize(text, tokenizer::new_file)?;

                assert_eq!(expected, parsed);
                Ok(())
            }
        };
    }

    test_tokens!(can_parse_just_vim9script, "", []);

    test_tokens!(
        can_parse_a_simple_comment,
        "# My first comment",
        [Token::dummy(Comment, "# My first comment")]
    );

    // Numbers
    test_tokens!(parses_a_single_digit_number, "5", [Token::dummy(Number, "5")]);

    test_tokens!(
        parses_an_addition,
        "5432 + 342",
        [
            Token::dummy(Number, "5432"),
            Token::dummy(Plus, "+"),
            Token::dummy(Number, "342")
        ]
    );

    test_tokens!(parse_singular_var, "var", [Token::dummy(CommandVar, "var")]);

    test_tokens!(
        parse_singular_var_statement,
        "var foo = 5",
        [
            Token::dummy(CommandVar, "var"),
            Token::dummy(Identifier, "foo"),
            Token::dummy(Equal, "="),
            Token::dummy(Number, "5")
        ]
    );

    test_tokens!(
        parse_var_var_statement,
        "var var = 54 + 32",
        [
            Token::dummy(CommandVar, "var"),
            Token::dummy(Identifier, "var"),
            Token::dummy(Equal, "="),
            Token::dummy(Number, "54"),
            Token::dummy(Plus, "+"),
            Token::dummy(Number, "32")
        ]
    );

    test_tokens!(
        parse_simple_list,
        "var x = [1, 2, 3]",
        [
            Token::dummy(CommandVar, "var"),
            Token::dummy(Identifier, "x"),
            Token::dummy(Equal, "="),
            Token::dummy(LeftBracket, "["),
            Token::dummy(Number, "1"),
            Token::dummy(Comma, ","),
            Token::dummy(Number, "2"),
            Token::dummy(Comma, ","),
            Token::dummy(Number, "3"),
            Token::dummy(RightBracket, "]")
        ]
    );

    test_tokens!(
        parse_list_with_addition,
        "var x = [1, 2 + foo, -3]",
        [
            Token::dummy(CommandVar, "var"),
            Token::dummy(Identifier, "x"),
            Token::dummy(Equal, "="),
            Token::dummy(LeftBracket, "["),
            Token::dummy(Number, "1"),
            Token::dummy(Comma, ","),
            Token::dummy(Number, "2"),
            Token::dummy(Plus, "+"),
            Token::dummy(Identifier, "foo"),
            Token::dummy(Comma, ","),
            Token::dummy(Minus, "-"),
            Token::dummy(Number, "3"),
            Token::dummy(RightBracket, "]")
        ]
    );

    test_tokens!(
        parse_global_var,
        "g:hello = 5",
        [
            Token::dummy(GlobalScope, "g:"),
            Token::dummy(Identifier, "hello"),
            T![=],
            Token::dummy(Number, "5")
        ]
    );

    test_tokens!(
        parse_regular_var,
        "hello = 5",
        [Token::dummy(Identifier, "hello"), T![=], Token::dummy(Number, "5")]
    );

    #[test]
    fn lex_a_def_statement() -> Result<()> {
        let mut text = "vim9script\n".to_string();
        text.push_str("def MyFunc(): bool\nenddef");

        insta::assert_debug_snapshot!(tokenize(text, tokenizer::new_file)?);
        Ok(())
    }

    test_tokens!(
        test_parses_a_function_call,
        "var x = abs(1)",
        [
            Token::dummy(CommandVar, "var"),
            Token::dummy(Identifier, "x"),
            T![=],
            Token::dummy(Identifier, "abs"),
            Token::dummy(LeftParen, "("),
            Token::dummy(Number, "1"),
            Token::dummy(RightParen, ")")
        ]
    );

    #[test]
    fn test_parses_comment_with_space_after_vim9script() -> Result<()> {
        let parsed = tokenize("vim9script  \n# my first comment".into(), tokenizer::new_file)?;
        assert_eq!(
            parsed,
            vec![
                v9!("  "),
                Token::dummy(NewLine, "\n"),
                Token::dummy(Comment, "# my first comment")
            ]
        );

        Ok(())
    }

    #[test]
    fn test_fails_when_bad_vim9script() {
        assert!(tokenize("vim9scrt".into(), tokenizer::new_file).is_err());
    }

    #[test]
    fn test_parses_a_multiple_digit_number() -> Result<()> {
        assert_eq!(
            tokenize("5432".into(), tokenizer::generic)?,
            vec![Token::dummy(Number, "5432")]
        );

        Ok(())
    }

    #[test]
    fn peek_until() {
        let lex = Lexer::new("hello".chars().collect());
        assert_eq!(vec!['h', 'e', 'l'], lex.peek_until(3).chars().collect::<Vec<char>>())
    }

    #[test]
    fn test_can_parse_terrible_benchmark() {
        insta::assert_debug_snapshot!(tokenize(
            format!("vim9script\n{}", constants::TERRIBLE_BENCHMARK),
            tokenizer::new_file
        )
        .unwrap());
    }
}
