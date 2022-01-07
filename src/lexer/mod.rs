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

#[derive(PartialEq, Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
}

impl Token {
    pub fn new<A: AsRef<str>>(kind: TokenKind, text: A) -> Self {
        Self {
            kind,
            text: text.as_ref().into(),
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
    TypeDeclaration,

    // Commands
    CommandVar,

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
}

impl Lexer {
    fn new(input: Vec<char>) -> Lexer {
        Lexer {
            input,
            position: 0,
            read_position: 0,
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
    ($kind:expr, $l:expr) => {
        Token::new($kind, $l.to_string())
    };
}

macro_rules! T {
    [=] => { Token::new(TokenKind::Equal, "=") };
    [,] => { Token::new(TokenKind::Comma, ",") };
}
pub(crate) use T;

// all combinators have (lexer, tok)
mod combinator {
    use super::*;

    pub fn scoped_var(lexer: &mut Lexer, c: char) -> Token {
        lexer.read_char();

        match c {
            'g' => Token::new(TokenKind::GlobalScope, "g:"),
            't' => Token::new(TokenKind::TabScope, "t:"),
            'w' => Token::new(TokenKind::WindowScope, "w:"),
            'b' => Token::new(TokenKind::BufferScope, "b:"),
            's' => Token::new(TokenKind::ScriptScope, "s:"),
            'l' => Token::new(TokenKind::LocalScope, "l:"),
            _ => Token::new(TokenKind::ParseError, c.to_string()),
        }
    }
}

mod tokenizer {
    use log::trace;

    use super::*;

    macro_rules! tok_error {
        () => {
            Some(State {
                tokens: vec![Token::new(TokenKind::ParseError, String::new())],
                next: panic_next,
            })
        };
    }

    fn shared(lexer: &mut Lexer) -> Option<Token> {
        let kind = match lexer.ch {
            '#' => return Some(Token::new(TokenKind::Comment, read_until_eol(lexer))),

            EOF => TokenKind::EOF,
            EOL => TokenKind::NewLine,
            ' ' => TokenKind::Ignore,
            '-' => TokenKind::Minus,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,

            _ => return None,
        };

        Some(Token::new(kind, lexer.ch.to_string()))
    }

    fn panic_next(_: &mut Lexer) -> Option<State> {
        panic!("PANIC TOKENIZER")
    }

    pub fn new_file(lexer: &mut Lexer) -> Option<State> {
        trace!("Newfile token: {:?}", lexer.ch);

        if !lexer.is_start() {
            return tok_error!();
        };

        if "im9script" != lexer.peek_until("im9script".len()) {
            return tok_error!();
        }

        Some(State {
            tokens: vec![Token {
                kind: TokenKind::Vim9Script,
                text: read_until_eol(lexer),
            }],
            next: new_statement,
        })
    }

    pub fn new_statement(lexer: &mut Lexer) -> Option<State> {
        trace!("Current token: {:?}", lexer.ch);

        let tok = match shared(lexer) {
            Some(t) => t,
            None => match lexer.ch {
                '\n' => Token {
                    kind: TokenKind::NewLine,
                    text: "\n".to_string(),
                },

                scope if lexer.peek_char() == ':' => combinator::scoped_var(lexer, scope),

                val => {
                    let text = read_while(lexer, |ch| !is_whitespace(ch));

                    match text.as_str() {
                        "var" => Token::new(TokenKind::CommandVar, text),
                        //
                        // TODO: Need to handle all the different commands here
                        //
                        _ => {
                            if is_digit(val) {
                                Token::new(TokenKind::Number, text)
                            } else if text.chars().all(|c| is_identifier(c)) {
                                Token::new(TokenKind::Identifier, text)
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
            next: if tok.kind == TokenKind::NewLine {
                new_statement
            } else if tok.kind == TokenKind::CommandVar {
                new_var
            } else {
                generic
            },
            tokens: vec![tok],
        })
    }

    pub fn new_var(lexer: &mut Lexer) -> Option<State> {
        trace!("new_var:: {:?}", lexer.ch);

        let mut tokens = Vec::new();

        // All vars MUST have a whitespace afterwards...
        // Not sure if this should be in lexer or not, but it makes life easier to think about this
        // way.
        tokens.push(Token::new(
            TokenKind::Ignore,
            read_while(lexer, |ch| is_whitespace(ch) && ch != EOF),
        ));
        lexer.read_char();

        // Now we can have an identifier
        let mut text = String::new();
        loop {
            let ch = lexer.ch;
            let next = lexer.peek_char();

            text.push(ch);
            if next == EOF {
                break;
            } else if next == '\n' || ch == '\n' {
                break;
            } else if next.is_whitespace() {
                tokens.push(Token::new(TokenKind::Identifier, text));
                break;
            } else if next == ':' {
                match lexer.peek_n(2) {
                    Some(ch) => {
                        if ch.is_whitespace() {
                            // TYPE DECLS
                            // panic!("TYPE DECLS");
                            tokens.push(Token::new(TokenKind::Identifier, text));
                            lexer.read_char();

                            // read ':'
                            lexer.read_char();

                            // read whitespace until type decl
                            read_while(lexer, |ch| ch.is_whitespace());
                            lexer.read_char();

                            // TODO: This could be more complicated here...
                            // I don't know where to put all of this stuff
                            tokens.push(Token::new(
                                TokenKind::TypeDeclaration,
                                read_while(lexer, |ch| ch != '='),
                            ));
                            break;
                        }
                    }
                    None => unreachable!("new_var problems."),
                }
            }

            lexer.read_char();
        }

        // TODO: Could be generic or new_statement
        Some(State { next: generic, tokens })
    }

    pub fn generic(lexer: &mut Lexer) -> Option<State> {
        trace!("generic:: {:?}", lexer.ch);

        let tok = match shared(lexer) {
            Some(t) => t,
            None => match lexer.ch {
                EOF => tok!(TokenKind::EOF, lexer.ch),
                '\n' => tok!(TokenKind::NewLine, lexer.ch),

                ' ' => tok!(TokenKind::Ignore, lexer.ch),
                '+' => tok!(TokenKind::Plus, lexer.ch),
                '*' => tok!(TokenKind::Star, lexer.ch),
                '=' => tok!(TokenKind::Equal, lexer.ch),
                '[' => tok!(TokenKind::LeftBracket, lexer.ch),
                ']' => tok!(TokenKind::RightBracket, lexer.ch),
                ',' => T![,],

                scope if lexer.peek_char() == ':' => combinator::scoped_var(lexer, scope),

                val => {
                    let text = read_while(lexer, |ch| is_identifier(ch));

                    if is_digit(val) {
                        Token::new(TokenKind::Number, text)
                    } else if text.chars().all(|c| is_identifier(c)) {
                        Token::new(TokenKind::Identifier, text)
                    } else {
                        panic!("OH NO! {}, {:?}", val, lexer)
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

    macro_rules! v9 {
        () => {
            Token::new(Vim9Script, "vim9script")
        };

        ($l:expr) => {
            Token {
                kind: Vim9Script,
                text: format!("vim9script{}", $l),
            }
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
                expected.push(Token { kind: TokenKind::NewLine, text: "\n".to_string(), });
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
        [Token::new(Comment, "# My first comment")]
    );

    // Numbers
    test_tokens!(parses_a_single_digit_number, "5", [Token::new(Number, "5")]);

    test_tokens!(
        parses_an_addition,
        "5432 + 342",
        [
            Token::new(Number, "5432"),
            Token::new(Plus, "+"),
            Token::new(Number, "342")
        ]
    );

    test_tokens!(parse_singular_var, "var", [Token::new(CommandVar, "var")]);

    test_tokens!(
        parse_singular_var_statement,
        "var foo = 5",
        [
            Token::new(CommandVar, "var"),
            Token::new(Identifier, "foo"),
            Token::new(Equal, "="),
            Token::new(Number, "5")
        ]
    );

    test_tokens!(
        parse_var_var_statement,
        "var var = 54 + 32",
        [
            Token::new(CommandVar, "var"),
            Token::new(Identifier, "var"),
            Token::new(Equal, "="),
            Token::new(Number, "54"),
            Token::new(Plus, "+"),
            Token::new(Number, "32")
        ]
    );

    test_tokens!(
        parse_simple_list,
        "var x = [1, 2, 3]",
        [
            Token::new(CommandVar, "var"),
            Token::new(Identifier, "x"),
            Token::new(Equal, "="),
            Token::new(LeftBracket, "["),
            Token::new(Number, "1"),
            Token::new(Comma, ","),
            Token::new(Number, "2"),
            Token::new(Comma, ","),
            Token::new(Number, "3"),
            Token::new(RightBracket, "]")
        ]
    );

    test_tokens!(
        parse_list_with_addition,
        "var x = [1, 2 + foo, -3]",
        [
            Token::new(CommandVar, "var"),
            Token::new(Identifier, "x"),
            Token::new(Equal, "="),
            Token::new(LeftBracket, "["),
            Token::new(Number, "1"),
            Token::new(Comma, ","),
            Token::new(Number, "2"),
            Token::new(Plus, "+"),
            Token::new(Identifier, "foo"),
            Token::new(Comma, ","),
            Token::new(Minus, "-"),
            Token::new(Number, "3"),
            Token::new(RightBracket, "]")
        ]
    );

    test_tokens!(
        parse_global_var,
        "g:hello = 5",
        [
            Token::new(GlobalScope, "g:"),
            Token::new(Identifier, "hello"),
            T![=],
            Token::new(Number, "5")
        ]
    );

    test_tokens!(
        parse_regular_var,
        "hello = 5",
        [Token::new(Identifier, "hello"), T![=], Token::new(Number, "5")]
    );

    test_tokens!(
        test_parses_a_function_call,
        "var x = abs(1)",
        [
            Token::new(CommandVar, "var"),
            Token::new(Identifier, "x"),
            T![=],
            Token::new(Identifier, "abs"),
            Token::new(LeftParen, "("),
            Token::new(Number, "1"),
            Token::new(RightParen, ")")
        ]
    );

    #[test]
    fn test_parses_comment_with_space_after_vim9script() -> Result<()> {
        let parsed = tokenize("vim9script  \n# my first comment".into(), tokenizer::new_file)?;
        assert_eq!(
            parsed,
            vec![
                v9!("  "),
                Token {
                    kind: NewLine,
                    text: "\n".into()
                },
                Token {
                    kind: Comment,
                    text: "# my first comment".into()
                }
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
            vec![Token {
                kind: Number,
                text: "5432".into()
            }]
        );

        Ok(())
    }

    #[test]
    fn peek_until() {
        let lex = Lexer::new("hello".chars().collect());
        assert_eq!(vec!['h', 'e', 'l'], lex.peek_until(3).chars().collect::<Vec<char>>())
    }
}
