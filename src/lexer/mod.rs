macro_rules! chars {
    ($s: expr) => {
        $s.chars().collect::<Vec<char>>()
    };
}

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
pub enum Token {
    StartOfFile,

    Vim9Script(Vec<char>),
    Comment(Vec<char>),

    Number(Vec<char>),
    Identifier(Vec<char>),

    // Commands
    CommandVar,

    // Whitespace
    NewLine,

    // Operations
    Plus,
    Minus,
    Multiply,
    Divide,

    Equal,

    LeftBracket,
    RightBracket,

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
    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = EOF;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position = self.read_position + 1;
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

// struct StateFn {
//     // f: Box<dyn FnMut(&mut Lexer) -> Option<State>>,
//     next: fn(&mut Lexer) -> Option<State>,
// }

mod tokenizer {
    use super::*;

    macro_rules! tok_error {
        () => {
            Some(State {
                tokens: vec![Token::ParseError],
                next: panic,
            })
        };
    }

    fn shared(lexer: &mut Lexer) -> Option<Token> {
        let tok = match lexer.ch {
            EOF => Token::EOF,
            EOL => Token::NewLine,
            '#' => Token::Comment(read_until_eol(lexer)),

            ' ' => Token::Ignore,

            _ => return None,
        };

        Some(tok)
    }

    fn panic(_: &mut Lexer) -> Option<State> {
        panic!("PANIC TOKENIZER")
    }

    pub fn new_file(lexer: &mut Lexer) -> Option<State> {
        println!("Newfile token: {:?}", lexer.ch);

        let mut result = Vec::new();
        if !lexer.is_start() {
            return tok_error!();
        };

        if "im9script" != lexer.peek_until("im9script".len()) {
            return tok_error!();
        }

        result.extend(read_until_eol(lexer));

        Some(State {
            tokens: vec![Token::Vim9Script(result)],
            next: new_statement,
        })
    }

    pub fn new_statement(lexer: &mut Lexer) -> Option<State> {
        // println!("Current token: {:?}", lexer.ch);

        let tok = match shared(lexer) {
            Some(t) => t,
            None => match lexer.ch {
                '\n' => Token::NewLine,

                val => {
                    let word = read_while(lexer, |ch| !is_whitespace(ch));

                    if word == chars!("var") {
                        Token::CommandVar
                    } else if is_digit(val) {
                        Token::Number(word)
                    } else {
                        panic!("OH NO! {}, {:?}", val, lexer)
                    }
                }
            },
        };

        if tok == Token::EOF {
            return None;
        }

        Some(State {
            next: if tok == Token::NewLine { new_statement } else { generic },
            tokens: vec![tok],
        })
    }

    pub fn generic(lexer: &mut Lexer) -> Option<State> {
        // println!("Generic token: {:?}", lexer.ch);

        let tok = match shared(lexer) {
            Some(t) => t,
            None => match lexer.ch {
                EOF => Token::EOF,
                '\n' => Token::NewLine,

                ' ' => Token::Ignore,
                '+' => Token::Plus,
                '*' => Token::Multiply,
                '=' => Token::Equal,
                '[' => Token::LeftBracket,
                ']' => Token::RightBracket,
                ',' => Token::Comma,

                val => {
                    let word = read_while(lexer, |ch| is_identifier(ch));

                    if is_digit(val) {
                        Token::Number(word)
                    } else if word.iter().all(|c| is_identifier(*c)) {
                        Token::Identifier(word)
                    } else {
                        panic!("OH NO! {}, {:?}", val, lexer)
                    }
                }
            },
        };

        Some(State {
            next: if tok == Token::NewLine { new_statement } else { generic },

            tokens: vec![tok],
        })
    }
}

#[derive(Debug)]
pub struct LexerError {
    tok: Token,
}

type Result<T> = std::result::Result<T, LexerError>;

fn read_until_eol(l: &mut Lexer) -> Vec<char> {
    read_while(l, |ch| ch != EOF && ch != EOL)
}

fn read_while(l: &mut Lexer, predicate: fn(ch: char) -> bool) -> Vec<char> {
    let mut result = Vec::new();
    if predicate(l.ch) {
        result.push(l.ch)
    }

    while predicate(l.peek_char()) {
        l.read_char();
        result.push(l.ch);
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
            if tok == Token::ParseError {
                return Err(LexerError { tok });
            }

            if tok == Token::EOF {
                break 'outer;
            }

            if tok == Token::Ignore {
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
    use Token::*;

    use super::*;

    macro_rules! v9 {
        () => {
            Vim9Script(vec!['v', 'i', 'm', '9', 's', 'c', 'r', 'i', 'p', 't'])
        };

        ($l: expr) => {
            Vim9Script({
                let mut v: Vec<char> = vec!['v', 'i', 'm', '9', 's', 'c', 'r', 'i', 'p', 't'];
                v.extend::<Vec<char>>($l.chars().collect());
                v
            })
        };
    }

    macro_rules! test_tokens {
        ($name: ident, $syntax: literal, [$( $x:expr ),*]) => {
            #[test]
            fn $name() -> Result<()> {
                let mut text = "vim9script\n".to_owned();
                text.push_str($syntax);

                let mut expected = Vec::new();
                expected.push(v9!());
                expected.push(Token::NewLine);
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
        [Comment(chars!("# My first comment"))]
    );

    // Numbers
    test_tokens!(parses_a_single_digit_number, "5", [Number(chars!("5"))]);
    test_tokens!(
        parses_an_addition,
        "5432 + 342",
        [Number(chars!("5432")), Plus, Number(chars!("342"))]
    );

    test_tokens!(parse_singular_var, "var", [CommandVar]);

    test_tokens!(
        parse_singular_var_statement,
        "var foo = 5",
        [CommandVar, Identifier(chars!("foo")), Equal, Number(chars!("5"))]
    );

    test_tokens!(
        parse_var_var_statement,
        "var var = 54 + 32",
        [
            CommandVar,
            Identifier(chars!("var")),
            Equal,
            Number(chars!("54")),
            Plus,
            Number(chars!("32"))
        ]
    );

    test_tokens!(
        parse_simple_list,
        "var x = [1, 2, 3]",
        [
            CommandVar,
            Identifier(chars!("x")),
            Equal,
            LeftBracket,
            Number(chars!("1")),
            Comma,
            Number(chars!("2")),
            Comma,
            Number(chars!("3")),
            RightBracket
        ]
    );

    test_tokens!(
        parse_list_with_addition,
        "var x = [1, 2 + foo, 3]",
        [
            CommandVar,
            Identifier(chars!("x")),
            Equal,
            LeftBracket,
            Number(chars!("1")),
            Comma,
            Number(chars!("2")),
            Plus,
            Identifier(chars!("foo")),
            Comma,
            Number(chars!("3")),
            RightBracket
        ]
    );

    #[test]
    fn test_parses_comment_with_space_after_vim9script() -> Result<()> {
        let parsed = tokenize("vim9script  \n# my first comment".into(), tokenizer::new_file)?;
        assert_eq!(parsed, vec![v9!("  "), NewLine, Comment(chars!("# my first comment"))]);

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
            vec![Number(chars!("5432"))]
        );

        Ok(())
    }

    #[test]
    fn peek_until() {
        let lex = Lexer::new("hello".chars().collect());
        assert_eq!(vec!['h', 'e', 'l'], lex.peek_until(3).chars().collect::<Vec<char>>())
    }
}
