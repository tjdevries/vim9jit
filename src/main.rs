#![allow(dead_code)]

fn is_letter(ch: char) -> bool {
    'a' <= ch && ch <= 'z' || 'A' <= ch && ch <= 'Z' || ch == '_'
}

fn is_digit(ch: char) -> bool {
    '0' <= ch && ch <= '9'
}

fn is_alphanumeric(ch: char) -> bool {
    is_letter(ch) || is_digit(ch)
}

const EOF: char = '\0';
const EOL: char = '\n';

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    Vim9Script(Vec<char>),
    Comment(Vec<char>),

    Number(Vec<char>),

    // Operations
    Plus,
    Minus,
    Multiple,
    Divide,

    EOF,
    Ignore,

    ParseError,
}

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,         // Source code
    pub position: usize,      // Reading position
    pub read_position: usize, // Current moving reading position
    pub ch: char,             // Current read character
}

#[derive(Debug)]
pub struct ParseError {}

type Result<T> = std::result::Result<T, ParseError>;

fn read_vim9script(l: &mut Lexer) -> Token {
    let mut result = Vec::new();

    for e in "vim9script".chars() {
        if e != l.ch {
            return Token::ParseError;
        }

        result.push(l.ch);
        l.read_char();
    }

    result.extend(read_until_eol(l));

    Token::Vim9Script(result)
}

fn read_until_eol(l: &mut Lexer) -> Vec<char> {
    read_until(l, |l| l.ch != EOF && l.ch != EOL)
}

fn read_until(l: &mut Lexer, predicate: fn(l: &mut Lexer) -> bool) -> Vec<char> {
    let mut result = Vec::new();
    while predicate(l) {
        result.push(l.ch);
        l.read_char();
    }

    result
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
        if self.read_position >= self.input.len() {
            EOF
        } else {
            self.input[self.read_position]
        }
    }

    /// Match the read character and assign appropriate type
    pub fn next_token(&mut self) -> Result<Token> {
        let tok = match self.ch {
            // Check for top-level `vim9script` at the beginning of the file.
            'v' if self.is_start() => read_vim9script(self),

            '#' => Token::Comment(read_until_eol(self)),

            '+' => Token::Plus,
            ' ' => Token::Ignore,

            EOF => Token::EOF,

            val => {
                if is_digit(val) {
                    // This is not right
                    Token::Number(read_until(self, |l| is_digit(l.ch)))
                } else {
                    panic!("OH NO! {}, {:?}", val, self)
                }
            }
        };

        self.read_char();
        Ok(tok)
    }

    pub(crate) fn is_start(&self) -> bool {
        self.position == 0
    }
}

// TODO: This should probably return a result.
fn parse(input: String) -> Result<Vec<Token>> {
    let mut result = Vec::new();
    let mut l = Lexer::new(input.chars().collect());
    l.read_char();
    loop {
        let token = l.next_token()?;

        match token {
            Token::Ignore => continue,
            Token::ParseError => return Err(ParseError {}),
            Token::EOF => break,
            tok => result.push(tok),
        }
    }

    Ok(result)
}

fn main() {
    println!("Compiled Succesfully.");
}

#[cfg(test)]
mod test {
    use super::*;
    use Token::*;

    macro_rules! chars {
        ($s: expr) => {
            $s.chars().collect()
        };
    }

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

                let parsed = parse(text)?;

                let mut expected = Vec::new();
                expected.push(v9!());
                $(
                    expected.push($x);
                )*

                assert_eq!(parsed, expected);
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

    // var x = 5
    // [ Token::Var("var") Token::identifier("x"), Token::Equal, Token::Number("5") ]

    // var x = 5 + 6
    // [ ..., Token::Plus, Token::Number("5") ]

    // (let) -> (+) -> 5
    //           `---> 6
    //

    #[test]
    fn test_parses_comment_with_space_after_vim9script() -> Result<()> {
        let parsed = parse("vim9script  \n# my first comment".into())?;
        assert_eq!(
            parsed,
            vec![v9!("  "), Comment(chars!("# my first comment"))]
        );

        Ok(())
    }

    #[test]
    fn test_fails_when_bad_vim9script() {
        assert!(parse("vim9scrt".into()).is_err());
    }
    #[test]
    fn test_parses_a_multiple_digit_number() -> Result<()> {
        assert_eq!(parse("5432".into())?, vec![Number(chars!("5432"))]);

        Ok(())
    }
}
