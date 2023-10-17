use std::iter::Peekable;
use std::str::Chars;
use std::{env, fs::File, io::Write, path::Path};

use anyhow::Result;

pub struct FuncLexer<'a> {
    chars: Peekable<Chars<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FuncToken {
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Comma,
    Identifier(String),
    Number(usize),
    String(String),
    IfDef(IfDef),
    Skip,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfDef {
    pub proc_if: Box<FuncToken>,
    pub proc_else: Box<FuncToken>,
}

impl<'a> FuncLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            chars: input.chars().peekable(),
        }
    }

    pub fn lex(&mut self) -> Vec<FuncToken> {
        let mut result = vec![];

        while let Some(ch) = self.chars.next() {
            let tok = match ch {
                '{' => self.process_left_brace(),
                '}' => self.process_right_brace(),
                '(' => self.process_left_paren(),
                ')' => self.process_right_paren(),
                ',' => self.process_comma(),
                '"' => self.process_quotation(),
                '#' => self.process_hash(),
                ch if ch.is_numeric() => self.process_number(Some(ch)),
                ch if ch.is_alphabetic() => self.process_alphabets(Some(ch)),
                ch if ch.is_whitespace() => self.process_whitespace(),
                ';' => self.process_semi_colon(),
                '/' if *self.chars.peek().unwrap() == '/' => self.process_double_forward_slash(),
                ch => todo!("unhandled: {:?}", ch),
            };

            if tok != FuncToken::Skip {
                result.push(tok);
            }
        }

        result
    }

    fn process_left_brace(&mut self) -> FuncToken {
        FuncToken::LeftBrace
    }

    fn process_right_brace(&mut self) -> FuncToken {
        FuncToken::RightBrace
    }

    fn process_left_paren(&mut self) -> FuncToken {
        FuncToken::LeftParen
    }

    fn process_right_paren(&mut self) -> FuncToken {
        FuncToken::RightParen
    }

    fn process_comma(&mut self) -> FuncToken {
        FuncToken::Comma
    }

    fn process_quotation(&mut self) -> FuncToken {
        let is_terminal = |ch: char| ch == '"';
        let value = self.read_until_iter_terminal(is_terminal);
        let value = String::from_iter(value);

        FuncToken::String(value)
    }

    fn process_number(&mut self, first_ch: Option<char>) -> FuncToken {
        let is_terminal = |ch: char| !ch.is_numeric();
        let mut value = self.read_until_peek_terminal(is_terminal);
        if let Some(ch) = first_ch {
            value.insert(0, ch);
        }
        let value = String::from_iter(value).parse().unwrap();

        FuncToken::Number(value)
    }

    fn process_alphabets(&mut self, first_ch: Option<char>) -> FuncToken {
        let is_terminal = |ch: char| !(ch.is_alphanumeric() || ch == '_');
        let mut value = self.read_until_peek_terminal(is_terminal);
        if let Some(ch) = first_ch {
            value.insert(0, ch);
        }
        let value = String::from_iter(value);

        FuncToken::Identifier(value)
    }

    fn process_whitespace(&mut self) -> FuncToken {
        self.skip_whitespaces();
        FuncToken::Skip
    }

    fn process_semi_colon(&mut self) -> FuncToken {
        FuncToken::Skip
    }

    fn process_double_forward_slash(&mut self) -> FuncToken {
        self.skip_line();
        FuncToken::Skip
    }

    fn process_hash(&mut self) -> FuncToken {
        // read #ifdef ...
        self.skip_line();
        self.skip_whitespaces();
        let proc_if = self.process_alphabets(None).into();
        self.skip_whitespaces();

        // read #else
        self.skip_line();

        self.skip_whitespaces();
        let proc_else = self.process_alphabets(None).into();
        self.skip_whitespaces();

        // read #endif
        self.skip_line();

        FuncToken::IfDef(IfDef { proc_if, proc_else })
    }

    fn skip_whitespaces(&mut self) {
        let is_terminal = |ch: char| !ch.is_whitespace();
        self.read_until_peek_terminal(is_terminal);
    }

    fn skip_line(&mut self) {
        let is_terminal = |ch: char| ch == '\n';
        self.read_until_iter_terminal(is_terminal);
    }

    fn read_until_peek_terminal(&mut self, is_peek_terminal: fn(char) -> bool) -> Vec<char> {
        let mut result = vec![];
        while let Some(ch) = self.chars.peek() {
            if is_peek_terminal(*ch) {
                break;
            }

            result.push(*ch);
            self.chars.next();
        }
        result
    }

    fn read_until_iter_terminal(&mut self, is_iter_terminal: fn(char) -> bool) -> Vec<char> {
        let mut result = vec![];
        for ch in self.chars.by_ref() {
            if is_iter_terminal(ch) {
                break;
            }

            result.push(ch);
        }
        result
    }
}

// {
//     char         *f_name;    // function name
//     char         f_min_argc; // minimal number of arguments
//     char         f_max_argc; // maximal number of arguments
//     char         f_argtype;  // for method: FEARG_ values
//     argcheck_T   *f_argcheck;    // list of functions to check argument types
//     type_T       *(*f_retfunc)(int argcount, type2_T *argtypes, type_T **decl_type); // return type function
//     void         (*f_func)(typval_T *args, typval_T *rvar); // implementation of function
// } funcentry_T;
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncInfo {
    pub name: String,
    pub min_args: usize,
    pub max_args: usize,
    pub method_arg: Option<usize>,
    pub arg_check: String,
    pub return_type: FuncReturnType,
    pub func_impl: String,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FuncReturnType {
    Any,
    Bool,
    Float,
    Number,
    NumberBool,
    String,
    Void,
    Dict(Box<FuncReturnType>),
    List(Box<FuncReturnType>),
    Func(Box<FuncReturnType>),
}

pub struct FuncParser {
    tokens: Vec<FuncToken>,
}

impl FuncParser {
    pub fn new(tokens: Vec<FuncToken>) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Vec<FuncInfo> {
        let mut info = vec![];

        let mut token_stream = self.tokens.clone().into_iter().peekable();
        assert!(
            token_stream.next().expect("first token") == FuncToken::LeftBrace,
            "opening brace"
        );

        macro_rules! assert_comma {
            () => {{
                assert_eq!(token_stream.next().unwrap(), FuncToken::Comma);
            }};
        }

        macro_rules! after_comma {
            () => {{
                assert_comma!();
                token_stream.next().unwrap()
            }};
        }

        loop {
            if token_stream.peek().expect("peeked") == &FuncToken::RightBrace {
                break;
            }

            assert_eq!(token_stream.next().expect("internal"), FuncToken::LeftBrace);

            let name = match token_stream.next().unwrap() {
                FuncToken::String(name) => name,
                _ => unreachable!("not valid name"),
            };

            let min_args = match after_comma!() {
                FuncToken::Number(number) => number,
                _ => unreachable!("not valid min_args"),
            };

            let max_args = match after_comma!() {
                FuncToken::Number(number) => number,
                _ => unreachable!("not valid max_args"),
            };

            let method_arg = match after_comma!() {
                FuncToken::Identifier(s) => match s.as_str() {
                    "FEARG_1" => Some(1),
                    "FEARG_2" => Some(2),
                    "FEARG_3" => Some(3),
                    "FEARG_4" => Some(4),
                    _ => unimplemented!("invalid identifier"),
                },
                FuncToken::Number(s) => match s {
                    0 => None,
                    _ => unreachable!("invalid number"),
                },
                _ => unreachable!("not valid method_arg"),
            };

            let arg_check = match after_comma!() {
                FuncToken::Identifier(identifier) => identifier,
                _ => unreachable!("not valid arg_check"),
            };

            info.push(FuncInfo {
                name,
                min_args,
                max_args,
                method_arg,
                arg_check,
                return_type: {
                    token_stream.next();

                    // TODO: Handle the rest of these that we care about.
                    // It doesn't have to be perfect, but it could still provide
                    // some useful information for us when doing type resolution.
                    let identifier = match token_stream.next().unwrap() {
                        FuncToken::Identifier(identifier) => identifier,
                        _ => unreachable!("not a valid return identifier"),
                    };

                    match identifier.as_str() {
                        "ret_any" => FuncReturnType::Any,
                        "ret_bool" => FuncReturnType::Bool,
                        "ret_number" => FuncReturnType::Number,
                        "ret_number_bool" => FuncReturnType::NumberBool,
                        "ret_string" => FuncReturnType::String,
                        "ret_void" => FuncReturnType::Void,
                        _ => FuncReturnType::Any,
                    }
                },
                func_impl: {
                    // Read the actual function name
                    match after_comma!() {
                        FuncToken::Identifier(s) => {
                            if token_stream.peek().unwrap() == &FuncToken::LeftParen {
                                token_stream.next();
                                token_stream.next();
                                token_stream.next();
                            }

                            s
                        }
                        FuncToken::IfDef(_) => "<conditional>".to_string(),
                        _ => unreachable!("invalid func_impl"),
                    }
                },
            });

            token_stream.next_if(|t| t == &FuncToken::Comma);

            assert_eq!(
                token_stream.next().expect("internal"),
                FuncToken::RightBrace,
                "{info:?}",
            );

            token_stream.next_if(|t| t == &FuncToken::Comma);
        }

        info
    }
}

fn main() -> Result<()> {
    println!("cargo:rerun-if-changed=data/global_functions.txt");

    let out_path = Path::new(&env::var_os("OUT_DIR").unwrap()).join("generated_functions.rs");
    let mut lib = File::create(&out_path)?;

    // Write our struct definitions out here:
    writeln!(
        &mut lib,
        r#"// This file is generated by build.rs
// Do not edit.

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FuncInfo {{
    pub name: &'static str,
    pub min_args: usize,
    pub max_args: usize,
    pub method_arg: Option<usize>,
    pub arg_check: &'static str,
    pub return_type: FuncReturnType,
    pub func_impl: &'static str,
}}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum FuncReturnType {{
    Any,
    Bool,
    Float,
    Number,
    NumberBool,
    String,
    Void,
    Dict(Box<FuncReturnType>),
    List(Box<FuncReturnType>),
    Func(Box<FuncReturnType>),
}}
"#
    )?;

    let input = include_str!("./data/global_functions.txt");
    let mut lexer = FuncLexer::new(input);
    let tokens = lexer.lex();
    let mut parser = FuncParser::new(tokens);
    let info = parser.parse();

    let mut map = phf_codegen::Map::new();
    for func_info in info {
        let FuncInfo {
            name,
            min_args,
            max_args,
            method_arg,
            arg_check,
            return_type,
            func_impl,
        } = func_info;

        map.entry(
            name.clone(),
            &format!(
                r#"FuncInfo {{
   name: "{name}",
   min_args: {min_args},
   max_args: {max_args},
   method_arg: {method_arg:?},
   arg_check: "{arg_check}",
   return_type: FuncReturnType::{return_type:?},
   func_impl: "{func_impl}",
}}"#
            ),
        );
    }

    write!(
        &mut lib,
        "static FUNC_INFO: phf::Map<&'static str, FuncInfo> = {}",
        map.build()
    )
    .unwrap();

    writeln!(&mut lib, ";").unwrap();

    write!(
        &mut lib,
        r#"
pub fn get_func_info(name: &str) -> Option<FuncInfo> {{
    FUNC_INFO.get(name).cloned()
}}
"#
    )
    .unwrap();

    Ok(())
}
