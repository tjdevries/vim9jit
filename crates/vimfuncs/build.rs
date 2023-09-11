use std::{env, fs::File, io::Write, path::Path};

use anyhow::Result;

pub struct FuncLexer {
    position: usize,
    chars: Vec<char>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfDef {
    pub proc_if: Box<FuncToken>,
    pub proc_else: Box<FuncToken>,
}

impl FuncToken {
    fn string(self) -> String {
        match self {
            FuncToken::String(s) => s,
            _ => unreachable!("{:?}", self),
        }
    }

    fn number(self) -> usize {
        match self {
            FuncToken::Number(s) => s,
            _ => unreachable!("{:?}", self),
        }
    }

    fn identifier(self) -> String {
        match self {
            FuncToken::Identifier(s) => s,
            _ => unreachable!("{:?}", self),
        }
    }
}

impl FuncLexer {
    pub fn new(input: &str) -> Self {
        Self {
            position: 0,
            chars: input.chars().collect(),
        }
    }

    pub fn lex(&mut self) -> Vec<FuncToken> {
        let mut result = vec![];

        loop {
            let ch = self.read_char();
            match ch {
                Some(ch) => {
                    let tok = match ch {
                        '{' => FuncToken::LeftBrace,
                        '}' => FuncToken::RightBrace,
                        '(' => FuncToken::LeftParen,
                        ')' => FuncToken::RightParen,
                        ',' => FuncToken::Comma,
                        '"' => self.read_string(),
                        '#' => self.read_ifdef(),
                        ch if ch.is_numeric() => self.read_number(),
                        ch if ch.is_alphabetic() => self.read_identifier(),
                        ch if ch.is_whitespace() => continue,
                        ';' => continue,
                        '/' => {
                            self.skip_line();
                            continue;
                        }
                        ch => todo!("unhandled: {:?}", ch),
                    };

                    result.push(tok);
                }
                None => break,
            }
        }

        result
    }

    fn read_char(&mut self) -> Option<char> {
        let ch = self.chars.get(self.position).cloned();
        self.position += 1;

        ch
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.get(self.position + 1).cloned()
    }

    fn read_string(&mut self) -> FuncToken {
        let position = self.position;
        while self.read_char() != Some('"') {}

        FuncToken::String(String::from_iter(&self.chars[position..self.position - 1]))
    }

    fn read_number(&mut self) -> FuncToken {
        let position = self.position;
        loop {
            let ch = self.ch();
            if !ch.is_numeric() || ch == ',' {
                break;
            } else {
                self.read_char();
            }
        }

        FuncToken::Number(
            self.chars[position - 1..self.position]
                .iter()
                .collect::<String>()
                .parse::<usize>()
                .unwrap(),
        )
    }

    fn read_identifier(&mut self) -> FuncToken {
        let position = self.position - 1;
        while let Some(ch) = self.peek_char() {
            self.read_char();
            if !(ch.is_alphanumeric() || ch == '_') {
                break;
            }
        }

        FuncToken::Identifier(String::from_iter(&self.chars[position..self.position]))
    }

    fn read_ifdef(&mut self) -> FuncToken {
        // read #ifdef ...
        self.skip_line();
        self.skip_whitespace();
        let proc_if = self.read_identifier().into();
        self.skip_whitespace();

        // read #else
        self.skip_line();

        self.skip_whitespace();
        let proc_else = self.read_identifier().into();
        self.skip_whitespace();

        // read #endif
        self.skip_line();

        FuncToken::IfDef(IfDef { proc_if, proc_else })
    }

    fn skip_whitespace(&mut self) {
        if !self.ch().is_whitespace() {
            return;
        }

        while let Some(ch) = self.read_char() {
            if !ch.is_whitespace() {
                break;
            }
        }
    }

    fn skip_line(&mut self) {
        while self.read_char() != Some('\n') {}
    }

    fn ch(&self) -> char {
        self.chars[self.position]
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

            info.push(FuncInfo {
                name: token_stream.next().unwrap().string(),
                min_args: after_comma!().number(),
                max_args: after_comma!().number(),
                method_arg: match after_comma!() {
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
                },
                arg_check: after_comma!().identifier(),
                return_type: {
                    token_stream.next();

                    // TODO: Handle the rest of these that we care about.
                    // It doesn't have to be perfect, but it could still provide
                    // some useful information for us when doing type resolution.
                    match token_stream.next().unwrap().identifier().as_str() {
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
