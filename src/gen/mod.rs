use std::collections::HashMap;
use std::process::Command;
use std::process::Stdio;

use anyhow::anyhow;
use anyhow::Result;
use log::trace;
use log::warn;
// use pretty_assertions::assert_eq;
use rmpv::decode::read_value;
use rmpv::encode::write_value;
use rmpv::Value;

use crate::ast;
use crate::ast::Expression;
use crate::ast::Identifier;
use crate::ast::TypeDeclaration;
use crate::lexer::tokenize_file;
use crate::parser::parse;

#[derive(Debug)]
pub struct Variable {
    pub expr: Expression,
    pub decl: Option<TypeDeclaration>,
}

#[derive(Default, Debug)]
pub struct Scope {
    lookup: HashMap<Identifier, Variable>,
}

impl Scope {
    fn add_var(&mut self, identifier: Identifier, decl: Option<TypeDeclaration>, expr: Expression) {
        self.lookup.insert(identifier, Variable { decl, expr });
    }

    fn get_var(&self, identifier: &Identifier) -> Option<&Variable> {
        self.lookup.get(identifier)
    }
}

#[derive(Debug)]
pub struct GenDB {
    scopes: Vec<Scope>,
}

impl Default for GenDB {
    fn default() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }
}

impl GenDB {
    pub fn add_var(&mut self, identifier: Identifier, decl: Option<TypeDeclaration>, expr: Expression) {
        let scope = self.scopes.last_mut().expect("Should always have at least one scope");
        scope.add_var(identifier, decl, expr);
    }

    pub fn get_var(&self, identifier: &Identifier) -> Option<&Variable> {
        for scope in self.scopes.iter() {
            if let Some(expr) = scope.get_var(identifier) {
                return Some(expr);
            }
        }

        None
    }

    /// Checks whether certain items behave the same in vim9script and lua
    pub fn has_shared_behavior(&self, _decl: &Option<TypeDeclaration>, expr: &Expression) -> bool {
        // TODO: Actually use decl

        match expr {
            Expression::Empty => true,
            Expression::Number(_) => true,
            Expression::Identifier(identifier) => match self.get_var(identifier) {
                Some(variable) => self.has_shared_behavior(&variable.decl, &variable.expr),
                None => {
                    // If we don't know about this identifier, then we just default back to runtime checks.
                    warn!("unknown identifier: {:?}", identifier);
                    false
                }
            },
            Expression::VimVariable(_) => false,
            Expression::Call(_) => false,
            Expression::Prefix { .. } => false,
            Expression::Infix { .. } => false,
        }
    }
}

pub trait CodeGen
where
    Self: Sized,
{
    fn gen(&self, db: &mut GenDB) -> String;
}

pub fn generate(prog: ast::Program) -> String {
    trace!("{:?}", prog);

    let mut db = GenDB::default();
    prog.gen(&mut db)
}

pub fn to_lua(s: &str) -> String {
    let tokens = tokenize_file(format!("vim9script\n{}", s).into()).unwrap();
    let parsed = parse(tokens).unwrap();
    generate(parsed)
}

pub fn all_of_it(preamble: &str, result: &str) -> Result<rmpv::Value> {
    let lua = to_lua(preamble);
    println!("{}", &lua);

    eval(&lua, result)
}

pub fn eval_with_setup(setup: &str, s: &str, result: &str) -> Result<rmpv::Value> {
    let lua = to_lua(s);
    let lua = setup.to_string() + ";\n" + lua.as_str();

    eval(&lua, result)
}

pub fn eval(preamble: &str, result: &str) -> Result<rmpv::Value> {
    let contents = format!(
        r#"
            return (function()
                {}
                return {}
            end)()
        "#,
        preamble, result
    );

    // dbg!(&contents);

    // start a neovim job
    let mut child = Command::new("nvim")
        .args(["--embed"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let id = Value::Integer(0.into());
    let msg = Value::Array(
        vec![
            0.into(),
            id.clone(),
            "nvim_exec_lua".into(),
            Value::Array(
                vec![
                    // lua code to execute
                    contents.clone().into(),
                    // lua arguments to send, currently unused
                    Value::Array(vec![]),
                ]
                .into(),
            ),
        ]
        .into(),
    );

    let child_stdin = child.stdin.as_mut().unwrap();
    let child_stdout = child.stdout.as_mut().unwrap();
    write_value(child_stdin, &msg)?;

    let val = match read_value(child_stdout)? {
        Value::Array(val) => val,
        _ => unreachable!(),
    };
    let (_, response_id, err, val) = (&val[0], &val[1], &val[2], &val[3]);

    // Confirm that this is the response to the request that we sent.
    assert_eq!(&id, response_id, "Ids must be equal");

    // Confirm that we don't have any errors while executing lua
    match err {
        Value::Nil => {}
        err => {
            return Err(anyhow!("Error executing Lua:\n\n{}\n\n{}", contents, err));
        }
    }

    // Close stdin to finish and avoid indefinite blocking
    drop(child_stdin);

    // Wait til output has completed.
    let output = child.wait_with_output()?;
    assert!(output.status.success());

    // We good
    Ok(val.clone())
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::constants;

    #[test]
    fn test_can_eval_number() -> Result<()> {
        assert_eq!(eval("", "1")?, 1.into());
        assert_eq!(eval("", "25")?, 25.into());
        assert_eq!(eval("", "'hello world'")?, "hello world".into());
        assert_eq!(
            eval("", "vim.fn.add({1, 2, 3}, 4)")?,
            Value::Array(vec![1.into(), 2.into(), 3.into(), 4.into()].into())
        );

        Ok(())
    }

    #[test]
    fn test_can_do_whole_thing() -> Result<()> {
        assert_eq!(all_of_it("", "1 + 1")?, 2.into());
        Ok(())
    }

    #[test]
    fn test_can_return_val_of_simple_identifier() -> Result<()> {
        assert_eq!(all_of_it("var x = 1", "x")?, 1.into());
        assert_eq!(all_of_it("var x = 1", "x + 2")?, 3.into());
        assert_eq!(all_of_it("var x = 1 + 2", "x")?, 3.into());
        assert_eq!(all_of_it("var x = 1 * 2", "x")?, 2.into());

        assert_eq!(all_of_it("var x = 1\nvar y = 2\nvar z = x * y", "z")?, 2.into());

        Ok(())
    }

    #[test]
    fn test_can_call_a_function() -> Result<()> {
        assert_eq!(all_of_it("var x = abs(1)", "x")?, 1.into());
        assert_eq!(all_of_it("var x = abs(1 - 2)", "x")?, 1.into());
        assert_eq!(all_of_it("var x = abs(1 + 2)", "x")?, 3.into());

        assert_eq!(
            eval_with_setup("GLOBAL_VAL = 5", "var x = GLOBAL_VAL + 3", "x")?,
            8.into()
        );

        Ok(())
    }

    #[test]
    fn test_can_eval_terrible_benchmark() -> Result<()> {
        assert_eq!(
            all_of_it(constants::TERRIBLE_BENCHMARK, "sum")?,
            4499998500001u64.into()
        );
        Ok(())
    }
}
