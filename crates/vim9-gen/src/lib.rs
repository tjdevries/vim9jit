use lexer::new_lexer;
use parser;
use parser::new_parser;
use parser::ArrayLiteral;
use parser::AssignStatement;
use parser::Body;
use parser::CallExpression;
use parser::DefCommand;
use parser::EchoCommand;
use parser::ExCommand;
use parser::Expression;
use parser::GroupedExpression;
use parser::Identifier;
use parser::IfCommand;
use parser::InfixExpression;
use parser::Operator;
use parser::RawIdentifier;
use parser::ReturnCommand;
use parser::ScopedIdentifier;
use parser::StatementCommand;
use parser::VarCommand;
use parser::Vim9ScriptCommand;
use parser::VimBoolean;
use parser::VimNumber;
use parser::VimString;

mod test_harness;

pub trait Generate {
    fn gen(&self) -> String;
}

impl Generate for ExCommand {
    fn gen(&self) -> String {
        match self {
            ExCommand::Vim9Script(cmd) => cmd.gen(),
            ExCommand::Var(cmd) => cmd.gen(),
            ExCommand::Echo(cmd) => cmd.gen(),
            ExCommand::Statement(cmd) => cmd.gen(),
            ExCommand::Return(cmd) => cmd.gen(),
            ExCommand::Def(cmd) => cmd.gen(),
            ExCommand::If(cmd) => cmd.gen(),
            // ExCommand::Call(_) => todo!(),
            // ExCommand::Finish(_) => todo!(),
            // ExCommand::Skip => todo!(),
            // ExCommand::EndOfFile => todo!(),
            ExCommand::Comment(token) => format!("-- {}", token.text),
            ExCommand::NoOp(token) => format!("-- {:?}", token),
            _ => todo!("Have not yet handled: {:?}", self),
        }
    }
}

impl Generate for ReturnCommand {
    fn gen(&self) -> String {
        format!("return {}", self.expr.gen())
    }
}

impl Generate for DefCommand {
    fn gen(&self) -> String {
        // TODO: If this command follows certain patterns,
        // we will also need to define a vimscript function,
        // so that this function is available.
        //
        // this could be something just like:
        // function <NAME>(...)
        //   return luaeval('...', ...)
        // endfunction
        //
        // but we haven't done this part yet.
        // This is a "must-have" aspect of what we're doing.
        format!(
            r#"
local {} = function()
  {}
end"#,
            self.name.gen(),
            self.body.gen()
        )
    }
}

impl Generate for StatementCommand {
    fn gen(&self) -> String {
        match self {
            StatementCommand::Assign(assign) => assign.gen(),
        }
    }
}

impl Generate for AssignStatement {
    fn gen(&self) -> String {
        format!("{} = {}", self.left.gen(), self.right.gen())
    }
}

impl Generate for IfCommand {
    fn gen(&self) -> String {
        format!(
            r#"
if {} then
  {}
end"#,
            self.condition.gen(),
            self.body.gen()
        )
        .trim()
        .to_string()
    }
}

impl Generate for Body {
    fn gen(&self) -> String {
        self.commands.iter().map(|cmd| cmd.gen()).collect()
    }
}

impl Generate for EchoCommand {
    fn gen(&self) -> String {
        // TODO: Probably should add some function that
        // pretty prints these the way they would get printed in vim
        // or maybe just call vim.cmd [[echo <...>]] ?
        //  Not sure.
        //  Maybe have to expose something to get exactly the same
        //  results
        //
        // format!("vim.api.nvim_echo({}, false, {{}})", chunks)
        format!("print({})", self.expr.gen())
    }
}

impl Generate for Vim9ScriptCommand {
    fn gen(&self) -> String {
        // TODO: Actually connect this
        // format!("require('vim9script').new_script {{ noclear = {} }}", self.noclear)
        format!("-- vim9script")
    }
}

impl Generate for VarCommand {
    fn gen(&self) -> String {
        format!("local {} = {}", self.name.gen(), self.expr.gen())
    }
}

impl Generate for Identifier {
    fn gen(&self) -> String {
        match self {
            Identifier::Raw(raw) => raw.gen(),
            Identifier::Scope(scoped) => scoped.gen(),
        }
    }
}

impl Generate for ScopedIdentifier {
    fn gen(&self) -> String {
        format!(
            "{}.{}",
            match self.scope {
                parser::VimScope::Global => "vim.g",
                _ => todo!(),
            },
            self.accessor.gen()
        )
    }
}

impl Generate for RawIdentifier {
    fn gen(&self) -> String {
        self.name.clone()
    }
}

impl Generate for Expression {
    fn gen(&self) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.gen(),
            Expression::Number(num) => num.gen(),
            Expression::String(str) => str.gen(),
            Expression::Boolean(bool) => bool.gen(),
            Expression::Grouped(grouped) => grouped.gen(),
            Expression::Call(call) => call.gen(),
            Expression::Prefix(_) => todo!(),
            Expression::Infix(infix) => infix.gen(),
            Expression::Array(array) => array.gen(),
            Expression::Empty => "".to_string(),
        }
    }
}

impl Generate for ArrayLiteral {
    fn gen(&self) -> String {
        format!(
            "{{ {} }}",
            self.elements
                .iter()
                .map(|x| x.gen())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Generate for VimString {
    fn gen(&self) -> String {
        match self {
            VimString::SingleQuote(s) => format!("'{}'", s),
            VimString::DoubleQuote(s) => format!("\"{}\"", s),
        }
    }
}

impl Generate for CallExpression {
    fn gen(&self) -> String {
        let func = match &(*self.expr) {
            Expression::Identifier(ident) => match ident {
                Identifier::Raw(raw) => {
                    if raw.name.to_lowercase() == raw.name {
                        format!("vim.fn.{}", raw.name)
                    } else {
                        raw.name.clone()
                    }
                }
                Identifier::Scope(_) => todo!(),
            },
            // Expression::String(_) => todo!(),
            // Expression::Grouped(_) => todo!(),
            // Expression::Call(_) => todo!(),
            // Expression::Prefix(_) => todo!(),
            // Expression::Infix(_) => todo!(),
            _ => unimplemented!(),
        };

        dbg!(format!(
            "{}({})",
            func,
            self.args.iter().map(|e| e.gen()).collect::<Vec<String>>().join(", ")
        ))
    }
}

impl Generate for GroupedExpression {
    fn gen(&self) -> String {
        format!("({})", self.expr.gen())
    }
}

impl Generate for VimBoolean {
    fn gen(&self) -> String {
        format!("{}", self.value)
    }
}

impl Generate for VimNumber {
    fn gen(&self) -> String {
        self.value.clone()
    }
}

impl Generate for InfixExpression {
    fn gen(&self) -> String {
        format!("({} {} {})", self.left.gen(), self.operator.gen(), self.right.gen())
    }
}

impl Generate for Operator {
    fn gen(&self) -> String {
        match self {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Bang => "not",
            // _ => todo!("{:?}", self),
        }
        .to_string()
    }
}

pub fn eval(program: parser::Program) -> String {
    let mut output = String::new();
    for command in program.commands.iter() {
        output += &command.gen();
        output += "\n";
    }

    output
}

pub fn generate(contents: &str) -> String {
    let lexer = new_lexer(contents);
    let mut parser = new_parser(lexer);
    let program = parser.parse_program();

    eval(program)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::test_harness::exec_lua;

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.bind(|| {
                    insta::assert_snapshot!(generate(contents));
                });
            }
        };
    }

    snapshot!(test_expr, "../testdata/snapshots/expr.vim");
    snapshot!(test_if, "../testdata/snapshots/if.vim");
    snapshot!(test_assign, "../testdata/snapshots/assign.vim");
    snapshot!(test_call, "../testdata/snapshots/call.vim");

    #[test]
    fn test_simple_def() {
        let contents = r#"
vim9script

def MyCoolFunc(): number
  return 5
enddef

var x = MyCoolFunc() + 1
"#;

        let generated = generate(contents);
        let eval = exec_lua(&generated, "x").unwrap();
        assert_eq!(eval, 6.into());
    }

    #[test]
    fn test_builtin_func() {
        let contents = r#"
vim9script

var x = len("hello")
"#;

        let generated = generate(contents);
        let eval = exec_lua(&generated, "x").unwrap();
        assert_eq!(eval, 5.into());
    }

    #[test]
    fn test_augroup_1() {
        let contents = r#"
vim9script

augroup matchparen
  # Replace all matchparen autocommands
  autocmd! CursorMoved,CursorMovedI,WinEnter * {
      echo "Block"
    }

  autocmd WinLeave * echo "Command"
augroup END

var x = len(nvim_get_autcomds({group: "matchparen"}))
"#;

        let generated = generate(contents);
        let eval = exec_lua(&generated, "x").unwrap();
        assert_eq!(eval, 2.into());
    }
}
