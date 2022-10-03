use lexer::new_lexer;
use parser;
use parser::new_parser;
use parser::ArrayLiteral;
use parser::AssignStatement;
use parser::AugroupCommand;
use parser::AutocmdCommand;
use parser::Body;
use parser::CallCommand;
use parser::CallExpression;
use parser::DeclCommand;
use parser::DefCommand;
use parser::DictAccess;
use parser::DictLiteral;
use parser::EchoCommand;
use parser::ElseCommand;
use parser::ElseIfCommand;
use parser::ExCommand;
use parser::Expandable;
use parser::Expression;
use parser::GroupedExpression;
use parser::Heredoc;
use parser::Identifier;
use parser::IfCommand;
use parser::IndexExpression;
use parser::IndexType;
use parser::InfixExpression;
use parser::InnerType;
use parser::Lambda;
use parser::Literal;
use parser::MethodCall;
use parser::PrefixExpression;
use parser::RawIdentifier;
use parser::Register;
use parser::ReturnCommand;
use parser::ScopedIdentifier;
use parser::SharedCommand;
use parser::Signature;
use parser::StatementCommand;
use parser::Type;
use parser::UserCommand;
use parser::VarCommand;
use parser::Vim9ScriptCommand;
use parser::VimBoolean;
use parser::VimKey;
use parser::VimNumber;
use parser::VimOption;
use parser::VimScope;
use parser::VimString;

pub mod call_expr;
mod test_harness;

pub struct State {
    pub augroup: Option<Literal>,
    pub is_test: bool,

    pub command_depth: i32,
    pub method_depth: i32,

    // TODO: We could modify the state as we are generating code.
    //  As we generate the code and notice certain identifiers are certain
    //  types, we can use that to do *some* optimizations
    pub scopes: Vec<VimscriptScope>,
}

pub struct VimscriptScope {}

pub trait Generate {
    fn gen(&self, state: &mut State) -> String;
}

impl Generate for ExCommand {
    fn gen(&self, state: &mut State) -> String {
        match self {
            ExCommand::Vim9Script(cmd) => cmd.gen(state),
            ExCommand::Var(cmd) => cmd.gen(state),
            ExCommand::Echo(cmd) => cmd.gen(state),
            ExCommand::Statement(cmd) => cmd.gen(state),
            ExCommand::Return(cmd) => cmd.gen(state),
            ExCommand::Def(cmd) => cmd.gen(state),
            ExCommand::If(cmd) => cmd.gen(state),
            ExCommand::Augroup(cmd) => cmd.gen(state),
            ExCommand::Autocmd(cmd) => cmd.gen(state),
            ExCommand::Call(cmd) => cmd.gen(state),
            ExCommand::Decl(cmd) => cmd.gen(state),
            ExCommand::Comment(token) => format!("\n-- {}", token.text),
            ExCommand::NoOp(token) => format!("\n-- {:?}", token),
            ExCommand::Heredoc(heredoc) => heredoc.gen(state),
            ExCommand::UserCommand(usercmd) => usercmd.gen(state),
            ExCommand::Eval(eval) => format!("{};", eval.expr.gen(state)),
            ExCommand::SharedCommand(shared) => shared.gen(state),
            _ => todo!("Have not yet handled: {:?}", self),
        }
    }
}

impl Generate for SharedCommand {
    fn gen(&self, _: &mut State) -> String {
        format!("vim.cmd [===[{}]===]", self.contents)
    }
}

fn make_user_command_arg(state: &State) -> String {
    format!("__vim9_arg_{}", state.command_depth)
}

impl Generate for UserCommand {
    fn gen(&self, state: &mut State) -> String {
        // TODO:
        // TODO: have not yet handled all the things here
        state.command_depth += 1;

        let result = format!(
            r#"
            vim.api.nvim_create_user_command(
                "{}",
                function({})
                    {}
                end,
                {{
                     nargs = '{}',
                     bang = {},
                }}
            )"#,
            self.name,
            make_user_command_arg(state),
            self.command.gen(state),
            self.command_nargs.clone().unwrap_or("0".to_string()),
            self.command_bang,
        );

        state.command_depth -= 1;

        result
    }
}

impl Generate for DeclCommand {
    fn gen(&self, state: &mut State) -> String {
        // TODO: default value should not be nil for everything here
        // i think?
        format!("local {} = {}", self.name.gen(state), "nil")
    }
}

impl Generate for Heredoc {
    fn gen(&self, state: &mut State) -> String {
        // this works for non-trim and non-eval heredocs perfect
        let inner = self
            .contents
            .iter()
            .map(|line| format!("[==[{}]==]", line))
            .collect::<Vec<String>>()
            .join(", ");

        let mut list = format!("{{ {} }}", inner);
        if self.trim {
            list = format!("require('vim9script').heredoc.trim({})", list)
        }

        format!("local {} = {}", self.name.gen(state), list)
    }
}

impl Generate for CallCommand {
    fn gen(&self, state: &mut State) -> String {
        let call: CallExpression = self.into();
        format!("{};", call.gen(state))
    }
}

impl Generate for AugroupCommand {
    fn gen(&self, state: &mut State) -> String {
        state.augroup = Some(self.augroup_name.clone());

        let group = self.augroup_name.token.text.clone();
        let result = format!(
            r#"
    vim.api.nvim_create_augroup("{}", {{ clear = false }})

    {}
"#,
            group,
            self.body.gen(state)
        );

        state.augroup = None;

        result
    }
}

impl Generate for AutocmdCommand {
    fn gen(&self, state: &mut State) -> String {
        let group = match &state.augroup {
            Some(group) => format!("group = '{}',", group.token.text),
            None => "".to_string(),
        };

        let event_list = self
            .events
            .iter()
            .map(|e| format!("'{}'", e.token.text))
            .collect::<Vec<String>>()
            .join(", ");

        let callback = match &self.block {
            parser::AutocmdBlock::Command(cmd) => {
                format!("function()\n{}\nend", cmd.gen(state))
            }
            parser::AutocmdBlock::Block(block) => {
                format!("function()\n{}\nend", block.body.gen(state))
            }
        };

        format!(
            r#"
vim.api.nvim_create_autocmd({{ {} }}, {{
    {}
    callback = {},
}})
"#,
            event_list, group, callback
        )
    }
}

impl Generate for ReturnCommand {
    fn gen(&self, state: &mut State) -> String {
        match &self.expr {
            Some(expr) => format!("return {}", expr.gen(state)),
            None => "return".to_string(),
        }
    }
}

impl Generate for DefCommand {
    fn gen(&self, state: &mut State) -> String {
        let name = self.name.gen(state);

        if state.is_test && name.starts_with("Test") {
            format!(
                r#"
                    it("{}", function()
                       -- Set errors to empty
                       vim.v.errors = {{}}

                       -- Actual test
                       {}

                       -- Assert that errors is still empty
                       assert.are.same({{}}, vim.v.errors)
                    end)
                "#,
                name,
                self.body.gen(state)
            )
        } else {
            let (signature, default_statements) =
                gen_signature(state, &self.args);

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
                    local {} = function({})
                        {}
                        {}
                    end
                "#,
                name,
                signature,
                default_statements,
                self.body.gen(state)
            )
        }
    }
}

fn gen_signature(state: &mut State, args: &Signature) -> (String, String) {
    (
        args.params
            .iter()
            .map(|p| p.name.gen(state))
            .collect::<Vec<String>>()
            .join(", "),
        args.params
            .iter()
            .filter(|p| p.default_val.is_some())
            .map(|p| {
                let name = p.name.gen(state);
                let default_val = p.default_val.clone().unwrap();
                let default_val = default_val.gen(state);

                format!("{name} = vim.F.if_nil({name}, {default_val}, {name})")
            })
            .collect::<Vec<String>>()
            .join("\n"),
    )
}

impl Generate for StatementCommand {
    fn gen(&self, state: &mut State) -> String {
        match self {
            StatementCommand::Assign(assign) => assign.gen(state),
        }
    }
}

impl Generate for AssignStatement {
    fn gen(&self, state: &mut State) -> String {
        format!("{} = {}", self.left.gen(state), self.right.gen(state))
    }
}

impl Generate for IfCommand {
    fn gen(&self, state: &mut State) -> String {
        let else_result = match &self.else_command {
            Some(e) => e.gen(state),
            None => "".to_string(),
        };

        format!(
            r#"
if {} then
  {}
{}
{}
end"#,
            self.condition.gen(state),
            self.body.gen(state),
            self.elseifs
                .iter()
                .map(|e| e.gen(state))
                .collect::<Vec<String>>()
                .join("\n"),
            else_result,
        )
        .trim()
        .to_string()
    }
}

impl Generate for ElseIfCommand {
    fn gen(&self, state: &mut State) -> String {
        format!(
            r#"
        elseif {} then
            {}
        "#,
            self.condition.gen(state),
            self.body.gen(state)
        )
    }
}

impl Generate for ElseCommand {
    fn gen(&self, state: &mut State) -> String {
        format!("else\n{}\n", self.body.gen(state))
    }
}

impl Generate for Body {
    fn gen(&self, state: &mut State) -> String {
        self.commands
            .iter()
            .map(|cmd| cmd.gen(state))
            .collect::<Vec<String>>()
            .join("\n")
    }
}

impl Generate for EchoCommand {
    fn gen(&self, state: &mut State) -> String {
        // TODO: Probably should add some function that
        // pretty prints these the way they would get printed in vim
        // or maybe just call vim.cmd [[echo <...>]] ?
        //  Not sure.
        //  Maybe have to expose something to get exactly the same
        //  results
        //
        // format!("vim.api.nvim_echo({}, false, {{}})", chunks)
        format!("print({})", self.expr.gen(state))
    }
}

impl Generate for Vim9ScriptCommand {
    fn gen(&self, _: &mut State) -> String {
        // TODO: Actually connect this
        // format!("require('vim9script').new_script {{ noclear = {} }}", self.noclear)
        format!("-- vim9script")
    }
}

impl Generate for VarCommand {
    fn gen(&self, state: &mut State) -> String {
        match self.ty {
            Some(Type {
                inner: InnerType::Bool,
                ..
            }) => format!(
                "local {} = require('vim9script').convert.decl_bool({})",
                self.name.gen(state),
                self.expr.gen(state)
            ),
            _ => format!(
                "local {} = {}",
                self.name.gen(state),
                self.expr.gen(state)
            ),
        }
    }
}

impl Generate for Identifier {
    fn gen(&self, state: &mut State) -> String {
        match self {
            Identifier::Raw(raw) => raw.gen(state),
            Identifier::Scope(scoped) => scoped.gen(state),
            Identifier::Unpacked(_) => todo!(),
        }
    }
}

impl Generate for ScopedIdentifier {
    fn gen(&self, state: &mut State) -> String {
        let scope = match self.scope {
            VimScope::Global => "vim.g",
            VimScope::VimVar => "vim.v",
            _ => todo!(),
        };

        format!("{}['{}']", scope, self.accessor.gen(state))
    }
}

impl Generate for RawIdentifier {
    fn gen(&self, _: &mut State) -> String {
        self.name.clone()
    }
}

impl Generate for Expression {
    fn gen(&self, state: &mut State) -> String {
        match self {
            Expression::Identifier(identifier) => identifier.gen(state),
            Expression::Number(num) => num.gen(state),
            Expression::String(str) => str.gen(state),
            Expression::Boolean(bool) => bool.gen(state),
            Expression::Grouped(grouped) => grouped.gen(state),
            Expression::Call(call) => call.gen(state),
            Expression::Prefix(prefix) => prefix.gen(state),
            Expression::Infix(infix) => infix.gen(state),
            Expression::Array(array) => array.gen(state),
            Expression::Dict(dict) => dict.gen(state),
            Expression::VimOption(opt) => opt.gen(state),
            Expression::Empty => "".to_string(),
            Expression::Index(index) => index.gen(state),
            Expression::DictAccess(access) => access.gen(state),
            Expression::Register(reg) => reg.gen(state),
            Expression::Lambda(lambda) => lambda.gen(state),
            Expression::Expandable(expandable) => expandable.gen(state),
            Expression::MethodCall(method) => method.gen(state),

            // Some expressions can only be triggered from containing expressions
            Expression::Slice(_) => unreachable!("cannot gen slice by itself"),
        }
    }
}

impl Generate for MethodCall {
    fn gen(&self, state: &mut State) -> String {
        call_expr::generate_method(self, state)
    }
}

impl Generate for DictAccess {
    fn gen(&self, state: &mut State) -> String {
        format!("{}['{}']", self.container.gen(state), self.index.gen(state))
    }
}

impl Generate for Expandable {
    fn gen(&self, state: &mut State) -> String {
        match &self.ident {
            Identifier::Raw(RawIdentifier { name }) if name == "q-args" => {
                format!("{}.{}", make_user_command_arg(state), "args")
            }
            Identifier::Raw(RawIdentifier { name }) if name == "q-bang" => {
                format!(
                    "({}.{} and '!' or '')",
                    make_user_command_arg(state),
                    "bang"
                )
            }
            _ => format!("vim.fn.expand('<{}>')", self.ident.gen(state)),
        }
    }
}

impl Generate for Lambda {
    fn gen(&self, state: &mut State) -> String {
        let (signature, default_statements) = gen_signature(state, &self.args);
        format!(
            r#"(function({})
                {}
                {}
                end)"#,
            signature,
            default_statements,
            self.body.gen(state)
        )
    }
}

impl Generate for Register {
    fn gen(&self, _: &mut State) -> String {
        format!("vim.fn.getreg({:?})", self.register)
    }
}

impl Generate for PrefixExpression {
    fn gen(&self, state: &mut State) -> String {
        format!(
            "require('vim9script').prefix['{:?}']({})",
            self.operator,
            self.right.gen(state)
        )
    }
}

impl Generate for IndexExpression {
    fn gen(&self, state: &mut State) -> String {
        // println!("self.index = {:#?}", self.index);

        match self.index.as_ref() {
            IndexType::Item(item) => {
                format!(
                    "require('vim9script').index({}, {})",
                    self.container.gen(state),
                    item.gen(state)
                )
            }
            IndexType::Slice(slice) => {
                format!(
                    "require('vim9script').slice({}, {}, {})",
                    self.container.gen(state),
                    slice
                        .start
                        .as_ref()
                        .map_or("nil".to_string(), |item| item.gen(state)),
                    slice
                        .finish
                        .as_ref()
                        .map_or("nil".to_string(), |item| item.gen(state)),
                )
            }
        }
    }
}

impl Generate for VimOption {
    fn gen(&self, state: &mut State) -> String {
        // TODO: not sure if i need to do something smarter than this
        format!(
            "vim.api.nvim_get_option_value('{}', {{}})",
            self.option.gen(state)
        )
    }
}

impl Generate for Literal {
    fn gen(&self, _: &mut State) -> String {
        self.token.text.clone()
    }
}

impl Generate for VimKey {
    fn gen(&self, _: &mut State) -> String {
        match self {
            VimKey::Literal(literal) => literal.token.text.clone(),
        }
    }
}

impl Generate for DictLiteral {
    fn gen(&self, state: &mut State) -> String {
        format!(
            "{{ {} }}",
            self.elements
                .iter()
                .map(|x| format!(
                    "{} = {}",
                    x.key.gen(state),
                    x.value.gen(state)
                ))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Generate for ArrayLiteral {
    fn gen(&self, state: &mut State) -> String {
        format!(
            "{{ {} }}",
            self.elements
                .iter()
                .map(|x| x.gen(state))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Generate for VimString {
    fn gen(&self, _: &mut State) -> String {
        match self {
            VimString::SingleQuote(s) => format!("'{}'", s),
            VimString::DoubleQuote(s) => format!("\"{}\"", s),
        }
    }
}

impl Generate for CallExpression {
    fn gen(&self, state: &mut State) -> String {
        call_expr::generate(self, state)
    }
}

impl Generate for GroupedExpression {
    fn gen(&self, state: &mut State) -> String {
        format!("({})", self.expr.gen(state))
    }
}

impl Generate for VimBoolean {
    fn gen(&self, _: &mut State) -> String {
        format!("{}", self.value)
    }
}

impl Generate for VimNumber {
    fn gen(&self, _: &mut State) -> String {
        self.value.clone()
    }
}

impl Generate for InfixExpression {
    fn gen(&self, state: &mut State) -> String {
        format!(
            "require('vim9script').ops['{:?}']({}, {})",
            self.operator,
            self.left.gen(state),
            self.right.gen(state)
        )

        // match self.operator {
        //     Operator::And => gen_operation(
        //         "AND",
        //         self.left.gen(state),
        //         self.right.gen(state),
        //     ),
        //     Operator::Or => {
        //         gen_operation("OR", self.left.gen(state), self.right.gen(state))
        //     }
        //     // Operator::Plus => todo!(),
        //     // Operator::Minus => todo!(),
        //     // Operator::Bang => todo!(),
        //     // Operator::Modulo => todo!(),
        //     // Operator::Or => todo!(),
        //     // Operator::EqualTo => todo!(),
        //     // Operator::LessThan => todo!(),
        //     // Operator::GreaterThan => todo!(),
        //     // Operator::LessThanOrEqual => todo!(),
        //     // Operator::GreaterThanOrEqual => todo!(),
        //     // Operator::StringConcat => todo!(),
        //     _ => format!(
        //         "({} {} {})",
        //         self.left.gen(state),
        //         self.operator.gen(state),
        //         self.right.gen(state)
        //     ),
        // }
    }
}

pub fn eval(program: parser::Program, is_test: bool) -> String {
    let mut state = State {
        augroup: None,
        command_depth: 0,
        method_depth: 0,
        scopes: vec![],
        is_test,
    };

    let mut output = String::new();
    if is_test {
        output += "describe(\"filename\", function()\n"
    }

    for command in program.commands.iter() {
        output += &command.gen(&mut state);
        output += "\n";
    }

    if is_test {
        output += "end)"
    }

    output
}

pub fn generate(contents: &str, is_test: bool) -> String {
    let lexer = new_lexer(contents);
    let mut parser = new_parser(lexer);
    let program = parser.parse_program();

    let result = eval(program, is_test);
    println!("{}", result);

    let config = stylua_lib::Config::new()
        .with_indent_type(stylua_lib::IndentType::Spaces)
        .with_indent_width(2)
        .with_column_width(120);

    stylua_lib::format_code(
        &result,
        config,
        None,
        stylua_lib::OutputVerification::None,
    )
    .unwrap()
}

#[cfg(test)]
mod test {
    use std::fs::File;
    use std::io::Write;

    use pretty_assertions::assert_eq;

    use super::*;
    use crate::test_harness::exec_busted;
    use crate::test_harness::exec_lua;

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.bind(|| {
                    insta::assert_snapshot!(generate(contents, false));
                });
            }
        };
    }

    macro_rules! busted {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let vim_contents = include_str!($path);
                let lua_contents = generate(vim_contents, true);

                let filepath = concat!(
                    env!("CARGO_MANIFEST_DIR"),
                    "/testdata/output/",
                    stringify!($name),
                    ".lua"
                );
                let mut file = File::create(filepath).unwrap();
                write!(file, "{}", lua_contents).unwrap();

                exec_busted(filepath).unwrap();
            }
        };
    }

    busted!(busted_simple_assign, "../testdata/busted/simple_assign.vim");
    busted!(busted_operations, "../testdata/busted/operations.vim");
    busted!(busted_assign, "../testdata/busted/assign.vim");
    busted!(busted_heredoc, "../testdata/busted/heredoc.vim");
    busted!(busted_indexing, "../testdata/busted/indexing.vim");
    busted!(busted_multiline, "../testdata/busted/multiline.vim");
    busted!(busted_function, "../testdata/busted/function.vim");
    busted!(busted_methods, "../testdata/busted/methods.vim");
    busted!(busted_megamethods, "../testdata/busted/megamethods.vim");
    busted!(busted_shared, "../testdata/busted/shared.vim");
    // busted!(busted_vimvars, "../testdata/busted/vimvars.vim");

    snapshot!(test_expr, "../testdata/snapshots/expr.vim");
    snapshot!(test_if, "../testdata/snapshots/if.vim");
    snapshot!(test_assign, "../testdata/snapshots/assign.vim");
    snapshot!(test_call, "../testdata/snapshots/call.vim");
    snapshot!(test_autocmd, "../testdata/snapshots/autocmd.vim");
    snapshot!(test_cfilter, "../testdata/snapshots/cfilter.vim");
    // snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");

    #[test]
    fn test_simple_def() {
        let contents = r#"
vim9script

def MyCoolFunc(): number
  return 5
enddef

var x = MyCoolFunc() + 1
"#;

        let generated = generate(contents, false);
        let eval = exec_lua(&generated, "x").unwrap();
        assert_eq!(eval, 6.into());
    }

    #[test]
    fn test_builtin_func() {
        let contents = r#"
vim9script

var x = len("hello")
"#;

        let generated = generate(contents, false);
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

var x = len(nvim_get_autocmds({group: "matchparen"}))

"#;

        let generated = generate(contents, false);
        let eval = exec_lua(&generated, "x").unwrap();
        assert_eq!(eval, 4.into());
    }
}
