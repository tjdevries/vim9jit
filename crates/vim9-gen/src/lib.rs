use std::{collections::HashMap, fmt::Write as _, path::Path};

use lexer::Lexer;
use parser::{
    self, new_parser, ArrayLiteral, AssignStatement, AugroupCommand, AutocmdCommand, Body,
    BreakCommand, CallCommand, CallExpression, ContinueCommand, DeclCommand, DefCommand,
    DeferCommand, DictAccess, DictLiteral, EchoCommand, ElseCommand, ElseIfCommand, ExCommand,
    ExecuteCommand, Expandable, ExportCommand, Expression, ForCommand, GroupedExpression, Heredoc,
    Identifier, IfCommand, ImportCommand, IndexExpression, IndexType, InfixExpression, Lambda,
    Literal, MethodCall, MutationStatement, Operator, PrefixExpression, Program, RawIdentifier,
    Register, ReturnCommand, ScopedIdentifier, SharedCommand, Signature, StatementCommand, Ternary,
    TryCommand, Type, UnpackIdentifier, UserCommand, VarCommand, Vim9ScriptCommand, VimBoolean,
    VimKey, VimNumber, VimOption, VimScope, VimString, WhileCommand,
};

pub mod call_expr;
mod test_harness;

pub type GenResult = Result<(), std::fmt::Error>;

#[derive(Debug, PartialEq)]
pub enum ParserMode {
    Test,
    Autoload { prefix: String },
    Standalone,
}

#[derive(Debug)]
pub struct ParserOpts {
    pub mode: ParserMode,
}

#[derive(Debug)]
pub struct Output {
    pub lua: String,
    pub vim: String,
}

// Writing related impls
impl Output {
    pub fn new() -> Self {
        Self {
            lua: String::new(),
            vim: String::new(),
        }
    }

    pub fn write(&mut self, generator: impl Generate, state: &mut State) {
        match state.opts.mode {
            ParserMode::Standalone => generator.write_default(state, self),
            ParserMode::Test => generator.write_test(state, self),
            ParserMode::Autoload { .. } => generator.write_autoload(state, self),
        }
    }

    pub fn write_lua(&mut self, contents: &str) {
        let _ = write!(self.lua, "{contents}");
    }

    pub fn write_vim(&mut self, contents: &str) {
        let _ = write!(self.vim, "{contents}");
    }
}

impl Default for Output {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct State {
    pub opts: ParserOpts,

    pub command_depth: i32,
    pub method_depth: i32,

    // TODO: Consider moving augroup -> scopes
    // with a new scope type of augroup.
    //
    // That might be a nicer strategy (and mirrors
    // the same thing as we've done before).
    pub augroup: Option<Literal>,

    // TODO: We could modify the state as we are generating code.
    //  As we generate the code and notice certain identifiers are certain
    //  types, we can use that to do *some* optimizations
    pub scopes: Vec<Scope>,
}

impl State {
    fn is_top_level(&self) -> bool {
        self.scopes.len() == 1
    }

    pub fn push_defer(&mut self) {
        let s = self
            .scopes
            .iter_mut()
            .rev()
            .find(|s| s.kind == ScopeKind::Function)
            .unwrap();

        s.deferred += 1
    }

    pub fn find_relevant_scope<F>(&self, filter: F) -> Option<&Scope>
    where
        F: Fn(&&Scope) -> bool,
    {
        self.scopes.iter().rev().find(filter)
    }

    fn with_scope<F, T>(&mut self, kind: ScopeKind, f: F) -> (T, Scope)
    where
        F: Fn(&mut State) -> T,
    {
        self.scopes.push(Scope::new(kind));
        let res = f(self);
        (res, self.scopes.pop().expect("balanced scopes"))
    }

    fn push_declaration(&mut self, expr_1: &Expression, expr_2: Type) -> Type {
        self.scopes
            .last_mut()
            .unwrap()
            .push_declaration(expr_1, expr_2)
    }

    #[allow(unused)]
    fn lookup_declaration(&self, expr_1: &Expression) -> Option<Type> {
        match Scope::declaration_key(expr_1) {
            Some(key) => self
                .scopes
                .iter()
                .rev()
                .find_map(|s| s.declarations.get(&key).cloned()),
            None => None,
        }
    }
}

macro_rules! find_scope {
    ($state:ident, $m:pat) => {{
        use ScopeKind::*;

        let scope = $state.find_relevant_scope(|s| matches!(s.kind, $m));
        match scope {
            Some(scope) => scope,
            None => panic!("Unexpect failure to find scope: {:#?}", $state),
        }
    }};
}

macro_rules! scope_or_empty {
    ($state:ident, $m:pat) => {{
        use ScopeKind::*;

        let scope = $state.find_relevant_scope(|s| matches!(s.kind, $m));
        match scope {
            Some(scope) => scope,
            None => &$state.scopes[0],
        }
    }};
}

#[derive(PartialEq, Eq, Debug, Default)]
pub enum ScopeKind {
    #[default]
    TopLevel,
    Function,
    While {
        has_continue: bool,
    },
    For {
        has_continue: bool,
    },
    If,
}

#[derive(Debug, Default)]
pub struct Scope {
    kind: ScopeKind,
    deferred: usize,
    declarations: HashMap<String, Type>,
}

impl Scope {
    pub fn new(kind: ScopeKind) -> Self {
        Self {
            kind,
            ..Default::default()
        }
    }

    /// Whether the current scope contains any unique behavior for embedded continues
    pub fn has_continue(&self) -> bool {
        match self.kind {
            ScopeKind::While { has_continue } => has_continue,
            ScopeKind::For { has_continue } => has_continue,
            _ => false,
        }
    }

    pub fn declaration_key(expr: &Expression) -> Option<String> {
        match expr {
            Expression::Identifier(ident) => match &ident {
                Identifier::Raw(raw) => Some(raw.name.clone()),
                Identifier::Scope(_) => None,
                Identifier::Unpacked(_) => None,
                Identifier::Ellipsis => None,
            },
            _ => None,
        }
    }

    pub fn push_declaration(&mut self, expr: &Expression, ty: Type) -> Type {
        let key = match Self::declaration_key(expr) {
            Some(key) => key,
            None => return ty,
        };

        self.declarations.insert(key, ty.clone());
        ty
    }
}

pub trait Generate {
    fn write(&self, state: &mut State, output: &mut Output) {
        match state.opts.mode {
            ParserMode::Standalone => self.write_default(state, output),
            ParserMode::Test => self.write_test(state, output),
            ParserMode::Autoload { .. } => self.write_autoload(state, output),
        }
    }

    fn write_default(&self, state: &mut State, output: &mut Output);

    fn write_test(&self, state: &mut State, output: &mut Output) {
        self.write_default(state, output)
    }

    fn write_autoload(&self, state: &mut State, output: &mut Output) {
        self.write_default(state, output)
    }

    fn gen(&self, state: &mut State) -> String {
        let mut output = Output::new();
        self.write(state, &mut output);
        output.lua
    }
}

impl Generate for ExCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        match self {
            ExCommand::NoOp(token) => {
                if !token.kind.is_whitespace() {
                    output.write_lua(&format!("\n-- {token:?}"))
                }
            }
            ExCommand::Comment(token) => output.write_lua(&format!("-- {}", token.text)),
            ExCommand::Finish(_) => output.write_lua("return M"),
            ExCommand::Vim9Script(cmd) => cmd.write(state, output),
            ExCommand::Var(cmd) => cmd.write(state, output),
            ExCommand::Echo(cmd) => cmd.write(state, output),
            ExCommand::Statement(cmd) => cmd.write(state, output),
            ExCommand::Return(cmd) => cmd.write(state, output),
            ExCommand::Def(cmd) => cmd.write(state, output),
            ExCommand::If(cmd) => cmd.write(state, output),
            ExCommand::Augroup(cmd) => cmd.write(state, output),
            ExCommand::Autocmd(cmd) => cmd.write(state, output),
            ExCommand::Call(cmd) => cmd.write(state, output),
            ExCommand::Decl(cmd) => cmd.write(state, output),
            ExCommand::Heredoc(heredoc) => heredoc.write(state, output),
            ExCommand::UserCommand(usercmd) => usercmd.write(state, output),
            ExCommand::SharedCommand(shared) => shared.write(state, output),
            ExCommand::ExportCommand(export) => export.write(state, output),
            ExCommand::ImportCommand(import) => import.write(state, output),
            ExCommand::For(f) => f.write(state, output),
            ExCommand::Try(t) => t.write(state, output),
            ExCommand::While(w) => w.write(state, output),
            ExCommand::Break(b) => b.write(state, output),
            ExCommand::Continue(c) => c.write(state, output),
            ExCommand::Defer(defer) => defer.write(state, output),
            ExCommand::Execute(exec) => exec.write(state, output),
            ExCommand::Eval(eval) => output.write_lua(&format!("{};", eval.expr.gen(state))),
            _ => todo!("Have not yet handled: {:?}", self),
        }
    }
}

impl Generate for DeferCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        state.push_defer();

        let mut temp = Output::new();
        self.call.write(state, &mut temp);
        let expr = self.call.gen(state);

        output.write_lua(&format!(
            "table.insert(nvim9_deferred, 1, function() {expr} end)"
        ))
    }
}

impl Generate for ContinueCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let scope = find_scope!(state, Function | While { .. } | For { .. });
        assert!(scope.kind != ScopeKind::Function, "continue: While/For");
        assert!(scope.has_continue(), "must have continue...");

        output.write_lua("return vim9.ITER_CONTINUE")
    }
}

impl Generate for BreakCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let scope = find_scope!(state, Function | While { .. } | For { .. });
        assert!(
            scope.kind != ScopeKind::Function,
            "continue: While/For {self:#?} // {scope:?}"
        );

        if scope.has_continue() {
            output.write_lua("return vim9.ITER_BREAK")
        } else {
            output.write_lua("break")
        }
    }
}

impl Generate for WhileCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let has_continue = continue_exists_in_scope(&self.body);
        let (body, _) = state.with_scope(ScopeKind::While { has_continue }, |s| self.body.gen(s));

        if has_continue {
            // let condition = self.condition.gen(state);
            let condition = self.condition.gen(state);
            return output.write_lua(&format!(
                r#"
                    local body = function()
                        {body}

                        return 0
                    end

                    while {condition} do
                        local nvim9_status, nvim9_ret = body()
                        if nvim9_status == 2 then
                            break
                        elseif nvim9_status == 3 then
                            return nvim9_ret
                        end
                    end
                "#,
            ));
        }

        // output.write_lua(&format!(
        //     "while {}\ndo\n{}\nend",
        //     self.condition.gen(state),
        //     body
        // ))
        output.write_lua("while ");
        output.write_lua(&self.condition.gen(state));
        output.write_lua(&format!("\ndo\n{body}\nend"));
    }
}

impl Generate for TryCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        // TODO: try and catch LUL
        // self.body.gen(state)
        output.write(&self.body, state)
    }
}

fn continue_exists_in_scope(body: &Body) -> bool {
    body.commands.iter().any(|c| match c {
        ExCommand::If(ex) => continue_exists_in_scope(&ex.body),
        ExCommand::Continue(_) => true,
        _ => false,
    })
}

impl Generate for ForCommand {
    // 0 => nothing
    // 1 => continue
    // 2 => break
    // 3 => return
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let has_continue = continue_exists_in_scope(&self.body);
        let (body, _) = state.with_scope(ScopeKind::For { has_continue }, |s| self.body.gen(s));

        let expr = &self.for_expr.gen(state);
        let (ident, unpacked) = match &self.for_identifier {
            Identifier::Unpacked(unpacked) => (
                "__unpack_result".to_string(),
                format!(
                    "local {} = unpack(__unpack_result)",
                    identifier_list(state, unpacked)
                ),
            ),
            _ => (self.for_identifier.gen(state), "".to_string()),
        };

        let result = if has_continue {
            format!(
                r#"
                    local body = function(_, {ident})
                        {unpacked}
                        {body}

                        return vim9.ITER_DEFAULT
                    end

                    for _, {ident} in vim9.iter({expr}) do
                        {unpacked}
                        local nvim9_status, nvim9_ret = body(_, {ident})
                        if nvim9_status == vim9.ITER_BREAK then
                            break
                        elseif nvim9_status == vim9.ITER_RETURN then
                            return nvim9_ret
                        end
                    end
                "#
            )
        } else {
            format!(
                r#"
                for _, {ident} in vim9.iter({expr}) do
                    {body}
                end
                "#,
            )
        };

        output.write_lua(&result);
    }
}

impl Generate for ImportCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&match self {
            ImportCommand::ImportImplicit {
                file,
                name,
                autoload,
                ..
            } => match name {
                Some(name) => {
                    let var = name.gen(state);
                    format!(
                        "local {var} = vim9.import({{ name = {file:?}, autoload = {autoload} }})"
                    )
                }
                None => {
                    let filepath = Path::new(file);
                    let stem = filepath.file_stem().unwrap().to_str().unwrap();
                    format!(
                        "local {stem} = vim9.import({{ name = {file:?}, autoload = {autoload} }})"
                    )
                }
            },
            ImportCommand::ImportUnpacked { names, file, .. } => names
                .iter()
                .map(|name| {
                    let name = name.gen(state);
                    format!("local {name} = vim9.import({{ name = {file:?} }})['{name}']")
                })
                .collect::<Vec<String>>()
                .join("\n"),
        })
    }
}

impl Generate for ExportCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let exported = self.command.gen(state);
        let ident = match self.command.as_ref() {
            ExCommand::Var(var) => var.name.gen(state),
            ExCommand::Def(def) => def.name.gen(state),
            // ExCommand::Heredoc(_) => todo!(),
            // ExCommand::Decl(_) => todo!(),
            _ => {
                unreachable!("not a valid export command: {:#?}", self.command)
            }
        };

        output.write_lua(&format!("{exported};\nM['{ident}'] = {ident};"))
    }

    fn write_autoload(&self, state: &mut State, output: &mut Output) {
        // Write the default output for a file
        self.write_default(state, output);

        let prefix = match &state.opts.mode {
            ParserMode::Autoload { prefix } => prefix.clone(),
            _ => unreachable!("not in autoload mode"),
        };

        match self.command.as_ref() {
            ExCommand::Def(def) => {
                let name = def.name.gen(state);
                let sig = def
                    .args
                    .params
                    .iter()
                    .map(|p| p.name.gen(state))
                    .collect::<Vec<String>>()
                    .join(", ");

                let call = def
                    .args
                    .params
                    .iter()
                    .map(|p| "a:".to_string() + &p.name.gen(state))
                    .collect::<Vec<String>>()
                    .join(", ");

                output.write_vim(&format!(
                    r#"
function! {prefix}#{name}({sig}) abort
 return s:nvim_module.{name}({call})
endfunction
"#
                ))
            }
            _ => unreachable!("not a valid export command: {:#?}", self.command),
        }
    }
}

impl Generate for SharedCommand {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        // Temp
        output.write_lua(&format!("pcall(vim.cmd, [[ {} ]])", self.contents.trim()))
    }
}

fn make_user_command_arg(state: &State) -> String {
    format!("__vim9_arg_{}", state.command_depth)
}

fn to_str_or_nil(s: &Option<String>) -> String {
    if let Some(complete) = s {
        format!("'{complete}'")
    } else {
        "nil".to_string()
    }
}

impl Generate for UserCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        state.command_depth += 1;

        let complete = to_str_or_nil(&self.command_complete);
        let nargs = match &self.command_nargs {
            Some(nargs) => match nargs.as_str() {
                "0" => "0".to_string(),
                "1" => "1".to_string(),
                _ => format!("'{nargs}'"),
            },
            None => "nil".to_string(),
        };

        let result = format!(
            r#"
            vim.api.nvim_create_user_command(
                "{}",
                function({})
                    {}
                end,
                {{
                     bang = {},
                     nargs = {nargs},
                     complete = {complete},
                }}
            )"#,
            self.name,
            make_user_command_arg(state),
            self.command.gen(state),
            self.command_bang,
        );

        state.command_depth -= 1;

        output.write_lua(&result)
    }
}

fn get_local(state: &State, name: &Identifier) -> String {
    if state.is_top_level() || !name.is_valid_local() {
        ""
    } else {
        "local"
    }
    .to_string()
}

impl Generate for DeclCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let local = get_local(state, &self.name);

        // TODO: default value should not be nil for everything here
        // i think?
        output.write_lua(&format!(
            "{local} {} = {}",
            self.name.gen(state),
            match &self.ty {
                Some(ty) => match ty {
                    Type::Any => "0",
                    Type::Bool => "false",
                    Type::BoolOrNumber => "false",
                    Type::Number => "0",
                    Type::Float => "0",
                    Type::String => "''",
                    Type::List { .. } => "{}",
                    Type::Dict { .. } => "vim.empty_dict()",
                    Type::Job => todo!(),
                    Type::Channel => todo!(),
                    Type::Void => "nil",
                    Type::Func(_) => "function() end",
                    Type::Blob => unreachable!(),
                },
                None => "nil",
            }
        ))
    }
}

impl Generate for Heredoc {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        // this works for non-trim and non-eval heredocs perfect
        let inner = self
            .contents
            .iter()
            .map(|line| format!("[==[{line}]==]"))
            .collect::<Vec<String>>()
            .join(", ");

        let mut list = format!("{{ {inner} }}");
        if self.trim {
            list = format!("vim9.heredoc.trim({list})")
        }

        output.write_lua(&format!("local {} = {}", self.name.gen(state), list))
    }
}

impl Generate for CallCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let call: CallExpression = self.into();
        output.write(call, state);
        output.write_lua(";");
    }
}

impl Generate for AugroupCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
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

        output.write_lua(&result)
    }
}

impl Generate for AutocmdCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
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

        output.write_lua(&format!(
            r#"
vim.api.nvim_create_autocmd({{ {event_list} }}, {{
    {group}
    callback = {callback},
}})
"#
        ))
    }
}

impl Generate for ReturnCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let scope = scope_or_empty!(state, Function | While { .. } | For { .. });

        output.write_lua(&if scope.has_continue() {
            match &self.expr {
                Some(expr) => {
                    format!("return vim9.ITER_RETURN, {}", expr.gen(state))
                }
                None => "return vim9.ITER_RETURN".to_string(),
            }
        } else {
            match &self.expr {
                Some(expr) => format!("return {}", expr.gen(state)),
                None => "return".to_string(),
            }
        })
    }
}

impl Generate for DefCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let name = self.name.gen(state);
        let (body, scope) = state.with_scope(ScopeKind::Function, |s| self.body.gen(s));

        let (signature, sig_statements) = gen_signature(state, &self.args);

        let local = get_local(state, &self.name);
        let result = if scope.deferred > 0 {
            // TODO: Should probably handle errors in default statements or body
            format!(
                r#"
                    {local} {name} = function({signature})
                        {sig_statements}

                        local nvim9_deferred = {{}}
                        local _, ret = pcall(function()
                            {body}
                        end)

                        for _, nvim9_defer in ipairs(nvim9_deferred) do
                            pcall(nvim9_defer)
                        end

                        return ret
                    end
                    "#
            )
        } else {
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
                    {local} {name} = function({signature})
                        {sig_statements}
                        {body}
                    end
                "#,
            )
        };

        output.write_lua(&result);
    }

    fn write_test(&self, state: &mut State, output: &mut Output) {
        let name = self.name.gen(state);
        if state.opts.mode != ParserMode::Test || !name.starts_with("Test") {
            return self.write_default(state, output);
        }

        let (body, scope) = state.with_scope(ScopeKind::Function, |s| self.body.gen(s));
        assert!(scope.deferred == 0, "have not handled deferred in tests");

        output.write_lua(&format!(
            r#"
                it("{name}", function()
                   -- Set errors to empty
                   vim.v.errors = {{}}

                   -- Actual test
                   {body}

                   -- Assert that errors is still empty
                   assert.are.same({{}}, vim.v.errors)
                end)
            "#,
        ))
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
            .filter(|p| p.default_val.is_some() || matches!(p.ty, Some(Type::BoolOrNumber)))
            .map(|p| {
                // TODO: What about val=true defaults?
                match p.ty {
                    Some(Type::BoolOrNumber) => {
                        let name = p.name.gen(state);
                        format!("{name} = vim9.bool({name})")
                    }
                    _ => {
                        let name = p.name.gen(state);
                        let default_val = p.default_val.clone().unwrap();
                        let default_val = default_val.gen(state);

                        format!("{name} = vim.F.if_nil({name}, {default_val}, {name})")
                    }
                }
            })
            .collect::<Vec<String>>()
            .join("\n"),
    )
}

impl Generate for StatementCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        match self {
            StatementCommand::Assign(assign) => assign.write(state, output),
            StatementCommand::Mutate(mutate) => mutate.write(state, output),
        }
    }
}

fn statement_lhs(expr: &Expression, state: &mut State) -> String {
    match &expr {
        Expression::Index(idx) => {
            format!(
                "{}[vim9.index_expr({})]",
                idx.container.gen(state),
                match idx.index.as_ref() {
                    IndexType::Item(item) => item.gen(state),
                    IndexType::Slice(_) => todo!("Unknown index type"),
                }
            )
        }
        _ => expr.gen(state),
    }
}

impl Generate for MutationStatement {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        // format!("--[[ {:#?} ]]", self)
        let left = statement_lhs(&self.left, state);
        let operator = match self.modifier.kind {
            lexer::TokenKind::PlusEquals => parser::Operator::Plus,
            lexer::TokenKind::MinusEquals => parser::Operator::Minus,
            lexer::TokenKind::MulEquals => parser::Operator::Multiply,
            lexer::TokenKind::DivEquals => parser::Operator::Divide,
            lexer::TokenKind::PercentEquals => parser::Operator::Modulo,
            lexer::TokenKind::StringConcatEquals => parser::Operator::StringConcat,
            _ => unreachable!(),
        };

        // TODO(clone)
        let infix = InfixExpression::new(
            operator,
            self.left.clone().into(),
            self.right.clone().into(),
        )
        .gen(state);

        output.write_lua(&format!("{left} = {infix}"))
    }
}

impl Generate for AssignStatement {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let left = statement_lhs(&self.left, state);
        let right = self.right.gen(state);

        output.write_lua(&format!("{left} = {right}"))
    }
}

impl Generate for IfCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let else_result = match &self.else_command {
            Some(e) => e.gen(state),
            None => "".to_string(),
        };

        let condition = self.condition.gen(state);

        // TODO: Numbers are automatically coerced into bools for vim
        // so sometimes the type system lies to us.
        //
        // Perhaps I need an additional type: BoolNumber that we use
        // for most cases of vim, and we only escalate to Bool when we're confident
        // that this what is happening.
        //
        // This would allow us to remove the vim9.bool condition (assuming the type
        // system is correct in vim9script)
        let condition = match guess_type_of_expr(state, &self.condition) {
            Type::Bool => condition,
            _ => format!("vim9.bool({condition})"),
        };

        output.write_lua(
            format!(
                r#"
if {condition} then
  {}
{}
{}
end"#,
                self.body.gen(state),
                self.elseifs
                    .iter()
                    .map(|e| e.gen(state))
                    .collect::<Vec<String>>()
                    .join("\n"),
                else_result,
            )
            .trim(),
        )
    }
}

impl Generate for ElseIfCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&format!(
            r#"
        elseif vim9.bool({}) then
            {}
        "#,
            self.condition.gen(state),
            self.body.gen(state)
        ))
    }
}

impl Generate for ElseCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&format!("else\n{}\n", self.body.gen(state)))
    }
}

impl Generate for Body {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        (&self).write(state, output)
    }
}

impl Generate for &Body {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(
            &self
                .commands
                .iter()
                .map(|cmd| cmd.gen(state))
                .collect::<Vec<String>>()
                .join("\n"),
        )
    }
}

impl Generate for EchoCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        // TODO: Probably should add some function that
        // pretty prints these the way they would get printed in vim
        // or maybe just call vim.cmd [[echo <...>]] ?
        //  Not sure.
        //  Maybe have to expose something to get exactly the same
        //  results
        //
        // format!("vim.api.nvim_echo({}, false, {{}})", chunks)
        output.write_lua(&format!("print({})", self.expr.gen(state)))
    }
}

impl Generate for ExecuteCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&format!("vim.api.nvim_command({})", self.expr.gen(state)))
    }
}

impl Generate for Vim9ScriptCommand {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        // TODO: Actually connect this
        // format!("vim9.new_script {{ noclear = {} }}", self.noclear)
        // write!(state.lua_contents, "-- vim9script")
        output.write_lua("-- vim9script")
    }
}

fn identifier_list(state: &mut State, unpacked: &UnpackIdentifier) -> String {
    unpacked
        .identifiers
        .iter()
        .flat_map(|i| [i.gen(state), ", ".to_string()])
        .take((2 * unpacked.identifiers.len()).saturating_sub(1))
        .collect()
}

fn vim_to_type(ret: &vimfuncs::FuncReturnType) -> Type {
    match ret {
        vimfuncs::FuncReturnType::Any => Type::Any,
        vimfuncs::FuncReturnType::Bool => Type::Bool,
        vimfuncs::FuncReturnType::Float => Type::Float,
        vimfuncs::FuncReturnType::Number => Type::Number,
        vimfuncs::FuncReturnType::NumberBool => Type::BoolOrNumber,
        vimfuncs::FuncReturnType::String => Type::String,
        // vimfuncs::FuncReturnType::Void => Type::Any,
        // vimfuncs::FuncReturnType::Dict(_) => Type,
        // vimfuncs::FuncReturnType::List(_) => todo!(),
        // vimfuncs::FuncReturnType::Func(_) => todo!(),
        _ => Type::Any,
    }
}

pub fn func_info_for_call(c: &CallExpression) -> Option<vimfuncs::FuncInfo> {
    match c.name() {
        Some(Identifier::Raw(raw)) => vimfuncs::get_func_info(&raw.name),
        _ => None,
    }
}

fn guess_type_of_expr(_state: &State, expr: &Expression) -> Type {
    match expr {
        Expression::Number(_) => Type::Number,
        Expression::String(_) => Type::String,
        Expression::Boolean(_) => Type::Bool,
        Expression::Call(c) => match func_info_for_call(c) {
            Some(info) => vim_to_type(&info.return_type),
            _ => Type::Any,
        },
        Expression::Infix(infix) => {
            if infix.operator.is_comparison() {
                return Type::Bool;
            }

            if infix.operator == Operator::StringConcat {
                return Type::String;
            }

            let left_ty = guess_type_of_expr(_state, infix.left.as_ref());
            let right_ty = guess_type_of_expr(_state, infix.right.as_ref());

            if matches!(infix.operator, Operator::And | Operator::Or)
                && left_ty == Type::Bool
                && right_ty == Type::Bool
            {
                return Type::Bool;
            }

            if left_ty == Type::Number && right_ty == Type::Number && infix.operator.is_math() {
                return Type::Number;
            }

            Type::Any
        }
        Expression::Grouped(g) => guess_type_of_expr(_state, g.expr.as_ref()),
        _ => Type::Any,
        // Expression::Index(_) => todo!(),
        // Expression::Slice(_) => todo!(),
        // Expression::Array(_) => todo!(),
        // Expression::Dict(_) => todo!(),
        // Expression::DictAccess(_) => todo!(),
        // Expression::VimOption(_) => todo!(),
        // Expression::Register(_) => todo!(),
        // Expression::Lambda(_) => todo!(),
        // Expression::Expandable(_) => todo!(),
        // Expression::MethodCall(_) => todo!(),
        // Expression::Ternary(_) => todo!(),
        // Expression::Prefix(_) => todo!(),
        // Expression::Infix(_) => todo!(),
    }
}

impl Generate for VarCommand {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        state.push_declaration(
            &self.expr,
            match &self.ty {
                Some(ty) => ty.clone(),
                None => guess_type_of_expr(state, &self.expr),
            },
        );

        let expr = match self.ty {
            Some(Type::Bool) => {
                format!("vim9.convert.decl_bool({})", self.expr.gen(state))
            }
            Some(Type::Dict { .. }) => {
                format!("vim9.convert.decl_dict({})", self.expr.gen(state))
            }
            _ => self.expr.gen(state),
        };

        let local = get_local(state, &self.name);
        match &self.name {
            Identifier::Unpacked(unpacked) => {
                let identifiers: String = identifier_list(state, unpacked);
                output.write_lua(&format!("{local} {identifiers} = unpack({expr})"))
            }
            _ => output.write_lua(&format!("{local} {} = {}", self.name.gen(state), expr)),
        }
    }
}

impl Generate for Identifier {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        match self {
            Identifier::Raw(raw) => raw.write(state, output),
            Identifier::Scope(scoped) => scoped.write(state, output),
            Identifier::Ellipsis => output.write_lua("..."),
            Identifier::Unpacked(_) => {
                unreachable!("must be handled higher {:#?}", self)
            }
        }
    }
}

impl Generate for ScopedIdentifier {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        if self.scope == VimScope::VimVar {
            if let Identifier::Raw(raw) = self.accessor.as_ref() {
                if raw.name == "version" {
                    // "lie" to vim9script code and say that we are
                    // version 9. This may need to be updated if
                    // vim updates their v:version variable
                    output.write_lua("900");
                    return;
                }
            }
        }

        let scope = match self.scope {
            VimScope::Global => "vim.g",
            VimScope::VimVar => "vim.v",
            VimScope::Tab => "vim.t",
            VimScope::Window => "vim.w",
            VimScope::Buffer => "vim.b",
            VimScope::Script => todo!("Not sure how to handle scriptvar"),
            VimScope::Local => todo!("Not sure how to handle local"),
        };

        output.write_lua(&format!("{}['{}']", scope, self.accessor.gen(state)))
    }
}

impl Generate for RawIdentifier {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        // There are a variety of keywords we have to make sure
        // that we don't use when creating identifiers
        output.write_lua(match self.name.as_str() {
            "end" => "__end__",
            "M" => "__M__",
            "vim9" => "__vim9__",
            _ => &self.name,
        })
    }
}

impl Generate for Expression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        (&self).write(state, output)
    }
}

impl Generate for &Expression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&match self {
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
            Expression::Ternary(ternary) => ternary.gen(state),

            // Some expressions can only be triggered from containing expressions
            Expression::Slice(_) => unreachable!("cannot gen slice by itself"),
        })
    }
}

mod expr_utils {
    use parser::Expression;

    pub fn has_possible_sideffects(expr: &Expression) -> bool {
        match expr {
            Expression::Empty => false,
            Expression::Identifier(_) => false,
            Expression::Number(_) => false,
            Expression::String(_) => false,
            Expression::Boolean(_) => false,
            Expression::Grouped(g) => has_possible_sideffects(&g.expr),
            Expression::Slice(s) => {
                let mut has_side_effect = false;
                if let Some(start) = &s.start {
                    has_side_effect |= has_possible_sideffects(start)
                }

                if let Some(finish) = &s.finish {
                    has_side_effect |= has_possible_sideffects(finish)
                }

                has_side_effect
            }
            Expression::Array(_) => false,
            Expression::Dict(_) => false,
            Expression::DictAccess(_) => false,
            Expression::Lambda(_) => false,
            Expression::Expandable(_) => false,
            Expression::Ternary(t) => {
                has_possible_sideffects(&t.cond)
                    || has_possible_sideffects(&t.if_true)
                    || has_possible_sideffects(&t.if_false)
            }
            Expression::Prefix(_) => false,
            Expression::Infix(_) => true,
            Expression::Call(_) => true,
            Expression::MethodCall(_) => true,
            // TODO: Actually not sure which way to go for this,
            //          Since it could be changed unexpectedly
            //          by other things executing
            Expression::Register(_) => true,
            Expression::VimOption(_) => false,
            // TODO:
            Expression::Index(_) => {
                // has_possible_sideffects(&i.container)
                //     || has_possible_sideffects(&i.index)
                true
            }
        }
    }
}

impl Generate for Ternary {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let if_true = if expr_utils::has_possible_sideffects(&self.if_true) {
            format!("function() return {} end", self.if_true.gen(state))
        } else {
            self.if_true.gen(state)
        };

        let if_false = if expr_utils::has_possible_sideffects(&self.if_false) {
            format!("function() return {} end", self.if_false.gen(state))
        } else {
            self.if_false.gen(state)
        };

        output.write_lua(&format!(
            "vim9.ternary({}, {if_true}, {if_false})",
            self.cond.gen(state),
        ))
    }
}

impl Generate for MethodCall {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&call_expr::generate_method(self, state))
    }
}

mod ident {
    pub fn str_is_keyword(s: &str) -> bool {
        matches!(s, "end" | "repeat" | "for")
    }

    pub fn is_safe_str(name: &str) -> bool {
        !str_is_keyword(name) && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_')
    }

    // pub fn is_lua_keyword(ident: &Identifier) -> bool {
    //     match ident {
    //         Identifier::Raw(raw) => str_is_keyword(&raw.name),
    //         Identifier::Scope(_) => false,
    //         Identifier::Unpacked(idents) => idents.identifiers.iter().any(is_lua_keyword),
    //         Identifier::Ellipsis => false,
    //     }
    // }

    // pub fn is_safe_identifier(ident: &Identifier) -> bool {
    //     !is_lua_keyword(ident)
    //         && match ident {
    //             Identifier::Raw(raw) => is_safe_str(&raw.name),
    //             Identifier::Scope(_) => false,
    //             Identifier::Unpacked(_) => false,
    //             Identifier::Ellipsis => false,
    //         }
    // }
}

impl Generate for DictAccess {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        if ident::is_safe_str(&self.index.name) {
            output.write_lua(&format!(
                "{}.{}",
                self.container.gen(state),
                self.index.gen(state)
            ))
        } else {
            output.write_lua(&format!(
                "{}['{}']",
                self.container.gen(state),
                self.index.gen(state)
            ))
        }
    }
}

impl Generate for Expandable {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&match &self.ident {
            Identifier::Raw(RawIdentifier { name }) if name == "q-args" => {
                format!("{}.{}", make_user_command_arg(state), "args")
            }
            Identifier::Raw(RawIdentifier { name }) if name == "q-mods" => {
                format!("{}.{}", make_user_command_arg(state), "mods")
            }
            Identifier::Raw(RawIdentifier { name }) if name == "q-bang" => {
                format!(
                    "({}.{} and '!' or '')",
                    make_user_command_arg(state),
                    "bang"
                )
            }
            _ => format!("vim.fn.expand('<{}>')", self.ident.gen(state)),
        })
    }
}

impl Generate for Lambda {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let (signature, default_statements) = gen_signature(state, &self.args);
        output.write_lua(&format!(
            r#"(function({})
                {}
                {}
                end)"#,
            signature,
            default_statements,
            self.body.gen(state)
        ))
    }
}

impl Generate for Register {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        output.write_lua(&format!("vim.fn.getreg({:?})", self.register))
    }
}

impl Generate for PrefixExpression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        let right = self.right.gen(state);
        output.write_lua(&match &self.operator {
            parser::Operator::Increment => format!("{right} = {right} + 1"),
            parser::Operator::Decrement => format!("{right} = {right} - 1"),
            _ => {
                let ty = guess_type_of_expr(state, &self.right);
                match ty {
                    Type::Number => {
                        let operator = match &self.operator {
                            parser::Operator::Minus => "-",
                            _ => todo!("OPERATOR: {:?}", self.operator),
                        };

                        format!("{operator}{right}")
                    }
                    _ => {
                        format!("vim9.prefix['{:?}']({right})", self.operator,)
                    }
                }
            }
        })
    }
}

impl Generate for IndexExpression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        // println!("self.index = {:#?}", self.index);

        output.write_lua(&match self.index.as_ref() {
            IndexType::Item(item) => {
                format!(
                    "vim9.index({}, {})",
                    self.container.gen(state),
                    item.gen(state)
                )
            }
            IndexType::Slice(slice) => {
                format!(
                    "vim9.slice({}, {}, {})",
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
        })
    }
}

impl Generate for VimOption {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        // TODO: not sure if i need to do something smarter than this
        output.write_lua(&format!(
            // "vim.api.nvim_get_option_value('{}', {{}})",
            "vim.o['{}']",
            self.option.gen(state)
        ))
    }
}

impl Generate for Literal {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        output.write_lua(&self.token.text)
    }
}

impl Generate for VimKey {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&match self {
            // TODO: For literals, we could simplify this to do
            // direct key access if it doesn't contain any illegal characters
            VimKey::Literal(literal) => format!("['{}']", literal.token.text),
            VimKey::Expression(expr) => format!("[{}]", expr.gen(state)),
        })
    }
}

impl Generate for DictLiteral {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&format!(
            "{{ {} }}",
            self.elements
                .iter()
                .map(|x| format!("{} = {}", x.key.gen(state), x.value.gen(state)))
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

impl Generate for ArrayLiteral {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&format!(
            "{{ {} }}",
            self.elements
                .iter()
                .map(|x| x.gen(state))
                .collect::<Vec<String>>()
                .join(", ")
        ))
    }
}

impl Generate for VimString {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        output.write_lua(&match self {
            VimString::SingleQuote(s) => format!(
                "'{}'",
                s.chars()
                    .map(|c| if c == '\\' {
                        "\\\\".to_string()
                    } else {
                        c.to_string()
                    })
                    .collect::<String>()
            ),
            VimString::DoubleQuote(s) => {
                let mut cs = s.chars();
                // TODO: This is ugly
                while let Some(ch) = cs.next() {
                    if ch == '\\' {
                        if let Some(ch) = cs.next() {
                            if ch == '<' {
                                output.write_lua(&format!(
                                    "vim.api.nvim_replace_termcodes(\"{s}\", true, true, true)"
                                ));
                                return;
                            }
                        }
                    }
                }

                format!("\"{s}\"")
            }
            VimString::Interpolated(interp) => {
                format!("string.format([=[{interp}]=])")
            }
            VimString::InterpolatedLit(interp) => {
                format!("string.format([=[{interp}']=])")
            }
            VimString::EnvironmentVariable(env) => {
                format!("vim.env['{env}']")
            }
        })
    }
}

impl Generate for CallExpression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&call_expr::generate(self, state));
    }
}

impl Generate for GroupedExpression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        output.write_lua(&format!("({})", self.expr.gen(state)));
    }
}

impl Generate for VimBoolean {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        output.write_lua(&self.value.to_string())
    }
}

impl Generate for VimNumber {
    fn write_default(&self, _: &mut State, output: &mut Output) {
        output.write_lua(&self.value)
    }
}

impl Generate for InfixExpression {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        use Type::*;

        let ty = guess_type_of_expr(state, &Expression::Infix(self.clone()));

        let left = self.left.gen(state);
        let right = self.right.gen(state);

        output.write_lua(&if let Some(literal) = self.operator.literal() {
            match ty {
                Bool => format!("{left} {literal} {right}",),
                Number if self.operator.is_math() => {
                    format!("{left} {literal} {right}")
                }
                String if self.operator == Operator::StringConcat => {
                    format!("{left} .. {right}")
                }
                _ => {
                    format!("vim9.ops.{:?}({left}, {right})", self.operator)
                }
            }
        } else {
            format!("vim9.ops.{:?}({left}, {right})", self.operator)
        })

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

fn toplevel_ident(s: &mut State, command: &ExCommand) -> Option<String> {
    match command {
        ExCommand::Decl(decl) => Some(decl.name.gen(s)),
        ExCommand::Def(def) => {
            if def.name.is_valid_local() {
                let name = def.name.gen(s);
                if s.opts.mode == ParserMode::Test && name.starts_with("Test") {
                    None
                } else {
                    Some(name)
                }
            } else {
                None
            }
        }
        ExCommand::ExportCommand(e) => toplevel_ident(s, e.command.as_ref()),
        ExCommand::Heredoc(here) => Some(here.name.gen(s)),
        // This might make sense, but I don't think it allows you to do this?
        ExCommand::Var(var) => Some(var.name.gen(s)),
        // These make sense
        ExCommand::Vim9Script(_) => None,
        ExCommand::Echo(_) => None,
        ExCommand::Execute(_) => None,
        ExCommand::Return(_) => None,
        ExCommand::If(_) => None,
        ExCommand::For(_) => None,
        ExCommand::While(_) => None,
        ExCommand::Try(_) => None,
        ExCommand::Call(_) => None,
        ExCommand::Eval(_) => None,
        ExCommand::Finish(_) => None,
        ExCommand::Break(_) => None,
        ExCommand::Continue(_) => None,
        ExCommand::Augroup(_) => None,
        ExCommand::Autocmd(_) => None,
        ExCommand::Statement(_) => None,
        ExCommand::UserCommand(_) => None,
        ExCommand::SharedCommand(_) => None,
        ExCommand::ImportCommand(_) => None,
        ExCommand::Skip => None,
        ExCommand::EndOfFile => None,
        ExCommand::Comment(_) => None,
        ExCommand::NoOp(_) => None,
        ExCommand::Defer(_) => None,
    }
}

mod generate_program {
    use super::*;

    pub fn preamble(_: &State, output: &mut Output) {
        output.write_lua(
            r#"
----------------------------------------
-- This file is generated via github.com/tjdevries/vim9jit
-- For any bugs, please first consider reporting there.
----------------------------------------

-- Ignore "value assigned to a local variable is unused" because
--  we can't guarantee that local variables will be used by plugins
-- luacheck: ignore 311

local vim9 = require('_vim9script')
local M = {}
"#,
        );
    }

    // "hoist" top-level declaractions to top of program, to ensure proper names
    pub fn hoist(program: &Program, state: &mut State, output: &mut Output) {
        for command in program.commands.iter() {
            if let Some(toplevel) = toplevel_ident(state, command) {
                output.write_lua(&format!("local {toplevel} = nil\n"));
            }
        }
    }
}

impl Generate for Program {
    fn write_default(&self, state: &mut State, output: &mut Output) {
        generate_program::preamble(state, output);
        generate_program::hoist(self, state, output);

        for command in self.commands.iter() {
            command.write(state, output);
            output.write_lua("\n");
        }

        output.write_lua("return M");
    }

    fn write_autoload(&self, state: &mut State, output: &mut Output) {
        output.write_vim(
            r#"" Generated vim file by vim9jit. Please do not edit
let s:path = expand("<script>")
let s:lua_path = fnamemodify(s:path, ":r") . ".lua"
let s:nvim_module = luaeval('require("_vim9script").autoload(_A)', s:lua_path)
"#,
        );

        self.write_default(state, output);
    }

    fn write_test(&self, state: &mut State, output: &mut Output) {
        generate_program::preamble(state, output);
        generate_program::hoist(self, state, output);

        output.write_lua("describe(\"filename\", function()\n");

        for command in self.commands.iter() {
            command.write(state, output);
            output.write_lua("\n");
        }

        output.write_lua("end)");
        output.write_lua("return M");
    }
}

pub fn eval(program: parser::Program, opts: ParserOpts) -> anyhow::Result<Output> {
    let mut state = State {
        augroup: None,
        command_depth: 0,
        method_depth: 0,
        scopes: vec![Scope::new(ScopeKind::TopLevel)],
        opts,
    };

    let mut output = Output::new();
    output.write(program, &mut state);
    Ok(output)
}

pub fn generate(contents: &str, opts: ParserOpts) -> Result<Output, (Output, String)> {
    let lexer = Lexer::new(contents);
    let parser = new_parser(&lexer);
    let program = parser.parse_program();

    let mut result = eval(program, opts).unwrap();
    result.lua = format::lua(&result.lua).expect("format lua code");

    Ok(result)
}

#[cfg(test)]
mod test {
    use std::{fs::File, io::Write, process::Command};

    use pretty_assertions::assert_eq;

    use super::*;
    use crate::test_harness::{exec_busted, exec_lua};

    fn ensure_plenary_downloaded() {
        static DOWNLOADED: std::sync::Once = std::sync::Once::new();
        // Download and install Plenary if needed
        DOWNLOADED.call_once(|| {
            let path = std::env::var("HOME").expect("to have a home var")
                + "/.local/share/nvim/site/pack/vendor/start/plenary.nvim";

            if Path::new(&path).exists() {
                return;
            }

            // Download git repo
            let status = Command::new("git")
                .arg("clone")
                .arg("--depth")
                .arg("1")
                .arg("https://github.com/nvim-lua/plenary.nvim")
                .arg(path)
                .status()
                .expect("failed to execute git command");

            assert!(status.success());
        });
    }

    macro_rules! snapshot {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                let contents = include_str!($path);
                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("../testdata/output/");
                settings.bind(|| {
                    insta::assert_snapshot!(
                        generate(
                            contents,
                            ParserOpts {
                                mode: ParserMode::Standalone,
                            }
                        )
                        .unwrap()
                        .lua
                    );
                });
            }
        };
    }

    macro_rules! busted {
        ($name:tt, $path:tt) => {
            #[test]
            fn $name() {
                ensure_plenary_downloaded();

                let vim_contents = include_str!($path);
                let lua_contents = generate(
                    vim_contents,
                    ParserOpts {
                        mode: ParserMode::Test,
                    },
                )
                .unwrap()
                .lua;

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
    busted!(busted_shared, "../testdata/busted/shared.vim");
    busted!(busted_loops, "../testdata/busted/loops.vim");
    busted!(busted_defer, "../testdata/busted/defer.vim");

    // TODO: Fix this
    // busted!(busted_vimvars, "../testdata/busted/vimvars.vim");

    busted!(busted_methods, "../testdata/busted/methods.vim");
    busted!(busted_methods_1, "../testdata/busted/methods_1.vim");
    busted!(busted_methods_2, "../testdata/busted/methods_2.vim");
    busted!(busted_megamethods, "../testdata/busted/megamethods.vim");
    busted!(
        busted_methods_shifted,
        "../testdata/busted/methods_shifted.vim"
    );

    snapshot!(test_expr, "../testdata/snapshots/expr.vim");
    snapshot!(test_if, "../testdata/snapshots/if.vim");
    snapshot!(test_assign, "../testdata/snapshots/assign.vim");
    snapshot!(test_call, "../testdata/snapshots/call.vim");
    snapshot!(test_autocmd, "../testdata/snapshots/autocmd.vim");
    snapshot!(test_cfilter, "../testdata/snapshots/cfilter.vim");
    snapshot!(test_export, "../testdata/snapshots/export.vim");
    // snapshot!(test_matchparen, "../../shared/snapshots/matchparen.vim");

    #[test]
    fn test_simple_def() {
        let contents = r#"
            vim9script

            def MyCoolFunc(): number
              return 5
            enddef

            export var x = MyCoolFunc() + 1
        "#;

        let generated = generate(
            contents,
            ParserOpts {
                mode: ParserMode::Standalone,
            },
        )
        .unwrap();
        let eval = exec_lua(&generated.lua).unwrap();
        assert_eq!(eval["x"], 6.into());
    }

    #[test]
    fn test_builtin_func() {
        let contents = r#"
            vim9script

            export var x = len("hello")
        "#;

        let generated = generate(
            contents,
            ParserOpts {
                mode: ParserMode::Standalone,
            },
        )
        .unwrap();
        let eval = exec_lua(&generated.lua).unwrap();
        assert_eq!(eval["x"], 5.into());
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

            export var x = len(nvim_get_autocmds({group: "matchparen"}))
        "#;

        let generated = generate(
            contents,
            ParserOpts {
                mode: ParserMode::Standalone,
            },
        )
        .unwrap();
        let eval = exec_lua(&generated.lua).unwrap();
        assert_eq!(eval["x"], 4.into());
    }
}
