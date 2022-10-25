use std::collections::HashSet;

use parser::{CallExpression, Expression, Identifier};

use crate::{Generate, State};

#[derive(Debug)]
pub struct VimFuncMutability {
    returned: Option<usize>,
    modified_args: HashSet<usize>,
}

fn expr_is_func_mutable(arg: &Expression) -> bool {
    match arg {
        Expression::Identifier(ident) => match ident {
            Identifier::Raw(_) => true,
            Identifier::Scope(_) => true,
            Identifier::Unpacked(_) => todo!(),
            Identifier::Ellipsis => true,
        },
        Expression::Grouped(_) => todo!(),
        Expression::VimOption(_) => false,
        Expression::Prefix(pre) => expr_is_func_mutable(&pre.right),
        Expression::Infix(infix) => expr_is_func_mutable(&infix.left),
        Expression::MethodCall(meth) => expr_is_func_mutable(&meth.left),
        Expression::Ternary(tern) => {
            expr_is_func_mutable(&tern.if_true)
                || expr_is_func_mutable(&tern.if_false)
        }
        Expression::Number(_) => false,
        Expression::String(_) => false,
        Expression::Boolean(_) => false,
        Expression::Call(call) => match call.args.len() {
            0 => false,
            _ => expr_is_func_mutable(&call.args[0]),
        },
        // Expression::Call(_) => false,
        Expression::Array(_) => false,
        Expression::Dict(_) => false,
        Expression::Register(_) => false,
        Expression::Lambda(_) => false,
        Expression::Expandable(_) => false,

        // TODO: These are (as of now) unhandled
        Expression::DictAccess(_) => true,
        Expression::Index(_) => true,

        // TODO: I think this is the case...
        Expression::Slice(_) => unreachable!("Slice"),
        Expression::Empty => unreachable!("Empty"),
    }
}

pub fn mutates(
    expr: &CallExpression,
    data: &FunctionData,
) -> Option<VimFuncMutability> {
    match data {
        // FunctionData::VimFunc { .. } => return None,
        _ => {}
    };

    // Check if any args can even be mutated
    //  If there are none, then it doesn't matter if the function
    //  mutates things or not.
    if !expr.args.iter().any(expr_is_func_mutable) {
        return None;
    }

    match expr.name() {
        Some(ident) => match ident {
            Identifier::Raw(raw) => match raw.name.as_str() {
                // We have overriden insert
                "add" | "insert" | "extend" => None,

                "remove" => Some(VimFuncMutability {
                    returned: None,
                    modified_args: HashSet::from_iter(vec![0].into_iter()),
                }),

                "reverse" | "sort" | "filter" => Some(VimFuncMutability {
                    returned: Some(0),
                    modified_args: HashSet::from_iter(vec![0].into_iter()),
                }),
                _ => None,
            },
            Identifier::Scope(_) => todo!(),
            Identifier::Unpacked(_) => None,
            Identifier::Ellipsis => None,
        },
        None => None,
    }
}

pub fn args_to_generated_list(
    state: &mut State,
    args: &[Expression],
) -> String {
    args.iter()
        .map(|e| e.gen(state))
        .collect::<Vec<String>>()
        .join(", ")
}

// len('hello')
// -> call expr { expr: Raw(len), args: ['hello', ] }
// -> vim func { name: len, mutability: None, args: ['hello', ] }
// -> vim.fn['len']('hello')

// function('getloclist', [0])
// -> call expr { expr: Raw(function), args: ['getloclist', [0]] }
// -> vim func ref { name: 'getloclist', arglist: [0], dict; None }
// -> function(...) deepcopy(...); vim.fn['getloclist'](...) return ... end

pub enum FunctionData {
    ApiFunc {
        name: String,
        args: Vec<Expression>,
    },
    VimFunc(VimFunc),
    VimFuncRef {
        name: Expression,
        arglist: Option<Expression>,
        dict: Option<Expression>,
    },
    GeneratedFunc {
        name: String,
        args: Vec<Expression>,
    },
    ExprFunc {
        caller: Expression,
        args: Vec<Expression>,
    },
}

impl FunctionData {
    fn name(&self) -> &str {
        match self {
            FunctionData::ApiFunc { name, .. } => name,
            FunctionData::VimFunc(vimfunc) => vimfunc.name.as_str(),
            FunctionData::VimFuncRef { .. } => todo!("VimFuncRef.name()"),
            FunctionData::GeneratedFunc { name, .. } => name,
            FunctionData::ExprFunc { .. } => todo!("ExprFunc.name()"),
        }
    }
}

pub struct VimFunc {
    name: String,
    args: Vec<Expression>,
}

impl VimFunc {
    fn inplace(
        &self,
        mutability: &VimFuncMutability,
        state: &mut State,
    ) -> String {
        let name = &self.name;
        let args = self.args.gen(state);
        let replaced = match mutability.returned {
            Some(idx) => idx.to_string(),
            None => "nil".to_string(),
        };

        // let x = self
        //     .args
        //     .iter()
        //     .enumerate()
        //     .filter_map(|(idx, expr)| match expr {
        //         Expression::Identifier(id) => Some((idx, id)),
        //         _ => None,
        //     })
        //     .collect::<Vec<_>>();

        generate_mutable_fn_call(&name, &args, &replaced)
    }
}

impl From<&CallExpression> for FunctionData {
    fn from(expr: &CallExpression) -> Self {
        match expr.expr.as_ref() {
            Expression::Identifier(id) => {
                ident_to_func_data(expr.clone(), id.clone())
            }
            Expression::DictAccess(_) | Expression::Index(_) => {
                FunctionData::ExprFunc {
                    caller: *expr.expr.clone(),
                    args: expr.args.clone(),
                }
            }
            _ => todo!("{:#?}", expr),
        }
    }
}

fn ident_to_func_data(call: CallExpression, ident: Identifier) -> FunctionData {
    match ident {
        Identifier::Raw(raw) => {
            if raw.name.to_lowercase() == raw.name {
                if raw.name.starts_with("nvim_") {
                    FunctionData::ApiFunc {
                        name: raw.name,
                        args: call.args,
                    }
                } else if raw.name == "function" {
                    FunctionData::VimFuncRef {
                        name: call.args[0].clone(),
                        arglist: call.args.get(1).cloned(),
                        dict: call.args.get(2).cloned(),
                    }
                } else {
                    FunctionData::VimFunc(VimFunc {
                        name: raw.name,
                        args: call.args,
                    })
                }
            } else {
                FunctionData::GeneratedFunc {
                    name: raw.name,
                    args: call.args,
                }
            }
        }
        Identifier::Scope(_) => FunctionData::ExprFunc {
            caller: *call.expr,
            args: call.args,
        },
        _ => todo!("{:#?}", ident),
    }
}

impl Generate for Vec<Expression> {
    fn gen(&self, state: &mut State) -> String {
        self.iter()
            .map(|e| e.gen(state))
            .collect::<Vec<String>>()
            .join(", ")
    }
}

pub fn generate(call: &CallExpression, state: &mut State) -> String {
    let func_data: FunctionData = call.into();

    match mutates(call, &func_data) {
        Some(mutability) => match func_data {
            FunctionData::ApiFunc { .. } => {}
            FunctionData::GeneratedFunc { .. } => {}
            FunctionData::VimFuncRef { .. } => todo!(),
            FunctionData::VimFunc(vimfunc) => {
                return vimfunc.inplace(&mutability, state);
            }
            // TODO: This probably should have some smarter ways of managing this
            //          but at the same time, it's nice to just assume that functions
            //          that are expressions are really just generated funcs
            FunctionData::ExprFunc { .. } => {}
        },
        None => {}
    };

    match func_data {
        FunctionData::ApiFunc { name, args } => {
            format!("vim.api['{}']({})", name, args.gen(state))
        }
        FunctionData::VimFunc(VimFunc { name, args }) => {
            format!("NVIM9.fn['{}']({})", name, args.gen(state))
        }
        FunctionData::VimFuncRef { name, arglist, .. } => match arglist {
            Some(arglist) => {
                format!(
                    r#"
                            function(...)
                              local copied = vim.deepcopy({})
                              for _, val in ipairs({{...}}) do
                                table.insert(copied, val)
                              end

                              local funcref = {}
                              if type(funcref) == "function" then
                                return funcref(unpack(copied))
                              elseif type(funcref) == "string" then
                                return vim.fn[funcref](unpack(copied))
                              else
                                error(string.format("unable to call funcref: %s", funcref))
                              end
                            end
                            "#,
                    arglist.gen(state),
                    name.gen(state)
                )
            }
            None => {
                format!(
                    r#"function(...) return vim.fn[{}](...) end"#,
                    call.args[0].gen(state),
                )
            }
        },
        FunctionData::GeneratedFunc { name, args } => {
            format!("{}({})", name, args.gen(state))
        }
        FunctionData::ExprFunc { caller, args } => {
            format!("{}({})", caller.gen(state), args.gen(state))
        }
    }
}

fn generate_mutable_fn_call(name: &str, args: &str, replace: &str) -> String {
    return format!(
        "NVIM9.fn_mut('{name}', {{ {args} }}, {{ replace = {replace} }})"
    );
}

pub fn generate_method(
    method: &parser::MethodCall,
    state: &mut State,
) -> String {
    let mut call = *method.right.clone();
    call.args.insert(0, *method.left.clone());

    let func_data: FunctionData = (&call).into();
    if expr_is_func_mutable(&method.left) {
        let mutability = mutates(&call, &func_data);
        if let Some(mutability) = mutability {
            if mutability.returned == Some(0)
                && mutability.modified_args.contains(&0)
            {
                let name = func_data.name();
                let args = call.args.gen(state);
                let replace = mutability.returned.unwrap().to_string();
                return generate_mutable_fn_call(&name, &args, &replace);
            }
        }
    }

    let mut expr = method.right.clone();
    expr.args.insert(0, *method.left.clone());

    expr.gen(state)
}
