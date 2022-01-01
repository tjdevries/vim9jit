use crate::gen::CodeGen;

#[derive(Debug, Clone, PartialEq)]
pub struct StatementVim9 {}

impl CodeGen for StatementVim9 {
    fn gen(&self) -> String {
        "require('vim9jit')".to_string()
    }
}
