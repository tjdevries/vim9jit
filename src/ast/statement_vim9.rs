use crate::gen::{CodeGen, GenDB};

#[derive(Debug, Clone, PartialEq)]
pub struct StatementVim9 {}

impl CodeGen for StatementVim9 {
    fn gen(&self, _: &mut GenDB) -> String {
        "require('vim9jit')".to_string()
    }
}
