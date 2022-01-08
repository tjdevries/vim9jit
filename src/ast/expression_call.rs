use super::Expression;
use crate::gen::CodeGen;
use crate::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    // TODO: Maybe I should make sure that this always is a literal?
    //  or perhaps a different type for FunctionCall -> LiteralFunctionCall
    //  and then other types for other kinds... that seems pretty reasonable
    pub function: Box<Expression>,
    // token: Token,
    pub args: Vec<Expression>,
    // pub rparen: Token,
}

// impl FunctionCall {}

impl CodeGen for FunctionCall {
    fn gen(&self, db: &mut crate::gen::GenDB) -> String {
        let args = self.args.iter().map(|e| e.gen(db)).collect::<Vec<String>>();
        format!(r#"vim.fn["{}"]({})"#, self.function.gen(db), args.join(","))
    }
}
