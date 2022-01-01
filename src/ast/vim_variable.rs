use super::Identifier;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub enum VimVariableScope {
    Global,
    Tab,
    Window,
    Buffer,
    Script,
    Local,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VimVariable {
    pub scope: VimVariableScope,
    pub identifier: Identifier,
}
impl Parse for VimVariable {
    fn parse(_p: &mut Parser) -> ParseResult<Self> {
        todo!()
        // let token = p.next_token();
        // match token.kind {
        //     TokenKind::Identifier => Ok(Self { name: token.text }),
        //     _ => panic!("NOPE"),
        // }
    }
}
