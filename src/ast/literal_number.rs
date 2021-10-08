use crate::parser::Parse;
use crate::parser::ParseResult;

pub struct LiteralNumber {
    pub value: f64,
}

impl Parse for LiteralNumber {
    fn parse(p: &mut crate::parser::Parser) -> ParseResult<Self> {
        let token = p.token();

        todo!()
    }
}
