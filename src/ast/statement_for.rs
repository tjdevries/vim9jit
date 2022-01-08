use super::Expression;
use super::Identifier;
use super::Statement;
use crate::lexer::Token;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct StatementFor {
    open: Token,
    close: Token,

    pub args: Expression,
    // TODO: SHould be not this
    in_: Identifier,
    pub iterator: Expression,
    // TODO: Vec
    // pub body: Box<Statement>,
    // end:
}

impl Parse for StatementFor {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        // panic!("Statement For: {:?}", p.token());
        // p.next_token();

        let open = p.token();
        let args = p.parse()?;
        dbg!(&args);
        let in_ = p.parse()?;
        dbg!(&in_);
        let iterator = p.parse()?;
        dbg!(&iterator);

        // panic!("We got iterator: {:?}", iterator);

        Ok(Self {
            open,
            args,
            in_,
            iterator,
            // body: ,
            close: p.expect(TokenKind::CommandEndFor)?,
        })
    }
}
