use super::Identifier;
use super::Statement;
use crate::gen::CodeGen;
use crate::lexer::TokenKind;
use crate::parser::Parse;
use crate::parser::ParseResult;
use crate::parser::Parser;

#[derive(Debug, Clone, PartialEq)]
pub struct StatementDef {
    pub name: Identifier,
    // pub args: Vec<Identifier>,
    pub body: Vec<Statement>,
}

impl Parse for StatementDef {
    fn parse(p: &mut Parser) -> ParseResult<Self> {
        let name = p.parse()?;

        // Left Paren, Right Paren (TODO: Args)
        p.expect(&TokenKind::LeftParen)?;
        p.expect(&TokenKind::RightParen)?;

        // Return type :
        p.expect(&TokenKind::Colon)?;

        // Return type :
        p.expect(&TokenKind::Identifier)?;

        // panic!("Current State: {:?}", p.token());
        let mut body = Vec::new();
        loop {
            // Keep looping until we encounter an enddef
            match p.parse() {
                Ok(statement) => body.push(statement),
                Err(err) => match p.token().kind {
                    TokenKind::CommandEndDef => {
                        break;
                    }
                    _ => return Err(err),
                },
            }
        }

        Ok(StatementDef {
            name,
            body
            // TODO:
            // args: p.parse()?,
        })
    }
}

impl CodeGen for StatementDef {
    fn gen(&self, db: &mut crate::gen::GenDB) -> String {
        format!(
            r#"local {} = function()
  {}
end"#,
            self.name.gen(db),
            self.body
                .iter()
                .map(|s| s.gen(db))
                .collect::<Vec<String>>()
                .join("\n")
                .to_string()
        )
    }
}
