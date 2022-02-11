use super::Expression;
use super::Identifier;
use super::Statement;
use crate::gen::CodeGen;
use crate::gen::GenDB;
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
    pub body: Vec<Statement>,
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

        let mut body = Vec::new();
        loop {
            // Keep looping until we encounter an enddef
            match p.parse::<Statement>() {
                Ok(statement) => body.push(statement),
                Err(err) => match p.token().kind {
                    TokenKind::CommandEndFor => {
                        break;
                    }
                    _ => return Err(err),
                },
            }
        }

        Ok(Self {
            open,
            args,
            in_,
            iterator,
            body,
            close: p.token(),
        })
    }
}

impl CodeGen for StatementFor {
    fn gen(&self, db: &mut GenDB) -> String {
        match &self.args {
            Expression::Identifier(identifier) => db.add_var(identifier.clone(), None, self.iterator.clone()),
            _ => {}
        }

        format!(
            r#"for _, {} in ipairs({}) do
  {}
end
"#,
            self.args.gen(db),
            self.iterator.gen(db),
            self.body
                .iter()
                .map(|s| s.gen(db))
                .collect::<Vec<String>>()
                .join("\n")
                .to_string()
        )
    }
}
