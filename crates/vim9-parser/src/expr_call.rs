use anyhow::Result;
use vim9_lexer::Token;
use vim9_lexer::TokenKind;

use crate::CallCommand;
use crate::Expression;
use crate::Identifier;
use crate::Parser;

#[derive(PartialEq, Clone)]
pub struct CallExpression {
    pub expr: Box<Expression>,
    open: Token,
    pub args: Vec<Expression>,
    close: Token,
}

impl CallExpression {
    pub fn name(&self) -> Option<&Identifier> {
        match self.expr.as_ref() {
            Expression::Identifier(ident) => Some(ident),
            _ => None,
        }
    }

    pub fn parse(
        parser: &mut Parser,
        left: Box<Expression>,
    ) -> Result<CallExpression> {
        Ok(CallExpression {
            expr: left,
            open: parser.ensure_token(TokenKind::LeftParen)?,
            args: parser.parse_expression_list(TokenKind::RightParen, false)?,
            close: parser.expect_peek(TokenKind::RightParen)?,
        })
    }
}

impl Into<CallExpression> for &CallCommand {
    fn into(self) -> CallExpression {
        CallExpression {
            expr: Box::new(Expression::Identifier(self.name.clone())),
            open: self.open.clone(),
            args: self.args.clone(),
            close: self.close.clone(),
        }
    }
}
