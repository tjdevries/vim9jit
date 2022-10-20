// pub mod cmd_auto;
pub mod cmd_if;
// pub mod cmd_try;
// pub mod cmd_user;

// #[derive(Debug, PartialEq, Clone)]
// pub struct DeferCommand {
//     defer_: Token,
//     pub call: CallExpression,
// }
//
// impl DeferCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         Ok(ExCommand::Defer(DeferCommand {
//             defer_: parser.expect_identifier_with_text("defer")?,
//             call: {
//                 // Parse up to the point it would be a call expr
//                 let base = Expression::parse(parser, Precedence::Call)
//                     .expect("base")
//                     .into();
//
//                 // Create the call expr from the first base expression
//                 let right =
//                     CallExpression::parse(parser, base).expect("call").into();
//
//                 // Closing on right paren, DO NOT advance
//                 parser
//                     .expect_token(TokenKind::RightParen)
//                     .expect("rightparen");
//
//                 right
//             },
//         }))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub enum ImportCommand {
//     ImportImplicit {
//         import: Token,
//         autoload: bool,
//         file: String,
//         // Optional `as` qualifier to rename the import locally
//         name: Option<Expression>,
//     },
//     ImportUnpacked {
//         import: Token,
//         names: Vec<Identifier>,
//         from: Token,
//         file: String,
//     },
// }
//
// impl ImportCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         let import = parser.expect_identifier_with_text("import")?;
//         let command = match parser.current_token.kind {
//             TokenKind::LeftBrace => ImportCommand::ImportUnpacked {
//                 import,
//                 names: {
//                     let names =
//                         parser.parse_identifier_list(TokenKind::RightBrace)?;
//                     parser.expect_peek(TokenKind::RightBrace)?;
//                     parser.next_token();
//                     names
//                 },
//                 from: parser.expect_identifier_with_text("from")?,
//                 file: parser.pop().text.to_string(),
//             },
//             TokenKind::Identifier
//             | TokenKind::SingleQuoteString
//             | TokenKind::DoubleQuoteString => {
//                 let autoload =
//                     if parser.current_token.text.as_str() == "autoload" {
//                         parser.next_token();
//                         true
//                     } else {
//                         false
//                     };
//
//                 ImportCommand::ImportImplicit {
//                     import,
//                     autoload,
//                     file: parser.pop().text.to_string(),
//                     name: {
//                         if parser.current_token.text.as_str() == "as" {
//                             // pop as
//                             parser.pop();
//
//                             Some(Expression::parse(parser, Precedence::Lowest)?)
//                         } else {
//                             None
//                         }
//                     },
//                 }
//             }
//             _ => unreachable!("{:?}", parser),
//         };
//
//         Ok(ExCommand::ImportCommand(command))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct ExportCommand {
//     export: Token,
//     pub command: Box<ExCommand>,
// }
//
// impl ExportCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         Ok(ExCommand::ExportCommand(Self {
//             export: parser.expect_identifier_with_text("export")?,
//             command: parser.parse_command()?.into(),
//         }))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct BreakCommand {
//     pub br: Token,
//     eol: Token,
// }
//
// impl BreakCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         Ok(ExCommand::Break(BreakCommand {
//             br: parser.expect_identifier_with_text("break")?,
//             eol: parser.expect_eol()?,
//         }))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct ContinueCommand {
//     pub cont: Token,
//     eol: Token,
// }
//
// impl ContinueCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         Ok(ExCommand::Continue(ContinueCommand {
//             cont: parser.expect_identifier_with_text("continue")?,
//             eol: parser.expect_eol()?,
//         }))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct EvalCommand {
//     eval: Option<Token>,
//     pub expr: Expression,
//     eol: Token,
// }
//
// impl EvalCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         Ok(ExCommand::Eval(EvalCommand {
//             eval: None,
//             expr: Expression::parse(parser, Precedence::Lowest)?,
//             eol: parser.expect_eol()?,
//         }))
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct EchoCommand {
//     echo: Token,
//     pub expr: Expression,
//     eol: Token,
// }
//
// impl EchoCommand {
//     pub fn parse(parser: &mut Parser) -> Result<ExCommand> {
//         let echo = parser.expect_fn_token(
//             |t| matches!(t.text.as_str(), "echo" | "echon" | "echomsg"),
//             true,
//         )?;
//
//         Ok(ExCommand::Echo(EchoCommand {
//             echo,
//             expr: Expression::parse(parser, Precedence::Lowest)?,
//             eol: parser.expect_eol()?,
//         }))
//     }
// }
