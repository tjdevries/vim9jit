use rustyline::completion::Completer;
use rustyline::highlight::Highlighter;
use rustyline::hint::Hinter;
use rustyline::validate::Validator;
use rustyline::Editor;
use rustyline::Helper;
use stylua_lib::Config;
use vim9jit::gen::to_lua;

pub struct SemiColon {}

impl Completer for SemiColon {
    type Candidate = String;
}
impl Hinter for SemiColon {
    type Hint = String;
}
impl Highlighter for SemiColon {}
impl Validator for SemiColon {
    fn validate(
        &self,
        ctx: &mut rustyline::validate::ValidationContext,
    ) -> rustyline::Result<rustyline::validate::ValidationResult> {
        let _ = ctx;
        let input = ctx.input();
        if input.to_string().contains(";") {
            Ok(rustyline::validate::ValidationResult::Valid(None))
        } else {
            Ok(rustyline::validate::ValidationResult::Incomplete)
        }
    }

    fn validate_while_typing(&self) -> bool {
        false
    }
}
impl Helper for SemiColon {}

fn main() {
    println!("=================================================================================");
    println!("To send something to be evaluated, you need to end your message with a `;`");
    println!("=================================================================================");
    let config = Config::default();

    let mut rl = Editor::new();
    // rl.config_mut()
    rl.set_helper(Some(SemiColon {}));
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let line = line.replace(";", "");

                let contents = to_lua(line.as_str());
                let contents = contents.replace("require('vim9jit')", "");
                let contents =
                    stylua_lib::format_code(&contents, config, None, stylua_lib::OutputVerification::None).unwrap();

                // let contents = contents.lines().map(|l| format!("  {}", l)).collect::<String>();
                println!("{}", contents);
            }
            _ => {
                break;
            }
        }
    }
}
