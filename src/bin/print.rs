use rustyline::Editor;
use vim9jit::gen::to_lua;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                println!("Lua:\n{}", to_lua(line.as_str()));
            }
            _ => {
                break;
            }
        }
    }
}
