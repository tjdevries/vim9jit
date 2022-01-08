use rustyline::Editor;
use vim9jit::gen::to_lua;

fn main() {
    let mut rl = Editor::<()>::new();
    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                let contents = to_lua(line.as_str());
                let contents = contents.replace("require('vim9jit')", "");
                let contents = contents.lines().map(|l| format!("  {}", l)).collect::<String>();
                println!("Lua:\n{}", contents);
            }
            _ => {
                break;
            }
        }
    }
}
