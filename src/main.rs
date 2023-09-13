use std::path::Path;

use anyhow::{bail, ensure, Context, Result};
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    file: String,

    #[arg(short, long)]
    outdir: String,
}

fn main() -> Result<()> {
    parser::setup_trace();
    let args = Args::parse();

    let filepath = Path::new(&args.file);
    ensure!(filepath.is_file(), "not a valid file");

    let contents = std::fs::read_to_string(filepath)?;
    let generated = gen::generate(
        &contents,
        gen::ParserOpts {
            mode: gen::ParserMode::Autoload {
                // TODO: Calculate this correctly...
                prefix: "ccomplete".to_string(),
            },
        },
    )
    .unwrap_or_else(|err| {
        println!("  FAILED: {}", err.1);
        err.0
    });

    match std::fs::metadata(&args.outdir) {
        Ok(m) if m.is_dir() => {}
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => std::fs::create_dir(&args.outdir)?,

        Ok(_) => bail!("Can't create directory: a file with the same name already exists!"),
        Err(e) => Err(e).context("Failed to check out_dir")?,
    }

    let file_name = filepath.file_name().unwrap();
    let generated_file = Path::new(&args.outdir)
        .join(file_name)
        .with_extension("lua");
    println!("generated lua: {generated_file:?}");
    std::fs::write(generated_file, generated.lua)?;
    if !generated.vim.is_empty() {
        let generated_file = Path::new(&args.outdir)
            .join(file_name)
            .with_extension("vim");
        println!("generated vim: {generated_file:?}");
        std::fs::write(generated_file, generated.vim)?;
    }

    Ok(())
}
