use std::{ffi::OsStr, path::Path};

use anyhow::Result;
use clap::Parser;

/// Simple program to greet a person
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short, long)]
    dir: String,
}

fn main() -> Result<()> {
    let args = Args::parse();
    println!("dir: {}", args.dir);

    let dir_base = Path::new(&args.dir);
    let dir_src = dir_base.join("src");

    let dir_gen = dir_base.join("gen");
    if !dir_gen.is_dir() {
        std::fs::create_dir(dir_gen.clone())?;
    }

    println!("is_dir: {:?}", dir_src.is_dir());

    let src_dir_plugin = dir_src.join("plugin");
    let gen_dir_plugin = dir_gen.join("plugin");
    if src_dir_plugin.is_dir() {
        if !gen_dir_plugin.is_dir() {
            std::fs::create_dir(&gen_dir_plugin)?;
        }

        for f in src_dir_plugin.read_dir()? {
            let f = f?;
            if f.path().extension().unwrap() != "vim" {
                continue;
            }

            let contents = std::fs::read_to_string(f.path())?;
            let path = f.path();
            let stem = path.file_stem().unwrap();
            let stem = Path::with_extension(&Path::new(stem), "lua");
            let generated_file = gen_dir_plugin.join(stem);
            println!("plugin: {:?}", f);
            // println!("  contents: {}", contents);
            println!("  filename: {:?}", generated_file);

            let generated = gen::generate(&contents, false);
            std::fs::write(generated_file, generated)?;
        }
    }

    Ok(())
}
