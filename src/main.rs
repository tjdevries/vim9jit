use std::path::Path;

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

fn gen_directory(src: &Path, gen: &Path, subdir: &str) -> Result<()> {
    // base: /path/src/
    // subdir: autoload,
    // subdir: plugin

    let src_subdir = src.join(subdir);
    anyhow::ensure!(src_subdir.is_dir());

    let gen_subdir = gen.join(subdir);
    if !gen_subdir.is_dir() {
        std::fs::create_dir(&gen_subdir)?;
    }

    for f in src_subdir.read_dir()? {
        let f = f?;
        if f.file_type()?.is_dir() {
            gen_directory(src, gen, f.path().to_str().unwrap())?
        }

        match f.path().extension() {
            Some(ext) => {
                if ext != "vim" {
                    continue;
                }
            }
            None => continue,
        }

        let contents = std::fs::read_to_string(f.path())?;
        let path = f.path();
        let stem = path.file_stem().unwrap();
        let stem = Path::with_extension(&Path::new(stem), "lua");
        let generated_file = gen_subdir.join(stem);

        println!("plugin: {:?}", f);
        println!("  filename: {:?}", generated_file);

        let generated = gen::generate(&contents, false);
        std::fs::write(generated_file, generated)?;

        // TODO: Go DEEPER
    }

    Ok(())
}

fn main() -> Result<()> {
    parser::setup_trace();
    let args = Args::parse();
    println!("dir: {}", args.dir);

    let dir_base = Path::new(&args.dir);
    let dir_src = dir_base.join("src");

    let dir_gen = dir_base.join("gen");
    if !dir_gen.is_dir() {
        std::fs::create_dir(dir_gen.clone())?;
    }

    gen_directory(&dir_src, &dir_gen, "plugin")?;
    gen_directory(&dir_src, &dir_gen, "autoload")?;

    Ok(())
}
