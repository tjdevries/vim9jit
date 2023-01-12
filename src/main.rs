use std::{ffi::OsStr, path::Path};

use anyhow::Result;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The directory for generating paths
    #[arg(short, long)]
    dir: Option<String>,

    #[arg(long)]
    file: Option<String>,

    #[arg(short, long)]
    outdir: String,
}

fn gen_directory(src: &Path, gen: &Path, subdir: &OsStr) -> Result<()> {
    // base: /path/src/
    // subdir: autoload,
    // subdir: plugin

    dbg!(src);
    dbg!(gen);
    dbg!(subdir);

    let src_subdir = src.join(subdir);
    anyhow::ensure!(src_subdir.is_dir());

    let gen_subdir = gen.join(subdir);
    if !gen_subdir.is_dir() {
        std::fs::create_dir(&gen_subdir)?;
    }

    for f in src_subdir.read_dir()? {
        let f = f?;

        // Recursively traverse directories until complete
        if f.file_type()?.is_dir() {
            gen_directory(&src_subdir, &gen_subdir, &f.file_name())?;
            continue;
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
        let stem = Path::with_extension(Path::new(stem), "lua");
        let generated_file = gen_subdir.join(stem);

        println!("plugin: {f:?}");
        println!("  filename: {generated_file:?}");

        let generated = match gen::generate(
            &contents,
            gen::ParserOpts {
                mode: gen::ParserMode::Standalone,
            },
        ) {
            Ok(generated) => generated,
            Err(err) => {
                println!("  FAILED: {}", err.1);
                err.0
            }
        };
        std::fs::write(generated_file, generated.lua)?;
    }

    Ok(())
}

fn main() -> Result<()> {
    parser::setup_trace();
    let args = Args::parse();

    if let Some(dir) = args.dir {
        println!("dir: {dir}");

        let dir_base = Path::new(&dir);
        let dir_src = dir_base.join("src");

        let dir_gen = dir_base.join("gen");
        if !dir_gen.is_dir() {
            std::fs::create_dir(dir_gen.clone())?;
        }

        gen_directory(&dir_src, &dir_gen, OsStr::new("plugin"))?;
        gen_directory(&dir_src, &dir_gen, OsStr::new("autoload"))?;
        return Ok(());
    } else if let Some(file) = args.file {
        let path = Path::new(&file);
        if !path.is_file() {
            return Err(anyhow::anyhow!("not a valid file"));
        }

        let contents = std::fs::read_to_string(path)?;
        let generated = match gen::generate(
            &contents,
            gen::ParserOpts {
                mode: gen::ParserMode::Autoload {
                    // TODO: Calculate this correctly...
                    prefix: "ccomplete".to_string(),
                },
            },
        ) {
            Ok(generated) => generated,
            Err(err) => {
                println!("  FAILED: {}", err.1);
                err.0
            }
        };

        let file_name = path.file_name().unwrap();
        let generated_file = Path::new(&args.outdir)
            .join(file_name)
            .with_extension("lua");
        // let generated_file = Path::with_extension(path, "lua");
        println!("generated lua: {generated_file:?}");
        std::fs::write(generated_file, generated.lua)?;
        if !generated.vim.is_empty() {
            let generated_file = Path::new(&args.outdir)
                .join(file_name)
                .with_extension("vim");
            println!("generated vim: {generated_file:?}");
            std::fs::write(generated_file, generated.vim)?;
        }

        return Ok(());
    }

    Err(anyhow::anyhow!("Need to pass either --dir or --file"))
}
