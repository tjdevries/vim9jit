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

fn write_file(infile: &Path, outdir: &Path) -> Result<()> {
    println!("plugin: {infile:?}");
    let contents = std::fs::read_to_string(infile)?;

    let generated = gen::generate(
        &contents,
        gen::ParserOpts {
            mode: gen::ParserMode::from_path(infile),
        },
    )
    .unwrap_or_else(|err| {
        eprintln!("  failed to compile: {}", err.1);
        err.0
    });

    let vimfile = Path::new(infile.file_name().unwrap());

    let lua_outfile = outdir.join(vimfile).with_extension("lua");
    std::fs::write(dbg!(lua_outfile), generated.lua)?;

    if !generated.vim.is_empty() {
        let vim_outfile = Path::new(&outdir).join(vimfile).with_extension("vim");
        std::fs::write(dbg!(vim_outfile), generated.vim)?;
    }

    Ok(())
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

        write_file(&f.path(), &gen_subdir)?;
    }

    Ok(())
}

fn main() -> Result<()> {
    parser::setup_trace();
    let args = Args::parse();

    if let Some(dir) = args.dir {
        println!("dir: {dir}");

        let dir_src = Path::new(&dir);

        let dir_gen = Path::new(&args.outdir);
        if !dir_gen.is_dir() {
            std::fs::create_dir(dir_gen)?;
        }

        gen_directory(dir_src, dir_gen, OsStr::new("plugin"))?;
        gen_directory(dir_src, dir_gen, OsStr::new("autoload"))?;

        return Ok(());
    } else if let Some(file) = args.file {
        let path = Path::new(&file);
        if !path.is_file() {
            return Err(anyhow::anyhow!("not a valid file"));
        }

        return write_file(path, Path::new(&args.outdir));
    }

    Err(anyhow::anyhow!("Need to pass either --dir or --file"))
}
