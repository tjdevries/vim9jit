use std::{fmt::Write, fs, path::Path};

fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=src/lua/");

    let luadir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crates_dir")
        .parent()
        .expect("root_dir")
        .join("lua");

    if !luadir.is_dir() {
        std::fs::create_dir_all(&luadir).expect("to create lua directory");
    }

    let mut file = String::new();

    writeln!(
        &mut file,
        r#"
-------------------------------------------------------------------------------
-- This file is auto generated by vim9jit. Do not edit by hand.
--  All content is in the source repository.
--  Bugs should be reported to: github.com/tjdevries/vim9jit
--
-- In addition, this file is considered "private" by neovim. You should
-- not expect any of the APIs, functions, etc to be stable. They are subject
-- to change at any time.
-------------------------------------------------------------------------------

"#
    )?;

    let source_lua = Path::new("src/lua");
    let mut entries = Vec::new();
    for entry in source_lua.read_dir().unwrap() {
        if let Ok(entry) = entry {
            entries.push(entry.path());
        }
    }

    // always sort `init` to the top of the list
    entries.sort_by(|a, b| {
        if a.file_stem().unwrap() == "init" {
            return std::cmp::Ordering::Less;
        }

        if b.file_stem().unwrap() == "init" {
            return std::cmp::Ordering::Greater;
        }

        a.cmp(b)
    });

    // Iterate over each of the entries and write them to the new file
    for entry in entries {
        // get entry without the extension
        let name = entry
            .file_stem()
            .expect("file_stem")
            .to_str()
            .expect("to_str");

        let contents = fs::read_to_string(&entry)?;
        let contents = contents.replace("require('_vim9script')", "vim9");
        let contents = contents.replace("require(\"_vim9script\")", "vim9");
        let contents = contents.replace("require \"_vim9script\"", "vim9");

        if name == "_" {
            writeln!(&mut file, "{}\n", contents)?;
        } else if name == "init" {
            // init.lua declares our base module and it's guaranteed to be first.
            writeln!(&mut file, "local vim9 = (function() {} end)()\n", contents)?;
        } else {
            writeln!(
                &mut file,
                "vim9['{}'] = (function() {} end)()",
                name, contents
            )?;
        }
    }

    writeln!(&mut file, "")?;
    writeln!(&mut file, "return vim9")?;

    let file = format::lua(&file).expect(&format!("to format code: {}", file));

    let init_lua = luadir.join("_vim9script.lua");
    fs::write(init_lua, file)?;

    Ok(())
}
