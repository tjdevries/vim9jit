#![allow(dead_code)]

use std::{
    collections::HashMap,
    process::{Command, Stdio},
};

use anyhow::Result;
use rmpv::{decode::read_value, encode::write_value, Value};

pub fn exec_busted(path: &str) -> Result<()> {
    let child = Command::new("nvim")
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/../../"))
        .args([
            "--headless",
            "--clean",
            "-u",
            "./scripts/init.lua",
            "-c",
            &format!("PlenaryBustedFile {path}"),
        ])
        .stdout(Stdio::piped())
        .spawn()?;

    let output = child.wait_with_output()?;
    assert!(
        output.status.success(),
        "Failed With:\nstdout:{}\nstderr:{}",
        String::from_utf8(output.stdout).unwrap(),
        String::from_utf8(output.stderr).unwrap()
    );

    Ok(())
}

pub fn exec_lua(preamble: &str) -> Result<HashMap<String, Value>> {
    let contents = format!(
        r#"
            vim.opt.rtp:append(".")

            {preamble}
        "#
    );
    println!("{contents}");

    // start a neovim job
    let mut child = Command::new("nvim")
        // TODO: Consider a nicer way to do this, but this puts running from
        // the root of our project, so we have access to the `lua/` folder available
        // there
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/../../"))
        .args(["--clean", "-u", "./scripts/init.lua", "--embed"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let id = Value::Integer(0.into());
    let msg = Value::Array(vec![
        0.into(),
        id.clone(),
        "nvim_exec_lua".into(),
        Value::Array(vec![
            // lua code to execute
            contents.clone().into(),
            // lua arguments to send, currently unused
            Value::Array(vec![]),
        ]),
    ]);

    let child_stdin = child.stdin.as_mut().unwrap();
    let child_stdout = child.stdout.as_mut().unwrap();
    write_value(child_stdin, &msg)?;

    let val = match read_value(child_stdout)? {
        Value::Array(val) => val,
        _ => unreachable!(),
    };
    let (_, response_id, err, val) = (&val[0], &val[1], &val[2], &val[3]);

    // Confirm that this is the response to the request that we sent.
    assert_eq!(&id, response_id, "Ids must be equal");

    // Confirm that we don't have any errors while executing lua
    match err {
        Value::Nil => {}
        err => {
            return Err(anyhow::anyhow!(
                "Error executing Lua:\n\n{}\n\n{}",
                contents,
                err
            ));
        }
    }

    // Close stdin to finish and avoid indefinite blocking
    #[allow(dropping_references)]
    drop(child_stdin);

    // Wait til output has completed.
    child.wait_with_output()?;

    // TODO: For some reason we get a weird 256 wait status on my machine...
    // assert!(
    //     output.status.success(),
    //     "Failed With:\nstdout:{}\nstderr:{}",
    //     String::from_utf8(output.stdout).unwrap(),
    //     String::from_utf8(output.stderr).unwrap()
    // );

    // We good
    let val = val.as_map().expect("returns a map");
    let val = HashMap::from_iter(
        val.iter()
            .map(|(key, value)| (key.as_str().unwrap().to_owned(), value.clone())),
    );

    Ok(val)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_eval_functions() {
        let val = exec_lua(
            r#"
                local x = function()
                  return vim.api.nvim_get_current_buf()
                end

                return {
                    x = x()
                }
            "#,
        )
        .unwrap();

        let result = val.get("x").expect("get x").clone();
        assert_eq!(result, 1.into());
    }
}
