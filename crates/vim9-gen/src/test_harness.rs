use std::process::Command;
use std::process::Stdio;

use anyhow::Result;
use rmpv::decode::read_value;
use rmpv::encode::write_value;
use rmpv::Value;

pub fn exec_lua(preamble: &str, result: &str) -> Result<Value> {
    let contents = format!(
        r#"
            return (function()
                {}
                return {}
            end)()
        "#,
        preamble, result
    );
    println!("{}", contents);

    // start a neovim job
    let mut child = Command::new("nvim")
        .args(["--clean", "--embed"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;

    let id = Value::Integer(0.into());
    let msg = Value::Array(
        vec![
            0.into(),
            id.clone(),
            "nvim_exec_lua".into(),
            Value::Array(
                vec![
                    // lua code to execute
                    contents.clone().into(),
                    // lua arguments to send, currently unused
                    Value::Array(vec![]),
                ]
                .into(),
            ),
        ]
        .into(),
    );

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
            return Err(anyhow::anyhow!("Error executing Lua:\n\n{}\n\n{}", contents, err));
        }
    }

    // Close stdin to finish and avoid indefinite blocking
    drop(child_stdin);

    // Wait til output has completed.
    let output = child.wait_with_output()?;
    assert!(output.status.success());

    // We good
    Ok(val.clone())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn can_eval_numbers() {
        assert_eq!(exec_lua("", "1").unwrap(), 1.into());
    }

    #[test]
    fn can_eval_functions() {
        assert_eq!(
            exec_lua(
                r#"
local x = function()
  return vim.api.nvim_get_current_buf()
end"#,
                "x()"
            )
            .unwrap(),
            1.into()
        );
    }
}
