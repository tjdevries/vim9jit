vim9script

execute "normal ixxx\<Esc>"
execute "!ls " .. shellescape('./rustfmt.toml', 1)
execute "!ls " .. shellescape('./rustfmt.toml', 1) .. shellescape('./Cargo.toml', 1)
execute "!ls" .. " -la"
