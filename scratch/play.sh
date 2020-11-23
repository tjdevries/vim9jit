#!/bin/bash

set -e
set -u

LRS="${HOME}/.luarocks/share/lua/5.1"
LRL="${HOME}/.luarocks/lib/lua/5.1"

export LUA_PATH=$(tr '\n' ';' <<__
../lua/?.lua
../lua/?/init.lua
$(luarocks path --lr-path)
__
)

export LUA_CPATH=$(luarocks path --lr-cpath)

# exec lua5.1 ./play.lua "$@"
exec luajit ./play.lua "$@"
