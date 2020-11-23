#!/bin/bash

set -e
set -u

export LUA_PATH=$(tr '\n' ';' <<__
../lua/?.lua
../lua/?/init.lua
${LUA_PATH:-}
__
)

exec lua ./play.lua "$@"
