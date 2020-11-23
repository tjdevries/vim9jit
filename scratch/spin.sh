#!/bin/bash

# rationale:
# sometimes you want to see everything you're breaking IN REAL TIME
#
# A: ./spin.sh
# 1. safely polls for changes to ../lua/vim9jit/**.lua + ./play.lua
# 2. defers to ./play.lua to generate ./test.lpeg/*.* output
# 3. shows a git diff for these files
# 4. loops
#
# B: ./spin.sh ./test.lpeg...
# 1. same as A
# 2. same as A - by default ./play.lua still generates every output
# 3. shows a git diff only for directory passed on the command line
#
# C: ./spin.sh ./test.lpeg -- args...
# 1. same as A
# 2. script args following -- are forwarded to ./play.lua
# 3. same as B

set -e
set -u

DIFF=(
    './test.lpeg/*.lpeg'
)

for (( N=1; N<=$#; N++))
do
    ARGN="${!N}"
    if [[ ${ARGN} != '--' ]]
    then
        DIFF[$((N-1))]="${ARGN}"
    else
        shift $N
        break
    fi
done

function NEXT {
    find ./play.lua ../lua/vim9jit -type f -name '*.lua' -exec stat --format '%Y' {} + | sort -rn | head -n 1
}

PREV=0
NEXT=0

while true
do
    NEXT="$(NEXT)"
    if [[ ${NEXT} == ${PREV} ]]
    then
        sleep 1
        continue
    else
        clear
        echo ${PREV}
        echo ${NEXT}
    fi

    PREV=${NEXT}
    NEXT=0

    echo -e "\033[31m"
    ./play.sh "$@" || NEXT=$?
    echo -e "\033[39m"

    if [[ ${NEXT} == 0 ]]
    then
        git --no-pager -c 'diff.json.xfuncname="id":"([^"]+)"' \
        diff --no-ext-diff --ignore-space-change "${DIFF[@]}"
    fi
done
