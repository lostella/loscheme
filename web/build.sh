#!/bin/sh
# Build the web page into pkg/: the wasm module, index.html and the example scripts.
set -eu
cd "$(dirname "$0")/.."

wasm-pack build --target web --features web
cp web/index.html web/worker.js pkg/

mkdir -p pkg/scripts
cp scripts/*.scm pkg/scripts/
(
    cd scripts
    printf '['
    sep=''
    for f in *.scm; do
        printf '%s"%s"' "$sep" "${f%.scm}"
        sep=','
    done
    printf ']\n'
) > pkg/scripts/index.json
