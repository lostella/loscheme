# loscheme

Run in REPL mode:

```bash
cargo run --release
```

Execute a script:

```bash
cargo run --release path/to/script.scm
```

Or [run in the browser](https://lostella.github.io/loscheme/).

To build the web page locally (requires [wasm-pack](https://rustwasm.github.io/wasm-pack/)):

```bash
web/build.sh
uv run python -m http.server -d pkg
```
