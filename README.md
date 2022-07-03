# Teeny BASIC

My implementation of the Teeny BASIC-to-C compiler described in [this blog series][orig-blog],
with minor additions and adjustments.

## Building

```shell
cargo build --release
```

## Usage

```shell
target/release/teeny-basic script.bas
```

## Teeny BASIC grammar

See [grammar.md](./grammar.md).

[orig-blog]: https://austinhenley.com/blog/teenytinycompiler1.html
