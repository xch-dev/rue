# Getting Started

Rue is a pure functional programming language that compiles to [CLVM](https://chialisp.com/clvm). It's strictly typed to prevent common mistakes.

## Installation

Clone the repository and `cd` into it:

```bash
git clone https://github.com/rigidity/rue
cd rue
```

Then you can install the CLI with Cargo:

```bash
cargo install --path crates/rue-cli
```

You should now have the `rue` CLI command available.

## Hello World

This is the classic hello world example in Rue:

```rue
fun main() -> Bytes {
    "Hello, world!"
}
```
