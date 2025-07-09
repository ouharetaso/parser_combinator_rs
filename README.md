# parser_combinator

A parser combinator library written in Rust, with a sample JSON parser implementation.

## Overview

This project implements parser combinators in Rust and includes a sample JSON parser.  
The core combinator logic is in `src/parser.rs`, and the JSON parser is implemented in `src/json.rs`.

## Directory Structure

```
src/
  json.rs      // JSON parser implementation
  lib.rs       // Library module
  main.rs      // CLI: parses JSON from standard input
  parser.rs    // Parser combinator implementations
Cargo.toml     // Package configuration
```

## Usage

### Build

```sh
cargo build
```

### Test

```sh
cargo test
```

### Example

Parse JSON from standard input:

```sh
echo '{"key": 123, "arr": [true, null, "str"]}' | cargo run
```

## Features

- Basic parser combinators (and_then, or_else, many0, many1, option, not, pair, etc.)
- Parsing of all JSON elements (numbers, strings, arrays, objects, booleans, null)
- Unit tests for all major components

## License

[MIT LICENSE](LICENSE)
