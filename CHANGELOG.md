# Changelog

All notable changes to this project are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project uses
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2026-11-27

### Added

- Lexical scoping with chained environments, closures, and recursion.
- `null`, bare `return`, and `.len` on strings and arrays.
- Unicode-aware string length and string indexing.
- String escapes, block comments, and source positions on tokens.
- Runtime type checking for conditions, operators, and array indexes.
- A library API (`run_source`) and a `wasm32` `run` export.
- A WebAssembly playground with examples, keyboard shortcuts, and saved source.
- Unit tests, end-to-end language tests, and a golden-output showcase test.
- A GitHub Actions CI workflow, Pages deployment, MIT license, and design notes.

### Fixed

- Division being lexed as a line comment.
- Missing operator precedence (`1 + 2 * 3` now evaluates to `7`).
- Closures and recursive calls failing because functions could not see their
  declaring scope.
- `return` continuing to execute loops and blocks after it was evaluated.
- Array bounds, division by zero, and const assignment producing panics or
  silently incorrect results.

### Changed

- Variables without an initializer now default to `null` instead of `0`.
- `print` separates multiple arguments with spaces and appends a newline.
- Removed the unused `thiserror` dependency.
- Rewrote the README with a language tour, grammar, and architecture overview.

## [0.1.0] - 2024-08-10

- Initial lexer, parser, and tree-walking interpreter.
- Variables, functions, conditionals, loops, and arrays.
