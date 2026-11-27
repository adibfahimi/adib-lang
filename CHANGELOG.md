# Changelog

All notable changes to this project are documented here. The format is based on
[Keep a Changelog](https://keepachangelog.com/en/1.1.0/), and this project uses
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.3.0] - 2026-11-27

### Added

- `break` and `continue`, including correct propagation through nested blocks
  and loops.
- Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`) and standalone `++`/`--`.
- Index assignment for arrays and nested arrays, including const array contents.
- A standard library: `len`, `push`, `pop`, `last`, `slice`, `join`, `split`,
  `to_string`, `parse_number`, `abs`, `floor`, `ceil`, `round`, `sqrt`, `min`,
  `max`, and `type_of`.
- Structured diagnostics with source lines, carets, and function stack traces.
- Source spans on statements and operator expressions.
- Parser errors that name both the expected and found token.

### Changed

- Errors are now `Diagnostic` values rendered against the original source.
- The playground has examples for mutation, control flow, and the standard
  library.

### Fixed

- Runtime errors now point at the failing operator instead of only the enclosing
  statement.
- `break` and `continue` escaping a function are reported instead of unwinding
  the caller's loop.

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
