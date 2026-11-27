# Design notes

This document explains how Adib Lang is put together and why the implementation
makes the choices it does. It is meant for anyone reading the source, not for
people learning the language itself — the [README](../README.md) covers that.

## Goals

Adib Lang is a small language, but it is not a toy parser. The goals are:

- A complete, hand-written pipeline: source → tokens → AST → execution.
- Real language semantics: lexical scope, closures, recursion, and structured
  control flow.
- Clear diagnostics instead of panics.
- One implementation that runs both natively and in the browser.
- A codebase small enough to read in an afternoon.

Non-goals are objects, a garbage collector, libraries, and performance parity
with production runtimes. Those are intentionally left for later.

## Pipeline

```
source
  │
  ▼
Lexer ── tokens (kind, value, line, column)
  │
  ▼
Parser ── AST
  │
  ▼
Interpreter ── values, environments, output
```

Each stage is a separate module:

| File | Responsibility |
| --- | --- |
| `src/lexer.rs` | Source text to tokens, comments, strings, numbers, positions |
| `src/ast.rs` | AST node definitions and source-spanned nodes |
| `src/parser.rs` | Tokens to AST, precedence climbing, statement grammar |
| `src/interpreter.rs` | Tree walking, control flow, runtime errors |
| `src/value.rs` | Runtime values, function values, value formatting, index validation |
| `src/environment.rs` | Scope chain, bindings, constants, closure environments |
| `src/builtins.rs` | Standard-library dispatch and implementations |
| `src/error.rs` | Structured diagnostics and source rendering |
| `src/lib.rs` | Embedding API (`run_source`) and the `wasm-bindgen` export |
| `src/main.rs` | CLI wrapper around the library |
| `web/` | Static playground that loads the wasm build |

## Lexer

The lexer loads the whole source file into a `Vec<char>` and walks it with a
cursor. Loading everything at once keeps the code simple and makes lookahead
trivial. The previous streaming implementation had to deal with tokens split
across chunk boundaries; for a scripting language the memory trade-off is not
worth that complexity.

A few details matter:

- **Positions are captured before consuming a token.** Every token carries a
  1-based line and column, which the parser and interpreter reuse in errors.
- **Comments are handled during whitespace skipping.** Both `//` and `/* */`
  are supported. Division is lexed normally; the lexer only treats `/` as a
  comment when the next character is `/` or `*`.
- **Keywords live in a perfect-hash map** (`phf`), so keyword lookup is a
  compile-time-generated hash rather than a chain of string comparisons.
- **Strings support escapes** for `\n`, `\t`, `\r`, `\0`, `\\`, `\"`, and `\'`.
- **Numbers accept an optional fractional part**, but only when the dot is
  followed by a digit. That keeps `value.len` and `1.5` unambiguous.

## Parser

The parser is a recursive-descent parser with precedence climbing for binary
operators. The precedence chain is one function per level:

```
or → and → equality → comparison → addition → multiplication → unary → postfix → primary
```

This is the classic structure used by hand-written expression parsers. Each
level consumes its own operator and delegates to the next tighter level, so
`1 + 2 * 3` naturally becomes `1 + (2 * 3)` without a separate precedence table.

Postfix access is handled in a single loop, which allows chains such as
`matrix[row][column]`, `value.len`, and `get_value()[0]`. Calls are restricted
to identifiers (`foo(...)`), which keeps the AST simple: a `FunctionCall` stores
a name rather than an arbitrary callee expression. Functions are still
first-class values — an identifier bound to a function can be called.

Statements are deliberately small: declarations, assignment, calls, `if`,
`while`, `for`, `return`, `break`, `continue`, function declarations, and blocks.
Semicolons are accepted as empty statements, so programs can use either newlines
or semicolons.

Assignment targets are restricted to variables and array elements. The parser
flattens nested index targets such as `grid[row][column]` into a root name plus a
list of index expressions, which the interpreter can evaluate once and then use
for both reads and writes. Every statement and every operator expression is
wrapped in a source span, so runtime failures can point at the exact operator
rather than only the enclosing statement.

## Values

```rust
enum Value {
    Null,
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Rc<FunctionValue>),
    Array(Vec<Value>),
}
```

Numbers are `f64`, matching the JavaScript flavor of the language. Arrays are
owned vectors and are copied when assigned, so the language has value semantics
rather than JavaScript-style references. Index assignment clones the root array,
walks the index path, writes the new value, and stores the result back into the
binding. This keeps the runtime simple, but it means large array updates are
copies rather than in-place mutations.

A `const` binding cannot be rebound, but its array contents can still be updated
through index assignment. The environment therefore tracks const-ness separately
from the stored value.

Functions are reference-counted because they can be captured by closures and
stored in environments.

`Value` implements `PartialEq` manually. Numbers, strings, booleans, null, and
arrays compare structurally; functions compare by pointer identity.

## Standard library

Built-ins are dispatched by name before user-defined functions, so `print`,
`push`, or `sqrt` always resolve to the runtime implementation. They evaluate
their arguments in the caller's environment and return ordinary `Value`s.

The array helpers follow the language's value semantics: `push(array, value)`
and `pop(array)` return new arrays rather than mutating their argument. This is
less convenient than JavaScript's reference semantics, but it keeps the runtime
free of shared mutable containers and the cycle problems that come with them.

## Environments and closures

Variables live in a chain of environments:

```rust
struct Environment {
    values: HashMap<String, Value>,
    constants: HashSet<String>,
    parent: Option<Rc<RefCell<Environment>>>,
}
```

A lookup walks from the current environment up through its parents. Assignment
does the same but mutates the binding it finds. This gives:

- **Lexical scope**: a block creates a child environment, so declarations inside
  `{ ... }` do not leak out.
- **Closures**: a function stores the environment where it was declared. Calling
  it creates a child of that captured environment, which keeps captured
  variables alive as long as the function exists.
- **Recursion**: a function is inserted into its declaring environment before it
  can be called, so its own name resolves through the closure.
- **Constants**: `const` bindings are tracked separately and reject assignment
  at runtime.

`Rc<RefCell<_>>` is the simplest way to model shared mutable scopes in a
single-threaded interpreter. It is not thread-safe, and it does not collect
cycles. That is acceptable for this project; a production interpreter would
likely use an arena or a bytecode compiler with explicit slots.

## Control flow

`return`, `break`, and `continue` cannot be implemented as boolean flags,
because they have to escape nested blocks, loops, and `if` branches in the right
order. The interpreter uses an explicit control-flow value:

```rust
enum Flow {
    Normal,
    Return(Value),
    Break(Option<Position>),
    Continue(Option<Position>),
}
```

Every statement returns a `Flow`. Blocks stop executing once a non-normal flow
is produced; loops catch `Break` and `Continue`; function calls consume
`Return` and turn it into the call's result. `Break` and `Continue` carry their
source position so an illegal use outside a loop can still produce a caret. A
`break` or `continue` that escapes a function is reported as an error instead of
unwinding the caller's loop.

Conditions are strict booleans. Rather than treating every non-zero value as
true, the interpreter reports a type error with the actual type, which makes
mistakes easier to find.

## Errors

There are three error surfaces:

- **Lexer errors** for unexpected characters, unterminated strings, and
  unterminated block comments.
- **Parser errors** for unexpected tokens, with the offending token position.
- **Runtime errors** for undefined names, type mismatches, division by zero,
  out-of-bounds access, wrong argument counts, and const assignment.

All three produce a structured `Diagnostic` containing a message, an optional
source position, and a call stack. `Diagnostic::render` turns that into:

```
error: division by zero
  --> 2:12
  |
2 |   return a / b
  |            ^

stack trace:
  at `divide` (5:7)
```

The CLI prints partial output before reporting a runtime error, which matters
when a program prints during a loop and then fails. Parser errors name both the
expected and found token, for example ``expected `)`, found `{` ``.

## WebAssembly

The library is a `cdylib` and exports one function on `wasm32`:

```rust
#[wasm_bindgen]
pub fn run(source: &str) -> String
```

It returns the program's combined output, or an error string prefixed with
`error:`. The playground imports that function, runs it on the main thread, and
renders the result. No server is involved; the interpreter itself is the wasm
module.

The Pages workflow builds the package with `wasm-pack`, uploads `web/` as a
Pages artifact, and deploys it. That keeps the playground in sync with the
interpreter source instead of committing generated wasm files.

## Testing

The test suite has three layers:

1. **Unit tests** next to the lexer, parser, and interpreter.
2. **Language tests** in `tests/language.rs` that compile and execute real
   programs, including assignment forms, `break`/`continue`, standard-library
   functions, diagnostics, error cases, and every example.
3. **A golden-output test** for `examples/showcase.adib`, a 300+ line program
   that exercises recursion, closures, arrays, matrices, sorting, searching,
   strings, mutation, and control flow.

CI runs `cargo fmt --check`, `cargo clippy -- -D warnings`, and `cargo test`.
The playground build is verified separately by the Pages workflow.

## Trade-offs and future work

The current design optimizes for readability:

- **Tree walking is simple, not fast.** A bytecode compiler and VM would be the
  next performance step.
- **Values are cloned on lookup.** Small arrays and strings make this fine; a
  larger runtime would use references or an arena. Index assignment also copies
  the root array before writing it back.
- **Assignment targets are rooted at variables.** `arr[i] = x` works, but
  `make_array()[i] = x` does not, because the language has no reference values.
- **Function calls are named.** Supporting arbitrary callee expressions would
  require changing `FunctionCall` to hold an `Expression`.
- **No objects or maps yet.** They are the largest missing language feature.
- **No cycle collection.** Closures that capture themselves can leak.

The roadmap in the README tracks the features that would move the project from
a strong learning project toward a more complete language.
