# sailc — a simple SAIL → Yul compiler in Rust

This is a small reference implementation of a SAIL front-end and Yul
back-end in Rust. It exists to demonstrate that the SAIL surface syntax
described in `doc/src/sail/` can be parsed and lowered to Yul; it is not
a replacement for the Haskell compiler in `src/`.

## Build

```sh
cd rust-compiler
cargo build --release
```

The binary lands at `target/release/sailc`.

## Use

```sh
# Parse only (validates the input)
./target/release/sailc --parse-only ../std/std.solc

# Compile to Yul, writing to a file
./target/release/sailc ../std/std.solc -o /tmp/std.yul

# Or stream Yul to stdout
./target/release/sailc ../std/std.solc
```

The emitted Yul is wrapped in an `object "<stem>" { code { … } object
"<stem>_runtime" { code { … } } }` skeleton accepted by
`solc --strict-assembly`.

## What is implemented

- **Lexer** — full SAIL token set (keywords, integer/hex/string literals,
  operators, comments).
- **Parser** — `import`, `export`, `pragma`, `data`, `type`, contracts,
  free functions (long & short form), class & instance declarations,
  `forall` prefixes and constraint lists, match equations with nested
  patterns, `let` bindings, expression operators with standard
  precedence, inline `assembly { ... }` blocks with the full Yul
  sub-language (`let`, `:=`, `if`, `switch/case/default`, `for`,
  `break/continue/leave`, nested `function`).
- **Codegen** — emits one Yul function per concrete free function and
  per instance method (mangled as `<Class>__<Type>__<method>`). Inline
  assembly blocks are reproduced verbatim, with shadowed SAIL locals
  uniquified to satisfy Yul's no-redeclaration rule.

## What is *not* implemented

- Full type checking, type-class instance resolution.
- Monomorphisation / specialisation of polymorphic functions
  (`forall`-prefixed definitions are skipped by the back-end; instance
  methods are emitted under a mangled name but their bodies, where
  non-assembly, become stubs).
- Match compilation (Augustsson's algorithm).
- Lowering of SAIL-level expressions and `match` statements to Yul.

The result is a syntactically valid Yul object: solc accepts it
under `--strict-assembly`, but its runtime behaviour is limited to the
parts that come straight from `assembly { ... }`.

## Verified inputs

All six files in `std/` parse and produce Yul that `solc 0.8.30`
accepts with `--strict-assembly`:

| File                  |
| --------------------- |
| `std/std.solc`        |
| `std/NumLib.solc`     |
| `std/assign.solc`     |
| `std/dispatch.solc`   |
| `std/rtdispatch.solc` |
| `std/types.solc`      |

265 of 305 example programs in `test/examples/` also parse. The 40
failures are all advanced surface features absent from `std/` (lambda
syntax, `if/then/else` *expressions*, struct-style `data` blocks).
