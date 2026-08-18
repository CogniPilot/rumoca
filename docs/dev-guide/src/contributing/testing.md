# Testing and Quality Gates

## The Verification Surface

Cargo-make provides two repository-wide verification aggregates while focused
Rust checks remain ordinary Cargo commands:

| Command | Scope |
|---|---|
| `cargo make verify-quick` | Lint, architecture checks, workspace tests, and doctests |
| `cargo make verify-full` | Complete local/CI verification DAG |
| `cargo make verify-lint` | Formatting, repository policy, and clippy |
| `cargo test --workspace` | Cargo-native workspace tests |
| `cargo make docs-build` | Both mdBook books |
| `cargo make msl-parity` | MSL parity gate |
| `cargo make verify-template-runtimes` | Generated-code execution tests |

Editor surfaces have their own gates:

```bash
cargo make vscode-test       # extension compile + tests
cargo make playground-test   # wasm build + browser smoke tests
```

`verify-full` includes coverage, VS Code, and wasm gates, so it needs the same
prerequisites CI installs: `cargo-llvm-cov`, Node/npm, and the wasm Rust
target/tooling.

## During Development

Plain Cargo works for tight loops:

```bash
cargo test -p rumoca-phase-dae
cargo test -p rumoca-phase-structural some_test_name
```

When testing failure paths, assert the *specific* phase error you expect —
the codebase's expect-vs-error discipline exists so a passing test means
the right thing failed for the right reason.

## The MSL Quality Gate

The strongest regression net is the Modelica Standard Library gate: CI
compiles and simulates a large MSL model population and compares against
recorded baselines, blocking silent regressions in compile success,
simulation success, and trace parity. Details, baseline policy, and
promotion workflow: [MSL Quality Gate](../tooling/msl-quality-gate.md).

For compiler changes that could affect MSL behavior, run the parity gate
(or at minimum `cargo make verify-quick` plus a targeted MSL model) before opening the
PR — [SPEC_0025](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0025_PR_REVIEW_PROCESS.md)
defines what evidence a PR needs.

## Coverage

```bash
cargo make coverage report
```

CI enforces a coverage gate; locally you need `cargo-llvm-cov`.

## Architecture Tests

Dependency boundaries from
[SPEC_0029](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0029_CRATE_BOUNDARIES.md)
are enforced by tests. If one fails on your change, the answer is a design
conversation (possibly a spec change) — not loosening the test.
