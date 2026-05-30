# Crate Boundaries

Compiler crate boundaries are normative in `spec/SPEC_0029_CRATE_BOUNDARIES.md`.
Read that spec before changing dependencies.

The short version:

- foundation types live in low-level crates
- phases depend forward through explicit IR contracts
- tools use compiler facades instead of reaching into internals
- backend/runtime crates stay thin around shared solver and execution APIs

Keep target-language-specific behavior out of Rust phase logic. Code generation
targets should describe language-specific output through `target.toml` and
templates.

Architecture tests enforce important dependency edges. If a dependency feels
necessary but violates a spec, update the spec first instead of bypassing the
test.
