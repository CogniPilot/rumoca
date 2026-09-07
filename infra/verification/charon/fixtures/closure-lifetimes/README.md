# Closure lifetime reproductions

Preserved diagnostic inputs, not an adopted closure-lifetime implementation.
These files exercise the pinned Rust nightly
`1.100.0-nightly-2026-08-18` (commit
`8fa1c96cfd489e4c27654c144ae871ce2c4db6c6`) and Rumoca's pinned Charon.

| Input | Purpose |
|---|---|
| `closure-lifetime.rs` | Actual wrapper-return shape: direct, closure and iterator paths |
| `simpler-closures.rs` | Named lifetimes versus higher-ranked closure signatures |
| `preserved-signature.rs` | Higher-ranked signatures with unrelated outer lifetimes; captured-reference controls |
| `external-caller.rs` | Cross-crate calls into a `simpler_closures` rlib |
| `external-preserved.rs` | Cross-crate calls into a `preserved_signature` rlib |
| `consumer-controls.rs` | Distinct arguments, captures, paired and swapped reference results |
| `consumer-probe.rs` | rustc-dev driver checking seventeen closures, owned positional facts and refusal controls |
| `two-region-iterator.rs` | First/second captured lifetimes, reference-valued iterator items and caller controls |
| `closure-requirements-api.rs` | Deliberate compile-fail availability probe: missing `closure_requirements` field |

The consumer probe links the [closure-region candidate](../../candidates/closure-regions/README.md),
observes existing compiler APIs and delegates to the original borrow checker.
Its assertions are intentionally tied to these exact fixtures. It is **not** a
production witness issuer or modification of rustc, and cannot prove arbitrary
closure signature transport. Source/cfg/dependency binding and faithful
cross-crate transport remain unimplemented. Follow the candidate's build recipe
to provide the library and its additional region-shape fixture.

Compile the self-contained Rust test fixtures with the pinned
`rustc --test --edition=2021`; write binaries and metadata to a build-output
directory. For cross-crate controls, compile their named provider as an rlib
and pass it explicitly via `--extern`, using the same pinned sysroot.
The diagnostic executable needs matching `rustc-dev` libraries; run it as a
rustc driver over `simpler-closures.rs`, `consumer-controls.rs`,
`closure-lifetime.rs`, `two-region-iterator.rs` and the candidate's `region-shapes.rs`.
The availability probe is expected to fail with E0609 for the named field,
not with an unrelated linking or toolchain error.

No downloaded rustc sources, generated IR, compiled outputs, or abandoned
heuristic patch is retained here. No rustc bug or need for a rustc fork has
been established. Keep all unsupported cases visible.
