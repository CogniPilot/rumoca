# Ordinary periodic trigger memory

The integrated scheduling role and trigger-memory admission used different
predicates. A Boolean periodic task can have scheduled clock metadata without
MLS16 variable ownership. The old memory admission therefore discarded its
exact ordinary branch edge.

The fix reuses `is_ordinary_target(clocks, target.variable)` at that admission
site. It forwards the same branch condition IDs and preserves synchronous
ownership exclusions, the existing clock schedule, and pulse memory semantics.
This follows SOLVE-C58 and MLS §8.3.5.1/§8.5; synchronous ownership remains
separate under §16.2.1 (CLK-005/006). No IR schema or condition construction changed.

The two constructor controls use the existing checked Boolean task projection
`Or(Clock, Not(Always))` and a genuine clock-owned negative. Both prove periodic
task metadata; only the ordinary exact trigger slot must have a
`PulseConditionMemory` row. This is an IR admission proof, not full authored
sample-alias runtime evidence.

- Corrected BEFORE: **1 pass / 1 fail**, missing the ordinary exact memory row.
- Dirty AFTER: **2 pass**, scoped all-target/all-feature Clippy and fmt pass.
- Frozen `40243d817`: **2 pass**; complete phase-solve **197 pass / 1 fail**.
  The existing unconditional discrete derivative test expects a refusal, but
  actual lowering succeeds. Its governing acceptance boundary is under
  investigation; no full-suite pass is claimed.
- The initial direct-Clock ordinary fixture was refused before lowering with
  `MissingClockOwnership`. Its immutable receipt is retained as a harness
  error, distinct from the corrected BEFORE counterexample.

Exact selector: `cargo test -p rumoca-phase-solve --lib ordinary_periodic_trigger`.
`receipt.json` binds commands, source/diff manifests, raw logs and guardians,
counts and resource data. Every attempt retained the 600-second, 4-GiB aggregate
resident-plus-swap, 16-GiB host reserve, 8-GiB target/floor limits and 4/4/4
parallelism. Source guards held; no resource stop occurred. The two focused
controls are included in the 198-test suite, not additional unique tests.
