# Internal false condition memory

Actual `614109f2eab2b3702ab32de2f65a546caf6ae4dc` periodic regression
execution passed fifteen tests and failed two. The exact sample-alias test
failed before its runtime checks: it expected one ordinary condition-memory
row for the unrelated time relation, and received two. The independent
zero-phase initialization tick failure belongs to a separate runtime repair.

The first source divergence is ordinary Boolean sample lowering in
`WhenLowering::lower_condition`: its existing checked activation is
`Or(Clock, Not(Always))`, preserving ordinary event ownership rather than a
synchronous value partition. `lower_condition_memory` omits the internal
`Always` refresh but emits a regular refresh row for its negation. That
negation is false at every instant and every initialization pass. Its already
zeroed memory slot represents the same truth without a refresh row.

The candidate fixes that schedule owner. A per-lowering memoized iterative checked-node query
accepts only a chain of `Not` nodes ending in the internal `Always`, and omits
the row only for an odd number of negations. It reads no source expression,
initial flag, relation or clock. It leaves the checked layout and memory slot
identity intact and changes no wire/schema. Authored literal Boolean `when`
conditions are `Discrete` nodes and retain their buffers. A non-false internal
chain, `Initial`, `AnyRise` and the pulse activation retain their existing
memory discipline. No new evaluator, flag or model-name exception is added.

This preserves the accepted SPEC0007 independent condition system and its
catalog distinction between B.1c, condition memory and clock ownership;
SPEC0022 EQN-023/EQN-029 and MLS §8.3.5.1/§8.5 govern real activation edges
and pulse buffers. The always-false internal expression has no rising edge.
The existing source `when true`, initial/vector and unconditional time tests
remain controls, not normalization targets.

Test-only BEFORE commit `e5d3257e5cff9e84637f8058aa69d085defe0b38`
adds a constructor-issued control with one and three internal negations,
a non-false double negation, authored true/false conditions, Initial and a
vector activation. It asserts exact refresh targets, preserving all real
activation owners. It and the production candidate are **unrun** here.
The unchanged original sample-alias regression remains the public proof.

Focused commands (one constructor control, one original public control):

```sh
cargo test --locked --offline --jobs 4 -p rumoca-phase-solve --lib \
  tests::events::constant_condition_memory::internal_false_condition_omits_only_its_redundant_memory_row \
  -- --exact --nocapture
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  periodic_source_counter_regression::exact_sample_alias_fires_every_tick_and_does_not_allocate_condition_memory \
  -- --exact --nocapture
# Existing source when true and initial/vector controls; 32 tests at 614109.
cargo test --locked --offline --jobs 4 -p rumoca --test suite_core \
  time_event_when_activation:: -- --nocapture
```

Actual BEFORE bindings, all SHA256, under attempt
`pr427-combined-arm-owners-full-controls-614109-20261012`:

- recipe: `f9b4dfbc37183d4db3146a7a605febf72e1ab2160c84d9a9e8ec0f2a001b41ce`
- periodic log: `0682ee1ed1a2dad4ffdf4e641e78fcfcb55fd4c7185bf2ae90e5e81054d38c47`
- periodic guardian: `903d1a8ce4c4247a1b805d98cbbbe7d0b3214538e715c104c85e8a46f81054eb`

The preceding unconditional time suite passed all 32 tests on the same
source; neither that result nor source review qualifies this candidate's
new refresh schedule. No runtime or cohort success is claimed for it.

The followup caches both proved Boolean constants and unknown nodes in a
private deterministic map. Each reachable internal chain node is inspected
once per lowering, with O(n log n) map work and O(n) scratch storage; condition
IDs need not be topological. The construction owner requires defined operands
and defines each node once, so the issued graph is acyclic. A direct 1024-edge
shared-chain control reserves IDs first and defines them in reverse index
order, checks every parity result and checks reuse of the same proof entries.
It also retains an authored literal as unknown. This control is **unrun**.

The initial constructor control failed before row assertions because its DAE
contained no variables or equations. The separate test-only correction adds
an actual checked `Real x; equation der(x) = 0;` continuous model; all condition
identities and the five retained refresh-target assertions remain unchanged.
That fixture correction and the memoized candidate require fresh execution.
