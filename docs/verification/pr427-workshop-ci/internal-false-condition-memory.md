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
activation owners. Its corrected BEFORE execution is recorded below; the
production candidate's AFTER execution is recorded below.
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
It also retains an authored literal as unknown. The AFTER full phase suite
executes this control as recorded below.

The initial constructor control failed before row assertions because its DAE
contained no variables or equations. The separate test-only correction adds
an actual checked `Real x; equation der(x) = 0;` continuous model; all condition
identities and the five retained refresh-target assertions remain unchanged.
The corrected fixture reaches the intended BEFORE assertion; the memoized
candidate's AFTER execution is recorded below.

## Corrected constructor BEFORE

At `66db3e4d2fc076b2444949f1583c1ef9760f697a`, the exact constructor selector
above executed **0 passed, 1 failed, 0 ignored, 186 filtered**. It failed at
`constant_condition_memory.rs:73`, `internal false/Always 1`, after checked DAE
construction and Solve lowering. The earlier empty-model failure was a fixture
admission error and did not establish the extra-row counterexample. The
corrected control includes a checked state and continuous derivative equation;
it preserves the condition identities and exact five retained targets.

Attempt `pr427-periodic-internal-false-corrected-before-20261012` retains the
following SHA256 bindings:

- recipe: `dd6943f97c7bfd399d986c4c1e49f4fe315a88cc0f273ecb0f8c9923353d063e`
- raw log: `33760df8613c6cfde5b2a4fad6fc24d0a88f29114778e083428deb09ef72ab5e`
- guardian: `049ee54283e18fe9689312ed5df3e48081ce96e58a96fd7f826593241793c6b7`

The command ran in the existing full development shell. Cargo finished in
60 seconds; the guardian elapsed 65.126 seconds, exit 101, signal absent,
peak aggregate RSS 2,371,932 KiB and peak cache 8,092,323,840 bytes. No resource
limit stopped this execution. These are build-plus-test measurements, not model
runtime measurements.

## Storage and query ownership

`RuntimeVectorBuilder::build` in `model_values.rs` initializes the complete
parameter vector to zero. `append_runtime_layout` in `layout.rs` appends
condition-memory slots after checked variable and pre storage, before the
separate clock/delay/flag storage. Variable initialization writes only checked
variable bindings; alias seeding writes Y. The initialization condition-memory
seeder writes only issued discrete row targets, and event action preparation
copies the existing condition pre values. An omitted internal false row thus
retains its zero slot; no authored variable or other hidden owner writes it.

The query cache is local to one immutable checked DAE view and one lowering.
`conditions::checked_node` requires each Not operand already defined and
`conditions::define` fills a reserved node exactly once, proving acyclicity
without assuming ID order. The iterative memo retains both unknown and proved
values. Each reachable node is inspected once; deterministic map operations
cost O(n log n), with O(n) private scratch. It changes no source expression,
physical layout or retained proof/wire contract.

## Paired AFTER execution

Source `9aec6edc9fd68ef422566ba9b83462916402987e` passed all **188 phase-Solve
tests**, including the corrected constructor and reverse-reserved 1024-edge
memo control. The unchanged original periodic public suite passed **17 tests**,
including the exact sample alias and zero-phase accumulator; the latter also
contains the separate clock-initialization runtime repair. Existing source
when/initial controls passed **32 tests**, and guarded deadlines passed **5**.
All four summaries report zero failed and zero ignored; required counts were
accepted. The 188-test result subsumes the two new unit controls, not 190
independent executions.

These executions belong to attempt
`pr427-periodic-public-and-owner-after-9aec-20261012`, recipe SHA256
`01877584adacd589bb748c33e818041decbefa924f880c2491682e50e3a10058`:

| Gate | Raw log SHA256 | Guardian SHA256 | Test time |
|---|---|---|---|
| Phase Solve 188 | `0f7b272c9d5630c2fe46827121df66ba37af16226ff7674d0c3945744ed11fda` | `fd809c654644f2bad07e1f311d7af0d8379d157d7b37776d0cf78a6f3f67c2bd` | 0.14 s |
| Original periodic 17 | `869e5e995de3eedfa6e16ebc776ef09dbcf241d316c379631a7741625dbbb292` | `98c9b8dbd796fecac73c74cd1ac6b97b0db507ae03829c635c4cc4da88c98276` | 0.02 s |
| Unconditional time 32 | `598bda0996bac52bbfdefbbb297a3f5aa4b42fce3ce6c5778ce76e3c601abe67` | `d072c0e3bd8810685f4840bc5e577b4ab023934c516eb8cb5189a230a49883e2` | 0.06 s |
| Guarded deadlines 5 | `d0d5c42203ee2aaf363edbd09a90d1a142e33beb32878a2c9dc8d7dcb860e1a8` | `e89ee8366208b90d9b3f14b08baa3ce418c103537daaa09090b327bf14c78902` | 0.04 s |

Guardians exited zero with no resource stop. Phase-Solve build-plus-test took
65.818 seconds; original periodic build-plus-test took 209.787 seconds. These
measurements do not assert improved runtime performance. A separate full
solver-crate run at the same source passed 555 and failed 46; fixture-owner
and clock-witness investigation remains open. These focused positive receipts
therefore do not constitute a full-workspace or final-CI success claim.
