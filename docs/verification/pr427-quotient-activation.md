# Conditional quotient activation

The unchanged `InactiveDivision` model selects `1` until time 0.5 and otherwise
evaluates `mod(1/time, 7.0)`. At initialization, the sealed compiler from source
`4b3e7c22fb478438715c11cae462e26091b28ad8` instead evaluates the inactive ratio
while constructing its event root and fails with EX001/non-finite NaN. The
selected-invalid control faults as expected; a valid selected quotient and a
guarded nonsingular quotient both simulate successfully. Raw evidence is kept
outside the build cache in `workshop427-ci/evidence/inactive-quotient-division-4b3e`.
The compiler binary SHA256 is
`faf711401a2d4d2a7e5ef66d4b4739cbb05023bb02a96f01cd4d57b1921598de`.
This is a source-level inactive-arm failure, not a native/backend comparison.

The repair issues model conditional arms at DAE construction and closes them
with the corresponding conditional value. Quotient roots and numeric memory
derive their activation from that same arm. Solve evaluates the ratio only in
the selected checked region; the other region holds the producer's exact
private P cell. A compact Map retains its checked domain and exact affine held
load. The shared term checker proves that the fallback consumes the particular
reaching load shifted by the Map, including Moves and conditional captures;
another unshifted load of the same base cell cannot supply that proof.

Activation observation uses a scoped relation-observation context. Initial and
accepted event producers and their roots observe live relations, including
composite guards. Ordinary continuous evaluation still consumes held relation
memory, and `noEvent` retains its existing semantics. Function construction
suspends model arm scope at its actual operation boundary. The existing
function owner now carries that boundary so wire and structural replay can
reissue the same suspension when a shared function is first built in a nested
arm and subsequently called elsewhere.

The governing active contracts are SPEC_0007 and its DAE-C19/DAE-C22 and Solve
stage rows in SPEC_0040, SPEC_0032's compact-domain/scalar-view boundary, and
SPEC_0029/SPEC_0041's canonical-owner rules. MLS conditional selection
(section 3.6.5) and event buffering (section 8.5) require the selected value and
its event owner to agree. SPEC_0036 and SPEC_0037 remain DRAFT; construction and
proof controls here do not promote their status.

DAE wire version 47 requires conditional operations, expression contexts, and
function construction boundaries. Solve wire version 78 requires the revised
activated numeric-memory certificate. Older wire products are refused;
current positive WASM fixtures must be regenerated from their six original
sources with the canonical issuer, never edited to change a version number.
Generated targets must retain their existing capability refusal for unsupported
numeric Map initialization, rather than emit an empty initializer.

## Qualification manifest

All candidate results below are **UNRUN** at this source freeze. Formatting and
`git diff --check` passed. The original public before tests are retained.

| Command suffix (ordinary locked Cargo tests) | Selected tests | Purpose |
| --- | ---: | --- |
| `-p rumoca-ir-dae --lib tests::model_conditionals::` | 6 | issued-source diagnostics, sibling/escape refusal, foreign guards, replay entry invariants, mandatory-wire mutation, nested global-function JSON/bincode replay |
| `-p rumoca-phase-structural --lib nested_model_arm_function_interval_survives_structural_replay` | 1 | two actual whole-DAE reconstructions retain the nested shared function's closed dead tail inside its issued interval |
| `-p rumoca-ir-solve --lib quotient_memory::` | 12 | original scalar/compact certificates and activated hold/root/affine-load tampering |
| `-p rumoca-ir-solve --lib conditional_value_proofs_do_not_merge_distinct_execution_owners` | 1 | term proofs preserve ordinary call-owner cardinality |
| `-p rumoca --test suite_core reported_bug_runtime::main_dae_integer::real_quotient_` | 4 | original inactive/selected-invalid cases, nested first-true guards, per-point initialization/hold and sampled integral, Interpreter and Auto |

Full DAE, phase-structural and current-wire suites remain necessary before
publication because the operation-boundary representation affects shared
function replay. Auto alone is not proof that a particular backend ran.
