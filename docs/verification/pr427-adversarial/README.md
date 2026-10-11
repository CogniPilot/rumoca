# PR 427 adversarial regressions

Review baseline: `cfe53b91d18d53c988ea2b391f2bdde8f8697e5b`.
The fixtures exercise the public compiler and target APIs; they do not construct
an alternative compiler IR. The candidate fixes require qualification before
these results can be described as passing after the repair.

| Severity | Trigger and baseline failure | Expected behavior | Regression coverage |
| --- | --- | --- | --- |
| High | A runtime Real pure call computes `mod(9007199254740993,7)`. The inliner converts Integer constants to binary64, so the generated C derivative differs from the exact result 6. | Preserve the checked Integer arithmetic before explicit conversion to Real. | `c_ode_pure_call_semantics::exact_integer_arithmetic_survives_a_runtime_real_call` executes generated C. |
| High | A local array write is invalid only when a runtime branch becomes active. The speculative inliner discards that write and emits a successful RHS. | Preserve the fault, or refuse a target whose scalar ABI cannot express it. A statically inactive branch remains supported. | `a_guarded_invalid_array_write_cannot_become_a_successful_rhs` checks native active/inactive controls and six target refusals; `a_statically_inactive_invalid_write_is_not_expanded` executes valid generated C. |
| High | `integer(u)` in a runtime pure call is emitted as unchecked floating point rounding, including values outside the Integer domain. | Preserve checked conversion failure or refuse the unsupported scalar projection. | `runtime_integer_conversions_retain_the_checked_domain_failure` checks valid/invalid native controls and six target refusals. |
| High | Two component instances replay one tuple-call when-equation declaration. The global span-keyed plan map rejects the second with ED013. | Distinguish resolved receiver instances while retaining source provenance. | Compiler library `adversarial_when_call_tests`; runtime `when_multi_output_calls::component_instances_have_distinct_when_call_owners` checks four independent receiver values. |
| Resource correctness | Constant-only maps/folds can traverse huge domains without emitting enough operations to exhaust the old budget; direct tensor expansion has no admission budget. | Check work and allocation before expansion, including folded operations, domain tuples and capture metadata. | Public constant-only loop refusal plus eval-solve compact tensor and overflow tests. |
| Medium | An Algorithm Code browser preview renders a custom target declaring `events = false` for a fixed-sample event model. | Run the declared DAE capability gate before projection and rendering. | Unchanged `source_preview_tests::algorithm_preview_honors_declared_event_capability`. |
| Medium | A metadata-only Algorithm Code target returns successful `Ok([])` from the browser source preview. | Refuse a preview with no source files. | Unchanged `algorithm_preview_refuses_an_empty_source_output_set`. |
| Medium | A custom source template reads fabricated packaging facts, including `1970-01-01T00:00:00Z`. | Expose only the checked source context; templates requiring unavailable artifact facts must fail. | Unchanged `algorithm_preview_does_not_invent_artifact_evidence` checks timestamp, tool, identities and checksums. |

The existing C template selector selects the `c_ode_pure_call_semantics` module.
Compiler and eval-solve library tests run in normal Cargo CI. The strict target
selection test remains in place. Workshop numerical tests use the checked-in
SympyDecay and four NeuralODE model sources; C executes initialization and RHS,
while JAX/CasADi additionally compare differentiation with the native runtime.
They are selected by the existing C, JAX and CasADi template groups.

The projection belongs to eval-solve and reuses its canonical checked numeric
helpers. Integer/Boolean constants retain their types, immutable payloads are
shared across captures and slots, and updates copy before mutation. Admission
uses a cumulative limit of 4,000,000 units per scalar projection. This does not
claim a global model budget. Runtime Integer lanes, checked runtime conversion,
Binary32 and unrepresentable faults are explicit unsupported profiles.

Baseline guarded attempts (machine-local receipts, outside the repository):

- `pr427-before-public-scalar-corrected-20261011`, frozen `bdd5315e7`: all three scalar semantic regressions failed, exit 101.
- `pr427-before-when-compiler-20261011`, frozen `ce783eb6a`: distinct component-instance fixture failed with ED013, exit 101.

An earlier scalar attempt contained a defective zero-parameter C harness and
is not evidence for the Integer precision finding. Transport/FMI feature build
failures from separate attempts are likewise not compiler semantic evidence.
External capability checks and initialization rendering still repeat some
projection work. Derivative scalar admission now borrows the same prepared
renderer view consumed by the target, with one fallible cache initialization
even for concurrent callers. Within that renderer, initialization updates are
prepared once and partitioned into parameter/state views. The external
capability checks still independently inspect initialization and algebraic
renderability; this does not claim every repeated pass was removed.

Additional source audit: canonical `SolveValueType::tensor` rejects every zero
extent and any positive product beyond u32, so huge empty typed subshape
products cannot enter the pure-call projection. Constructor-backed controls
pin that boundary. Direct compact `LinearOp` empty matrices/transposes have a
different contract: zero output cardinality previously allowed enormous empty
outer loops. Those now stop before unused products and traversal; usize::MAX
axis controls expect an empty scalar view. This case has source evidence and
pending after-test qualification, without a timed-out before execution claim.

Partial semantic qualification at `5c1a9757abbd12d874af52603a99ed4087bf6105`:

- The seven canonical scalar-owner tests passed in `pr427-semantic-after-identity-20261011`.
- The original two-instance compiler fixture passed in `pr427-semantic-after-remaining-5c1a-20261011`; this is compiler boundary proof, not the separate four-receiver simulation test.
- The exact Integer and statically inactive invalid-write C execution tests passed. The other three public scalar tests still failed because refusal occurred during template rendering rather than the required early target admission. The prepared renderer admission repair addresses that first divergence and awaits requalification.
- The actual workshop C gate did not execute after the preceding failure. JAX/CasADi execution and differentiation remain unqualified locally.

Browser before proof: `pr427-browser-preview-before-qualified-20261011`, frozen
`b7925f511`, ran the four unchanged checked-source preview cases. The actual
built-in source preview passed; the other three failed with the concrete
outcomes above (1 passed, 3 failed, exit 101). The raw log SHA-256 is
`7533d64f8e266a2af0fbc3709b70565e500596416d46aa2b2a3ee9b6a37273c7`.
The candidate removes fabricated artifact facts and reuses one checked
Algorithm Code renderer for source paths and contents. JavaScript ABI wrappers
are compiled for wasm32, their pure Rust helpers remain native-tested, and
`cargo xtask playground test` now runs the actual exported target smoke against
its existing FullWeb build. Source syntax checks pass; after execution remains
pending. No coverage baseline, suppression or assertion was weakened.

A followup retains the source map owned by the checked DAE instead of passing
an empty map to this renderer. Its source and inherited-workspace fixtures
assert the original declaration's filename and equation line/column in the
actual model C file, rather than assigning an inherited equation the extending
model's location. The followup has source proof and awaits execution; it does
not change the frozen `d623d84f9` qualification source.
