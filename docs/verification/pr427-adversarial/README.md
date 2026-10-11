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
projection work; the repair removes duplicate algebraic materialization inside
candidate construction without claiming every repeated pass was removed.

Additional source audit: canonical `SolveValueType::tensor` rejects every zero
extent and any positive product beyond u32, so huge empty typed subshape
products cannot enter the pure-call projection. Constructor-backed controls
pin that boundary. Direct compact `LinearOp` empty matrices/transposes have a
different contract: zero output cardinality previously allowed enormous empty
outer loops. Those now stop before unused products and traversal; usize::MAX
axis controls expect an empty scalar view. This case has source evidence and
pending after-test qualification, without a timed-out before execution claim.
