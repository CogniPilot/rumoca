# Golden model records

[SPEC_0033 §6c](../../../spec/SPEC_0033_DEVELOPMENT_PROCESS.md#6c-working-model-proof-admission)
owns model admission and the golden coverage definition.
[`registry.toml`](registry.toml) binds candidate sources, exact scenarios,
endpoint cones, content digests, review times and review notes.

## Current implementation

Only candidate capture is implemented. A structurally valid `reviewed`
record returns `ReviewedAdmissionNotImplemented`; it cannot issue golden
credit. The authority is
[`golden_registry::check_candidate_capture`](../../../crates/xtask/src/golden_registry/admission.rs).
Both current model records remain candidates.

`coverage golden-all` checks the candidate-only registry, measures the
workspace denominator and emits a `CandidateOnlyAggregateReport` with zero
admitted models and zero covered lines. It does not implement a nonzero
reviewed-model union yet. Tests for this behavior live in the xtask library,
not in the former architecture-test registry module.

## Available commands

Run from the repository root with the prerequisites in
[CONTRIBUTING.md](../../../CONTRIBUTING.md). The full Nix shell provides the
coverage tools when Nix is used.

```text
cargo test -p xtask --lib golden_registry
cargo xtask coverage golden UnitDerivative
cargo xtask coverage golden-transition UnitDerivative typed-instanced-to-flat
cargo xtask coverage golden-all
```

The first capture command runs the candidate's declared scenarios and writes
`target/golden-coverage/UnitDerivative/footprint.json`.
The transition command supports only the named pair and writes
`target/golden-transition-coverage/UnitDerivative/typed-instanced-to-flat/capture.json`
and `review.md`. It builds the predecessor before resetting LLVM counters,
calls the production `flatten_typed` function once, and dumps before result
inspection. It repeats the capture and compares the profiles.

These outputs are review inputs. Transition attribution and source/build
binding remain under adversarial review; a successful capture is not
admission. The current transition output has no implemented whole-scenario
containment check. Registry, footprint, transition capture and aggregate
schemas are distinct; their versions come from their respective code owners.

## Reading coverage

The intended golden percentage uses the complete workspace instrumentable
production-line denominator and the union of admitted scenario lines.
Overlap counts once. Candidate capture density within linked files is a
different statistic and must not be read as reviewed workspace coverage.

Per-transition captures help locate semantic review obligations. Their
executed prefix may warm shared state, so raw transition totals do not form
independent contributions. Generic and macro bodies also require the exact
executed instantiation or expansion context. Neither merged line counts nor
a file's crate directory alone identifies the semantic owner.

Compiler coverage of emission and behavioral evidence for the emitted FMI or
eFMI artifact remain distinct. An early continuous-model eFMI refusal provides
no evidence of GALEC production behavior.

The current campaign queue and review packet form are maintained in
[`dev/golden-review-workboard.md`](../../../dev/golden-review-workboard.md).
The Markdown records preserve historical evidence; inspect their timestamps,
bound digests and current code before relying on an earlier claim.
