# Workshop DAE module review

This report inventories production source at `5e6f9cb9afec4caf992a370db78c1a5b342d5336` under SPEC_0043 §1/§1a.
The adjacent machine-readable inventory names every included module and the
original PR comparison. Focused qualification is recorded in the conditional,
clock and selected-value reports; the final combined quick/full gates remain
pending. Earlier architecture tests passed at their documented source, which
is not a qualification of this later combined source.

| Source | Core lines | Wire lines | Total lines |
|---|---:|---:|---:|
| Original PR head `21d64ecaa9d205b671598991ee8c4d2f556fed6c` | 18803 | 5759 | 24562 |
| Combined workshop source `5e6f9cb9afec4caf992a370db78c1a5b342d5336` | 19967 | 6037 | 26004 |

The §1b rule rounds these measurements up to the next 250 lines:
20,000 core, 6,250 wire and 26,250 total.
These are derived acknowledgments; review thresholds, measurement exclusions
and totality-debt ceilings remain unchanged. This is an increase in DAE source.

The largest addition is the model-conditional construction owner and its view:
checked arm identity, begin/close scope, foreign-arm refusal, and provenance.
Function-local conditional owners cannot substitute for these model-level
scopes. Current wire replay reconstructs those same owners through checked
construction; its records and stream correlation are trust-boundary code,
not a second semantic lowering implementation. The private closed record
retains the already-issued expression and guard operations so frozen views
can consume their proofs without repeated recovery assertions. Current wire
round-trip tests cover exact JSON and bincode bytes.

Quotient-boundary reconstruction keeps the source expression/root correlation,
compact range identity, and selected operand ownership. Its first divergent
constructor is repaired rather than admitting escaped arm-local values in a
validator. Existing record/view types are reused. The remaining condition,
expression, arena and export changes carry the required typed scalar/array and
conditional identities between those same owners. The 48-line operand-view
module carries fixed binary pairs and conditional branch pairs; Solve consumers
use those shapes instead of repeating fallible operand extraction.

The reviewed patch contains no replacement wire reader, model-specific branch,
scalar tensor expansion, runtime recovery, duplicated model-arm constructor,
or obsolete conditional view to retire. Reusing function-local owners here
would erase their different activation and scope contracts. Necessary explicit
construction and current replay code remains; future reduction remains owned
by consolidation of downstream recovery and correlation replay, as named in
the ledger. Focused IR-DAE/IR-Solve/PhaseSolve and checked round-trip tests pass
on the source commits documented in `operand-construction-proofs.md`; a final
combined quick/full gate remains required.

The later checked model-arm derivative facade carries one closed owner and
its already-issued expression witnesses into reconstruction. It does not
reopen the old construction scope. The condition entry now owns the strict
synchronous clock fact once, distinct from its Boolean clock projection:
Solve consumes the former rather than running a duplicate recursive analysis.
Both facts are derived at construction and omitted from wire. The shared
selected-value traversal delegates literal/constant branch selection to the
existing evaluation owner; full source-DAG traversal retains every arm. The
initialization binding consumer retains its existing stronger immutable
parameter proof and binder-dependent controls. The query adds no alternate
expression grammar or tensor enumeration.

Changed production modules are listed below. Unchanged modules remain in the
adjacent full inventory. This is the orchestrator's module review; maintainer
PR/spec approval remains a separate requirement.

| Module under `rumoca-ir-dae/src` | Before | After | Change |
|---|---:|---:|---:|
| `conditions.rs` | 436 | 493 | +57 |
| `conditions/clock_projection.rs` | 0 | 91 | +91 |
| `error.rs` | 419 | 422 | +3 |
| `expr_query.rs` | 349 | 402 | +53 |
| `expression.rs` | 340 | 368 | +28 |
| `expression/arena.rs` | 85 | 91 | +6 |
| `expression/array_nodes.rs` | 152 | 156 | +4 |
| `expression/type_rules.rs` | 944 | 974 | +30 |
| `lib.rs` | 199 | 200 | +1 |
| `model.rs` | 1882 | 1917 | +35 |
| `model/function_checks.rs` | 593 | 594 | +1 |
| `model/model_conditionals.rs` | 0 | 577 | +577 |
| `model/runtime_quotients.rs` | 472 | 506 | +34 |
| `model/storage.rs` | 790 | 858 | +68 |
| `model/view.rs` | 1941 | 1952 | +11 |
| `model/view/arena_walks.rs` | 330 | 333 | +3 |
| `model/view/expression_operands.rs` | 0 | 48 | +48 |
| `model/view/model_conditionals.rs` | 0 | 107 | +107 |
| `model/view/quotient_owners.rs` | 111 | 118 | +7 |
| `model/wire.rs` | 1924 | 1954 | +30 |
| `model/wire/expression_wire.rs` | 535 | 543 | +8 |
| `model/wire/function_graph.rs` | 333 | 357 | +24 |
| `model/wire/function_replay.rs` | 1500 | 1521 | +21 |
| `model/wire/model_conditionals.rs` | 0 | 165 | +165 |
| `model/wire/quotient_projection.rs` | 454 | 479 | +25 |
| `model/wire/records.rs` | 606 | 608 | +2 |
| `model/wire/structured_quotients.rs` | 91 | 94 | +3 |

The final 30 production lines in `expression/type_rules.rs` give discrete
Integer `div`, `mod` and `rem` their checked scalar-Integer domain before the
existing Real static-numeric path. A known zero divisor still fails
construction; continuous or Real operands retain the existing domain rule.
The helper reuses issued type and variability facts without a second
floating-point probe or a second expression grammar. Its focused regressions
are included in the frozen source; this review does not claim that the
combined quick/full gates pass. No other production module changed since
the preceding inventory.
