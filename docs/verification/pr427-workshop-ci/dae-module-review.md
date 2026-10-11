# Workshop DAE module review

The architecture gate at `4aaaf7872ece5d60c13217565ae4bd5a4aa450c6` passes all 247 hardening tests, including
the unchanged totality-debt ceilings. Its policy suite passes 18/19 tests:
the DAE LOC acknowledgment is stale. This report accounts for the production
source under SPEC_0043 §1/§1a, rather than changing that measurement or deleting
coverage. The machine-readable inventory lists every included module.

| Source | Core lines | Wire lines | Total lines |
|---|---:|---:|---:|
| Original PR head `21d64ecaa9d205b671598991ee8c4d2f556fed6c` | 18803 | 5759 | 24562 |
| Combined workshop source `4aaaf7872ece5d60c13217565ae4bd5a4aa450c6` | 19686 | 6037 | 25723 |

The §1b rule rounds these measurements up to the next 250 lines: 19750 core,
6250 wire, and 25750 total. These are derived acknowledgments, not spare
headroom; the existing review thresholds and totality-debt ceilings are retained.
This review does not claim a net reduction in DAE production source.

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

Changed production modules are listed below. Unchanged modules remain in the
adjacent full inventory. This is the orchestrator's module review; maintainer
PR/spec approval remains a separate requirement.

| Module under `rumoca-ir-dae/src` | Before | After | Change |
|---|---:|---:|---:|
| `conditions.rs` | 436 | 461 | +25 |
| `error.rs` | 419 | 422 | +3 |
| `expression.rs` | 340 | 368 | +28 |
| `expression/arena.rs` | 85 | 91 | +6 |
| `expression/array_nodes.rs` | 152 | 156 | +4 |
| `lib.rs` | 199 | 200 | +1 |
| `model.rs` | 1882 | 1918 | +36 |
| `model/function_checks.rs` | 593 | 594 | +1 |
| `model/model_conditionals.rs` | 0 | 503 | +503 |
| `model/runtime_quotients.rs` | 472 | 506 | +34 |
| `model/storage.rs` | 790 | 858 | +68 |
| `model/view.rs` | 1941 | 1952 | +11 |
| `model/view/arena_walks.rs` | 330 | 331 | +1 |
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
