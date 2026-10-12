# Nested initialization read query

Exact source `5040d2079e3bf9c2cc2a4b7e4b798486b65106c2` selected one constructor-backed runtime-query regression and failed its expected algebraic-read assertion. The checked single-iteration Fold and checked scalar initialization update compiled, and the canonical evaluator returned the actual algebraic value `51.0` before the query returned false. The first Fold failure stopped the test before its conditional and parameter-only controls; those controls require AFTER execution.

The ordinary `rumoca-solver` lib test is `runtime::solve_runtime::initial_projection::tests::initial_updates_find_algebraic_reads_in_checked_nested_regions`. It constructs checked Fold/conditional regions and a checked initialization aggregate, then tests the existing runtime read query. It does not establish that a particular authored Modelica program lowers to this region shape, or claim a complete simulation trace.

The first divergent owner is `initial_projection::updates_read_algebraic_slots`, whose top-level scan omitted nested reads. `LinearOp` has no `LoadIndexedY` variant; Y loads are `LoadY` and `TensorLoad(Y)`. Nested Fold updates and conditional regions share the enclosing Y/P storage; `rumoca_ir_solve::visitor::walk_linear_op_slice` already owns that recursive traversal. The focused repair shares this walker between scalar initialization updates and compact quotient Map updates, preserving the leaf read predicate and all solver ordering/policies. It follows MLS §8.6 simultaneous initialization and the accepted SPEC_0007/SPEC_0029 single-owner boundary; it adds no IR vocabulary, schema change, or scalar tensor expansion.

```sh
cargo test --locked --offline --jobs 4 -p rumoca-solver --lib initial_updates_find_algebraic_reads_in_checked_nested_regions -- --nocapture
```

Actual BEFORE: `0 passed; 1 failed; 0 ignored`, Cargo exit 101, 63.134 seconds total, peak RSS 1,571,660 KiB. Guardian limits remained 600 seconds, 4 GiB RSS, 8 GiB cache, 8 GiB scratch free-space floor and 16 GiB available host memory; no resource stop occurred. Raw evidence is under `$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-nested-initialization-before-5040-20261011`.

| File | SHA-256 |
|---|---|
| `recipe.json` | `7ee40db0083483210f181f09963de9d3794cd49ad34484e67c0516710be9afc9` |
| `nested-initialization-read-before.log` | `7e9e019d6cb5704eb65a442b75dce438202620d48e8948357d8ba25c4cc8f49d` |
| `nested-initialization-read-before.guardian.json` | `e89790a88d0e30ac54321b40faefe30f2df8996d50654b3b9e480d020bf2a397` |

The minimal repair is commit `115029c4d`; AFTER execution remains pending in this receipt. The unchanged test is selected by the ordinary lib CI suite without skips or a new runner.
