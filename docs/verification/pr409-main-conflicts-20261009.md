# PR409 conflicts against main

PR409 source head: `f016950c18560e8ccaa5a4fbb36b2d8401ed2afe`.
Target main: `bd153c9788bb4ffecbe41647f243e1f4517eda23`.
The prepared resolution combines these two revisions in PR409's existing branch.

The five conflicts arise from overlapping helper refactors. The resolution
preserves both branches' functionality without an intended semantic change:

- `refresh_plan.rs`: retain main's `exact_seed_assignments` and deterministic
  `BTreeSet` lookup. It computes the same certified `(equation, target)` pairs
  and singleton block predicate as PR409's inline hash-set implementation.
- `refresh_plan/row_analysis.rs`: retain main's stable, output-sorted shapes and
  bisected output slices. It preserves PR409's per-program derivation and
  per-output lookup, including first matching shape and empty-output handling.
- `lower/scalar/functions.rs`: retain main's moved conditional-emission driver
  and required helper visibility; remove the old duplicate emitters. Carry
  PR409's one-range allocation into `conditional_emission.rs`, where the driver
  now owns emission. Other automatically merged range-allocation changes remain.
- `lower/typed_functions.rs`: retain main's `value_types` ownership and exports;
  remove the old duplicate implementations. Keep PR409's leaf-count helper,
  import primitive/type-provenance helpers from their moved owner, and use the
  count in `value_types::record_field_leaf_range` instead of building leaf types.
- `SPEC_0040`: retain all main rows and PR409's C81/C82 in ordinal order.

The governing rules are SPEC_0007 and its DAE-C33/SOLVE-C51/C80/C81/C82 rows,
SPEC_0032, SPEC_0036, SPEC_0021, SPEC_0029 and SPEC_0033/0025. Existing phase-local
errors and source provenance remain with their owners.

## Qualification limits

This resolution preserves PR409's existing native `row_program` behavior and
its C82 text: a multi-output discrete program is cloned with one store per
native output stage. PR417's later canonical-program stage repair supersedes
that behavior. Clearing these conflicts does not qualify this older behavior
as the final runtime design, or prove a complete unchanged SLAM FMU export.

These focused results do not satisfy the landing or full-SLAM gates. PR409
remains draft while the known assertion counterexample/full-SLAM qualification
hold is open. Main may advance with PR417 before landing, so this resolution is
tied to the exact target above and must be reconciled again after that advance.

## Focused verification

`cargo fmt --check` and `git diff --check` passed; no unmerged index entries or
conflict markers remain. The following checks passed with the existing warm
`msl-fast` cache, using
`CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4`:

```sh
cargo test --profile msl-fast -p rumoca-phase-solve -p rumoca-eval-solve \
  -p rumoca-ir-solve -p rumoca-phase-dae --lib
cargo clippy --profile msl-fast -p rumoca-phase-solve -p rumoca-eval-solve \
  -p rumoca-ir-solve -p rumoca-phase-dae --lib -- -D warnings
```

All 1,478 library tests passed with zero failures/ignored tests: eval-solve 278,
IR-Solve 593, phase-dae 426 and phase-solve 181. Clippy passed. The cache was
idle when reused; no new heavyweight target directory was created.
Full workspace/coverage/MSL/export gates have not been run on this resolution.
