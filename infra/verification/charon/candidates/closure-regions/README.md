# Closure-region recovery candidate

Work in progress, not imported by Rumoca's translator package. This source is
an extraction building block, **not** a completed Charon/Aeneas repair or a
proof premise. The live translators and Rumoca's branded types are unchanged.

## Implemented boundary

`src/lib.rs` constructs owned observations from one rustc
`BodyWithBorrowckFacts`. It checks the body/closure owner and equality of the
complete region-erased, binder-anonymized closure types before matching all
free occurrence positions in parent arguments, signature and captures.
The private aggregate retains the owner `DefPathHash`, structural slots and
directed solution relations; no compiler-lifetime values or `RegionVid` escape.

`src/positions.rs` uses rustc's `TypeVisitor` skeleton with explicit binder
depth. It traverses erased and phantom type occurrences too. The convenience
`for_each_free_region` helper skips types with only erased regions through its
`HAS_FREE_REGIONS` optimization; that is inappropriate for structural matching.
An arity mismatch or unresolved inferred region refuses the complete result.

The receiver's own borrow is not one of these declaration positions. A
callable-method projection must account for that additional lifetime. Bound
occurrences are excluded from the free-slot list, not replaced or erased.

## Evidence boundary

The existing [consumer probe](../../fixtures/closure-lifetimes/consumer-probe.rs)
links this library and checks seventeen closures. Ten controls compare the
original body/signature matrices and the owned result; three more exercise a
phantom-only brand beside an actual capture, mixed bound/free regions and a
by-value `FnOnce` receiver; four cover the one/two-region iterator controls.
Wrong body owners, missing receivers, non-closure
receivers and substitution of a different genuine closure are rejected.
The new `fixtures/region-shapes.rs` also has two native behavior/layout tests.

The probe requests `ConsumerOptions::PoloniusInputFacts`. This supplies the
public `universal_region` and `known_placeholder_subset` facts without running
the Polonius solver. It prints solution equivalence between signature variables
and those universals. Equivalence in one inferred solution is **not** source
lifetime identity, and the candidate does not claim otherwise. An absent
outlives edge means not observed, not a proved negation of an outlives bound.

All tested signature occurrences get fresh inference variables in rustc's
`renumber_mir`; repeated source lifetimes cannot be recovered by comparing those
variable numbers. Source generic order is not a substitute: a closure's free
signature/capture regions introduce universals beyond its parent's parameters.

## Reproduction

Use the pinned rustc-dev toolchain, nightly 2026-08-18
(`8fa1c96cfd489e4c27654c144ae871ce2c4db6c6`), with its matching linker/runtime
libraries. From the repository root, where `OUTPUT` is a fresh build directory:

```console
rustc --edition=2021 --crate-type=rlib --crate-name=closure_region_facts infra/verification/charon/candidates/closure-regions/src/lib.rs --out-dir "$OUTPUT" -Dwarnings
rustc --edition=2021 --extern closure_region_facts="$OUTPUT/libclosure_region_facts.rlib" infra/verification/charon/fixtures/closure-lifetimes/consumer-probe.rs -o "$OUTPUT/consumer-probe" -Dwarnings
```

Run the probe as a rustc driver with `--sysroot` naming that same toolchain,
`--edition=2021 --crate-type=rlib --emit=metadata --out-dir "$OUTPUT"`, an
explicit underscore-spelled `--crate-name`, and each of these input files:

- `../../fixtures/closure-lifetimes/simpler-closures.rs`
- `../../fixtures/closure-lifetimes/consumer-controls.rs`
- `../../fixtures/closure-lifetimes/closure-lifetime.rs`
- `../../fixtures/closure-lifetimes/two-region-iterator.rs`
- `fixtures/region-shapes.rs`

The above input paths are relative to this README. Run native fixtures with
`rustc --test --edition=2021`, then execute their binaries. Use four workers and
a 90-second per-probe watchdog. Strict scoped lint uses the pinned
`clippy-driver` on the same library, probe and test inputs with
`-Dwarnings -Dclippy::all -Dclippy::too_many_lines -Dclippy::excessive_nesting`.
No handwritten lint suppression is used. These upstream-tool controls do not
replace `cargo xtask verify quick/full` for integration/adoption.

## Required completion, not deferred compatibility work

1. Establish a faithful projection from inference facts to closure and
   enclosing-function declarations, including generic/binder identity. Do not
   promote solution-relative observations to universal assumptions by fiat.
2. Integrate that producer at Charon's declaration boundary; preserve facts
   across crates with exact source/configuration/tool/dependency binding.
   Missing facts must not become a fabricated empty relation set.
3. Remove Aeneas's two signature-guessing prepasses when their actual cases
   are handled by the producer. Do not widen or add another heuristic.
4. Require complete translation and fresh generated-Lean checks on positive
   controls and the two-region iterator, negative/mutation controls, the real
   DAE path, upstream regression suites and adversarial review. Moving a
   failure to another assertion is not successful translation.

The opaque hidden-type experiment shows that rustc already retains capture
lifetimes for the returned-iterator controls. Charon's
`translate_closure_type_ref` drops the instantiated capture types when passing
only `closure.item`; `translate_item_maybe_enqueue` then supplies erased
upvar arguments by count. Fix that producer loss using the available capture
facts; a new cross-crate fact artifact is not established as necessary for
this particular axis. Closure-body constraint recovery remains separate.

No actual DAE diagnostic, UnitDerivative relation, golden admission or
full-core translation milestone has closed from this candidate alone.
