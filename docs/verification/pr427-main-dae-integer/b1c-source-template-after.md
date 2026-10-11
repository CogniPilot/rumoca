# B1c source-template coverage repair

This records the unqualified 817 candidate. Its binder-authority claim was
incomplete; [the provenance followup](structured-binder-provenance-after.md)
supersedes that admission claim and the four-test count below.

Actual BEFORE source `4907c9b5a`: original OuterInteger fails SolveLowering EL005
at point 1, operation 1, PureCall/PureCall, destinations Some(1)/Some(1), widths
1/1, owners 0/1, input counts 1/1, no differing input ordinal. Root receipt:
`pr427-outer-integer-diagnostic-before-20261011/`
`structured-outer-integer-diagnostic-before-1.log`. Independently, the unchanged
OwnedInteger comprehension passes both Interpreter and Auto at n=0/1, all
11 output samples. That proves the refusal belongs to the materialized outer
family, not to owned comprehension binder capture or modulo evaluation.

The earliest divergent producer is ToDAE's source family partition: aggregate
coverage packs the materialized rows into distinct quotient ExprIds, and the
family uses that packed array rather than its original binder-substituted body.
The exact typed owner correctly issues different identities for those ExprIds;
Solve's affine certificate correctly refuses to equate the resulting call sites.

The repair retains a compact ordered row-range fact when the existing aggregate
builder still owns its checked members. Its source-family recognizer reuses the
original RHS only after proving complete matching member range and count,
resolved target InstanceId, full direct 1:extent axes in row-major order, unique
issued binder identities, and absence of target self reads or same-spelling
foreign binder capture. Generated binders must explicitly be generated; source
binders use their existing DefIds. Domain, clock, activation, provenance and
constructor coverage/type checks remain unchanged. Self-dependent recurrence
keeps the packed aggregate's existing strictly-backward constructor proof.
Other family profiles retain their existing checked packed path/refusal.

This adds no canonical IR field, public API, scalar owner, extent-sized list,
new arithmetic evaluator, or owner equality relaxation. Per-group witness
storage is O(1); target/scope proof work follows source rank/body size. The
existing compact Map and its affine certificate consume one checked source
body with one exact typed expression owner and live issued binder captures.

Governing MLS §8.3.3 for-equations, §10.6.1 array equations, Appendix B solved
form; SPEC_0007/0040 DAE-C14 and SOLVE-C20; SPEC_0032 §§1–4; SPEC_0029 single
ownership; SPEC_0008 provenance; SPEC_0033 §§2–6 before/fix/after proof.
SPEC_0036/0037 remain draft context.

Exact after gates, all **UNRUN** while root owns guarded Cargo:

```sh
cargo test --locked --offline -p rumoca-phase-dae --lib \
  construction::tests::b1c_source_templates::
# expected 4: constructor positive, axes/source identity, member range/order,
# and scalar-body recurrence-proof refusal controls
cargo test --locked --offline -p rumoca --features fmu-packaging --test suite_core \
  reported_bug_runtime::main_dae_integer::main_dae_integer_quotients_keep_issued_outer_and_owned_comprehension_binders
# expected 1, original model/assertions unchanged
cargo test --locked --offline -p rumoca --features fmu-packaging --test suite_core \
  reported_bug_runtime::main_dae_integer::main_dae_integer_quotients_owned_comprehension_executes_independently
# expected 1, exact original owned case independently selected
```

New tests are registered in the existing unconditional ToDAE lib test module;
original public tests stay in the existing CI-selected suite_core runtime module.
Formatting and diff checks pass. No execution, forced-native parity, complete
CI success, broader family profile or MSL cohort result is claimed here.
