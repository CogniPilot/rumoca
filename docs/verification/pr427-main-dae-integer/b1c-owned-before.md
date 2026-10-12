# Structured Integer owner before qualification

Source pin: `1447ed716d44cc3b020935f38bfb16372bab7fda`. The actual combined public
runtime gate selected 14 tests: 12 passed and two additional tests failed.
`main_dae_integer_quotients_keep_issued_outer_and_owned_comprehension_binders`
failed at its first `simulate_dae` of the original `OuterInteger` source with
Interpreter, SolveLowering EL005: "structured B.1c body does not have one proven
affine scalar program". The owned-comprehension simulation was never reached.
Raw root receipt: `pr427-workshop-and-native-final-headroom-after-20261011/`
`original-public-runtime-plus-native-14.log`, lines 57–60 and 87.

This separate test executes exactly the original `OwnedInteger` source, now
shared as one test constant, without requiring the outer-family simulation to
succeed first. Original combined test assertions and both source models remain
unchanged. Each policy uses the same compiled model at live input n=0 and n=1.
Independent Integer sum is `2*9007199254740993+3+2*n`; its modulo 7 is 6 and 1.
All 11 returned values are checked, including exact trace endpoints. Interpreter
and Auto are observed policies; Auto alone is not proof of native execution.

Exact command selects **1** test:

```sh
cargo test --locked --offline -p rumoca --features fmu-packaging --test suite_core \
  reported_bug_runtime::main_dae_integer::main_dae_integer_quotients_owned_comprehension_executes_independently
```

The original outer/owned combined selector also selects **1** test and stays
CI-selected. New owned outcome is **UNRUN**; root owns the guarded Cargo lane.
Formatting and diff checks pass; no semantic production repair is present.
A separate diagnostic-only change preserves all certificate decisions and adds
bounded first-mismatch context: domain point ordinal, operation position/kind,
output destination/width, and (for PureCall) issued owner ids and the first
differing input ordinal. It never dumps a typed body, dependency graph, or
expanded register list. The original combined selector produces the diagnostic;
no new execution is claimed before root runs it.

The refusing owner is `lower/events/structured.rs::derive_affine_program_certificate`:
compact scalar programs must have equal lengths; Const and LoadY/P/Seed changes
must be affine, while every other operation must remain equal. The current
receipt does not identify the differing operation. That proof must precede a
semantic repair. DAE-C14 and SOLVE-C20 (SPEC_0007/0040), SPEC_0032 §§1–4, and
SPEC_0029 require preserving the authoritative structured B.1c/domain/value owner
and canonical typed arithmetic; scalar unrolling, weakening certificate equality,
or changing the Integer source target to Real would evade the contract.
SPEC_0033 §§2–6 governs first-divergence evidence and honest qualification.
SPEC_0036/0037 remain draft context.
