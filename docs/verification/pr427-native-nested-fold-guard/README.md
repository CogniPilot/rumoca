# Native nested-fold false-arm before probe

This test-only source retains the original native emission. No production
repair or test execution is included. It depends on the #411 source candidate
e4693ec3e and the parent test-type correction 718d131d5 (picked as 5208ef103).

`enter_nested_fold_guard` compares a scalar condition with zero, then emits
`bnot` for a false-arm nested fold. Cranelift I8 comparison values are 0/1;
`bnot(1)` is nonzero, so `brif` enters a nested fold on an inactive false arm.
This is a concrete source hypothesis pending execution of the original probe.

The checked constructor witness retains a compact inner fold with one runtime
index 2 into an extent-1 aggregate. An outer fold selects it with a live
captured scalar condition and either polarity. The canonical evaluator must
retain initial value 3 on the inactive arm and expose the selected typed
axis-1/index-2/extent-1 fault. Native execution must do the same. The existing
true-arm controls execute first, then the false-arm selected-fault control,
then the formerly untested false-arm inactive control. The probe admits the same input/output slices as the production wrapper and
invokes the issued kernel through its existing native call wrapper. This
avoids the test-mode interpreter precheck masking an active native fault.
Explicit JIT call counts distinguish native calls from independent canonical
reference evaluation.

Expected before outcome: one failing test at false-arm inactive execution;
all preceding canonical and native controls reached. Actual outcome: **UNRUN**.
Public Modelica source reachability is unproven; this witness tests the actual
checked compact Solve owner and its native consumer, without claiming a
broader compiler source profile.

Exact gate (expected selected tests: **1**):

```sh
cargo test --locked --offline -p rumoca-exec-cranelift --lib \
  emit::fold_tests::nested_guard::nested_fold_guard_preserves_both_polarities_and_selected_access_faults
```

The child is registered in the existing native fold suite and discovered by
CI workspace verification. Governing semantics: MLS §11.2.1 selected algorithm
branches, SPEC_0022 ALG-019, SPEC_0007/SPEC_0040 checked compact function-fold
execution, SPEC_0032 §4 tensor-native compact execution, and SPEC_0033 before
proof. No model exception, scalar expansion, ignored test, or broad error
oracle is introduced.
