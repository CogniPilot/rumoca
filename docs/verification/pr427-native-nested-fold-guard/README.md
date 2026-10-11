# Native nested-fold false-arm before probe

The original test-only source retained the native emission without repair.
Its qualification depends on the #411 source candidate
e4693ec3e and the parent test-type correction 718d131d5 (picked as 5208ef103).

`enter_nested_fold_guard` compares a scalar condition with zero, then emits
`bnot` for a false-arm nested fold. Cranelift I8 comparison values are 0/1;
`bnot(1)` is nonzero, so `brif` enters a nested fold on an inactive false arm.
This is a concrete source hypothesis pending execution of the original probe.

The checked constructor witness retains a compact inner fold with a live captured
index 2 into an extent-1 aggregate; it is a second runtime input so constant
projection folding cannot hide the actual index fault. An outer fold selects it with a live
captured scalar condition and either polarity. The canonical evaluator must
retain initial value 3 on the inactive arm and expose the selected typed
axis-1/index-2/extent-1 fault. Native execution must do the same. The existing
true-arm controls execute first, then the false-arm selected-fault control,
then the formerly untested false-arm inactive control. The probe admits the same input/output slices as the production wrapper and
invokes the issued kernel through its existing native call wrapper. This
avoids the test-mode interpreter precheck masking an active native fault.
Explicit JIT call counts distinguish native calls from independent canonical
reference evaluation.

The first executed before source, `4171d7953`, failed at the true-arm selected
native access: it returned success instead of a bounds fault. The false-arm
control was not reached. That source still held the inner index in a constant,
despite the earlier live-index description. The root qualification retains its
exact source seal and raw receipt separately from this revised source.

This revised test-only probe first executes four valid-index controls with
independently tabulated results: true-arm conditions 0/1 yield 3/13; false-arm
conditions 0/1 yield 13/3. Both canonical evaluation and actual native execution
must agree. It then retains the selected-invalid/inactive controls, now loading
the inner index from the existing live capture. No production repair is included.
Expected before: the false-arm condition-1 valid-index control returns 13 instead
of 3, independently of the separate invalid-index fault defect. Actual revised
outcome: **UNRUN**.
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

## Separate known-index before probe

A second original constructor/native control keeps the runtime index register
known to the native emitter. `lower_indexed_register` currently maps the
invalid constant-offset result to NaN. The probe first checks valid index 1
and an inactive true-arm guard around known-invalid index 2, then requires
the selected canonical/native fault on both a direct access and the same
protected program with the arm selected. The original live-index guard probe
is unchanged; this additional probe isolates constant-offset fault retention.
Expected before: direct native selected access erroneously succeeds with NaN.
Actual: **UNRUN**, no production repair.

Exact additional selector, expected **1**:

```sh
cargo test --locked --offline -p rumoca-exec-cranelift --lib \
  emit::fold_tests::known_runtime_access::known_runtime_index_preserves_selected_fault_and_inactive_protection
```

## Bounded native-owner repair candidate

The native false-arm guard now compares its I8 condition flag with zero.
Known-invalid offsets return the existing native index-fault status. Live
offsets pass that same status check before projection loads. Register, carried,
and captured projections reuse one private checked-offset wrapper and one
private constant-invalid emitter; the latter's dummy continuation value is
unreachable after the unconditional failure return, never a successful output.
Invalid floating coordinates use saturating conversion during address
calculation so they reach the status check without a conversion trap. The shared
offset query still supplies its separate scalar-update consumer with the
original `(offset, valid)` contract; this change makes no new update-policy claim.

Checked constructors remain responsible for rank, nonempty extents, register and
tuple coverage, stride, and constant coordinate admission. The repair changes the
machine-code consumer of those same checked owners, not their representation or
admission. No tensor expansion, public API, input/output admission, or native
pointer contract changes are introduced.

Two supplementary registered native tests exercise all three projection storage
owners. Each checks index 1 against the independent value 10, indices 0/2 against
exact canonical axis/index/extent faults, and fractional/NaN/infinite coordinates
against the canonical invalid-Integer index fault. Native calls must return the
existing index-fault status and leave the output sentinel untouched. The known
and live cases separately retain explicit JIT call counts. Native status 1 does
not carry the canonical fault's axis/index payload; no diagnostic parity beyond
its existing status contract is claimed.

Exact supplementary selector, expected **2**:

```sh
cargo test --locked --offline -p rumoca-exec-cranelift --lib \
  emit::fold_tests::projection_faults::
```

The two unchanged selected original tests plus these two new controls total
**4** tests under the three exact selectors above. After execution is **UNRUN**:
the root agent owns the guarded Cargo lane. Source formatting and `git diff
--check` passed. This is a checked-constructor/native-kernel profile; public
Modelica source reachability of the nested-fold guard defect remains unproven.
