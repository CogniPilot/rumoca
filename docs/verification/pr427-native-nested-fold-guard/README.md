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

## Separate scalar-patch address repair

This followup starts from `5aa0b10bf`. Compact fold patches have a distinct
canonical matching contract: an invalid coordinate matches no element and
leaves the carried tensor unchanged. This is not a tensor READ or a typed
`UpdateElement` bounds-fault contract, and does not declare an invalid Modelica
assignment legal. MLS §10.5 and §11.2.1 remain the source rules; this witness
tests the retained compact IR's native consumer, including invalid-input safety.

The checked constructor admits a live runtime-index register while proving a
nonempty carried range. In native scalar-patch emission, a live NaN coordinate
reaches `lower_tensor_offset`: pinned Cranelift **0.125.4** `fmin`/`fmax` propagate
NaN; its `fcvt_to_sint_sat` converts NaN to zero, then one-based adjustment
produces offset -1 with `valid = false`. The original scalar-patch consumer
forms and loads/stores that address before using the validity predicate.
For carried base 0 this addresses memory before its checked stack slot. This
is a concrete source proof of an unsafe access; no observable wrong trace at
`5aa0b10bf` is claimed. The exact installed primary definitions inspected are
`cranelift-codegen-meta-0.125.4/src/shared/instructions.rs`, definitions `fmin`,
`fmax`, and `fcvt_to_sint_sat`; `Cargo.lock` pins that version.

The earliest divergent owner is
`rumoca-exec-cranelift::emit::try_lower_scalar_fold_tensor_update`. It now selects
element zero when address validity is false, before multiplying the stride or
loading/storing memory. The checked nonempty range and lane/stride coverage
prove that zero is a valid address. Existing node matching still uses the
original validity predicate, so unmatched patches write back the unchanged
value at that safe address. This adds one integer constant and one SSA select,
without allocation, tensor expansion, a new error status, or a public API.

One registered checked-constructor test exercises live indices 1/2 against
independent results `[99, 20]` / `[10, 99]`; 0, 3, 1.5, both infinities, and NaN
must preserve `[10, 20]`. Each runs under both activation polarities. It compares
canonical block evaluation with actual issued native calls, preserves input
bits (including NaN), and requires 16 JIT calls. Output agreement is a value
regression control, not a substitute for the source address-safety proof.
The existing native test helper's admission is reused and unchanged.

Exact selector, expected **1**:

```sh
cargo test --locked --offline -p rumoca-exec-cranelift --lib \
  emit::fold_tests::scalar_update_address::scalar_fold_patch_matching_keeps_invalid_coordinates_safe_and_unchanged
```

Formatting and diff checks passed. Cargo qualification is **UNRUN** because
the root agent owns the guarded lane. An `rg` audit finds no remaining `bnot`
calls anywhere in the native crate at this source. Governing ownership remains
SPEC_0007/SPEC_0040 compact fold execution, SPEC_0032 §4 native compact tensors,
SPEC_0029 single-owner helpers, and SPEC_0033 source-bound evidence.
