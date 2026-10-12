# Conditional array operand scope

The 566-model run at `86c3bc1a6128e3d3174fefa169ae7bd5e03243e9`
rejected 14 previously certified models during ToDae with ED020,
"model expression escaped its construction-issued conditional arm".
The BooleanTable source used by several of these models contains a tunable
conditional whose selected value constructs an array comprehension.

The first confirmed divergence is in `rumoca-ir-dae` array construction.
`comprehension` checked its body as a completed domain-owned model expression;
`range` checked its bounds as completed model expressions. Those checks require
the model arm to have been discharged even while construction is inside that
exact issued arm. Array construction only closes its binder domain. The
enclosing conditional remains responsible for discharging the arm.

The repair reuses the existing coordinate/domain checks for these operands.
The common expression-insertion owner still checks each child's lexical arm
visibility and stamps the resulting array with the current arm. Completed
equation, variable, and function owners retain their original discharge checks.
No checker accepts an escaped or sibling-arm expression.

Grounding: SPEC_0007 Stage 3 and SPEC_0040 DAE-C22; SPEC_0029 crate ownership;
SPEC_0032 §1 compact domains; MLS §3.6.5 conditional selection and §10.4.1 array
constructor iterator scope. Shapes, operands and activation ownership remain
compiler-issued; this introduces no scalar expansion or duplicate proof.

The test-only before commit and production after commit, counts and raw-log
digests are in `conditional-array-operand-scope.json`. Both new positive tests
failed before with InvalidModelActivationScope. After the repair, all 223 DAE
unit tests passed, including 14 conditional-owner tests. The new fixtures also
prove sibling/outside access is rejected, current JSON replay preserves issued
ownership, and changing extent from 2 to 1,000,000 does not increase expression
count.

Commands use CARGO_BUILD_JOBS=4, RUST_TEST_THREADS=4, RAYON_NUM_THREADS=4:

```sh
cargo test --locked --offline -p rumoca-ir-dae --lib tests::model_conditionals::operand_scope -- --nocapture
cargo test --locked --offline -p rumoca-ir-dae --lib tests::model_conditionals -- --nocapture
cargo test --locked --offline -p rumoca-ir-dae --lib
```

This focused producer proof does not establish that all 14 originating MSL
models now pass. Their full-model and fixed-canary verification remains pending,
as does a complete post-repair OMC comparison. No parity or issue-closure claim
is made here.
