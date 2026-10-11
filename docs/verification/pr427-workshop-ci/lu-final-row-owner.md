# LU final-row construction and inverse fixture boundaries

The ten `native_lu_inverse::` tests actually ran at
`b4e38503723816d0529168c46ac1b027ca1ea73d`: five passed and five failed.
The retained raw log is
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/pr427-lu-inverse-10-b4-20261011/lu-inverse-10.log`,
SHA-256 `8cad714402ec5bc2b7ab96c48327eb853b109a5afaaf0dd1dcd53be9b1ff516f`.
The three square-LU failures reached ToDae `InvalidExpressionArity`,
expected one conditional arm and found zero. The continuous inverse fixture
was refused for reading continuously defined model Integer pivots. The
literal invalid-pivot fixture reached EL005 during constant evaluation,
before the runtime assertion message the test expected.

`LapackBuilder::lu_factorization` visits every pivot step. For a square
matrix its final `swap_selected_row` receives one row and one selection
condition. Previously the shared helper passed empty `taken[1..]` and
`rows[1..]` slices to the checked conditional constructor. That constructor
correctly rejected the empty conditional. A rectangular 3-by-2 LU retains
two rows at its final step and passed, matching this first divergence.
The repair returns the unchanged sole row for this identity interchange.
Pivot publication, singular-status calculation and retained multipliers
still execute; the conditional constructor stays strict.

The helper is private and has two callers. LU builds `taken` from exactly
the magnitudes of `rows[step..]`; `first_largest` emits one Boolean per
magnitude. Thus both slices have the same positive length. The checked
LU interface requires both matrix extents to be positive, and
`step < min(height,width)` ensures at least one remaining row. The other
caller is `zero_pivot_step` through `eliminate`, guarded by `step + 1 < n`:
it always supplies at least two rows, so its behavior is unchanged. No
empty-row or unaligned-selection admission is added.

The ownership contract is accepted SPEC_0007's referenced DAE-C34 row in
SPEC_0040, with MLS §12.9 and declaration initialization in §12.4.4.
The change stays in the existing shared DAE LAPACK body constructor and
retains compact row arithmetic under accepted SPEC_0032. It introduces no
foreign interface exemption, evaluator, dependency or wire change.

The continuous inverse now keeps LU factors, Integer pivots and statuses
inside a function and publishes Real model outputs. The original illegal
model is retained as a separate refusal test under MLS §3.8.5 / EXPR-012;
its pivots are read, so the documented unread-observation extension cannot
apply. The inverse still checks all four independent analytic coordinates
on exactly 0, 0.1 and 0.2, together with both success statuses.

Invalid-pivot coverage uses one compiled model with a live Integer input.
Both Interpreter and Auto run the valid identity case first, then consumed
pivots 0 and 3. The exact `LAPACK dgetri pivot index out of range` assertion
remains required. The original literal cases are separately retained as
EL005 constant-call assertion refusals. This distinguishes translation
failure from the runtime fault without accepting a fabricated inverse.
Auto alone does not prove native execution.

The new one-by-one positive and singular LU controls isolate the shared
identity-interchange owner and require pivot 1, factor preservation and
status 0 or 1 across the complete grid. The existing rectangular tie,
noncommuting pivot, later-singular, unused-last-pivot, singular-inverse and
ordered-interface refusal controls remain selected.

Proposed focused qualification, **not run by this source-only agent**:

```sh
cargo test --locked --offline -p rumoca --test suite_core native_lu_inverse:: -- --nocapture
```

This selector now selects **13 tests**, with no ignored tests or fixture
downloads. Its module is registered unconditionally in `suite_core/main.rs`,
an ordinary default-feature test target selected by the existing workspace
CI tests. Shared-owner follow-ups are `native_linear_solve::` (two tests)
and `native_least_squares::` (three tests), then the unchanged authored
Kalman workshop regression. These follow-ups and instrumented coverage
remain pending; no full CI, MSL parity or 2 GiB Euler result is claimed.

Signed source commits are `3e3a32232e3fa7aa03a4c292552903c25487b723`
(legal/live and single-row controls),
`74c6171548beb80a0e77d8ceac37eb877b943f32` (shared constructor repair),
and `9038d5b95` (retained constant refusal controls). Rustfmt and
`git diff --check` passed locally. ROOT owns all build qualification.
