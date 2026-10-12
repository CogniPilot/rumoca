# Compiler boundaries exposed by scalar projection coverage

The frozen `d623d84f994b194c00e242970d9f9026989f3ffe` qualification passed eight
scalar-owner tests and the original two-instance compiler test. Its fresh
public scalar run passed all five original regression tests, then failed the
two added positive coverage fixtures before rendering. The original seven
tests remain unchanged.

`tensor_call_views_and_products_preserve_row_major_values` failed ED020 at
`cross({u,2,3},{4,5,6})`: the checked builtin requires Real vectors, but its
Integer argument was not promoted. `finite_fold_break_and_runtime_branch_keep_the_carried_values`
failed ED019 because every function containing `break` was routed to a narrow
Integer arithmetic-series proof, including a finite Real/local-array loop.
The stopped run did not execute the subsequent workshop C or browser gates.

Compiler-only before probes are frozen at
`f31453c79593556269f6761308954e8c06f7894c`. They add mixed numeric cross,
Integer-only cross/skew, and finite Real prefix-break admission cases. These
additional probes are not yet executed.

The repair reuses the existing Integer-to-Real value projection before checked
cross/skew construction (SPEC_0022 ARR-009 and ARR-037). The existing proved
runtime Integer reduction remains its own path. A leading break selected only
by the loop's own binder and settled numeric arithmetic trims a finite loop to
its exact execution prefix. Canonical progression intersection preserves stride
and direction without visiting domain points. Empty, first, never, reversed
comparison, and descending cases are covered. Other break profiles retain
their checked refusal. Source anchors and lexical loop ownership are retained.

Five core tests include 1,512 independent reference executions, using the
existing statement interpreter with actual innermost break control flow. They
also check runtime/nonleading refusal, nested binder shadowing, an invalid
suffix that must never execute, and oversized progression metadata admission.
Supplementary checks project existing constructor-backed nested call and fold
fixtures, then evaluate both the original checked call and its scalar view
against independent expected values. No test or coverage thresholds changed.

The settled-Integer helper used for the new proof also exposed an unchecked
`i64::MIN % -1` operation. Its before test is frozen at
`2cc4ec454`, before the one-line checked-rem/checked-div repair `b31785f41`.
The test pins a refused result for overflow, zero and fractional division and
retains valid negative and minimum-Integer controls. This probe is unexecuted.

Source formatting and `git diff --check` passed. The new compiler, progression,
settled-Integer and supplementary scalar assertions have not been executed;
the final exact seven public cases must still qualify before claiming success.
