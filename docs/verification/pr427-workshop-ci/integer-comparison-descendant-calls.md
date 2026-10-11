# Exact Integer comparisons containing calls

The public model in `integer_call_comparison.rs` compares `inc(n)` with zero
using a live Integer input. For `n=0`, its state must be `x=t`; for `n=-1`,
`x=0`. Both execution policies inspect all eleven output times from zero to
one and report the actual session execution receipt. Auto is not assumed to
have used a native engine.

The test-only BEFORE is `08abe08374aad0c439e5e9742319d094325ca37c`, based on
`053683fe559a446e3a983a18fd677ee1db2cbe02`. At that source the comparison is
claimed by `ExactExpressionForm`, whose admission rejects every descendant
Call before the existing canonical typed call lowerer is reached. The
numerical failure remains unmeasured in this source-only candidate.

The repair reuses `register_expression_calls` and the existing
`ExpressionLowerer`, retaining original call owners and typed Integer values
inside the enclosing checked owner. The expression registry retains that
owner's complete result and assertion interface. Each existing assertion
predicate and captured message slot is projected into the enclosing tuple by
one checked helper shared with ordinary nested-call registration. Root,
action, and message projections replay that same issued invocation.

The two additional controls exercise a nested asserting callee. An active
holding assertion and an inactive violating call both retain `x=t` on the
complete output grid. An active failure must report the declaring callee's
captured `n=-2` and `y=-1`, rather than the enclosing function's result. They
run under Interpreter and Auto without an assertion-free admission exception.

This follows SPEC_0007/SPEC_0040 SOLVE-C24, C25, and C51, and MLS sections
3.5, 6.7, 8.3.7, and 11.5. Unsupported function bodies and unsupported message
conversions retain the existing checked-construction refusals. No separate
Integer evaluator, scalar CallResult transport, or target dialect is added.

All three tests belong to the ordinary `suite_core` workspace suite. The
focused selector is `reported_bug_runtime::integer_call_comparison::`, count
three. The original BEFORE selector is its
`integer_comparison_descendant_call_preserves_live_input_and_full_trace` test,
count one. Formatting and diff checks were run; Cargo qualification is pending
the parent's exclusive resource lane.
