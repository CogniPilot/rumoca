# Main-DAE Integer quotient comparison before probe

This test-only source is based on root `1447ed716` and includes no production
repair or execution. The existing #411 quotient, native arithmetic, scalar ABI,
iterator, and tensor repairs remain unchanged.

The public model retains tunable `parameter Integer n=0`, adds live
`input Integer m(start=0)`, and preserves the expression
`div(63050394783186944+n+m,7)+1 > div(63050394783186944+n+m,7)` as the guard of
`der(x)=if ... then 1 else 0`, with `x(start=0,fixed=true)`. Independent Integer
arithmetic gives quotients 9007199254740991 when n+m=-7 and 9007199254740992
when n=m=0.
Both are exactly representable in Binary64; adding Integer 1 then comparing
with the original quotient is true in both cases, so every trace sample is
`x(t)=t`. The parameter/input pairs (-7,0) and (0,-7) run before (0,0) on
the same compiled model. Explicit parameter overrides and initial input values
exercise separate runtime slots; the input prevents translation-time settlement
of the guard. The test requests the existing solver settings unchanged and
checks all 11 samples, exact endpoints, unique channel, and initial value under
Interpreter and Auto. Auto does not prove forced native execution.

The source hypothesis is at Solve scalar lowering: `exact_integer_quotient`
admits a root Div/Mod/Rem before f64 folding, but enclosing Integer arithmetic
and comparisons still pass through `binary_expression`/`binary` and their f64
`LinearOp::Binary`/`Compare` projection. At n=m=0 the floating addition rounds
9007199254740992+1 back to 9007199254740992, making the comparison false. Actual
public compiler/runtime outcome is **UNRUN**; source construction, frontend
selection, or another checked admission may still affect reachability. Expected
before, if admitted as inspected: both -7 controls pass and (0,0) yields x=0 instead
of t. This is not a claim that all Integer arithmetic was repaired by #411.

Governing rules: MLS §3.4 arithmetic and comparison types, §3.7.2 div (truncating
Integer quotient), §4.5 parameter variability, and §3.6.5 conditional expressions;
SPEC_0022 EXPR-024; SPEC_0007/SPEC_0040 checked arithmetic preservation;
SPEC_0029 shared canonical arithmetic; SPEC_0033 first divergence and before
evidence. SPEC_0036/0037 remain draft context, not accepted validation authority.

If the public before confirms this hypothesis, the narrow owner proposal is
to admit the whole supported Integer-based Boolean predicate through the
existing `ExactExpressionForm`/`ExpressionLowerer` typed owner before scalar
Binary/Compare projection, preserving exact i64 intermediates and publishing
only the Boolean result. Existing semantic expression identity, binder captures,
overflow/domain checks, lazy regions, and capability refusal must remain
authoritative. No second evaluator, f64 rounding patch, model-name exception, or
new public API is proposed. Unsupported source profiles must refuse explicitly
instead of computing the false branch.

CI registration uses the existing public runtime child suite. Exact gate,
expected selected tests **1**:

```sh
cargo test --locked --offline -p rumoca --features fmu-packaging --test suite_core \
  reported_bug_runtime::integer_quotient_comparison::integer_quotient_enclosing_comparison_preserves_exact_branch_and_full_trace
```

Formatting and diff checks passed. Cargo is **UNRUN**: root owns the guarded
lane. No original regression or acceptance assertion was weakened.
