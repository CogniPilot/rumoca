# Exact Integer comparisons after quotient lowering

The public BEFORE at `4907c9b5afb86c1bf1571203b15387aa8b3ef781` confirms
the source hypothesis in `enclosing-comparison-before.md`. On the same compiled
model, Interpreter passes the parameter/input controls `(-7,0)` and `(0,-7)`.
At `(0,0)` it produces `x(0.1)=0`, where exact Integer arithmetic requires
`x(0.1)=0.1`. The first failure stops the test before its Auto cases.
The frozen raw log, guardian and source recipe are retained under
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/`
`pr427-wide-integer-comparison-before-1-diagnostic-source-20261011/`.

The first divergent producer is Solve scalar projection, not parsing, the
quotient's rounding rule, or the ODE integrator: the exact quotient is `2^53`,
but its enclosing Integer addition passes through a Real register before the
comparison. It rounds `q+1` back to `q`. Below-boundary controls distinguish
this defect from a constant branch or a stale parameter/input read.

MLS §3.5 and §6.7 require Integer operands to retain their Integer type during
comparison. SPEC_0007/SPEC_0040 SOLVE-C84 place this expression in the existing
typed Solve owner. SPEC_0029 requires reuse of that owner and its arithmetic;
SPEC_0033 requires the original negative proof and independent controls.

`ExactExpressionForm::try_admit` now selects Integer quotients and comparisons
of two Integer operands before scalar literal folding. It admits the entire
expression once, captures issued coordinates/free domain binders, and uses
the existing `ExpressionLowerer`, checked arithmetic, owner cache and native
emitter. Only the Boolean result is published for a comparison. Unsupported
typed expression vocabulary retains explicit refusal. No alternate evaluator,
floating rounding repair, model-name branch, new public API or tensor expansion
is introduced. Real/Integer comparisons retain Modelica's Real conversion.

Integer event-root programs use the exact predicate's signed truth surface
(-1 for true, +1 for false); Integer operands change at discrete events
(MLS §3.8.5). Subtracting scalar operands would repeat the precision defect or
introduce an overflow absent from the source comparison. Real root-distance
programs retain their existing numerical surface.

The existing original regression remains intact. Additional controls cover
all six comparison operators, deliberate mixed Real promotion, direct native
compilation of every retained owner, and eleven actual Cranelift session
samples without a refusal or decline. Qualification of this repair is pending:

```sh
CARGO_BUILD_JOBS=4 RUST_TEST_THREADS=4 RAYON_NUM_THREADS=4 \
  cargo test --locked --offline -p rumoca --no-default-features \
  --features fmu-packaging --test suite_core \
  reported_bug_runtime::integer_quotient_comparison:: -- --nocapture
```

Expected selection: three tests. The structured B.1c outer-loop failure is
separate: its actual BEFORE identifies two different PureCall owners at
point 1, operation 1. The independent original owned-comprehension and the
supported parameter-divisor domain/overflow controls both passed at the
same `4907c9b5a` source. None of those results qualifies this later repair.
