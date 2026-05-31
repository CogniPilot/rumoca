# Strict Eval Rollout

This page records the roll-forward plan for removing silent default-to-zero
evaluation from production compiler/runtime paths. Normative phase and crate
rules remain in `spec/`; this guide explains the migration sequence.

## Goal

Production evaluation should return an error when an expression, variable
reference, runtime slot, table, or backend input is missing or unsupported.
Defaulting to zero is allowed only at explicitly named caller boundaries, such
as a solver initial-guess policy.

The first targets are:

- `rumoca-eval-dae` internal helpers that currently default unhandled
  expressions or missing bindings to zero.
- `rumoca-eval-solve` runtime reads that currently default missing state,
  parameter, seed, or register slots to zero.
- Backend interpreter paths that mirror the permissive Solve-IR behavior.

## Roll Forward

The migration is intentionally not compatibility-preserving. New production
evaluator APIs should expose errors directly; callers that still need a
fallback must choose an explicit policy at the boundary.

1. Convert exported evaluator APIs to return `Result<T, EvalError>` or a runtime
   layout error.
2. Replace implicit defaults with explicit caller policies such as
   `EvalGuess::SolverInitialGuess`.
3. Add source guards that prevent the known default-zero fallback surface from
   growing while each helper is converted.
4. Run the MSL quality gates after each behavioral slice and promote baselines
   only through the normal reviewed promotion flow.
5. Remove each permissive fallback as its caller has an explicit error or
   initial-guess policy.

## Acceptance Checks

- No production evaluator helper silently substitutes `0.0` for an unsupported
  expression or missing binding.
- Solve/runtime APIs validate vector dimensions before residual or Jacobian
  evaluation.
- Any remaining default-to-zero behavior is named as an initial-guess or
  recovery policy at the caller boundary.
- MSL trace and balance gates stay active while strict behavior rolls through
  the evaluator stack.

## Test Expectations

Each converted evaluator branch should add a negative test that proves the
new error is reported. MSL parity should be run before promoting any baseline
after behavior changes.
