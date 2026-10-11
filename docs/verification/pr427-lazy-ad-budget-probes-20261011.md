# PR 427 selected-region AD and final projection-budget probes

## Frozen source and evidence status

These test-only probes start from candidate
`d623d84f994b194c00e242970d9f9026989f3ffe`. No production algorithm, template,
budget, capability, accepted specification, or fixture in a frozen worktree is
changed. This document records source-backed hypotheses, not executed failures.

Rustfmt and `git diff --check` passed. Cargo compilation, exported Python
execution, and native evaluation have **not run**: ROOT holds the exclusive
shared build/cache lease. No timeout or memory measurement is claimed.

## Selected-region differentiation

`backend_template_runtime_regression/jax_lazy_conditionals.rs` compiles an
actual Modelica function with an algorithm `if u > 0 then sqrt(u) else
sqrt(-u)`. At `u = -1` and `u = 1`, the independent value is 1 and the
independent derivative is respectively -0.5 and 0.5. The test first checks the
native public DAE value and exact checked state JVP, then renders the actual
`jax-ode` target and executes exported RHS, JIT RHS, `jacfwd`, `jacrev`, and
`grad`. All Python observations are printed; all mode failures are collected
before reporting the Python assertion. The positive control uses the same
source with a constant-true selector at `u = 1`.

The expected source semantics follow [MLS §11.2.6]
(https://specification.modelica.org/maint/3.6/statements-and-algorithm-sections.html#if-statement):
only the selected algorithm body contributes to the evaluation. These tests
do not assume short circuit evaluation of Boolean `and`/`or`, do not test the
switching point `u = 0`, and do not substitute finite differences for checked
native AD.

The suspected divergence is `scalar_ops/pure_calls.rs` lowering both runtime
conditional regions and selecting their results, followed by the JAX template
emitting `jnp.where`. The [official JAX FAQ]
(https://docs.jax.dev/en/latest/faq.html#gradients-contain-nan-where-using-where)
explains that an undefined inactive `where` branch can contaminate reverse AD.
Whether this exact exported program exhibits the defect remains unmeasured.
If compilation, native AD, or target admission fails earlier, preserve that
actual first divergence instead of reporting an exported AD failure.

Both tests have `jax_` in their names. They are reachable through the existing
JAX group in `crates/xtask/src/verify_cmd.rs`, which selects the actual
`suite_template_runtime` binary with `filters: &["jax_"]`; no new group or
external-toolchain bypass is introduced.

## Final operand expansion

`scalar_ops/budget_tests.rs` builds a compact checked `ScalarProgramBlock`
through its canonical constructor: one identity tensor, one filled RHS,
`LinearSolveComponent` per output, and one compact output range. It calls the
actual shared `lower_scalar_ops` admission boundary, with no mock evaluator.
The size-2 control executes both original and projected programs through the
native checked scalar evaluator and independently verifies `I*x=[2,2]`.

The size-200 negative control independently counts the matrix and RHS register
references that the C and JAX scalar templates list for each solve component:
`200 * (200*200 + 200) = 8,040,000`. That exceeds the existing 4,000,000
projection budget. The test expects admission refusal and never invokes a
template or allocates generated text. The suspected divergence is
`expansion_cost` charging `LinearSolveComponent` one scalar operation, although
its final template operands scale quadratically per component. The original
owner stays compact; only the admitted scalar consumer needs the full cost.

This follows SPEC_0032 §§6–8 (shared scalar projection and compact owner),
SPEC_0007's faithful target admission contract, and SPEC_0033 §§2–3,6 (first
divergence and actual focused proof). It does not change the budget ceiling or
claim that the candidate has timed out.

## Queued recipes

Run serially under ROOT's fixed-source resource guard after the lease is
granted; use the existing owned target and Cargo home, fixed 4-thread limits,
debug information off, incremental off, and pinned JAX Python environment.
Retain source SHA, actual exit status, full stdout/stderr, and guard receipt.

```sh
cargo test --locked --offline -p rumoca-eval-solve --lib scalar_dense_solve_ -- --nocapture
cargo test --locked --offline -p rumoca --features template-runtime-tests --test suite_template_runtime jax_lazy_conditionals:: -- --nocapture
```

These are four tests total: two native/projection budget tests and two public
source/exported JAX tests. They must be recorded individually. A compile-only
failure is not a semantic negative receipt. No production fix or issue closure
is authorized by this source-only record.
