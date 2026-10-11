# Selected function regions in scalar exports

Before source: `8e5ba0c3778ca705ce8774ae23c0c885d82accf2`.
Root executed the two original public JAX probes in
`pr427-grouped-jax-selected-region-before-20261011`, under the shared bounded
guardian. The runtime-predicate probe failed; the constant-predicate control
passed (1 passed, 1 failed, 0 ignored, 43 filtered).
The raw `jax-selected-region-before.log` SHA-256 is
`d8f77ced04283e8b148e0cf42f8f0f5f19ae999f74eeb02347e4734bea41ae5d`.
The attempt directory is beneath the machine-local
`$HOME/scratch/rumoca/slam-integration-cycle-20261010/`.

For `selectedRoot(u)`, the algorithm selects `sqrt(u)` when `u > 0`, otherwise
`sqrt(-u)`. At both `u = -1` and `u = 1`, native values and checked directional
derivatives were correct. Actual exported RHS, JIT and forward Jacobians were
also correct; reverse Jacobians and gradients were `NaN` instead of `-0.5`
and `0.5`. This rules out source branch selection and native AD as the first
divergence. These are CPU functional results; no GPU or performance claim is
made.

The first divergent producer is the shared pure-call scalar view:
`scalar_ops/pure_calls.rs::conditional` expanded both dynamic branch bodies
into the enclosing instruction list, then selected their already computed
values. Canonical `typed_program::eval_conditional` evaluates only the selected
region. JAX `where` cannot recover that execution boundary from eager operands.

The repair projects each existing checked `SolveProgramRegion` once into a
private scalar register file, through the same inliner and cumulative expansion
budget. It uses the existing checked `FunctionConditionalProgram` constructor,
whose correlated widths come directly from the original typed region outputs.
Runtime Real/Boolean capture elements use the existing numeric capture ABI;
exact construction-known Integer binders remain typed constants. No source
names, target language, new IR owner, or parallel AD body enter this projection.
Original operation provenance remains in the inliner's checked arithmetic.
The final scalar view has no invented reusable conditional-owner identity.

C, Rust, fixed Rust, CUDA, JAX and CasADi templates consume the same existing
region view. C/Rust/CUDA use ordered native branches; JAX uses branch functions
and scalar `lax.cond`; CasADi uses `Function.if_else` switches over MX function
graphs. The CasADi public `rhs` Function signature is unchanged. JAX comparisons
produce the binary64 scalar ABI consistently, including Boolean results of
correlated branches. Unsupported checked faults and expansion budgets remain
fail-closed; guards and model sources are unchanged.

Governing contracts: MLS 3.7 §11.2.6 / SPEC_0022 ALG-019 selected algorithm
branches; SPEC_0007 §3 and §3.6 / SPEC_0040 §4 executable target products;
SPEC_0029 §12 target-neutral ownership and template-only language syntax;
SPEC_0032 §3–5 shared scalar projections of authoritative structured owners;
SPEC_0033 §2–4 earliest divergence, proof and strict failure behavior.
SPEC_0036 remains DRAFT; no exception or status change is requested.

The two original selected-sqrt tests are unchanged. Five additional public
runtime tests exercise nested vector regions with runtime Boolean captures
and exact static Integer loop indices: C (including absence of inactive
`FE_INVALID`), Rust, fixed Rust, CasADi value/Jacobian, and JAX value/JIT/forward
and reverse AD. Each first verifies the original native values and checked
Jacobians. They are selected by the existing `c_ode_`, `rust_ode_`,
`rust_fixed_ode_`, `casadi_` and `jax_` backend filters; no selector is weakened.
The new CUDA template path still requires actual CUDA qualification; the CPU
tests do not qualify a GPU execution claim.

This source candidate has not been built or tested. Only rustfmt and
`git diff --check` were run while root held the heavy/cache lease. Required
next focused gates: the original two JAX controls, then the five new consumer
controls, then the existing target cohorts including actual workshop examples.

Batch frontier: JAX documents that `vmap` over predicates converts `lax.cond`
to eager `select`. This scalar repair does not establish direct `vmap` reverse
AD when an inactive branch has an undefined derivative. A separate unignored
batched probe preserves that counterexample; `lax.map` is the intended scalar
selection-preserving batching control. Neither is claimed qualified here.
`sequential_vmap` alone is unsuitable because its underlying `custom_vmap`
does not support reverse AD.

Primary references: [JAX cond](https://docs.jax.dev/en/latest/_autosummary/jax.lax.cond.html),
[JAX where gradient caveat](https://docs.jax.dev/en/latest/faq.html#gradients-contain-nan-where-using-where),
[JAX custom_vmap](https://docs.jax.dev/en/latest/_autosummary/jax.custom_batching.custom_vmap.html),
[CasADi conditional evaluation](https://web.casadi.org/docs/#conditional-evaluation).
