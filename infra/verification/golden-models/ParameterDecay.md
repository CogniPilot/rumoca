# Golden model: `ParameterDecay`

## Record identity

[`registry.toml`](registry.toml) is authoritative for this record's claim kind,
review time, canonical source path and digest, declared scenarios and endpoint
dispositions, coverage-footprint binding, and, on promotion, the outstanding
receipt ledger. The canonical fixture is
[`crates/rumoca/tests/fixtures/golden/ParameterDecay.mo`](../../../crates/rumoca/tests/fixtures/golden/ParameterDecay.mo);
the bound core tests consume it directly and this record does not duplicate
its source text.

## Source

```modelica
model ParameterDecay
  parameter Real a = -1.0;
  Real x(start = 2.0, fixed = true);
equation
  der(x) = a * x;
end ParameterDecay;
```

One parameter, one state, one equation. No arrays, events, clocks, discrete
variables, functions, algebraic loops, connectors or connections.

## Why this model is the next increment

**It is the smallest addition to the trusted core that adds exactly one axis.**
Its cone is `UnitDerivative`'s cone plus a bounded delta: a parameter
declaration, evaluation of its binding into the P storage column, and a
derivative kernel that reads parameter and state storage and multiplies them
instead of storing a constant. Every later axis depends on parameters, so this
is a prerequisite rather than a detour.

**It strengthens an oracle that `UnitDerivative` cannot.** Every consistent
integration method integrates a constant derivative exactly at any step size,
so `x(t) = 2 + t` proves the execution plumbing and is insensitive to
integrator accuracy, stability and step control. The oracle here is
`x(t) = 2 e^{a t}`, which no Runge-Kutta method integrates exactly, so the
native-execution endpoint is a genuine check on the integrator.

**It reaches the second `fixed`-default site.** The MLS 3.6 §4.8.1 parameter
default (`fixed = true` when unspelled) is decided at the checked-DAE join in
`crates/rumoca-ir-dae/src/model.rs` (`effective_fixity`), and separately in
the flat layer: `crates/rumoca-ir-flat/src/lib.rs`
(`unbound_fixed_parameters`, `fixed.unwrap_or(true)`) and
`crates/rumoca-phase-flatten/src/pipeline/context_and_tests/parameter_lookup.rs`
(`fixed != Some(false)` selecting structural parameters). `UnitDerivative` has
no parameter, so neither flat-layer site is reached. `a` spells no `fixed`, so
this model executes the flatten-side decision on its path and the duplication
is inside this cone rather than outside it.

## Profile: `parameter-decay-v1`

The profile is deliberately narrower than `unit-derivative-v1`. It requires:

1. `native-interpreter-rk-trace`: native interpreter and RK execution against
   `x(t) = 2 e^{-t}` on `0 <= t <= 1`, with the tolerance stated below.
2. `direct-galec-continuous-refusal`: the direct phase-GALEC
   continuous-dynamics refusal, typed at the state.
3. `registered-galec-continuous-refusal`: the registered `galec` target's
   refusal, typed at the state.
4. `registered-efmu-continuous-refusal`: the registered `efmu` target's
   refusal, typed at the state.

It omits two endpoint groups on purpose:

- **`dae-solve-production-refinement` is not in the profile.** The C61
  profile `EventFreeBinary64ScalarConstantDerivative` admits exactly one DAE
  variable with the derivative kernel `[Const(c), StoreOutput]`. This model
  declares two variables and its kernel is
  `[LoadP, LoadY, Binary(Mul), StoreOutput]`, so the production lowering
  carries the typed disposition `VariableCount { actual: 2 }` instead of a
  receipt. A profile that requested the receipt would have to carry it as
  permanent `outstanding` debt for a receipt that can never be minted; the
  honest shape is to leave it out, and the registry checker refuses both a
  request for it and a ledger entry naming it under this profile. The unbound
  test `parameter_decay_is_outside_the_scalar_constant_derivative_profile`
  pins the disposition so that a widened C61 profile changes a test rather
  than silently starting to mint.
- **No FMI3 endpoint is in the profile.** The value of FMI3 for this model
  is the parameter write policy (`fmi3SetFloat64` on `a` admitted in the
  modes FMI 3.0.2 names and refused elsewhere) exercised through generated C.
  No scenario exercises that yet; requesting the six FMI3 endpoints without
  it would certify the same lifecycle `UnitDerivative` already certifies and
  make no claim about the one thing this model adds at the FMI boundary. A
  later profile revision adds them together with that scenario.

CasADi, CUDA, DAE-Modelica, FMI 2, FMI-LS, JAX, MLIR, Rust, WGSL, and every
other unrequested backend are outside this record.

## Intended semantic facts, as observed by the bound tests

- Resolved `a` carries the `parameter` prefix and an explicit binding that is
  the unary negation of the unsigned literal `1.0`; it spells no attribute
  modifier. Resolved `x` is a scalar `Real` with `start = 2.0` and
  `fixed = true` retained exactly, and `der(x)` resolves to that declaration.
- Flat retains both declarations in source order with distinct instance
  identities. The parameter binding is still the negation of the literal,
  `fixed` is still absent on `a` (Flat carries absence through), and the one
  residual is `der(x) - a * x` over instance-identified references.
- The checked DAE gives `a` role `Parameter`, variability `Parameter`,
  causality `Parameter`, `fixed = Fixed` (the §4.8.1 default applied at the
  join), tunable, with the binding retained unevaluated; `x` is a `Fixed`
  continuous state with start `2.0`. The single continuous residual is
  `Subtract(Derivative(x), Multiply(Parameter(a), State(x)))` with provenance
  `der(x) = a * x`, and there are no initialization owners: the binding is a
  value, not an equation.
- Solve evaluates the binding bit-exactly to `-1.0` in the P column
  (`ScalarSlot::P { index: 0 }`), catalogs `a` as a tunable parameter with
  start `[-1.0]` and `x` as an exactly initialized state in
  `ScalarSlot::Y { index: 0 }`, and the derivative program is exactly
  `[LoadP{0}, LoadY{0}, Binary(Mul), StoreOutput]`.
- Native RK execution produces eleven samples on the `0.1` grid, each within
  `1e-9` of `2 e^{-t}`.
- Both eFMI routes refuse at the state declaration span: `EGT001` with
  `ContinuousDynamics { states: 1, equations: 1 }` then `NoPeriodicClock` on
  the direct route, and `EC009` `continuous_states` on the registered `galec`
  and `efmu` targets.

## Trace tolerance

The trace is not integrated exactly, so the bound is derived from the
integrator contract rather than read off the residual. The options request
`rtol = atol = 1e-12` with a unit state nominal, so the Dormand-Prince 5(4)
controller accepts a step only when its embedded estimate is at most
`atol + rtol * |x| <= 3e-12`. The system contracts (`a < 0`), so accepted
local errors accumulate without amplification: after `N` accepted steps the
endpoint error is at most `N * 3e-12`. Each `0.1` output sample is read from
the order-four continuous extension of the accepted step containing it,
whose error is of the same order as the accepted estimate. The stated bound
`1e-9` therefore covers roughly three hundred accepted steps plus the
interpolation, which the controller never approaches on `[0, 1]` from a
`0.01` initial step. It is also tight enough to matter: any method of order
four or lower that stepped only on the `0.1` output grid would miss by at
least three orders of magnitude, so the check exercises the step controller
and the dense output rather than the tableau alone. The residual observed at
this compliance check was below `6e-13`, roughly three orders of magnitude
inside the bound; the bound was not adjusted to it.

## Governing specifications

- `SPEC_0007`: IR stage and target-emission contracts.
- `SPEC_0008`: typed phase diagnostics.
- `SPEC_0022`: Modelica derivative and initialization semantics.
- `SPEC_0029`: crate and authority ownership.
- `SPEC_0033` §6c: working-model proof admission.
- `SPEC_0034`: eFMI/GALEC restriction and refusal behavior.
- `SPEC_0037`: proof claim levels and checker discipline.

## Claim and open criteria

This record is a `candidate`. Every endpoint of `parameter-decay-v1` is
observed or typed-refused by the exact tests bound in `registry.toml`. No
coverage footprint is bound yet: the capture at this compliance check ran all
three bound scenarios to a pass under instrumentation, but its merge was
refused because the scenario captures disagreed on the digest of
`crates/rumoca-eval-flat/src/phase_constant/mod.rs`, which was edited while
the capture was running. That refusal is the tool working as specified; the
footprint was not hand-assembled from the partial scenario captures. A fresh
`cargo xtask coverage golden ParameterDecay` on a settled tree binds it.

Promotion is withheld for reasons this record states rather than the checker
enforces:

1. The §4.8.1 parameter `fixed` default is still decided at two layers, and
   the flatten-side decision is executed for `a` on this model's path. The
   review criterion for admitting the first parameterized golden model is that
   the default be decided at exactly one site, with the flat duplication
   closed rather than left out of cone. That is a production change and is
   not made by this revision.
2. The parameter's FMI write policy is not exercised. The FMI3 endpoints are
   outside the profile until a scenario drives `fmi3SetFloat64` on `a`
   through generated C in the admitted and refused modes.
3. The working tree is under active revision: the `UnitDerivative` reviewed
   binding on the same tree already fails its digest check on files that
   moved after its capture, and this record's own capture raced a concurrent
   edit under `crates/rumoca-eval-flat/`. A pinned binding needs a settled
   tree.

The checker-enforced bar is met. The registry gate accepts a reviewed claim on
this profile with `outstanding = []` (the gate fixture
`parameter_decay_profile_with_empty_debt_is_accepted` proves the shape), so
promotion is a claim-kind edit plus a fresh capture once the criteria above
are closed.

## Evidence observed in this compliance check

- `cargo test -p rumoca --test suite_core parameter_decay`: `4 passed,
  0 failed`. The four tests are the three bound scenarios and the unbound C61
  disposition witness.
- `cargo test -p rumoca --test architecture_hardening_test
  golden_model_registry`: the profile, fixture-consumption and gate-fixture
  tests pass; the whole-registry check on this tree fails on the
  pre-existing `UnitDerivative` digest drift, not on this record.
- `cargo xtask coverage golden ParameterDecay` under the `full` development
  shell: each of the three bound scenarios listed exactly once and passed
  under instrumentation (`1 passed, 0 failed` three times); the merge was
  refused with `scenario captures disagree on source digest for
  crates/rumoca-eval-flat/src/phase_constant/mod.rs`, so no footprint was
  written or bound.
