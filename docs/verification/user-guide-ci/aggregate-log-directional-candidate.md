# Aggregate logarithm directional-owner candidate

Base `d65f1f659`; test-only BEFORE `9f8355509`, generic producer candidate
`30372aaad`. All new tests, producer code and original-model AFTER are UNRUN.
The temporary91ed aggregate-Log refusal diagnostic is also UNRUN: no assertion
is made yet that this operation causes original TestGlycol's missing outer
owner. Measured E91 chain is retained separately in
`glycol-directional-e91-actual.md`.

MLS3.7 section12.4.6 and SPEC0022 FUNC-027 require element-wise scalar-function
vectorization with matching shapes. Existing checked Solve UnaryLog/Log10
primal operations already preserve that shape. The directional construction
currently supports those operators only on scalar results. The proposed
extension stays within SPEC0007/SOLVE-C51's one checked call owner and
SPEC0032 sections2/4 compact scalar-view rule; SPEC0029 keeps the constructor
in its existing typed-owner module and evaluation in eval-solve. It creates
no scalar function-body copy, new evaluator, source-name branch, target syntax
or new wire representation.

The original aggregate primal operation is emitted once. A single checked Map
loads each operand/tangent cell and invokes the same scalar logarithm partial
helper. Its generated domain reuses broadcast quotient's checked shape/index
profile, now named pointwise_domain in the same pointwise module. Unsupported
index profiles retain the valid primal owner and decline its directional
certificate. The scalar construction order is unchanged: Log10 multiplies
its operand by ln(10), takes the reciprocal, keeps finite partials or zero,
then multiplies by the tangent. NaN/Infinity/signed-zero and negative-primal
behavior follows that existing scalar rule. Assertion predicates/message
outputs, caller identities, activation and callee metadata are untouched.

Focused exact selectors:

```sh
# BEFORE9f835 and AFTER: two tests, vector/matrix values, full shapes,
# independent analytic/central-difference tangents and nonfinite controls.
cargo test --locked --offline --jobs 4 -p rumoca-eval-solve --lib aggregate_logarithm_directional_ -- --nocapture
# Candidate AFTER: one profile/compactness test; six operator/profile cases.
cargo test --locked --offline --jobs 4 -p rumoca-ir-solve --lib aggregate_logarithm_directional_ -- --nocapture
# Existing scalar singular-value policy and broadcast index-profile controls:
cargo test --locked --offline --jobs 4 -p rumoca-eval-solve --lib directional_scalar_guards_match_checked_ad_at_singular_values -- --nocapture
cargo test --locked --offline --jobs 4 -p rumoca-ir-solve --lib broadcast_division_directional_preserves_narrow_integer_profile_primal_owner -- --nocapture
# Mandatory original end-to-end AFTER, feature selected and count exactly1:
cargo test --locked --offline --jobs 4 -p rumoca --features msl-sim-tests --test suite_core msl_glycol_viscosity_uses_its_checked_coefficient_interface -- --nocapture
```

These ordinary unit tests are selected by workspace CI; the original Glycol
regression is selected by the existing msl-sim-tests feature lane. A successful
constructor or finite-difference unit does not substitute for the unchanged
original model, settled initial state or actual simulation trace. Required
CPU qualification remains mandatory; no new skip or dependency flag is added.
