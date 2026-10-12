# Checked broadcast division directional candidate

The actual `9188a0b313085a5cc3459c7ffec4f4a937dbe224` temporary diagnostic
compiled all nine primal call owners for the unchanged Glycol viscosity probe,
then failed in scalar differentiation at Integer equality expression 1576 in
`Modelica.Math.Matrices.LAPACK.dgelsy_vec`, with four active function frames.
That diagnostic and its artifact hashes are recorded in
`glycol-context-diagnostic-before.json`. It produced no CSV. Temporary tracing
changes and the separate contextual-owner reconstruction proposal are excluded
from this candidate.

The source chain explains how that scalar fallback is reached:

- `TypedProgram::derive_directional_owner` returns `None` when
  `program_supports_directional` refuses an operation or a nested call whose
  directional owner is unavailable.
- The previous table explicitly refused scalar-on-left `BroadcastBinary Divide`.
  The authored Glycol initializer contains `1 ./ Cv.from_degC(tableViscosity[:,1])`.
- `emit_typed_pure_call` returns `None` for an unclocked continuous call without
  its directional relation. Its scalar fallback enters nested function frames;
  the actual diagnostic identifies the later contextual Integer comparison
  refusal there. The diagnostic did not record each owner's directional flag;
  closure of this chain for Glycol still requires the actual repaired MSL gate.

The candidate retains the original direct primal broadcast quotient and builds
one scalar-bodied checked `Map` for its tangent, over the checked aggregate
dimensions. The map applies the existing scalar quotient partial rule in operand
order. Scalar division and both broadcast orientations use one private partial
constructor; there is no function-body copy, independent evaluator, target
renderer, new public API, wire change or model-specific case.

Normative owners are SPEC_0007 / SPEC_0040 SOLVE-C51, C52 and C56, SPEC_0029,
SPEC_0032, and MLS §10.6 elementwise scalar expansion (SPEC_0022 FUNC-027).
Generated axes carry explicit Generated provenance and checked Integer indices.
The primal preserves IEEE binary64 division, including signed zeros, infinities
and NaNs. At singular points the derivative retains the existing finite-local-
partial kink rule; it does not change primal domain admission. Incorrect tangent
shapes remain typed errors.

The generated index domain is checked by the existing Map-domain constructor
against the owner's Integer profile before advertising the derivative. A shape
whose ordinal indices do not fit that profile retains its admitted primal owner
and has no directional relation. A constructor regression covers both supported
and unsupported extents and a profile excluding index 1. The canonical tensor
constructor refuses zero extents before owner issuance; the test asserts that
actual refusal instead of claiming an admitted empty tensor.

Two ordinary `rumoca-phase-solve` tests reuse the source-backed division fixture:
independent central differences for scalar-only, tensor-only and mixed tangents
in both orientations and three ranks; plus singular, signed-zero primal-bit and
invalid-shape controls. The original direct-division test remains unchanged.

```sh
cargo test -p rumoca-phase-solve --lib broadcast_division_directional_ -- --nocapture
cargo test -p rumoca-phase-solve --lib plain_and_elementwise_tensor_division_preserve_direct_binary64_results -- --nocapture
cargo test -p rumoca-ir-solve --lib broadcast_division_directional_preserves_narrow_integer_profile_primal_owner -- --nocapture
cargo test -p rumoca --features msl-sim-tests --test suite_core msl_glycol_viscosity_uses_its_checked_coefficient_interface -- --nocapture
```

Expected counts are 2, 1, 1 and 1 respectively. Workspace CI selects the first three
without a feature gate; the original MSL test uses its existing mandatory
`msl-sim-tests` fixture owner. This candidate has passed rustfmt and diff checks
only. Compilation, unit execution, the original MSL numerical oracle and backend
qualification remain **UNRUN** pending the root's serialized cache lease.
