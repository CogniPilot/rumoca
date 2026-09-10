# Pinned FMI-LS-DAE draft fixtures

Upstream: <https://github.com/modelica/fmi-ls-dae>

Pinned commit: `5cd461aba3a00673fb9bffcd2f6565363cced1ce`

Vendored files (SHA-1):

- `fmi3LayeredStandardDaeManifest.xsd`: `4e2955629616c4de613691d0261a18ffa1ac3e72`
- `fmi3LayeredStandardManifest.xsd`: `3bcb471d4b94a082033e8569e88d7640d086bfa5`
- `LICENSE.txt`: `b296dee6f153d4f425e16b47e1785460ac05fc88`

## Use case

These schemas are retained as upstream test fixtures for a future target that exposes a Model Exchange FMU that is a normal
FMI 3 ODE component by default and can enter the pinned FMI-LS-DAE residual
profile in Configuration Mode. It is intended for DAE-capable importers that
need the original algebraic residual structure rather than a reconstructed
post-solve approximation.

## Contract

- Input: a constructor-linked aggregate containing the checked DAE residual
  facet and the checked FMI ODE execution facet derived from the same model.
- Output: an FMI 3 Model Exchange source FMU plus the layered-standard manifest
  and exact vendored schemas under the required `extra/` path.
- Algebraic variables, residual identities, dependencies, tensor domains, and
  provenance come directly from checked DAE owners.
- The default ODE mode and enabled DAE mode must both remain valid, as required
  by the pinned draft.

## Unsupported

The target must not reconstruct residual ownership from scalar Solve rows.
Until both profiles execute and the exact draft schemas and negative controls
pass, the directory is not registered as a built-in target and no conformance
claim is made.

## Verification

- Construction tests cover residual/variable bijection, tensor domains,
  dependency closure, value-reference disjointness, and same-model provenance.
- Package tests validate both official FMI 3 metadata and the pinned layered
  schemas, then execute default ODE and enabled DAE modes independently.
- Ablated manifests, mismatched DAE/ODE aggregates, and missing residual owners
  are required rejection controls before registration.

There is intentionally no `fmi-ls-dae` target registration while this contract
is incomplete.
