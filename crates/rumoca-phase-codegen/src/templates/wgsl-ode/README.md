# `wgsl-ode`

## Use case

Use this target for explicit-ODE derivative evaluation through WebGPU in
a browser or native `wgpu` host. It is intended for ensemble simulation and
large tensor families where a GPU dispatch is more useful than a scalar CPU
call.

## Contract

- Readiness 0: shader parsing, semantic validation, and browser dispatch tests
  cover a deliberately narrow explicit-ODE profile.
- Input: checked Solve IR.
- Output: WGSL derivative compute kernels plus a JSON dispatch/layout manifest.
- Map and affine-stencil domains remain native compact GPU kernels; remaining
  supported rows use explicitly catalogued scalar chunks.
- The layout manifest is the sole host/kernel storage and dispatch contract.

## Unsupported

The target does not implement an integrator or event iteration. Residual
systems, linear solves, events, callbacks, dynamic shapes, and unsupported
operations fail closed.
Its current `f32` numerical profile must not be called equivalent to an `f64`
kernel without tolerance-qualified differential evidence.

## Verification

- `wgsl_ode_tests` independently parses and semantically validates generated
  scalar, map, and stencil shaders with Naga; it also covers layout
  metadata, invalid strides, and unsupported linear solves.
- `wgsl_ode_tests` checks that each compact map or affine-stencil family emits
  one native kernel and that layout metadata remains family-sized rather than
  scalar-row-sized.
- `wgsl_ode_tests` validates the emitted dispatch/layout contract, while the
  standalone `wgsl-ode` target remains independent of any browser simulation
  facade.

## Example

```sh
rumoca compile Wave.mo --model Wave --target wgsl-ode --output generated
```
