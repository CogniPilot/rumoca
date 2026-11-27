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
- Output: WGSL derivative and optional periodic event kernels plus a JSON dispatch/layout manifest.
- Map and affine-stencil domains remain native compact GPU kernels; remaining
  supported rows use explicitly catalogued scalar chunks.
- The layout manifest is the sole host/kernel storage and dispatch contract.

## Exact algebraic assignments

When the checked assignment schedule covers every algebraic and output slot,
the shader evaluates that schedule in dependency order. The host dispatches it
before each RK4 derivative, before each sampled event, and for every reported
state after the event. All outputs from a single assignment use the same
incoming state. Coupled residual projection and tearing require a solver and
remain outside this profile.

## Periodic updates

For an explicit ODE with a single certified periodic clock, the manifest includes
its rational phase, period, anchor, and entry point. The browser splits RK4
steps at ticks and dispatches the generated kernel once per tick, including a
start-time tick when due. Scalar guarded updates to P slots run in the issued
clock-partition order. Pulse memories release and ordinary `pre` bindings
commit after all updates. Source parameters and live inputs share the P buffer.

## Unsupported

The target does not implement an integrator or event iteration. Residual
systems, linear solves, root or dynamic-time events, multiple clocks, event
actions, clocked history, multi-target assignments, callbacks, dynamic shapes, and unsupported
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
- `gpu_schedule.test.mjs` validates the host-side dispatch plan, while
  `book_live_smoke.mjs` is the hardware-dependent end-to-end WebGPU execution
  smoke for Wave2D, a sampled drive, and the connected RC circuit in
  `examples/models/Circuit.mo`. The circuit checks capacitor voltage and resistor
  current against the analytical charging response.

## Example

```sh
rumoca compile Wave.mo --model Wave --target wgsl-ode --output generated
```
