# Solve IR and Execution

## Solve Lowering (`rumoca-phase-solve`)

Solve lowering converts the structurally prepared DAE into the Solve IR: a
register-machine format that execution backends consume directly. The MLS
B.1 functions become `ComputeBlock` graphs mixing two kinds of nodes:

| Node | Contents |
|---|---|
| `ScalarProgramBlock` | Flat register programs (`Vec<LinearOp>`), one scalar output per program |
| Tensor program nodes (`ComputeNode::MatMul`, `LinSolve`, `AffineStencil`, …) | Tensor kernels with explicit shape/layout metadata and a scalar fallback |

Solve adds no mathematics; it changes format. Keeping tensor structure
explicit above the scalar layer is what lets a backend choose between
scalar expansion (embedded C) and native kernels (BLAS/faer, CUDA, MLIR
`linalg`) without re-deriving structure.

`ComputeNode::AffineStencil` represents a source-proven DAE `for`-equation
subdomain. It carries the preserved source iteration domain plus affine
`LoadY`, `LoadP`, and `Const` operand strides. This gives PDE-style
method-of-lines systems a native GPU/codegen path while preserving exact scalar
fallback behavior.

`SolveProblem` is the base lowered problem. Expensive or non-canonical
products (mass-matrix form, output projections) are separate artifacts
requested by the backends that need them.

Reverse-mode AD, steady-state objectives, lift/drag projections, and optimizer
sensitivities are artifact/runtime layers over Solve. They are not fields in
the base `SolveProblem`.

## Execution Adapters

Execution adapters wrap toolchains and runtime APIs around generated or
lowered code. They must not own semantics — no DAE lowering, no structural
rewrites, no template policy:

| Crate | Role |
|---|---|
| `rumoca-exec-cranelift` | In-process JIT via Cranelift |
| `rumoca-exec-mlir` | MLIR-based compilation path |
| `rumoca-exec-wasm` | WASM execution backend |

The generated-code targets (`rust-solve`, `c-solve`, `embedded-c`,
`cuda-c`, `cuda-nvrtc-solve-jit`, `fmi2`/`fmi3`) consume Solve through the
codegen engine instead — see
[Code Generation Engine](../runtime/codegen.md).

## Adding a New Backend

The pathway for a new execution backend (the same one a future WebGPU/WGSL
backend would take):

1. Decide the consumption model: a codegen *target* (templates rendering
   kernels, like `cuda-c`) or an execution *adapter* (an API wrapper, like
   `rumoca-exec-cranelift`) — or both, like the NVRTC JIT.
2. Consume Solve IR. If the backend needs tensor structure, use the tensor
   program nodes; every valid node has a fallible scalar fallback, so a backend
   can start scalar-only, propagate malformed tensor metadata, and specialize
   incrementally.
3. Declare capabilities honestly in the target manifest — readiness level
   and per-feature support columns are what `rumoca targets` reports.
4. Keep language/toolchain specifics in the target's `target.toml` and
   templates, never in phase logic
   ([SPEC_0029](https://github.com/CogniPilot/rumoca/blob/main/spec/SPEC_0029_CRATE_BOUNDARIES.md)).
