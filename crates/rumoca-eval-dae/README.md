# rumoca-eval-dae

Compiled evaluation kernels and lowering infrastructure for runtime simulation.

## Role in Rumoca
Lowers DAE expressions into executable kernels for residuals, Jacobian-vector products, and root-condition evaluation (native and WASM backends).

## Public Surface
- Lowering/build APIs: `lower_residual_ad`, `lower_expression`, `lower_residual`, `lower_discrete_rhs`, `lower_root_conditions`.
- Native compile APIs (feature-gated): `compile_residual`, `compile_jacobian_v`, `compile_discrete_rhs`, `compile_root_conditions`.
- WASM compile APIs (feature-gated): `compile_residual_wasm`, `compile_jacobian_v_wasm`, and related expression/root builders.
- Runtime layout/IR APIs: `VarLayout`, `ScalarSlot`, `LinearOp`, `BinaryOp`, `UnaryOp`, `CompareOp`.

## Inputs
- Canonical DAE expressions and runtime layout metadata.
- Compile target backend features (`cranelift`, `wasm`).

## Outputs
- Callable compiled kernel objects and lowered operation graphs.

## Design Constraints
- Keep runtime layout deterministic across targets.
- Separate lowering/compilation from solver orchestration.
- Maintain semantic parity between native and WASM code paths.
