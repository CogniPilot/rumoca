//! MLIR compiled execution adapter for Rumoca Solve-IR.
// dlopen and raw function-pointer calls require unsafe.
#![allow(unsafe_code)]
//!
//! This crate is an execution adapter: `rumoca-phase-codegen` owns rendering
//! Solve-IR to textual MLIR through the built-in Jinja template, while this
//! crate owns tool invocation, shared-library loading, and ergonomic runtime
//! calls.
//! Keeping this separate from codegen is warranted because the adapter surface
//! is larger than the generated text: it owns MLIR/LLVM command execution,
//! runtime helper compilation, dynamic loading, the memref ABI bridge, and
//! CUDA/GPU artifact handling.
//!
//! Compiles one `SolveModel`'s derivative rows to a native shared library by:
//! 1. Asking `rumoca-phase-codegen` to render solve-IR as MLIR textual IR.
//! 2. Running `mlir-opt-18` to lower all dialects to the LLVM dialect.
//! 3. Running `mlir-translate-18 --mlir-to-llvmir` to produce LLVM IR.
//! 4. Running `llc-18 -filetype=obj -relocation-model=pic` to produce a `.o`.
//! 5. Running `clang-18 -shared -fPIC` to link the shared library.
//! 6. `dlopen`-ing the library and resolving the `eval_derivative` symbol.
//!
//! Requires on `$PATH`: `mlir-opt-18`, `mlir-translate-18`, `llc-18`, `clang-18`.
//! Install on Ubuntu: `sudo apt-get install mlir-18-tools clang-18`.

mod compile;
mod compiled;
mod cuda_driver;
mod cuda_gpu_model;
mod error;
mod euler_kernel;
mod gpu_blob;
pub mod libdevice;
pub mod options;
pub mod sim;

pub use compile::{compile_derivative_rhs, compile_derivative_rhs_with_opts};
pub use compiled::{CompiledMlirResidual, MlirResidualAbi};
pub use cuda_gpu_model::{CudaGpuOdeModel, build_cuda_ode_model};
pub use error::{MlirAbiArgument, MlirError};
pub use euler_kernel::compile_euler_update_ptx;
pub use gpu_blob::{GpuCompiledBlob, GpuLaunchAbi, compile_to_gpu_blob};
pub use options::{MlirBackendOptions, MlirTarget, OptLevel};
pub use sim::{CompiledOdeModel, build_ode_model, build_ode_model_with_opts};

fn render_mlir_model(
    model: std::sync::Arc<rumoca_ir_solve::SolveModel>,
    model_name: &str,
) -> Result<String, MlirError> {
    rumoca_phase_codegen::render_mlir_execution_model(model, model_name)
        .map_err(|error| MlirError::Template(error.to_string()))
}
