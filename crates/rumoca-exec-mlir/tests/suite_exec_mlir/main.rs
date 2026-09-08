mod support;

mod benchmark_matmul;
mod compile_basic;
mod implicit_euler;
mod integrate;
mod linsolve_mlir;
mod multi_fn_mlir;
mod options;

// The required MLIR CPU lane intentionally selects only the seven modules
// above. Portable workspace runs retain the GPU and rendering coverage.
#[cfg(not(feature = "required-mlir-cpu"))]
mod drone_monte_carlo;
#[cfg(not(feature = "required-mlir-cpu"))]
mod gpu_ptx;
#[cfg(not(feature = "required-mlir-cpu"))]
mod gpu_trig;
#[cfg(not(feature = "required-mlir-cpu"))]
mod render_mlir;
