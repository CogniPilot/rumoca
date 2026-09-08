/// Device-side forward-Euler update kernel.
///
/// Compiles a tiny NVPTX kernel that performs:
///   `y[i] += dt * xdot[i]` for all `i` in `[0, n)`
///
/// in parallel — one thread per state element.  This eliminates the per-step
/// host-device round-trips in `batch_euler_cuda`, replacing them with a single
/// device-side kernel launch per step (no d2h/h2d until the very end).
///
/// The LLVM IR is a manifest-declared MLIR target asset, so only
/// `llc-18 --march=nvptx64` is needed at runtime.
use crate::error::MlirError;
use crate::gpu_blob::{GpuCompiledBlob, run_tool_gpu};
use rumoca_phase_codegen::templates;
use std::process::Command;
use tempfile::TempDir;

/// Compile the Euler update kernel to PTX for the given CUDA chip.
///
/// Only requires `llc-18` on `$PATH` — no MLIR tools or CUDA SDK needed.
/// Returns a `GpuCompiledBlob` with `entry_point = "euler_update_kernel"`.
pub fn compile_euler_update_ptx(chip: &str) -> Result<GpuCompiledBlob, MlirError> {
    let tmpdir = TempDir::new()?;
    let ll_path = tmpdir.path().join("euler_update.ll");
    let ptx_path = tmpdir.path().join("euler_update.ptx");

    let source = templates::mlir_euler_update_llvm();
    std::fs::write(&ll_path, source)?;

    run_tool_gpu(
        "llc-18",
        Command::new("llc-18")
            .arg("--march=nvptx64")
            .arg(format!("--mcpu={chip}"))
            .arg("-filetype=asm")
            .arg(&ll_path)
            .arg("-o")
            .arg(&ptx_path),
    )?;

    let ptx = std::fs::read_to_string(&ptx_path)?;
    Ok(GpuCompiledBlob::euler_update(ptx, chip.to_string()))
}
