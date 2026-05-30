/// Device-side forward-Euler update kernel.
///
/// Compiles a tiny NVPTX kernel that performs:
///   `y[i] += dt * xdot[i]` for all `i` in `[0, n)`
///
/// in parallel — one thread per state element.  This eliminates the per-step
/// host-device round-trips in `batch_euler_cuda`, replacing them with a single
/// device-side kernel launch per step (no d2h/h2d until the very end).
///
/// The LLVM IR is embedded as a string literal so no MLIR toolchain is needed —
/// only `llc-18 --march=nvptx64`.
use crate::error::MlirError;
use crate::gpu_blob::{GpuCompiledBlob, run_tool_gpu};
use crate::options::MlirTarget;
use std::process::Command;
use tempfile::TempDir;

/// LLVM IR (LLVM 18, opaque-pointer style) for the GPU Euler update kernel.
///
/// Kernel signature:
///   `euler_update_kernel(y: ptr, xdot: ptr, dt: f64, n: i64)`
///
/// Each CUDA thread handles one state element: `y[i] += dt * xdot[i]`.
/// The NVVM annotation marks it as a kernel entry point (`.visible .entry` in PTX).
const EULER_UPDATE_LL: &str = r#"
target datalayout = "e-i64:64-i128:128-v16:16-v32:32-n16:32:64"
target triple = "nvptx64-nvidia-cuda"

define void @euler_update_kernel(ptr %y, ptr %xdot, double %dt, i64 %n) {
entry:
  %tidx_raw  = call i32 @llvm.nvvm.read.ptx.sreg.tid.x()
  %bidx_raw  = call i32 @llvm.nvvm.read.ptx.sreg.ctaid.x()
  %bdimx_raw = call i32 @llvm.nvvm.read.ptx.sreg.ntid.x()
  %tidx      = zext i32 %tidx_raw  to i64
  %bidx      = zext i32 %bidx_raw  to i64
  %bdimx     = zext i32 %bdimx_raw to i64
  %blk_off   = mul i64 %bidx, %bdimx
  %i         = add i64 %blk_off, %tidx
  %in_range  = icmp ult i64 %i, %n
  br i1 %in_range, label %active, label %exit
active:
  %y_ptr    = getelementptr inbounds double, ptr %y,    i64 %i
  %xdot_ptr = getelementptr inbounds double, ptr %xdot, i64 %i
  %yi    = load  double, ptr %y_ptr,    align 8
  %xi    = load  double, ptr %xdot_ptr, align 8
  %dxi   = fmul double %xi, %dt
  %new_y = fadd double %yi, %dxi
  store double %new_y, ptr %y_ptr, align 8
  br label %exit
exit:
  ret void
}

declare i32 @llvm.nvvm.read.ptx.sreg.tid.x()  #0
declare i32 @llvm.nvvm.read.ptx.sreg.ctaid.x() #0
declare i32 @llvm.nvvm.read.ptx.sreg.ntid.x()  #0

attributes #0 = { nounwind readnone }

!nvvm.annotations = !{!0}
!0 = !{ptr @euler_update_kernel, !"kernel", i32 1}
"#;

/// Compile the Euler update kernel to PTX for the given CUDA chip.
///
/// Only requires `llc-18` on `$PATH` — no MLIR tools or CUDA SDK needed.
/// Returns a `GpuCompiledBlob` with `entry_point = "euler_update_kernel"`.
pub fn compile_euler_update_ptx(chip: &str) -> Result<GpuCompiledBlob, MlirError> {
    let tmpdir = TempDir::new()?;
    let ll_path = tmpdir.path().join("euler_update.ll");
    let ptx_path = tmpdir.path().join("euler_update.ptx");

    std::fs::write(&ll_path, EULER_UPDATE_LL)?;

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

    let ptx_bytes = std::fs::read(&ptx_path)?;
    Ok(GpuCompiledBlob {
        device_ir: ptx_bytes,
        entry_point: "euler_update_kernel".to_string(),
        target: MlirTarget::GpuCuda,
        chip: chip.to_string(),
    })
}
