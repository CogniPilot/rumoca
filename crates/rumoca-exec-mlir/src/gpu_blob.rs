use crate::MlirResidualAbi;
use crate::error::MlirError;
use crate::libdevice::{find_libdevice, link_libdevice, ll_needs_libdevice};
use crate::options::{MlirBackendOptions, MlirTarget};
use rumoca_ir_solve::{
    PureExplicitLayoutDiagnostic, PureExplicitLayoutDisposition, PureExplicitStateCount, SolveModel,
};
use std::process::Command;
use std::sync::Arc;
use tempfile::TempDir;

/// The result of compiling a `SolveModel` to a GPU-native code blob.
///
/// The `device_ir` field contains the GPU assembly as UTF-8 bytes:
/// - **CUDA** targets: PTX text (`.ptx`)
/// - **ROCm** targets: AMDGPU ISA text (`.s`)
///
/// Use this blob with the CUDA or ROCm runtime to load and launch the kernel.
/// The entry point name is `eval_derivative_kernel`.
pub struct GpuCompiledBlob {
    device_ir: String,
    entry_point: String,
    target: MlirTarget,
    chip: String,
    kind: GpuBlobKind,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GpuLaunchAbi {
    SolveDerivative(MlirResidualAbi),
    EulerUpdate,
}

pub(crate) enum GpuBlobKind {
    SolveDerivative(DerivativeRuntimeInputs),
    EulerUpdate,
}

pub(crate) struct DerivativeRuntimeInputs {
    abi: MlirResidualAbi,
    parameters: Box<[f64]>,
    initial_y: Box<[f64]>,
    visible_names: Box<[String]>,
    pure_explicit_disposition: PureExplicitLayoutDisposition,
}

pub(crate) struct PureExplicitDerivativeRuntimeInputs {
    runtime: DerivativeRuntimeInputs,
    state_count: PureExplicitStateCount,
}

pub(super) struct DerivativeRuntimeParts {
    pub(super) abi: MlirResidualAbi,
    pub(super) parameters: Box<[f64]>,
    pub(super) initial_y: Box<[f64]>,
    pub(super) visible_names: Box<[String]>,
}

impl DerivativeRuntimeInputs {
    fn from_model(model: &SolveModel) -> Self {
        Self {
            abi: MlirResidualAbi::from_model(model),
            parameters: model.parameters().to_vec().into_boxed_slice(),
            initial_y: model.initial_y().to_vec().into_boxed_slice(),
            visible_names: model
                .visible_names()
                .map(str::to_string)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            pure_explicit_disposition: model
                .problem()
                .solve_layout()
                .pure_explicit_state_disposition(),
        }
    }

    #[cfg(test)]
    pub(crate) fn fixture(layout: &rumoca_ir_solve::SolveLayout) -> Self {
        Self {
            abi: MlirResidualAbi::fixture(
                layout.solver_scalar_count(),
                layout.state_scalar_count(),
            ),
            parameters: Box::new([]),
            initial_y: vec![0.0; layout.solver_scalar_count()].into_boxed_slice(),
            visible_names: Box::new([]),
            pure_explicit_disposition: layout.pure_explicit_state_disposition(),
        }
    }

    pub(super) fn admit_pure_explicit(
        self,
    ) -> Result<PureExplicitDerivativeRuntimeInputs, PureExplicitLayoutDiagnostic> {
        match self.pure_explicit_disposition {
            PureExplicitLayoutDisposition::Supported(state_count) => {
                Ok(PureExplicitDerivativeRuntimeInputs {
                    runtime: self,
                    state_count,
                })
            }
            PureExplicitLayoutDisposition::Unsupported(diagnostic) => Err(diagnostic),
        }
    }

    pub(super) fn into_runtime_parts(self) -> DerivativeRuntimeParts {
        DerivativeRuntimeParts {
            abi: self.abi,
            parameters: self.parameters,
            initial_y: self.initial_y,
            visible_names: self.visible_names,
        }
    }
}

impl PureExplicitDerivativeRuntimeInputs {
    pub(super) const fn state_count(&self) -> PureExplicitStateCount {
        self.state_count
    }

    pub(super) fn into_runtime_parts(self) -> DerivativeRuntimeParts {
        self.runtime.into_runtime_parts()
    }
}

impl GpuCompiledBlob {
    fn derivative(
        device_ir: String,
        entry_point: &'static str,
        target: MlirTarget,
        chip: String,
        runtime: DerivativeRuntimeInputs,
    ) -> Self {
        Self {
            device_ir,
            entry_point: entry_point.to_string(),
            target,
            chip,
            kind: GpuBlobKind::SolveDerivative(runtime),
        }
    }

    pub(crate) fn euler_update(device_ir: String, chip: String) -> Self {
        Self {
            device_ir,
            entry_point: "euler_update_kernel".to_string(),
            target: MlirTarget::GpuCuda,
            chip,
            kind: GpuBlobKind::EulerUpdate,
        }
    }

    pub(crate) fn into_parts(self) -> (String, String, MlirTarget, String, GpuBlobKind) {
        (
            self.device_ir,
            self.entry_point,
            self.target,
            self.chip,
            self.kind,
        )
    }
    pub fn device_ir_text(&self) -> &str {
        &self.device_ir
    }

    pub fn entry_point(&self) -> &str {
        &self.entry_point
    }

    pub fn target(&self) -> &MlirTarget {
        &self.target
    }

    pub fn chip(&self) -> &str {
        &self.chip
    }

    #[must_use]
    pub const fn launch_abi(&self) -> GpuLaunchAbi {
        match &self.kind {
            GpuBlobKind::SolveDerivative(runtime) => GpuLaunchAbi::SolveDerivative(runtime.abi),
            GpuBlobKind::EulerUpdate => GpuLaunchAbi::EulerUpdate,
        }
    }
}

/// Compile `solve.derivative_rhs` to a GPU-native code blob (PTX or AMDGPU ISA).
///
/// Requires on `$PATH`: `mlir-opt-18`, `mlir-translate-18`, `llc-18`.
/// No CUDA SDK or `ptxas` is needed — the LLVM NVPTX/AMDGCN backends are used directly.
///
/// The kernel entry point is named `eval_derivative_kernel` and accepts the
/// same expanded descriptor ABI as the CPU backend (15 + 1 parameters).
pub fn compile_to_gpu_blob(
    model: Arc<SolveModel>,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<GpuCompiledBlob, MlirError> {
    let runtime = DerivativeRuntimeInputs::from_model(&model);
    match &opts.target {
        MlirTarget::GpuCuda => {
            let chip = opts.gpu_chip.clone().unwrap_or_else(|| "sm_75".to_string());
            compile_cuda(model, runtime, model_name, &chip, opts)
        }
        MlirTarget::GpuRocm => {
            let chip = opts
                .gpu_chip
                .clone()
                .unwrap_or_else(|| "gfx906".to_string());
            compile_rocm(model, runtime, model_name, &chip)
        }
        other => Err(MlirError::ToolNotFound {
            tool: "GPU target not supported by compile_to_gpu_blob",
            source: std::io::Error::new(
                std::io::ErrorKind::Unsupported,
                format!("{other:?} is not a GPU target"),
            ),
        }),
    }
}

/// Compile to PTX via: CPU MLIR template → mlir-opt (CPU lowering) →
/// mlir-translate → inject NVVM kernel annotation → [libdevice linking] →
/// llc (nvptx64) → PTX text.
///
/// When the LLVM IR references transcendental ops (`sin`, `cos`, `exp`, `log`, …)
/// the function looks for `libdevice.10.bc` via `opts.libdevice_path` or the
/// standard search paths.  If found it is linked with `llvm-link-18` and
/// internalised with `opt-18 --nvvm-reflect`.  If not found it returns
/// `MlirError::LibdeviceNotFound` with install instructions.
fn compile_cuda(
    model: Arc<SolveModel>,
    runtime: DerivativeRuntimeInputs,
    model_name: &str,
    chip: &str,
    opts: &MlirBackendOptions,
) -> Result<GpuCompiledBlob, MlirError> {
    let mlir_text = crate::render_mlir_model(model, model_name)?;

    let tmpdir = TempDir::new()?;
    let mlir_path = tmpdir.path().join("model.mlir");
    let opt_path = tmpdir.path().join("model_opt.mlir");
    let ll_path = tmpdir.path().join("model.ll");
    let ptx_path = tmpdir.path().join("model.ptx");

    std::fs::write(&mlir_path, &mlir_text)?;

    // Lower to LLVM dialect (same passes as CPU path).
    run_tool_gpu(
        "mlir-opt-18",
        Command::new("mlir-opt-18")
            .arg(&mlir_path)
            .args([
                "--convert-math-to-llvm",
                "--convert-arith-to-llvm",
                "--convert-func-to-llvm",
                "--finalize-memref-to-llvm",
                "--reconcile-unrealized-casts",
            ])
            .arg("-o")
            .arg(&opt_path),
    )?;

    // MLIR LLVM dialect → LLVM IR text.
    run_tool_gpu(
        "mlir-translate-18",
        Command::new("mlir-translate-18")
            .arg("--mlir-to-llvmir")
            .arg(&opt_path)
            .arg("-o")
            .arg(&ll_path),
    )?;

    // Inject the NVVM annotation that marks eval_derivative as a CUDA kernel
    // (produces `.visible .entry` in PTX instead of a plain device function).
    let ll_text = std::fs::read_to_string(&ll_path)?;
    let ll_with_annotation = inject_nvvm_annotation(&ll_text, "eval_derivative");
    std::fs::write(&ll_path, &ll_with_annotation)?;

    // If the LLVM IR references trig/transcendental intrinsics, link libdevice.
    // This is what makes sin/cos/exp/log work on NVPTX for f64.
    let llc_input = if ll_needs_libdevice(&ll_with_annotation) {
        let libdevice_bc = find_libdevice(opts.libdevice_path.as_deref())?;
        link_libdevice(&ll_path, &libdevice_bc, "eval_derivative")?
    } else {
        ll_path.clone()
    };

    // LLVM IR / bitcode → PTX text via NVPTX backend.
    run_tool_gpu(
        "llc-18",
        Command::new("llc-18")
            .arg("--march=nvptx64")
            .arg(format!("--mcpu={chip}"))
            .arg("-filetype=asm")
            .arg(&llc_input)
            .arg("-o")
            .arg(&ptx_path),
    )?;

    let ptx = std::fs::read_to_string(&ptx_path)?;
    Ok(GpuCompiledBlob::derivative(
        ptx,
        "eval_derivative",
        MlirTarget::GpuCuda,
        chip.to_string(),
        runtime,
    ))
}

/// Compile to AMDGPU ISA via: CPU MLIR template → mlir-opt (CPU lowering) →
/// mlir-translate → inject amdgpu_kernel calling convention → llc (amdgcn) → GCN text.
fn compile_rocm(
    model: Arc<SolveModel>,
    runtime: DerivativeRuntimeInputs,
    model_name: &str,
    chip: &str,
) -> Result<GpuCompiledBlob, MlirError> {
    let mlir_text = crate::render_mlir_model(model, model_name)?;

    let tmpdir = TempDir::new()?;
    let mlir_path = tmpdir.path().join("model.mlir");
    let opt_path = tmpdir.path().join("model_opt.mlir");
    let ll_path = tmpdir.path().join("model.ll");
    let gcn_path = tmpdir.path().join("model.s");

    std::fs::write(&mlir_path, &mlir_text)?;

    run_tool_gpu(
        "mlir-opt-18",
        Command::new("mlir-opt-18")
            .arg(&mlir_path)
            .args([
                "--convert-math-to-llvm",
                "--convert-arith-to-llvm",
                "--convert-func-to-llvm",
                "--finalize-memref-to-llvm",
                "--reconcile-unrealized-casts",
            ])
            .arg("-o")
            .arg(&opt_path),
    )?;

    run_tool_gpu(
        "mlir-translate-18",
        Command::new("mlir-translate-18")
            .arg("--mlir-to-llvmir")
            .arg(&opt_path)
            .arg("-o")
            .arg(&ll_path),
    )?;

    // Change `define void @eval_derivative` to `define amdgpu_kernel void @eval_derivative`
    // so that llc recognises it as a GPU kernel (produces a kernel symbol in GCN ISA).
    let ll_text = std::fs::read_to_string(&ll_path)?;
    let ll_rocm = inject_amdgpu_kernel_cc(&ll_text, "eval_derivative");
    std::fs::write(&ll_path, &ll_rocm)?;

    run_tool_gpu(
        "llc-18",
        Command::new("llc-18")
            .arg("--march=amdgcn")
            .arg(format!("--mcpu={chip}"))
            .arg("-filetype=asm")
            .arg(&ll_path)
            .arg("-o")
            .arg(&gcn_path),
    )?;

    let gcn = std::fs::read_to_string(&gcn_path)?;
    Ok(GpuCompiledBlob::derivative(
        gcn,
        "eval_derivative",
        MlirTarget::GpuRocm,
        chip.to_string(),
        runtime,
    ))
}

/// Append the NVVM metadata that marks `fn_name` as a CUDA kernel.
/// This causes `llc --march=nvptx64` to emit `.visible .entry` in the PTX.
///
/// Finds the highest existing `!N = ` metadata slot and appends two new nodes
/// at N+1 and N+2 so the numbers don't collide with nodes emitted by
/// `mlir-translate-18`.
fn inject_nvvm_annotation(ll_text: &str, fn_name: &str) -> String {
    let next = next_metadata_id(ll_text);
    format!(
        "{ll_text}\n\
         !nvvm.annotations = !{{!{next}}}\n\
         !{next} = !{{ptr @{fn_name}, !\"kernel\", i32 1}}\n"
    )
}

/// Return one past the highest `!N` metadata node number already in `ll_text`.
fn next_metadata_id(ll_text: &str) -> u32 {
    let max = ll_text
        .lines()
        .filter_map(|line| {
            let line = line.trim_start();
            if !line.starts_with('!') {
                return None;
            }
            let rest = line.trim_start_matches('!');
            let num_end = rest.find(|c: char| !c.is_ascii_digit())?;
            if num_end == 0 {
                return None;
            }
            rest[..num_end].parse::<u32>().ok()
        })
        .max()
        .unwrap_or(0);
    max + 1
}

/// Replace `define void @fn_name` with `define amdgpu_kernel void @fn_name`
/// so that `llc --march=amdgcn` treats it as an AMDGPU kernel entry point.
fn inject_amdgpu_kernel_cc(ll_text: &str, fn_name: &str) -> String {
    ll_text.replacen(
        &format!("define void @{fn_name}"),
        &format!("define amdgpu_kernel void @{fn_name}"),
        1,
    )
}

pub(crate) fn run_tool_gpu(tool: &'static str, cmd: &mut Command) -> Result<(), MlirError> {
    let output = cmd
        .output()
        .map_err(|e| MlirError::ToolNotFound { tool, source: e })?;
    if !output.status.success() {
        return Err(MlirError::ToolFailed {
            tool,
            code: output.status.code().unwrap_or(-1),
            stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        });
    }
    Ok(())
}
