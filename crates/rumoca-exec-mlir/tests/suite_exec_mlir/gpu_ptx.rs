use rumoca_core::{SourceId, Span};
/// Compile Solve IR to GPU-native code blobs (PTX / AMDGPU ISA).
///
/// These tests exercise the full GPU compilation pipeline
/// (mlir-opt-18 → mlir-translate-18 → llc-18 with NVPTX/AMDGCN backend) and
/// verify structural properties of the emitted code.  No CUDA SDK or GPU hardware
/// is required; the tests skip gracefully when the required LLVM tools are absent.
use rumoca_exec_mlir::{
    GpuCompiledBlob, MlirBackendOptions, MlirError, MlirTarget,
    compile_to_gpu_blob as backend_compile_to_gpu_blob,
};
use rumoca_ir_solve::{ComputeBlock, LinearOp, ScalarProgramBlock, UnaryOp};

use super::support::{derivative, fixture};

fn compile_to_gpu_blob(
    solve: std::sync::Arc<rumoca_ir_solve::SolveModel>,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<GpuCompiledBlob, MlirError> {
    backend_compile_to_gpu_blob(solve, model_name, opts)
}

fn decay_solve() -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    let label = "gpu_ptx_decay.mo";
    let row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let problem = derivative::derivative_problem(
        fixture::ContinuousInventory::new(vec![fixture::FixtureScalar::state("x", 0.0, 1.0)]),
        ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(
                vec![row],
                Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
                    .require_provenance("MLIR GPU-code fixture")
                    .expect("fixture span is source-backed"),
            )
            .expect("fixture program is computable"),
        ),
    );
    fixture::complete_model(problem)
}

// ─── CUDA / PTX ──────────────────────────────────────────────────────────────

#[test]
fn cuda_ptx_contains_visible_entry() {
    let solve = decay_solve();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        ..Default::default()
    };

    let blob = match compile_to_gpu_blob(solve, "decay_cuda", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("CUDA compile failed: {e}"),
    };

    let ptx = blob.device_ir_text();
    // The PTX must expose the kernel as a callable entry point.
    assert!(
        ptx.contains(".visible .entry"),
        "expected `.visible .entry` in PTX, got:\n{ptx}"
    );
}

#[test]
fn cuda_ptx_contains_kernel_name() {
    let solve = decay_solve();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        ..Default::default()
    };

    let blob = match compile_to_gpu_blob(solve, "decay_cuda_name", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    let ptx = blob.device_ir_text();
    assert!(
        ptx.contains("eval_derivative"),
        "expected `eval_derivative` in PTX, got:\n{ptx}"
    );
    assert_eq!(blob.entry_point(), "eval_derivative");
    assert_eq!(blob.target(), &MlirTarget::GpuCuda);
}

#[test]
fn cuda_ptx_chip_override() {
    // Confirm that the chip string appears in the PTX .target directive.
    let solve = decay_solve();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        gpu_chip: Some("sm_86".to_string()),
        ..Default::default()
    };

    let blob = match compile_to_gpu_blob(solve, "decay_sm86", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    assert!(
        blob.device_ir_text().contains("sm_86"),
        "expected sm_86 in PTX; chip={}",
        blob.chip()
    );
    assert_eq!(blob.chip(), "sm_86");
}

// ─── ROCm / AMDGPU ISA ───────────────────────────────────────────────────────

#[test]
fn rocm_gcn_contains_kernel_symbol() {
    let solve = decay_solve();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuRocm,
        ..Default::default()
    };

    let blob = match compile_to_gpu_blob(solve, "decay_rocm", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("ROCm compile failed: {e}"),
    };

    let gcn = blob.device_ir_text();
    assert!(
        gcn.contains("eval_derivative"),
        "expected `eval_derivative` in GCN ISA:\n{gcn}"
    );
    assert_eq!(blob.target(), &MlirTarget::GpuRocm);
}

#[test]
fn rocm_gcn_amdgpu_isa_header() {
    let solve = decay_solve();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuRocm,
        ..Default::default()
    };

    let blob = match compile_to_gpu_blob(solve, "decay_rocm_hdr", &opts) {
        Ok(b) => b,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    let gcn = blob.device_ir_text();
    // AMDGPU ISA files include the target ISA string.
    assert!(
        gcn.contains("amdgcn"),
        "expected `amdgcn` in GCN ISA header:\n{gcn}"
    );
}
