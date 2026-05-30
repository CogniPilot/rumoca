use crate::compiled::CompiledMlirResidual;
use crate::error::MlirError;
use crate::options::{MlirBackendOptions, MlirTarget};
use rumoca_ir_solve::{SolveArtifacts, SolveProblem};
use rumoca_phase_codegen::{render_solve_template_with_name, templates};
use std::path::PathBuf;
use std::process::Command;
use tempfile::TempDir;

type EvalFnRaw = unsafe extern "C" fn(
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // y
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // p
    f64, // t
    *mut f64,
    *mut f64,
    i64,
    i64,
    i64, // out
);

type JvpFnRaw = unsafe extern "C" fn(
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // y
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // p
    *const f64,
    *const f64,
    i64,
    i64,
    i64, // seed
    f64, // t
    *mut f64,
    *mut f64,
    i64,
    i64,
    i64, // out
);

struct CpuCompileArtifacts {
    tmpdir: TempDir,
    so_path: PathBuf,
}

/// C source for the Rumoca MLIR runtime.
///
/// Provides `rumoca_solve_linear_component` — dense Gaussian elimination with
/// partial pivoting, called from MLIR-compiled `LinearSolveComponent` and
/// `LinSolve` nodes.  Pointers are passed as `long long` integers to avoid
/// the MLIR memref-descriptor ABI complexity.
const RUMOCA_MLIR_RUNTIME_C: &str = r#"
#include <stddef.h>
#include <math.h>

#define RUMOCA_LS_MAXN 64

/* A_ptr, b_ptr are row-major double arrays passed as pointer-integers.
   Returns x[comp] where A*x = b (Gauss elim with partial pivoting). */
double rumoca_solve_linear_component(
        long long A_ptr, long long b_ptr, long long n, long long comp) {
    double *Ain = (double *)(size_t)A_ptr;
    double *bin = (double *)(size_t)b_ptr;
    double A[RUMOCA_LS_MAXN * RUMOCA_LS_MAXN];
    double x[RUMOCA_LS_MAXN];
    long long i, j, row, col;

    for (i = 0; i < n * n; i++) A[i] = Ain[i];
    for (i = 0; i < n;     i++) x[i] = bin[i];

    /* Forward elimination with partial pivoting */
    for (col = 0; col < n; col++) {
        long long pivot = col;
        double max_val = A[col * n + col] < 0 ? -A[col * n + col] : A[col * n + col];
        for (row = col + 1; row < n; row++) {
            double v = A[row * n + col] < 0 ? -A[row * n + col] : A[row * n + col];
            if (v > max_val) { max_val = v; pivot = row; }
        }
        /* Swap rows col <-> pivot */
        for (j = 0; j < n; j++) {
            double t = A[col * n + j]; A[col * n + j] = A[pivot * n + j]; A[pivot * n + j] = t;
        }
        { double t = x[col]; x[col] = x[pivot]; x[pivot] = t; }
        /* Eliminate below */
        for (row = col + 1; row < n; row++) {
            double f = A[row * n + col] / A[col * n + col];
            for (j = col; j < n; j++) A[row * n + j] -= f * A[col * n + j];
            x[row] -= f * x[col];
        }
    }
    /* Back substitution */
    for (i = n - 1; i >= 0; i--) {
        x[i] /= A[i * n + i];
        for (j = i - 1; j >= 0; j--) x[j] -= A[j * n + i] * x[i];
    }
    return x[comp];
}
"#;

/// Compile using default options (`CpuNative`, `O2`).
pub fn compile_derivative_rhs(
    solve: &SolveProblem,
    artifacts: &SolveArtifacts,
    model_name: &str,
) -> Result<CompiledMlirResidual, MlirError> {
    compile_derivative_rhs_with_opts(solve, artifacts, model_name, &MlirBackendOptions::default())
}

/// Compile `solve.continuous.derivative_rhs` to a native shared library via MLIR toolchain
/// with explicit backend options.
///
/// Requires on `$PATH`: `mlir-opt-18`, `mlir-translate-18`, `llc-18`, `clang-18`.
/// For `GpuCuda`/`GpuRocm` targets the required GPU toolchain is not yet wired up
/// and the function returns `MlirError::ToolNotFound`.
pub fn compile_derivative_rhs_with_opts(
    solve: &SolveProblem,
    artifacts: &SolveArtifacts,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<CompiledMlirResidual, MlirError> {
    match opts.target {
        MlirTarget::GpuCuda => {
            return Err(MlirError::ToolNotFound {
                tool: "nvptx-llc (GpuCuda target not yet wired up)",
                source: std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "GpuCuda target requires CUDA toolchain",
                ),
            });
        }
        MlirTarget::GpuRocm => {
            return Err(MlirError::ToolNotFound {
                tool: "amdgpu-llc (GpuRocm target not yet wired up)",
                source: std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "GpuRocm target requires ROCm toolchain",
                ),
            });
        }
        _ => {}
    }
    compile_cpu(solve, artifacts, model_name, opts)
}

fn compile_cpu(
    solve: &SolveProblem,
    artifacts: &SolveArtifacts,
    model_name: &str,
    opts: &MlirBackendOptions,
) -> Result<CompiledMlirResidual, MlirError> {
    let mlir_text = render_solve_template_with_name(solve, artifacts, mlir_template(), model_name)
        .map_err(|e| MlirError::Template(e.to_string()))?;
    let rows = derivative_row_count(solve);
    let implicit_rows = rumoca_eval_solve::to_scalar_program_block(&solve.continuous.implicit_rhs)
        .programs
        .len();
    let artifacts = compile_cpu_shared_library(&mlir_text, opts)?;

    load_compiled_residual(artifacts, rows, implicit_rows)
}

fn mlir_template() -> &'static str {
    templates::builtin_target("mlir")
        .and_then(|target| target.template_source("mlir.mlir.jinja"))
        .expect("built-in mlir target must provide mlir.mlir.jinja")
}

fn derivative_row_count(solve: &SolveProblem) -> usize {
    rumoca_eval_solve::to_scalar_program_block(&solve.continuous.derivative_rhs)
        .programs
        .len()
}

fn compile_cpu_shared_library(
    mlir_text: &str,
    opts: &MlirBackendOptions,
) -> Result<CpuCompileArtifacts, MlirError> {
    let tmpdir = TempDir::new()?;
    let mlir_path = tmpdir.path().join("model.mlir");
    let opt_path = tmpdir.path().join("model_opt.mlir");
    let ll_path = tmpdir.path().join("model.ll");
    let obj_path = tmpdir.path().join("model.o");
    let rt_src_path = tmpdir.path().join("rumoca_runtime.c");
    let rt_obj_path = tmpdir.path().join("rumoca_runtime.o");
    let so_path = tmpdir.path().join("model.so");

    std::fs::write(&mlir_path, mlir_text)?;

    // Compile the MLIR runtime helper (LinearSolveComponent / LinSolve support).
    std::fs::write(&rt_src_path, RUMOCA_MLIR_RUNTIME_C)?;
    run_tool(
        "clang-18",
        Command::new("clang-18")
            .arg("-c")
            .arg("-O2")
            .arg("-fPIC")
            .arg(&rt_src_path)
            .arg("-o")
            .arg(&rt_obj_path),
    )?;

    // Step 1: lower all dialects to LLVM dialect.
    // Each memref<?xf64> arg expands to 5 LLVM params:
    //   (alloc_ptr, aligned_ptr, offset: i64, size: i64, stride: i64)
    // The Rust caller passes aligned_ptr twice and offset=0, size=len, stride=1.
    // Linalg passes are no-ops when the model has no MatMul/LinSolve nodes.
    run_tool(
        "mlir-opt-18",
        Command::new("mlir-opt-18")
            .arg(&mlir_path)
            .args([
                "--linalg-generalize-named-ops",
                "--convert-linalg-to-loops",
                "--convert-scf-to-cf",
                "--convert-cf-to-llvm",
                "--convert-math-to-llvm",
                "--convert-arith-to-llvm",
                "--convert-func-to-llvm",
                "--finalize-memref-to-llvm",
                "--reconcile-unrealized-casts",
            ])
            .arg("-o")
            .arg(&opt_path),
    )?;

    // Step 2: MLIR → LLVM IR text
    run_tool(
        "mlir-translate-18",
        Command::new("mlir-translate-18")
            .arg("--mlir-to-llvmir")
            .arg(&opt_path)
            .arg("-o")
            .arg(&ll_path),
    )?;

    // Step 3: LLVM IR → object file (PIC required for shared library)
    let mut llc_cmd = Command::new("llc-18");
    llc_cmd
        .arg("-filetype=obj")
        .arg("-relocation-model=pic")
        .arg(opts.opt_level.flag());
    if opts.target == MlirTarget::CpuVectorized {
        llc_cmd
            .arg("-mcpu=native")
            .arg("--vectorize-loops")
            .arg("--vectorize-slp");
    }
    llc_cmd.arg(&ll_path).arg("-o").arg(&obj_path);
    run_tool("llc-18", &mut llc_cmd)?;

    // Step 4: link model + runtime into a shared library
    run_tool(
        "clang-18",
        Command::new("clang-18")
            .arg("-shared")
            .arg("-fPIC")
            .arg(&obj_path)
            .arg(&rt_obj_path)
            .arg("-lm")
            .arg("-o")
            .arg(&so_path),
    )?;

    Ok(CpuCompileArtifacts { tmpdir, so_path })
}

fn load_compiled_residual(
    artifacts: CpuCompileArtifacts,
    rows: usize,
    implicit_rows: usize,
) -> Result<CompiledMlirResidual, MlirError> {
    // Step 5: dlopen and resolve symbols.
    // Each memref<?xf64> expands to 5 params: (alloc_ptr, aligned_ptr, offset, size, stride).
    let lib = unsafe { libloading::Library::new(&artifacts.so_path) }?;

    let eval_fn: libloading::Symbol<EvalFnRaw> = unsafe {
        lib.get(b"eval_derivative\0")
            .map_err(|_| MlirError::MissingSymbol("eval_derivative".into()))?
    };
    let eval_fn = *eval_fn;

    // Optional functions: present only when the model has non-empty implicit_rhs / jacobian_v.
    let implicit_fn: Option<EvalFnRaw> = unsafe {
        lib.get::<EvalFnRaw>(b"eval_implicit_rhs\0")
            .ok()
            .map(|s| *s)
    };
    let jvp_fn: Option<JvpFnRaw> =
        unsafe { lib.get::<JvpFnRaw>(b"eval_jacobian_v\0").ok().map(|s| *s) };

    Ok(CompiledMlirResidual::new(
        lib,
        artifacts.tmpdir,
        eval_fn,
        implicit_fn,
        jvp_fn,
        rows,
        implicit_rows,
    ))
}

fn run_tool(tool: &'static str, cmd: &mut Command) -> Result<(), MlirError> {
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
