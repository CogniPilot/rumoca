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
    let mlir_text = render_solve_template_with_name(solve, artifacts, mlir_template()?, model_name)
        .map_err(|e| MlirError::Template(e.to_string()))?;
    let rows = solve
        .continuous
        .derivative_rhs
        .output_count("mlir derivative_rhs output count")?;
    let implicit_rows = solve
        .continuous
        .implicit_rhs
        .output_count("mlir implicit_rhs output count")?;
    let artifacts = compile_cpu_shared_library(&mlir_text, opts)?;

    load_compiled_residual(artifacts, rows, implicit_rows)
}

fn mlir_template() -> Result<&'static str, MlirError> {
    templates::builtin_target("mlir")
        .and_then(|target| target.template_source("mlir.mlir.jinja"))
        .ok_or(MlirError::MissingBuiltinTemplate {
            target: "mlir",
            template: "mlir.mlir.jinja",
        })
}

fn mlir_asset(path: &'static str) -> Result<&'static [u8], MlirError> {
    templates::builtin_target("mlir")
        .and_then(|target| target.asset_bytes(path))
        .ok_or(MlirError::MissingBuiltinAsset {
            target: "mlir",
            asset: path,
        })
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
    std::fs::write(&rt_src_path, mlir_asset("runtime/rumoca_runtime.c")?)?;
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

    run_tool(
        "mlir-translate-18",
        Command::new("mlir-translate-18")
            .arg("--mlir-to-llvmir")
            .arg(&opt_path)
            .arg("-o")
            .arg(&ll_path),
    )?;

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

#[cfg(test)]
mod tests {
    use super::*;

    type SolveComponentFn = unsafe extern "C" fn(i64, i64, i64, i64) -> f64;

    #[test]
    fn target_owned_linear_runtime_rejects_singular_systems() {
        let tmpdir = TempDir::new().expect("temporary runtime build directory");
        let source_path = tmpdir.path().join("rumoca_runtime.c");
        let library_path = tmpdir.path().join("rumoca_runtime.so");
        std::fs::write(
            &source_path,
            mlir_asset("runtime/rumoca_runtime.c").expect("manifest-declared MLIR runtime"),
        )
        .expect("write embedded runtime asset");

        let mut command = Command::new("clang-18");
        command
            .arg("-shared")
            .arg("-fPIC")
            .arg(&source_path)
            .arg("-lm")
            .arg("-o")
            .arg(&library_path);
        if let Err(error) = run_tool("clang-18", &mut command) {
            if cfg!(feature = "required-mlir-cpu") {
                panic!("required MLIR CPU toolchain failed: {error}");
            }
            eprintln!("skipping MLIR runtime test: {error}");
            return;
        }

        let library =
            unsafe { libloading::Library::new(&library_path) }.expect("load compiled MLIR runtime");
        let solve: libloading::Symbol<SolveComponentFn> = unsafe {
            library
                .get(b"rumoca_solve_linear_component\0")
                .expect("linear component symbol")
        };

        let matrix = [3.0, 1.0, 1.0, 2.0];
        let rhs = [9.0, 8.0];
        let first = unsafe {
            solve(
                matrix.as_ptr() as usize as i64,
                rhs.as_ptr() as usize as i64,
                2,
                0,
            )
        };
        let second = unsafe {
            solve(
                matrix.as_ptr() as usize as i64,
                rhs.as_ptr() as usize as i64,
                2,
                1,
            )
        };
        assert_eq!((first, second), (2.0, 3.0));

        let singular = [1.0, 2.0, 2.0, 4.0];
        let singular_result = unsafe {
            solve(
                singular.as_ptr() as usize as i64,
                rhs.as_ptr() as usize as i64,
                2,
                0,
            )
        };
        assert!(singular_result.is_nan());
    }
}
