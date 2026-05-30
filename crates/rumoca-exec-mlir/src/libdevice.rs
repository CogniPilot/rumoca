/// Helpers for finding and linking CUDA libdevice (`libdevice.10.bc`).
///
/// NVPTX cannot lower f64 transcendental ops (`sin`, `cos`, `exp`, `log`, etc.)
/// without CUDA's libdevice bitcode library.  This module:
/// 1. Detects whether the LLVM IR text references any `__nv_*` or `llvm.sin`/`llvm.cos`
///    intrinsics that require libdevice.
/// 2. Searches well-known installation paths for `libdevice.10.bc`.
/// 3. Links it using `llvm-link-18` and strips unexported symbols via `opt-18 --internalize`.
use crate::error::MlirError;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Standard locations where `libdevice.10.bc` is installed.
const LIBDEVICE_SEARCH_PATHS: &[&str] = &[
    "/usr/lib/nvidia-cuda-toolkit/libdevice/libdevice.10.bc",
    "/usr/local/cuda/nvvm/libdevice/libdevice.10.bc",
    "/usr/local/cuda-12/nvvm/libdevice/libdevice.10.bc",
    "/usr/local/cuda-13/nvvm/libdevice/libdevice.10.bc",
    "/usr/local/cuda-11/nvvm/libdevice/libdevice.10.bc",
    "/opt/cuda/nvvm/libdevice/libdevice.10.bc",
];

/// Return the path to `libdevice.10.bc`, consulting `explicit` first then
/// the standard search paths.
pub fn find_libdevice(explicit: Option<&Path>) -> Result<PathBuf, MlirError> {
    if let Some(p) = explicit {
        if p.exists() {
            return Ok(p.to_path_buf());
        }
        return Err(MlirError::LibdeviceNotFound {
            searched: p.display().to_string(),
        });
    }
    for candidate in LIBDEVICE_SEARCH_PATHS {
        let p = Path::new(candidate);
        if p.exists() {
            return Ok(p.to_path_buf());
        }
    }
    Err(MlirError::LibdeviceNotFound {
        searched: LIBDEVICE_SEARCH_PATHS.join(", "),
    })
}

/// Return `true` if `ll_text` references any LLVM intrinsic that NVPTX cannot
/// lower without libdevice (sin, cos, tan, exp, log, sqrt, pow, etc.).
pub fn ll_needs_libdevice(ll_text: &str) -> bool {
    const TRIG_INTRINSICS: &[&str] = &[
        "llvm.sin.",
        "llvm.cos.",
        "llvm.tan.",
        "llvm.exp.",
        "llvm.exp2.",
        "llvm.log.",
        "llvm.log2.",
        "llvm.log10.",
        "llvm.pow.",
        "llvm.sqrt.",
        "llvm.fma.",
        "__nv_", // libdevice direct calls
    ];
    TRIG_INTRINSICS.iter().any(|pat| ll_text.contains(pat))
}

/// Link `libdevice.10.bc` into `ll_path` (text LLVM IR) and return the path
/// to the resulting optimised bitcode ready for `llc-18 --march=nvptx64`.
///
/// Steps:
///   1. `llvm-link-18 <ll_path> <libdevice.bc> -o <linked.bc>`
///   2. `opt-18 <linked.bc> --nvvm-reflect --internalize
///              --internalize-public-api-list=<entry> -o <opt.bc>`
///
/// The returned path is `<parent>/model_ld.bc` inside the same directory as
/// `ll_path`.
pub fn link_libdevice(
    ll_path: &Path,
    libdevice_bc: &Path,
    entry_name: &str,
) -> Result<PathBuf, MlirError> {
    let dir = ll_path.parent().unwrap();
    let linked_bc = dir.join("model_ld.bc");
    let opt_bc = dir.join("model_opt.bc");

    // Step 1 — link
    run_tool(
        "llvm-link-18",
        Command::new("llvm-link-18")
            .arg(ll_path)
            .arg(libdevice_bc)
            .arg("-o")
            .arg(&linked_bc),
    )?;

    // Step 2 — internalize + nvvm-reflect
    run_tool(
        "opt-18",
        Command::new("opt-18")
            .arg(&linked_bc)
            .arg("--nvvm-reflect")
            .arg("--internalize")
            .arg(format!("--internalize-public-api-list={entry_name}"))
            .arg("-o")
            .arg(&opt_bc),
    )?;

    Ok(opt_bc)
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
