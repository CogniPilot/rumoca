/// Target hardware for the MLIR compilation pipeline.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum MlirTarget {
    /// Native CPU, no explicit vectorization passes (default).
    #[default]
    CpuNative,
    /// Native CPU with `-O3 --vectorize-loops --vectorize-slp` passed to `llc`.
    CpuVectorized,
    /// CUDA GPU — infrastructure present; requires `nvptx` LLVM target and a CUDA runtime.
    GpuCuda,
    /// ROCm GPU — infrastructure present; requires `amdgpu` LLVM target and a ROCm runtime.
    GpuRocm,
}

/// Optimisation level forwarded to `llc-18`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OptLevel {
    O0,
    O1,
    #[default]
    O2,
    O3,
}

impl OptLevel {
    pub fn flag(self) -> &'static str {
        match self {
            OptLevel::O0 => "-O0",
            OptLevel::O1 => "-O1",
            OptLevel::O2 => "-O2",
            OptLevel::O3 => "-O3",
        }
    }
}

/// Options controlling how `compile_derivative_rhs_with_opts` invokes the
/// MLIR toolchain.
#[derive(Debug, Clone, Default)]
pub struct MlirBackendOptions {
    pub target: MlirTarget,
    pub opt_level: OptLevel,
    /// Override the GPU chip target string.
    ///
    /// For `GpuCuda` this is an SM level string such as `"sm_75"` or `"sm_86"`.
    /// For `GpuRocm` this is a GCN string such as `"gfx906"` or `"gfx1100"`.
    /// When `None` the backend chooses a conservative default.
    pub gpu_chip: Option<String>,
    /// Explicit path to `libdevice.10.bc`.
    ///
    /// When `None` the backend searches standard locations
    /// (`/usr/lib/nvidia-cuda-toolkit/libdevice/`, `/usr/local/cuda/nvvm/libdevice/`, …).
    /// Required for `GpuCuda` models that use transcendental ops (`sin`, `cos`, `exp`,
    /// `log`, etc.).  If absent and the LLVM IR references libdevice intrinsics, compilation
    /// returns `MlirError::LibdeviceNotFound`.
    pub libdevice_path: Option<std::path::PathBuf>,
}
