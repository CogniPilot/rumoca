use thiserror::Error;

#[derive(Debug, Error)]
pub enum MlirError {
    #[error("MLIR tool not found ({tool}): {source}")]
    ToolNotFound {
        tool: &'static str,
        source: std::io::Error,
    },
    #[error("{tool} failed (exit {code}):\n{stderr}")]
    ToolFailed {
        tool: &'static str,
        code: i32,
        stderr: String,
    },
    #[error("template rendering failed: {0}")]
    Template(String),
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("dlopen failed: {0}")]
    Loading(#[from] libloading::Error),
    #[error("symbol not found in compiled library: {0}")]
    MissingSymbol(String),
    /// NVPTX f64 trig/transcendental ops require CUDA libdevice but it was not found.
    #[error(
        "CUDA libdevice not found (needed for GPU sin/cos/exp/log on f64).\n\
         Install with: sudo apt-get install -y nvidia-cuda-toolkit\n\
         Or set MlirBackendOptions::libdevice_path explicitly.\n\
         Searched: {searched}"
    )]
    LibdeviceNotFound { searched: String },
}
