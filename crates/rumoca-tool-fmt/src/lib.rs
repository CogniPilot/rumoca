//! Modelica code formatter facade.
//!
//! This crate is a thin wrapper over `rumoca-session::analysis`.

pub use rumoca_session::analysis::{
    FMT_CONFIG_FILE_NAMES as CONFIG_FILE_NAMES, FmtConfigError as ConfigError, FormatError,
    FormatOptions, PartialFormatOptions, find_fmt_config as find_config,
    load_fmt_config as load_config, load_fmt_config_from_dir as load_config_from_dir,
};

/// Format Modelica source code.
pub fn format(source: &str, options: &FormatOptions) -> Result<String, FormatError> {
    rumoca_session::analysis::format_source(source, options)
}

/// Format Modelica source code, returning original input on error.
pub fn format_or_original(source: &str, options: &FormatOptions) -> String {
    rumoca_session::analysis::format_source_or_original(source, options)
}

/// Check if source code is valid Modelica syntax.
pub fn check_syntax(source: &str) -> Result<(), String> {
    rumoca_session::parsing::validate_source_syntax(source, "<check>")
        .map(|_| ())
        .map_err(|e| e.to_string())
}
