//! Modelica code formatter.

mod format_errors;
mod format_options;
mod formatter;

pub use format_errors::FormatError;
pub use format_options::{
    CONFIG_FILE_NAMES, ConfigError, FormatOptions, FormatProfile, PartialFormatOptions,
    find_config, load_config, load_config_from_dir,
};
pub use formatter::{
    format, format_or_original, format_or_original_with_source_name, format_with_source_name,
};

/// Check if source code is valid Modelica syntax.
pub fn check_syntax(source: &str) -> Result<(), String> {
    rumoca_compile::parsing::validate_source_syntax(source, "<check>").map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_syntax_reports_error_for_invalid_source() {
        let err = check_syntax("model M Real x end M;").expect_err("expected parse error");
        assert!(err.to_lowercase().contains("unexpected"));
    }
}
