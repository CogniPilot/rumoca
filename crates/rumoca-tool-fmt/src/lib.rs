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

/// Backward-compatible alias.
pub fn format_with_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> Result<String, FormatError> {
    format_with_source_name(source, options, source_name)
}

/// Backward-compatible alias.
pub fn format_or_original_with_name(
    source: &str,
    options: &FormatOptions,
    source_name: &str,
) -> String {
    format_or_original_with_source_name(source, options, source_name)
}

/// Check if source code is valid Modelica syntax.
pub fn check_syntax(source: &str) -> Result<(), String> {
    rumoca_session::parsing::validate_source_syntax(source, "<check>").map_err(|e| e.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn check_syntax_reports_error_for_invalid_source() {
        let err = check_syntax("model M Real x end M;").expect_err("expected parse error");
        assert!(err.to_lowercase().contains("unexpected"));
    }

    #[test]
    fn alias_format_with_name_matches_primary_function() {
        let source = "model M\n  Real x;\nend M;\n";
        let options = FormatOptions::default();

        let via_primary =
            format_with_source_name(source, &options, "sample.mo").expect("primary format");
        let via_alias = format_with_name(source, &options, "sample.mo").expect("alias format");

        assert_eq!(via_alias, via_primary);
    }

    #[test]
    fn alias_format_or_original_with_name_matches_primary_function() {
        let invalid = "model M Real x end M;";
        let options = FormatOptions::default();

        let via_primary = format_or_original_with_source_name(invalid, &options, "sample.mo");
        let via_alias = format_or_original_with_name(invalid, &options, "sample.mo");

        assert_eq!(via_alias, via_primary);
        assert_eq!(via_alias, invalid);
    }
}
