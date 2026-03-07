use crate::analysis_fmt as fmt;
use crate::analysis_lint as lint;

pub use crate::analysis_fmt_errors::FormatError;
pub use crate::analysis_fmt_options::{
    CONFIG_FILE_NAMES as FMT_CONFIG_FILE_NAMES, ConfigError as FmtConfigError, FormatOptions,
    PartialFormatOptions, find_config as find_fmt_config, load_config as load_fmt_config,
    load_config_from_dir as load_fmt_config_from_dir,
};
pub use crate::analysis_lint_options::{
    CONFIG_FILE_NAMES as LINT_CONFIG_FILE_NAMES, ConfigError as LintConfigError, LintOptions,
    PartialLintOptions, find_config as find_lint_config, load_config as load_lint_config,
    load_config_from_dir as load_lint_config_from_dir,
};
pub use crate::analysis_lint_rules::{LintLevel, LintMessage, LintRule};

pub fn format_source(source: &str, options: &FormatOptions) -> Result<String, FormatError> {
    fmt::format(source, options)
}

pub fn format_source_or_original(source: &str, options: &FormatOptions) -> String {
    fmt::format_or_original(source, options)
}

pub fn lint_source(source: &str, file_name: &str, options: &LintOptions) -> Vec<LintMessage> {
    lint::lint(source, file_name, options)
}
