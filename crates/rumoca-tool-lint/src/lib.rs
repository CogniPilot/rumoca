//! Modelica linter facade.
//!
//! This crate is a thin wrapper over `rumoca-session::analysis`.

pub use rumoca_session::analysis::{
    LINT_CONFIG_FILE_NAMES as CONFIG_FILE_NAMES, LintConfigError as ConfigError, LintLevel,
    LintMessage, LintOptions, LintRule, PartialLintOptions, find_lint_config as find_config,
    load_lint_config as load_config, load_lint_config_from_dir as load_config_from_dir,
};

/// Lint Modelica source code.
///
/// Returns a list of lint messages (warnings, errors, suggestions).
pub fn lint(source: &str, file_name: &str, options: &LintOptions) -> Vec<LintMessage> {
    rumoca_session::analysis::lint_source(source, file_name, options)
}
