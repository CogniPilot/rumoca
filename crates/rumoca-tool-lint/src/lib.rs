//! Modelica linter implementation crate.

mod lint;
mod lint_options;
mod lint_rules;

pub use lint::lint;
pub use lint_options::{
    CONFIG_FILE_NAMES, ConfigError, LintOptions, PartialLintOptions, find_config, load_config,
    load_config_from_dir,
};
pub use lint_rules::{LintLevel, LintMessage, LintRule};
