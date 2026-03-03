//! Linter configuration options.

use crate::rules::LintLevel;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Configuration file names to search for.
pub const CONFIG_FILE_NAMES: &[&str] = &[".rumoca_lint.toml", "rumoca_lint.toml"];

/// Error that can occur when loading configuration.
#[derive(Debug, Error)]
pub enum ConfigError {
    /// Failed to read the configuration file.
    #[error("failed to read config file: {0}")]
    ReadError(#[from] std::io::Error),
    /// Failed to parse the configuration file.
    #[error("failed to parse config file: {0}")]
    ParseError(#[from] toml::de::Error),
}

/// Find a configuration file by searching the given directory and its parents.
///
/// Returns the path to the first config file found, or None if no config file exists.
pub fn find_config(start_dir: &Path) -> Option<PathBuf> {
    let mut current = start_dir.to_path_buf();
    loop {
        for name in CONFIG_FILE_NAMES {
            let config_path = current.join(name);
            if config_path.is_file() {
                return Some(config_path);
            }
        }
        if !current.pop() {
            return None;
        }
    }
}

/// Load configuration from a specific file path.
pub fn load_config(path: &Path) -> Result<LintOptions, ConfigError> {
    let content = std::fs::read_to_string(path)?;
    let options: LintOptions = toml::from_str(&content)?;
    Ok(options)
}

/// Load configuration from a directory, searching parent directories.
///
/// Returns the loaded config, or None if no config file was found.
pub fn load_config_from_dir(dir: &Path) -> Result<Option<LintOptions>, ConfigError> {
    match find_config(dir) {
        Some(path) => Ok(Some(load_config(&path)?)),
        None => Ok(None),
    }
}

/// Linting options.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LintOptions {
    /// Minimum severity level to report.
    #[serde(default)]
    pub min_level: LintLevel,

    /// List of disabled rule names.
    #[serde(default)]
    pub disabled_rules: Vec<String>,

    /// Treat warnings as errors.
    #[serde(default)]
    pub warnings_as_errors: bool,

    /// Maximum number of messages to report.
    #[serde(default = "default_max_messages")]
    pub max_messages: usize,
}

fn default_max_messages() -> usize {
    100
}

impl Default for LintOptions {
    fn default() -> Self {
        LintOptions {
            min_level: LintLevel::Warning,
            disabled_rules: Vec::new(),
            warnings_as_errors: false,
            max_messages: default_max_messages(),
        }
    }
}

impl LintOptions {
    /// Create options that only report errors.
    pub fn errors_only() -> Self {
        LintOptions {
            min_level: LintLevel::Error,
            ..Default::default()
        }
    }

    /// Create options with specific disabled rules.
    pub fn with_disabled_rules(rules: Vec<String>) -> Self {
        LintOptions {
            disabled_rules: rules,
            ..Default::default()
        }
    }

    /// Merge with another set of options.
    ///
    /// Values from `other` override values in `self`. This is useful for
    /// merging CLI options (other) with file config (self).
    pub fn merge(self, other: PartialLintOptions) -> Self {
        let mut disabled = self.disabled_rules;
        if let Some(ref additional) = other.disabled_rules {
            disabled.extend(additional.iter().cloned());
        }

        LintOptions {
            min_level: other.min_level.unwrap_or(self.min_level),
            disabled_rules: disabled,
            warnings_as_errors: other.warnings_as_errors.unwrap_or(self.warnings_as_errors),
            max_messages: other.max_messages.unwrap_or(self.max_messages),
        }
    }
}

/// Partial linting options for CLI overrides.
///
/// Each field is optional, allowing CLI arguments to override only
/// the specific options that were explicitly provided.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PartialLintOptions {
    /// Minimum severity level to report.
    pub min_level: Option<LintLevel>,
    /// Additional rules to disable (merged with config file).
    pub disabled_rules: Option<Vec<String>>,
    /// Treat warnings as errors.
    pub warnings_as_errors: Option<bool>,
    /// Maximum number of messages to report.
    pub max_messages: Option<usize>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_default_options() {
        let opts = LintOptions::default();
        assert_eq!(opts.min_level, LintLevel::Warning);
        assert!(opts.disabled_rules.is_empty());
    }

    #[test]
    fn test_errors_only() {
        let opts = LintOptions::errors_only();
        assert_eq!(opts.min_level, LintLevel::Error);
    }

    #[test]
    fn test_merge_options() {
        let base = LintOptions::default();
        let overrides = PartialLintOptions {
            min_level: Some(LintLevel::Error),
            disabled_rules: Some(vec!["test-rule".to_string()]),
            ..Default::default()
        };
        let merged = base.merge(overrides);
        assert_eq!(merged.min_level, LintLevel::Error);
        assert!(merged.disabled_rules.contains(&"test-rule".to_string()));
    }

    #[test]
    fn test_load_config_from_toml() {
        let dir = tempfile::tempdir().unwrap();
        let config_path = dir.path().join(".rumoca_lint.toml");
        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, "min_level = \"error\"").unwrap();
        writeln!(file, "disabled_rules = [\"naming-convention\"]").unwrap();

        let opts = load_config(&config_path).unwrap();
        assert_eq!(opts.min_level, LintLevel::Error);
        assert!(
            opts.disabled_rules
                .contains(&"naming-convention".to_string())
        );
    }

    #[test]
    fn test_find_config() {
        let dir = tempfile::tempdir().unwrap();
        let config_path = dir.path().join(".rumoca_lint.toml");
        std::fs::File::create(&config_path).unwrap();

        let found = find_config(dir.path());
        assert!(found.is_some());
        assert_eq!(found.unwrap(), config_path);
    }

    #[test]
    fn test_find_config_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let found = find_config(dir.path());
        assert!(found.is_none());
    }

    #[test]
    fn test_load_config_from_dir() {
        let dir = tempfile::tempdir().unwrap();
        let config_path = dir.path().join("rumoca_lint.toml");
        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, "warnings_as_errors = true").unwrap();

        let opts = load_config_from_dir(dir.path()).unwrap();
        assert!(opts.is_some());
        assert!(opts.unwrap().warnings_as_errors);
    }

    #[test]
    fn test_load_config_from_dir_not_found() {
        let dir = tempfile::tempdir().unwrap();
        let opts = load_config_from_dir(dir.path()).unwrap();
        assert!(opts.is_none());
    }

    #[test]
    fn test_config_parse_error() {
        let dir = tempfile::tempdir().unwrap();
        let config_path = dir.path().join(".rumoca_lint.toml");
        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, "invalid toml [[[").unwrap();

        let result = load_config(&config_path);
        assert!(result.is_err());
    }
}
