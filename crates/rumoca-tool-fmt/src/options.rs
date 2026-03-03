//! Formatter configuration options.

use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Configuration file names to search for.
pub const CONFIG_FILE_NAMES: &[&str] = &[".rumoca_fmt.toml", "rumoca_fmt.toml"];

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
pub fn load_config(path: &Path) -> Result<FormatOptions, ConfigError> {
    let content = std::fs::read_to_string(path)?;
    let options: FormatOptions = toml::from_str(&content)?;
    Ok(options)
}

/// Load configuration from a directory, searching parent directories.
///
/// Returns the loaded config, or None if no config file was found.
pub fn load_config_from_dir(dir: &Path) -> Result<Option<FormatOptions>, ConfigError> {
    match find_config(dir) {
        Some(path) => Ok(Some(load_config(&path)?)),
        None => Ok(None),
    }
}

/// Formatting options.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatOptions {
    /// Number of spaces per indentation level.
    #[serde(default = "default_indent_size")]
    pub indent_size: usize,

    /// Use tabs instead of spaces.
    #[serde(default)]
    pub use_tabs: bool,

    /// Maximum line length before wrapping.
    #[serde(default = "default_max_line_length")]
    pub max_line_length: usize,

    /// Align annotation modifiers.
    #[serde(default = "default_true")]
    pub align_annotations: bool,

    /// Insert newline at end of file.
    #[serde(default = "default_true")]
    pub insert_final_newline: bool,

    /// Trim trailing whitespace.
    #[serde(default = "default_true")]
    pub trim_trailing_whitespace: bool,
}

fn default_indent_size() -> usize {
    2
}

fn default_max_line_length() -> usize {
    100
}

fn default_true() -> bool {
    true
}

impl Default for FormatOptions {
    fn default() -> Self {
        FormatOptions {
            indent_size: default_indent_size(),
            use_tabs: false,
            max_line_length: default_max_line_length(),
            align_annotations: true,
            insert_final_newline: true,
            trim_trailing_whitespace: true,
        }
    }
}

impl FormatOptions {
    /// Create options with tab indentation.
    pub fn with_tabs() -> Self {
        FormatOptions {
            use_tabs: true,
            ..Default::default()
        }
    }

    /// Create options with specific indent size.
    pub fn with_indent_size(size: usize) -> Self {
        FormatOptions {
            indent_size: size,
            ..Default::default()
        }
    }

    /// Merge with another set of options.
    ///
    /// Values from `other` override values in `self`. This is useful for
    /// merging CLI options (other) with file config (self).
    pub fn merge(self, other: PartialFormatOptions) -> Self {
        FormatOptions {
            indent_size: other.indent_size.unwrap_or(self.indent_size),
            use_tabs: other.use_tabs.unwrap_or(self.use_tabs),
            max_line_length: other.max_line_length.unwrap_or(self.max_line_length),
            align_annotations: other.align_annotations.unwrap_or(self.align_annotations),
            insert_final_newline: other
                .insert_final_newline
                .unwrap_or(self.insert_final_newline),
            trim_trailing_whitespace: other
                .trim_trailing_whitespace
                .unwrap_or(self.trim_trailing_whitespace),
        }
    }
}

/// Partial formatting options for CLI overrides.
///
/// Each field is optional, allowing CLI arguments to override only
/// the specific options that were explicitly provided.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PartialFormatOptions {
    /// Number of spaces per indentation level.
    pub indent_size: Option<usize>,
    /// Use tabs instead of spaces.
    pub use_tabs: Option<bool>,
    /// Maximum line length before wrapping.
    pub max_line_length: Option<usize>,
    /// Align annotation modifiers.
    pub align_annotations: Option<bool>,
    /// Insert newline at end of file.
    pub insert_final_newline: Option<bool>,
    /// Trim trailing whitespace.
    pub trim_trailing_whitespace: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_default_options() {
        let opts = FormatOptions::default();
        assert_eq!(opts.indent_size, 2);
        assert!(!opts.use_tabs);
        assert_eq!(opts.max_line_length, 100);
    }

    #[test]
    fn test_with_tabs() {
        let opts = FormatOptions::with_tabs();
        assert!(opts.use_tabs);
    }

    #[test]
    fn test_with_indent_size() {
        let opts = FormatOptions::with_indent_size(4);
        assert_eq!(opts.indent_size, 4);
    }

    #[test]
    fn test_merge_options() {
        let base = FormatOptions::default();
        let overrides = PartialFormatOptions {
            indent_size: Some(4),
            use_tabs: Some(true),
            ..Default::default()
        };
        let merged = base.merge(overrides);
        assert_eq!(merged.indent_size, 4);
        assert!(merged.use_tabs);
        // Other fields should remain default
        assert_eq!(merged.max_line_length, 100);
    }

    #[test]
    fn test_load_config_from_toml() {
        let dir = tempfile::tempdir().unwrap();
        let config_path = dir.path().join(".rumoca_fmt.toml");
        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, "indent_size = 4").unwrap();
        writeln!(file, "use_tabs = true").unwrap();

        let opts = load_config(&config_path).unwrap();
        assert_eq!(opts.indent_size, 4);
        assert!(opts.use_tabs);
    }

    #[test]
    fn test_find_config() {
        let dir = tempfile::tempdir().unwrap();
        let config_path = dir.path().join(".rumoca_fmt.toml");
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
        let config_path = dir.path().join("rumoca_fmt.toml");
        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, "indent_size = 8").unwrap();

        let opts = load_config_from_dir(dir.path()).unwrap();
        assert!(opts.is_some());
        assert_eq!(opts.unwrap().indent_size, 8);
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
        let config_path = dir.path().join(".rumoca_fmt.toml");
        let mut file = std::fs::File::create(&config_path).unwrap();
        writeln!(file, "invalid toml [[[").unwrap();

        let result = load_config(&config_path);
        assert!(result.is_err());
    }
}
