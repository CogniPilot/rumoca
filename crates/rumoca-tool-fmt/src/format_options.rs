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
    let overrides = load_config_overrides(path)?;
    Ok(FormatOptions::from_partial(overrides))
}

/// Load partial configuration overrides from a specific file path.
pub fn load_config_overrides(path: &Path) -> Result<PartialFormatOptions, ConfigError> {
    let content = std::fs::read_to_string(path)?;
    let options: PartialFormatOptions = toml::from_str(&content)?;
    Ok(options)
}

/// Load configuration from a directory, searching parent directories.
pub fn load_config_from_dir(dir: &Path) -> Result<Option<FormatOptions>, ConfigError> {
    match find_config(dir) {
        Some(path) => Ok(Some(load_config(&path)?)),
        None => Ok(None),
    }
}

/// Load partial configuration overrides from a directory, searching parent directories.
pub fn load_config_overrides_from_dir(
    dir: &Path,
) -> Result<Option<PartialFormatOptions>, ConfigError> {
    match find_config(dir) {
        Some(path) => Ok(Some(load_config_overrides(&path)?)),
        None => Ok(None),
    }
}

/// Formatter behavior profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum FormatProfile {
    /// Apply conservative Dymola/MSL-compatible whitespace rules.
    Dymola,
    /// Apply canonical whitespace rules that normalize more spacing.
    Canonical,
}

/// Output line-ending style.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum LineEnding {
    /// Keep CRLF only when the input is predominantly CRLF; otherwise use LF.
    Auto,
    /// Use Unix line endings.
    Lf,
    /// Use Windows line endings.
    Crlf,
}

/// Formatting options.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatOptions {
    /// Formatter profile.
    #[serde(default = "default_profile")]
    pub profile: FormatProfile,

    /// Number of spaces per indentation level.
    #[serde(default = "default_indent_size")]
    pub indent_size: usize,

    /// Use tabs instead of spaces when normalizing indentation.
    #[serde(default)]
    pub use_tabs: bool,

    /// Normalize structural indentation. Off by default because MSL/Dymola
    /// continuation indentation carries local alignment information.
    #[serde(default)]
    pub normalize_indentation: bool,

    /// Add missing structural indentation only to unindented lines.
    #[serde(default = "default_true")]
    pub repair_missing_indentation: bool,

    /// Normalize spaces around declaration bindings and equation/algorithm assignments.
    #[serde(default)]
    pub normalize_equation_spacing: bool,

    /// Normalize spaces around binary expression operators.
    #[serde(default)]
    pub normalize_operator_spacing: bool,

    /// Normalize named argument and modification assignments to compact `=`.
    #[serde(default)]
    pub normalize_argument_assignment_spacing: bool,

    /// Insert newline at end of file.
    #[serde(default = "default_true")]
    pub insert_final_newline: bool,

    /// Trim trailing horizontal whitespace outside strings and block comments.
    #[serde(default = "default_true")]
    pub trim_trailing_whitespace: bool,

    /// Output line endings.
    #[serde(default = "default_line_ending")]
    pub line_ending: LineEnding,
}

fn default_profile() -> FormatProfile {
    FormatProfile::Dymola
}

fn default_indent_size() -> usize {
    2
}

fn default_true() -> bool {
    true
}

fn default_line_ending() -> LineEnding {
    LineEnding::Auto
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self::for_profile(default_profile())
    }
}

impl FormatOptions {
    /// Default options for a formatter profile before user overrides.
    pub fn for_profile(profile: FormatProfile) -> Self {
        match profile {
            FormatProfile::Dymola => Self {
                profile,
                indent_size: default_indent_size(),
                use_tabs: false,
                normalize_indentation: false,
                repair_missing_indentation: true,
                normalize_equation_spacing: false,
                normalize_operator_spacing: false,
                normalize_argument_assignment_spacing: false,
                insert_final_newline: true,
                trim_trailing_whitespace: true,
                line_ending: default_line_ending(),
            },
            FormatProfile::Canonical => Self {
                profile,
                indent_size: default_indent_size(),
                use_tabs: false,
                normalize_indentation: true,
                repair_missing_indentation: true,
                normalize_equation_spacing: true,
                normalize_operator_spacing: true,
                normalize_argument_assignment_spacing: true,
                insert_final_newline: true,
                trim_trailing_whitespace: true,
                line_ending: default_line_ending(),
            },
        }
    }

    /// Resolve profile defaults and then apply partial overrides.
    pub fn from_partial(overrides: PartialFormatOptions) -> Self {
        let profile = overrides.profile.unwrap_or_else(default_profile);
        Self::for_profile(profile).merge(overrides)
    }

    /// Merge with another set of options.
    pub fn merge(self, other: PartialFormatOptions) -> Self {
        Self {
            profile: other.profile.unwrap_or(self.profile),
            indent_size: other.indent_size.unwrap_or(self.indent_size),
            use_tabs: other.use_tabs.unwrap_or(self.use_tabs),
            normalize_indentation: other
                .normalize_indentation
                .unwrap_or(self.normalize_indentation),
            repair_missing_indentation: other
                .repair_missing_indentation
                .unwrap_or(self.repair_missing_indentation),
            normalize_equation_spacing: other
                .normalize_equation_spacing
                .unwrap_or(self.normalize_equation_spacing),
            normalize_operator_spacing: other
                .normalize_operator_spacing
                .unwrap_or(self.normalize_operator_spacing),
            normalize_argument_assignment_spacing: other
                .normalize_argument_assignment_spacing
                .unwrap_or(self.normalize_argument_assignment_spacing),
            insert_final_newline: other
                .insert_final_newline
                .unwrap_or(self.insert_final_newline),
            trim_trailing_whitespace: other
                .trim_trailing_whitespace
                .unwrap_or(self.trim_trailing_whitespace),
            line_ending: other.line_ending.unwrap_or(self.line_ending),
        }
    }
}

impl PartialFormatOptions {
    /// Overlay another partial option set, using `other` when a field is set.
    pub fn overlay(self, other: PartialFormatOptions) -> Self {
        Self {
            profile: other.profile.or(self.profile),
            indent_size: other.indent_size.or(self.indent_size),
            use_tabs: other.use_tabs.or(self.use_tabs),
            normalize_indentation: other.normalize_indentation.or(self.normalize_indentation),
            repair_missing_indentation: other
                .repair_missing_indentation
                .or(self.repair_missing_indentation),
            normalize_equation_spacing: other
                .normalize_equation_spacing
                .or(self.normalize_equation_spacing),
            normalize_operator_spacing: other
                .normalize_operator_spacing
                .or(self.normalize_operator_spacing),
            normalize_argument_assignment_spacing: other
                .normalize_argument_assignment_spacing
                .or(self.normalize_argument_assignment_spacing),
            insert_final_newline: other.insert_final_newline.or(self.insert_final_newline),
            trim_trailing_whitespace: other
                .trim_trailing_whitespace
                .or(self.trim_trailing_whitespace),
            line_ending: other.line_ending.or(self.line_ending),
        }
    }
}

/// Partial formatting options for CLI overrides.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PartialFormatOptions {
    /// Formatter profile.
    pub profile: Option<FormatProfile>,
    /// Number of spaces per indentation level.
    pub indent_size: Option<usize>,
    /// Use tabs instead of spaces when normalizing indentation.
    pub use_tabs: Option<bool>,
    /// Normalize structural indentation.
    pub normalize_indentation: Option<bool>,
    /// Add missing structural indentation only to unindented lines.
    pub repair_missing_indentation: Option<bool>,
    /// Normalize spaces around declaration bindings and equation/algorithm assignments.
    pub normalize_equation_spacing: Option<bool>,
    /// Normalize spaces around binary expression operators.
    pub normalize_operator_spacing: Option<bool>,
    /// Normalize named argument and modification assignments to compact `=`.
    pub normalize_argument_assignment_spacing: Option<bool>,
    /// Insert newline at end of file.
    pub insert_final_newline: Option<bool>,
    /// Trim trailing horizontal whitespace outside strings and block comments.
    pub trim_trailing_whitespace: Option<bool>,
    /// Output line endings.
    pub line_ending: Option<LineEnding>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    #[test]
    fn test_default_options() {
        let opts = FormatOptions::default();
        assert_eq!(opts.profile, FormatProfile::Dymola);
        assert_eq!(opts.indent_size, 2);
        assert!(!opts.use_tabs);
        assert!(!opts.normalize_indentation);
        assert!(opts.repair_missing_indentation);
        assert!(!opts.normalize_equation_spacing);
        assert!(!opts.normalize_operator_spacing);
        assert!(!opts.normalize_argument_assignment_spacing);
        assert!(opts.insert_final_newline);
        assert!(opts.trim_trailing_whitespace);
        assert_eq!(opts.line_ending, LineEnding::Auto);
    }

    #[test]
    fn test_canonical_profile_defaults() {
        let opts = FormatOptions::for_profile(FormatProfile::Canonical);
        assert_eq!(opts.profile, FormatProfile::Canonical);
        assert_eq!(opts.indent_size, 2);
        assert!(!opts.use_tabs);
        assert!(opts.normalize_indentation);
        assert!(opts.repair_missing_indentation);
        assert!(opts.normalize_equation_spacing);
        assert!(opts.normalize_operator_spacing);
        assert!(opts.normalize_argument_assignment_spacing);
        assert!(opts.insert_final_newline);
        assert!(opts.trim_trailing_whitespace);
        assert_eq!(opts.line_ending, LineEnding::Auto);
    }

    #[test]
    fn test_merge_options() {
        let base = FormatOptions::default();
        let overrides = PartialFormatOptions {
            profile: Some(FormatProfile::Dymola),
            indent_size: Some(4),
            use_tabs: Some(true),
            normalize_indentation: Some(true),
            repair_missing_indentation: Some(false),
            normalize_equation_spacing: Some(false),
            normalize_operator_spacing: Some(true),
            normalize_argument_assignment_spacing: Some(true),
            insert_final_newline: Some(false),
            trim_trailing_whitespace: Some(false),
            line_ending: Some(LineEnding::Crlf),
        };
        let merged = base.merge(overrides);
        assert_eq!(merged.profile, FormatProfile::Dymola);
        assert_eq!(merged.indent_size, 4);
        assert!(merged.use_tabs);
        assert!(merged.normalize_indentation);
        assert!(!merged.repair_missing_indentation);
        assert!(!merged.normalize_equation_spacing);
        assert!(merged.normalize_operator_spacing);
        assert!(merged.normalize_argument_assignment_spacing);
        assert!(!merged.insert_final_newline);
        assert!(!merged.trim_trailing_whitespace);
        assert_eq!(merged.line_ending, LineEnding::Crlf);
    }

    #[test]
    fn test_partial_options_resolve_profile_defaults_before_overrides() {
        let opts = FormatOptions::from_partial(PartialFormatOptions {
            profile: Some(FormatProfile::Canonical),
            normalize_equation_spacing: Some(false),
            ..PartialFormatOptions::default()
        });
        assert_eq!(opts.profile, FormatProfile::Canonical);
        assert!(opts.normalize_indentation);
        assert!(!opts.normalize_equation_spacing);
        assert!(opts.normalize_operator_spacing);
        assert!(opts.normalize_argument_assignment_spacing);
    }

    #[test]
    fn test_partial_options_overlay() {
        let config = PartialFormatOptions {
            profile: Some(FormatProfile::Canonical),
            normalize_indentation: Some(true),
            normalize_equation_spacing: Some(true),
            ..PartialFormatOptions::default()
        };
        let cli = PartialFormatOptions {
            normalize_equation_spacing: Some(false),
            ..PartialFormatOptions::default()
        };
        let merged = config.overlay(cli);
        assert_eq!(merged.profile, Some(FormatProfile::Canonical));
        assert_eq!(merged.normalize_indentation, Some(true));
        assert_eq!(merged.normalize_equation_spacing, Some(false));
    }

    #[test]
    fn test_load_config_from_toml() {
        let dir = tempfile::tempdir().expect("tempdir");
        let config_path = dir.path().join(".rumoca_fmt.toml");
        let mut file = std::fs::File::create(&config_path).expect("create config");
        writeln!(file, "profile = \"dymola\"").expect("write profile");
        writeln!(file, "indent_size = 4").expect("write indent_size");
        writeln!(file, "use_tabs = true").expect("write use_tabs");
        writeln!(file, "normalize_indentation = true").expect("write normalize_indentation");
        writeln!(file, "repair_missing_indentation = false")
            .expect("write repair_missing_indentation");
        writeln!(file, "normalize_equation_spacing = false")
            .expect("write normalize_equation_spacing");
        writeln!(file, "normalize_operator_spacing = true")
            .expect("write normalize_operator_spacing");
        writeln!(file, "normalize_argument_assignment_spacing = true")
            .expect("write normalize_argument_assignment_spacing");
        writeln!(file, "insert_final_newline = false").expect("write insert_final_newline");
        writeln!(file, "trim_trailing_whitespace = false").expect("write trim_trailing");
        writeln!(file, "line_ending = \"crlf\"").expect("write line_ending");

        let opts = load_config(&config_path).expect("load config");
        assert_eq!(opts.profile, FormatProfile::Dymola);
        assert_eq!(opts.indent_size, 4);
        assert!(opts.use_tabs);
        assert!(opts.normalize_indentation);
        assert!(!opts.repair_missing_indentation);
        assert!(!opts.normalize_equation_spacing);
        assert!(opts.normalize_operator_spacing);
        assert!(opts.normalize_argument_assignment_spacing);
        assert!(!opts.insert_final_newline);
        assert!(!opts.trim_trailing_whitespace);
        assert_eq!(opts.line_ending, LineEnding::Crlf);
    }

    #[test]
    fn test_load_dymola_profile_from_toml() {
        let dir = tempfile::tempdir().expect("tempdir");
        let config_path = dir.path().join(".rumoca_fmt.toml");
        let mut file = std::fs::File::create(&config_path).expect("create config");
        writeln!(file, "profile = \"dymola\"").expect("write profile");

        let opts = load_config(&config_path).expect("load config");
        assert_eq!(opts.profile, FormatProfile::Dymola);
    }

    #[test]
    fn test_load_canonical_profile_from_toml() {
        let dir = tempfile::tempdir().expect("tempdir");
        let config_path = dir.path().join(".rumoca_fmt.toml");
        let mut file = std::fs::File::create(&config_path).expect("create config");
        writeln!(file, "profile = \"canonical\"").expect("write profile");
        writeln!(file, "normalize_equation_spacing = false")
            .expect("write normalize_equation_spacing");

        let opts = load_config(&config_path).expect("load config");
        assert_eq!(opts.profile, FormatProfile::Canonical);
        assert!(opts.normalize_indentation);
        assert!(!opts.normalize_equation_spacing);
        assert!(opts.normalize_operator_spacing);
        assert!(opts.normalize_argument_assignment_spacing);
    }
}
