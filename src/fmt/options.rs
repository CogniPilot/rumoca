//! Format options and configuration.

use serde::{Deserialize, Serialize};

/// Formatting options for Modelica code
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct FormatOptions {
    /// Number of spaces per indentation level (ignored if use_tabs is true)
    pub indent_size: usize,
    /// Use tabs instead of spaces for indentation
    pub use_tabs: bool,
    /// Maximum line length before wrapping arrays
    pub max_line_length: usize,
    /// Preserve unformatted content (annotations, etc.) by copying from source
    /// When true, any content not explicitly handled by the formatter is preserved
    #[serde(default)]
    pub preserve_unformatted: bool,
    /// Number of blank lines to insert between top-level class definitions (models, functions, etc.)
    #[serde(default = "default_blank_lines_between_classes")]
    pub blank_lines_between_classes: usize,
}

fn default_blank_lines_between_classes() -> usize {
    1
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            indent_size: 2,
            use_tabs: false,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        }
    }
}

/// Config file names to search for (in priority order)
pub const CONFIG_FILE_NAMES: &[&str] = &[".rumoca_fmt.toml", "rumoca_fmt.toml"];

impl FormatOptions {
    /// Create options with specified indent size using spaces
    pub fn with_spaces(indent_size: usize) -> Self {
        Self {
            indent_size,
            use_tabs: false,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        }
    }

    /// Create options using tabs for indentation
    pub fn with_tabs() -> Self {
        Self {
            indent_size: 1,
            use_tabs: true,
            max_line_length: 100,
            preserve_unformatted: true,
            blank_lines_between_classes: 1,
        }
    }

    /// Load format options from a config file.
    ///
    /// Searches for config files in the following order:
    /// 1. `.rumoca_fmt.toml` in the given directory
    /// 2. `rumoca_fmt.toml` in the given directory
    /// 3. Same files in parent directories, up to the root
    ///
    /// Returns `None` if no config file is found.
    pub fn from_config_file(start_dir: &std::path::Path) -> Option<Self> {
        let mut current = start_dir.to_path_buf();
        if current.is_file() {
            current = current.parent()?.to_path_buf();
        }

        loop {
            for config_name in CONFIG_FILE_NAMES {
                let config_path = current.join(config_name);
                if config_path.exists()
                    && let Ok(contents) = std::fs::read_to_string(&config_path)
                    && let Ok(options) = toml::from_str::<FormatOptions>(&contents)
                {
                    return Some(options);
                }
            }

            // Move to parent directory
            if let Some(parent) = current.parent() {
                current = parent.to_path_buf();
            } else {
                break;
            }
        }

        None
    }

    /// Merge CLI options into this config, with CLI taking precedence.
    ///
    /// Only overrides fields that were explicitly set (non-default).
    pub fn merge_cli_options(
        &mut self,
        cli_indent_size: Option<usize>,
        cli_use_tabs: Option<bool>,
        cli_max_line_length: Option<usize>,
    ) {
        if let Some(indent_size) = cli_indent_size {
            self.indent_size = indent_size;
        }
        if let Some(use_tabs) = cli_use_tabs {
            self.use_tabs = use_tabs;
        }
        if let Some(max_line_length) = cli_max_line_length {
            self.max_line_length = max_line_length;
        }
    }

    /// Merge CLI options into this config, with CLI taking precedence (extended version).
    ///
    /// Only overrides fields that were explicitly set (non-default).
    pub fn merge_cli_options_ext(
        &mut self,
        cli_indent_size: Option<usize>,
        cli_use_tabs: Option<bool>,
        cli_max_line_length: Option<usize>,
        cli_blank_lines_between_classes: Option<usize>,
    ) {
        self.merge_cli_options(cli_indent_size, cli_use_tabs, cli_max_line_length);
        if let Some(blank_lines) = cli_blank_lines_between_classes {
            self.blank_lines_between_classes = blank_lines;
        }
    }
}
