# SPEC_0018: Tool Configuration Loading

## Status
ACCEPTED

## Summary
Tool crates (formatter, linter) support configuration via TOML files with hierarchical lookup and CLI override merging.

## Motivation
Tools need configurable behavior:
- Different projects have different style preferences
- CI/CD may need different settings than local development
- Config files allow reproducible formatting/linting

## Specification

### Configuration File Names

Each tool searches for config files in order:

| Tool | File Names |
|------|------------|
| Formatter | `.rumoca_fmt.toml`, `rumoca_fmt.toml` |
| Linter | `.rumoca_lint.toml`, `rumoca_lint.toml` |

Hidden files (dot-prefixed) take precedence.

### Hierarchical Lookup

Configuration search starts from the target file's directory and walks up to root:

```
/project/src/model.mo → searches:
  /project/src/.rumoca_fmt.toml
  /project/src/rumoca_fmt.toml
  /project/.rumoca_fmt.toml
  /project/rumoca_fmt.toml
  /.rumoca_fmt.toml  (stops at root)
```

First file found wins (no merging between files).

### API

```rust
//! rumoca-tool-fmt/src/options.rs

/// Configuration file names to search for.
pub const CONFIG_FILE_NAMES: &[&str] = &[".rumoca_fmt.toml", "rumoca_fmt.toml"];

/// Error that can occur when loading configuration.
#[derive(Debug, Error)]
pub enum ConfigError {
    #[error("failed to read config file: {0}")]
    ReadError(#[from] std::io::Error),
    #[error("failed to parse config file: {0}")]
    ParseError(#[from] toml::de::Error),
}

/// Find a configuration file by searching directory and parents.
pub fn find_config(start_dir: &Path) -> Option<PathBuf>;

/// Load configuration from a specific file path.
pub fn load_config(path: &Path) -> Result<FormatOptions, ConfigError>;

/// Load configuration from a directory, searching parent directories.
pub fn load_config_from_dir(dir: &Path) -> Result<Option<FormatOptions>, ConfigError>;
```

### CLI Override Pattern

CLI arguments override file configuration using partial option types:

```rust
/// Full options (all fields required)
pub struct FormatOptions {
    pub indent_size: usize,
    pub use_tabs: bool,
    pub max_line_length: usize,
    // ...
}

/// Partial options for CLI overrides (all fields optional)
pub struct PartialFormatOptions {
    pub indent_size: Option<usize>,
    pub use_tabs: Option<bool>,
    pub max_line_length: Option<usize>,
    // ...
}

impl FormatOptions {
    /// Merge with partial options. CLI values override file values.
    pub fn merge(self, cli: PartialFormatOptions) -> Self {
        FormatOptions {
            indent_size: cli.indent_size.unwrap_or(self.indent_size),
            use_tabs: cli.use_tabs.unwrap_or(self.use_tabs),
            max_line_length: cli.max_line_length.unwrap_or(self.max_line_length),
            // ...
        }
    }
}
```

### Usage Pattern

```rust
// Load config from file (if exists)
let file_config = load_config_from_dir(input.parent().unwrap())?
    .unwrap_or_default();

// Merge with CLI overrides
let options = file_config.merge(cli_options);

// Use merged options
let formatted = format(&source, &options)?;
```

### TOML Format

```toml
# .rumoca_fmt.toml
indent_size = 4
use_tabs = false
max_line_length = 120
align_annotations = true
insert_final_newline = true
trim_trailing_whitespace = true
```

```toml
# .rumoca_lint.toml
min_level = "warning"  # "help", "note", "warning", "error"
disabled_rules = ["magic-number", "naming-convention"]
warnings_as_errors = false
max_messages = 100
```

### Error Handling

- Missing config file: Use defaults (not an error)
- Malformed TOML: Return `ConfigError::ParseError`
- Unreadable file: Return `ConfigError::ReadError`
- Unknown fields: Ignored (forward compatibility)

## Rationale
- Hierarchical lookup matches rustfmt, eslint patterns
- Partial options enable clean CLI override semantics
- TOML is Rust ecosystem standard for config
- Separate error type enables specific error handling

## References
- rustfmt.toml: https://rust-lang.github.io/rustfmt/
- ESLint config: https://eslint.org/docs/user-guide/configuring/
- SPEC_0015: Formatter specification (archived/deferred)
