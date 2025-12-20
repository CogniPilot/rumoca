//! Lint module for Modelica code analysis.
//!
//! Provides lint rules similar to Clippy for Rust:
//! - Style checks (naming conventions, documentation)
//! - Correctness checks (unused variables, undefined references)
//! - Performance suggestions
//! - Best practices
//!
//! ## Configuration
//!
//! The linter can be configured via:
//! - A `.rumoca_lint.toml` or `rumoca_lint.toml` file in the project root
//! - Command line options (override file settings)
//!
//! Example config file:
//! ```toml
//! min_level = "warning"
//! disabled_rules = ["magic-number", "missing-documentation"]
//! ```

mod rules;

pub use rules::*;

// Re-export shared symbol analysis from ir/analysis
pub use crate::ir::analysis::symbols::{
    DefinedSymbol, collect_defined_symbols, collect_used_symbols, is_class_instance_type,
};

use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::path::Path;

use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::{ClassDefinition, ClassType, StoredDefinition, Variability};
use crate::ir::transform::flatten::flatten;

/// Severity level for lint messages
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum LintLevel {
    /// Suggestions for improvement
    Help,
    /// Style or convention issues
    Note,
    /// Potential problems
    Warning,
    /// Definite errors
    Error,
}

impl std::fmt::Display for LintLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LintLevel::Help => write!(f, "help"),
            LintLevel::Note => write!(f, "note"),
            LintLevel::Warning => write!(f, "warning"),
            LintLevel::Error => write!(f, "error"),
        }
    }
}

/// A lint message
#[derive(Debug, Clone)]
pub struct LintMessage {
    /// The lint rule that generated this message
    pub rule: &'static str,
    /// Severity level
    pub level: LintLevel,
    /// Human-readable message
    pub message: String,
    /// File path
    pub file: String,
    /// Line number (1-based)
    pub line: u32,
    /// Column number (1-based)
    pub column: u32,
    /// Optional suggestion for fixing the issue
    pub suggestion: Option<String>,
}

impl LintMessage {
    pub fn new(
        rule: &'static str,
        level: LintLevel,
        message: impl Into<String>,
        file: impl Into<String>,
        line: u32,
        column: u32,
    ) -> Self {
        Self {
            rule,
            level,
            message: message.into(),
            file: file.into(),
            line,
            column,
            suggestion: None,
        }
    }

    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }
}

/// Config file names to search for (in priority order)
pub const LINT_CONFIG_FILE_NAMES: &[&str] = &[".rumoca_lint.toml", "rumoca_lint.toml"];

/// Configuration for which lints to run
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(default)]
pub struct LintConfig {
    /// Minimum severity to report
    pub min_level: LintLevel,
    /// Specific rules to disable
    pub disabled_rules: HashSet<String>,
    /// Specific rules to enable (if empty, all are enabled)
    pub enabled_rules: HashSet<String>,
    /// Whether to treat warnings as errors
    pub deny_warnings: bool,
}

impl Default for LintConfig {
    fn default() -> Self {
        Self {
            min_level: LintLevel::Help,
            disabled_rules: HashSet::new(),
            enabled_rules: HashSet::new(),
            deny_warnings: false,
        }
    }
}

impl LintConfig {
    /// Check if a rule should be run
    pub fn should_run(&self, rule: &str) -> bool {
        if self.disabled_rules.contains(rule) {
            return false;
        }
        if !self.enabled_rules.is_empty() && !self.enabled_rules.contains(rule) {
            return false;
        }
        true
    }

    /// Check if a message should be reported
    pub fn should_report(&self, msg: &LintMessage) -> bool {
        msg.level >= self.min_level && self.should_run(msg.rule)
    }

    /// Load lint config from a config file.
    ///
    /// Searches for config files in the following order:
    /// 1. `.rumoca_lint.toml` in the given directory
    /// 2. `rumoca_lint.toml` in the given directory
    /// 3. Same files in parent directories, up to the root
    ///
    /// Returns `None` if no config file is found.
    pub fn from_config_file(start_dir: &Path) -> Option<Self> {
        let mut current = start_dir.to_path_buf();
        if current.is_file() {
            current = current.parent()?.to_path_buf();
        }

        loop {
            for config_name in LINT_CONFIG_FILE_NAMES {
                let config_path = current.join(config_name);
                if config_path.exists()
                    && let Ok(contents) = std::fs::read_to_string(&config_path)
                    && let Ok(config) = toml::from_str::<LintConfig>(&contents)
                {
                    return Some(config);
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
    pub fn merge_cli_options(
        &mut self,
        cli_min_level: Option<LintLevel>,
        cli_disabled_rules: &[String],
        cli_enabled_rules: &[String],
        cli_deny_warnings: Option<bool>,
    ) {
        if let Some(min_level) = cli_min_level {
            self.min_level = min_level;
        }
        // CLI disabled rules are additive
        for rule in cli_disabled_rules {
            self.disabled_rules.insert(rule.clone());
        }
        // CLI enabled rules override file config if specified
        if !cli_enabled_rules.is_empty() {
            self.enabled_rules = cli_enabled_rules.iter().cloned().collect();
        }
        if let Some(deny_warnings) = cli_deny_warnings {
            self.deny_warnings = deny_warnings;
        }
    }
}

/// Result of linting a file
#[derive(Debug, Clone)]
pub struct LintResult {
    /// File that was linted
    pub file: String,
    /// All lint messages
    pub messages: Vec<LintMessage>,
    /// Whether parsing succeeded
    pub parsed: bool,
}

impl LintResult {
    pub fn new(file: impl Into<String>) -> Self {
        Self {
            file: file.into(),
            messages: Vec::new(),
            parsed: false,
        }
    }

    /// Count messages by level
    pub fn count_by_level(&self, level: LintLevel) -> usize {
        self.messages.iter().filter(|m| m.level == level).count()
    }

    /// Check if there are any errors
    pub fn has_errors(&self) -> bool {
        self.messages.iter().any(|m| m.level == LintLevel::Error)
    }

    /// Check if there are any warnings or errors
    pub fn has_warnings(&self) -> bool {
        self.messages.iter().any(|m| m.level >= LintLevel::Warning)
    }
}

/// Lint a Modelica source string
pub fn lint_str(source: &str, file_path: &str, config: &LintConfig) -> LintResult {
    let mut result = LintResult::new(file_path);

    // Parse the file
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    let mut grammar = ModelicaGrammar::new();
    match parse(source, file_path, &mut grammar) {
        Ok(_) => {
            result.parsed = true;
            if let Some(ref ast) = grammar.modelica {
                lint_ast(ast, source, file_path, config, &mut result);
            }
        }
        Err(e) => {
            result.parsed = false;
            result.messages.push(LintMessage::new(
                "parse-error",
                LintLevel::Error,
                format!("Failed to parse: {}", e),
                file_path,
                1,
                1,
            ));
        }
    }

    result
}

/// Lint a Modelica file
pub fn lint_file(path: &Path, config: &LintConfig) -> LintResult {
    let file_path = path.to_string_lossy().to_string();

    match std::fs::read_to_string(path) {
        Ok(source) => lint_str(&source, &file_path, config),
        Err(e) => {
            let mut result = LintResult::new(&file_path);
            result.messages.push(LintMessage::new(
                "io-error",
                LintLevel::Error,
                format!("Failed to read file: {}", e),
                &file_path,
                1,
                1,
            ));
            result
        }
    }
}

/// Lint an AST
fn lint_ast(
    ast: &StoredDefinition,
    source: &str,
    file_path: &str,
    config: &LintConfig,
    result: &mut LintResult,
) {
    // Build initial scope with peer class names (for cross-class type references)
    let mut base_scope = SymbolTable::new();
    for class_name in ast.class_list.keys() {
        base_scope.add_global(class_name);
    }

    // Run lints on each top-level class
    for (class_name, class) in &ast.class_list {
        lint_class(
            class,
            class_name,
            ast,
            source,
            file_path,
            config,
            result,
            &base_scope, // Start with builtins + peer class names
        );
    }
}

/// Lint a class definition
#[allow(clippy::too_many_arguments)]
fn lint_class(
    class: &ClassDefinition,
    class_path: &str,
    ast: &StoredDefinition,
    source: &str,
    file_path: &str,
    config: &LintConfig,
    result: &mut LintResult,
    parent_scope: &SymbolTable,
) {
    // Clone parent scope to build current scope (includes builtins from SymbolTable::new())
    let mut scope = parent_scope.clone();

    // Try to flatten for inherited symbol analysis
    let flattened = match flatten(ast, Some(class_path)) {
        Ok(fc) => Some(fc),
        Err(e) => {
            // Report flatten error as a lint warning
            let error_msg = e.to_string();
            let short_msg = error_msg.lines().next().unwrap_or(&error_msg);
            result.messages.push(LintMessage {
                level: LintLevel::Warning,
                rule: "flatten-error",
                message: format!("could not flatten '{}': {}", class_path, short_msg),
                file: file_path.to_string(),
                line: class.name.location.start_line,
                column: class.name.location.start_column,
                suggestion: None,
            });
            None
        }
    };
    let analysis_class = flattened.as_ref().unwrap_or(class);

    // Add symbols from the flattened class to the scope using SymbolTable's add_symbol
    for (name, comp) in &analysis_class.components {
        let is_parameter = matches!(comp.variability, Variability::Parameter(_));
        scope.add_symbol(name, name, &comp.type_name.to_string(), is_parameter);
    }
    // Add nested class names as global symbols (they're callable/usable)
    for nested_name in analysis_class.classes.keys() {
        scope.add_global(nested_name);
    }

    // Run individual lint rules
    if config.should_run("naming-convention") {
        lint_naming_conventions(class, file_path, result);
    }

    if config.should_run("missing-documentation") {
        lint_missing_documentation(class, file_path, result);
    }

    if config.should_run("unused-variable") {
        // Skip unused variable checking for records and connectors
        // since their fields are accessed externally
        if !matches!(class.class_type, ClassType::Record | ClassType::Connector) {
            lint_unused_variables(analysis_class, file_path, &scope, result);
        }
    }

    if config.should_run("undefined-reference") {
        lint_undefined_references(analysis_class, file_path, &scope, result);
    }

    if config.should_run("parameter-no-default") {
        lint_parameter_defaults(class, file_path, result);
    }

    if config.should_run("empty-section") {
        lint_empty_sections(class, file_path, result);
    }

    if config.should_run("magic-number") {
        lint_magic_numbers(class, file_path, source, result);
    }

    if config.should_run("complex-expression") {
        lint_complex_expressions(class, file_path, result);
    }

    if config.should_run("inconsistent-units") {
        lint_unit_consistency(class, file_path, result);
    }

    if config.should_run("redundant-extends") {
        lint_redundant_extends(class, file_path, result);
    }

    // Recursively lint nested classes, passing current scope
    for (nested_name, nested_class) in &class.classes {
        let nested_path = format!("{}.{}", class_path, nested_name);
        lint_class(
            nested_class,
            &nested_path,
            ast,
            source,
            file_path,
            config,
            result,
            &scope,
        );
    }
}
