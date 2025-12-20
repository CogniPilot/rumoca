//! Reference checking lint rules.
//!
//! Rules for detecting undefined references and unused variables.
//! Uses the unified reference checker from `ir::analysis::reference_checker`.

use crate::ir::analysis::reference_checker::check_class_references;
use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::ast::ClassDefinition;
use crate::lint::{
    LintLevel, LintMessage, LintResult, collect_defined_symbols, collect_used_symbols,
    is_class_instance_type,
};

/// Check for unused variables
pub fn lint_unused_variables(
    class: &ClassDefinition,
    file_path: &str,
    _scope: &SymbolTable,
    result: &mut LintResult,
) {
    let defined = collect_defined_symbols(class);
    let used = collect_used_symbols(class);

    for (name, sym) in &defined {
        // Skip if used, starts with underscore, is a parameter, is a class, or is a class instance
        if used.contains(name)
            || name.starts_with('_')
            || sym.is_parameter
            || sym.is_constant
            || sym.is_class
            || is_class_instance_type(&sym.type_name)
        {
            continue;
        }

        result.messages.push(
            LintMessage::new(
                "unused-variable",
                LintLevel::Warning,
                format!("Variable '{}' is declared but never used", name),
                file_path,
                sym.line,
                sym.col,
            )
            .with_suggestion(format!(
                "Remove the variable or prefix with underscore: _{}",
                name
            )),
        );
    }
}

/// Check for undefined references using the unified reference checker.
///
/// This delegates to `ir::analysis::reference_checker::check_class_references`
/// and converts the results to lint messages.
pub fn lint_undefined_references(
    class: &ClassDefinition,
    file_path: &str,
    scope: &SymbolTable,
    result: &mut LintResult,
) {
    let defined = collect_defined_symbols(class);
    let check_result = check_class_references(class, &defined, scope);

    for error in check_result.errors {
        result.messages.push(
            LintMessage::new(
                "undefined-reference",
                LintLevel::Error,
                error.message,
                file_path,
                error.line,
                error.col,
            )
            .with_suggestion("Check for typos or ensure the variable is declared"),
        );
    }
}
