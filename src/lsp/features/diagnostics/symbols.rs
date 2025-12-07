//! Symbol collection for diagnostics.
//!
//! Provides functions to collect and check symbols in expressions, equations, and statements.
//! Type checking is delegated to the shared `ir::analysis::type_checker` module.

use std::collections::{HashMap, HashSet};

use lsp_types::{Diagnostic, DiagnosticSeverity};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_checker::{TypeCheckResult, TypeErrorSeverity};
use crate::ir::ast::{ComponentReference, Equation, Expression, Statement};

use super::helpers::create_diagnostic;

/// Convert type errors from the type checker to LSP diagnostics
pub fn type_errors_to_diagnostics(type_result: &TypeCheckResult) -> Vec<Diagnostic> {
    type_result
        .errors
        .iter()
        .map(|err| {
            let severity = match err.severity {
                TypeErrorSeverity::Warning => DiagnosticSeverity::WARNING,
                TypeErrorSeverity::Error => DiagnosticSeverity::ERROR,
            };
            create_diagnostic(
                err.location.start_line,
                err.location.start_column,
                err.message.clone(),
                severity,
            )
        })
        .collect()
}

/// Collect symbols used in an equation and check for undefined references.
///
/// Note: Type checking is performed separately using `type_checker::check_equation()`.
/// Call `type_errors_to_diagnostics()` to convert type errors to LSP diagnostics.
pub fn collect_equation_symbols(
    eq: &Equation,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            collect_and_check_expression(lhs, used, diagnostics, defined, globals);
            collect_and_check_expression(rhs, used, diagnostics, defined, globals);
            // Type checking is handled separately via type_checker::check_equation()
        }
        Equation::Connect { lhs, rhs } => {
            collect_and_check_component_ref(lhs, used, diagnostics, defined, globals);
            collect_and_check_component_ref(rhs, used, diagnostics, defined, globals);
        }
        Equation::For { indices, equations } => {
            // For loop indices are locally defined
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
                collect_and_check_expression(
                    &index.range,
                    used,
                    diagnostics,
                    &local_defined,
                    globals,
                );
            }
            for sub_eq in equations {
                collect_equation_symbols(sub_eq, used, diagnostics, &local_defined, globals);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_eq in &block.eqs {
                    collect_equation_symbols(sub_eq, used, diagnostics, defined, globals);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_eq in &block.eqs {
                    collect_equation_symbols(sub_eq, used, diagnostics, defined, globals);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    collect_equation_symbols(sub_eq, used, diagnostics, defined, globals);
                }
            }
        }
        Equation::FunctionCall { comp, args } => {
            collect_and_check_component_ref(comp, used, diagnostics, defined, globals);
            for arg in args {
                collect_and_check_expression(arg, used, diagnostics, defined, globals);
            }
        }
    }
}

/// Collect symbols used in a statement
pub fn collect_statement_symbols(
    stmt: &Statement,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            collect_and_check_component_ref(comp, used, diagnostics, defined, globals);
            collect_and_check_expression(value, used, diagnostics, defined, globals);
        }
        Statement::FunctionCall { comp, args } => {
            collect_and_check_component_ref(comp, used, diagnostics, defined, globals);
            for arg in args {
                collect_and_check_expression(arg, used, diagnostics, defined, globals);
            }
        }
        Statement::For { indices, equations } => {
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
                collect_and_check_expression(
                    &index.range,
                    used,
                    diagnostics,
                    &local_defined,
                    globals,
                );
            }
            for sub_stmt in equations {
                collect_statement_symbols(sub_stmt, used, diagnostics, &local_defined, globals);
            }
        }
        Statement::While(block) => {
            collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
            for sub_stmt in &block.stmts {
                collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_stmt in &block.stmts {
                    collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                collect_and_check_expression(&block.cond, used, diagnostics, defined, globals);
                for sub_stmt in &block.stmts {
                    collect_statement_symbols(sub_stmt, used, diagnostics, defined, globals);
                }
            }
        }
        Statement::Return { .. } | Statement::Break { .. } => {}
    }
}

/// Collect used symbols from an expression (for unused variable detection)
pub fn collect_used_symbols(expr: &Expression, used: &mut HashSet<String>) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                used.insert(first.ident.text.clone());
            }
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            if let Some(first) = comp.parts.first() {
                used.insert(first.ident.text.clone());
            }
            for arg in args {
                collect_used_symbols(arg, used);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_used_symbols(lhs, used);
            collect_used_symbols(rhs, used);
        }
        Expression::Unary { rhs, .. } => {
            collect_used_symbols(rhs, used);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_used_symbols(elem, used);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_used_symbols(elem, used);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_used_symbols(cond, used);
                collect_used_symbols(then_expr, used);
            }
            collect_used_symbols(else_branch, used);
        }
        Expression::Range { start, step, end } => {
            collect_used_symbols(start, used);
            if let Some(s) = step {
                collect_used_symbols(s, used);
            }
            collect_used_symbols(end, used);
        }
        Expression::Parenthesized { inner } => {
            collect_used_symbols(inner, used);
        }
        Expression::ArrayComprehension { expr, indices } => {
            collect_used_symbols(expr, used);
            for idx in indices {
                collect_used_symbols(&idx.range, used);
            }
        }
    }
}

/// Collect and check expression for undefined references
pub fn collect_and_check_expression(
    expr: &Expression,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            collect_and_check_component_ref(comp_ref, used, diagnostics, defined, globals);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            // For function calls, the first part is the function name
            // Check if it's a known function or type
            if let Some(first) = comp.parts.first() {
                let name = &first.ident.text;
                // Functions are typically global builtins or user-defined
                // Mark as used if it's defined locally
                if defined.contains_key(name) {
                    used.insert(name.clone());
                }
                // Don't report error for function names - they might be external
            }
            for arg in args {
                collect_and_check_expression(arg, used, diagnostics, defined, globals);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            collect_and_check_expression(lhs, used, diagnostics, defined, globals);
            collect_and_check_expression(rhs, used, diagnostics, defined, globals);
        }
        Expression::Unary { rhs, .. } => {
            collect_and_check_expression(rhs, used, diagnostics, defined, globals);
        }
        Expression::Array { elements } => {
            for elem in elements {
                collect_and_check_expression(elem, used, diagnostics, defined, globals);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                collect_and_check_expression(elem, used, diagnostics, defined, globals);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                collect_and_check_expression(cond, used, diagnostics, defined, globals);
                collect_and_check_expression(then_expr, used, diagnostics, defined, globals);
            }
            collect_and_check_expression(else_branch, used, diagnostics, defined, globals);
        }
        Expression::Range { start, step, end } => {
            collect_and_check_expression(start, used, diagnostics, defined, globals);
            if let Some(s) = step {
                collect_and_check_expression(s, used, diagnostics, defined, globals);
            }
            collect_and_check_expression(end, used, diagnostics, defined, globals);
        }
        Expression::Parenthesized { inner } => {
            collect_and_check_expression(inner, used, diagnostics, defined, globals);
        }
        Expression::ArrayComprehension { expr, indices } => {
            collect_and_check_expression(expr, used, diagnostics, defined, globals);
            for idx in indices {
                collect_and_check_expression(&idx.range, used, diagnostics, defined, globals);
            }
        }
    }
}

/// Check component reference for undefined symbols
pub fn collect_and_check_component_ref(
    comp_ref: &ComponentReference,
    used: &mut HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
) {
    if let Some(first) = comp_ref.parts.first() {
        let name = &first.ident.text;

        // Mark as used
        used.insert(name.clone());

        // Check if defined
        if !defined.contains_key(name) && !globals.contains(name) {
            diagnostics.push(create_diagnostic(
                first.ident.location.start_line,
                first.ident.location.start_column,
                format!("Undefined variable '{}'", name),
                DiagnosticSeverity::ERROR,
            ));
        }

        // Check subscript expressions
        if let Some(subs) = &first.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(expr) = sub {
                    collect_and_check_expression(expr, used, diagnostics, defined, globals);
                }
            }
        }
    }

    // Check remaining parts' subscripts
    for part in comp_ref.parts.iter().skip(1) {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(expr) = sub {
                    collect_and_check_expression(expr, used, diagnostics, defined, globals);
                }
            }
        }
    }
}
