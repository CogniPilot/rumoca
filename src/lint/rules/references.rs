//! Reference checking lint rules.
//!
//! Rules for detecting undefined references and unused variables.

use std::collections::{HashMap, HashSet};

use crate::ir::ast::{ClassDefinition, Expression};
use crate::lint::{
    DefinedSymbol, LintLevel, LintMessage, LintResult, collect_defined_symbols,
    collect_used_symbols, is_class_instance_type,
};

/// Check for unused variables
pub fn lint_unused_variables(
    class: &ClassDefinition,
    file_path: &str,
    _globals: &HashSet<String>,
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

/// Check for undefined references
pub fn lint_undefined_references(
    class: &ClassDefinition,
    file_path: &str,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    let defined = collect_defined_symbols(class);

    // Check equations for undefined references
    for eq in &class.equations {
        check_equation_references(eq, file_path, &defined, globals, result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_equation_references(eq, file_path, &defined, globals, result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_statement_references(stmt, file_path, &defined, globals, result);
        }
    }

    // Check initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            check_statement_references(stmt, file_path, &defined, globals, result);
        }
    }

    // Check component start expressions
    for comp in class.components.values() {
        check_expression_references(&comp.start, file_path, &defined, globals, result);
    }
}

fn check_equation_references(
    eq: &crate::ir::ast::Equation,
    file_path: &str,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    match eq {
        crate::ir::ast::Equation::Empty => {}
        crate::ir::ast::Equation::Simple { lhs, rhs } => {
            check_expression_references(lhs, file_path, defined, globals, result);
            check_expression_references(rhs, file_path, defined, globals, result);
        }
        crate::ir::ast::Equation::Connect { lhs, rhs } => {
            check_comp_ref_references(lhs, file_path, defined, globals, result);
            check_comp_ref_references(rhs, file_path, defined, globals, result);
        }
        crate::ir::ast::Equation::For { indices, equations } => {
            // Add loop indices as locally defined
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
                check_expression_references(
                    &index.range,
                    file_path,
                    &local_defined,
                    globals,
                    result,
                );
            }
            for sub_eq in equations {
                check_equation_references(sub_eq, file_path, &local_defined, globals, result);
            }
        }
        crate::ir::ast::Equation::When(blocks) => {
            for block in blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_eq in &block.eqs {
                    check_equation_references(sub_eq, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_eq in &block.eqs {
                    check_equation_references(sub_eq, file_path, defined, globals, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation_references(sub_eq, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Equation::FunctionCall { comp, args } => {
            // Don't check function name - it might be external
            // But check if it's a locally defined variable being called
            if let Some(first) = comp.parts.first()
                && defined.contains_key(&first.ident.text)
            {
                // It's a local variable, mark as used
            }
            for arg in args {
                check_expression_references(arg, file_path, defined, globals, result);
            }
        }
    }
}

fn check_statement_references(
    stmt: &crate::ir::ast::Statement,
    file_path: &str,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    match stmt {
        crate::ir::ast::Statement::Empty => {}
        crate::ir::ast::Statement::Assignment { comp, value } => {
            check_comp_ref_references(comp, file_path, defined, globals, result);
            check_expression_references(value, file_path, defined, globals, result);
        }
        crate::ir::ast::Statement::FunctionCall { comp, args } => {
            // Don't check function name
            if let Some(first) = comp.parts.first()
                && defined.contains_key(&first.ident.text)
            {
                // It's a local variable
            }
            for arg in args {
                check_expression_references(arg, file_path, defined, globals, result);
            }
        }
        crate::ir::ast::Statement::For { indices, equations } => {
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
                check_expression_references(
                    &index.range,
                    file_path,
                    &local_defined,
                    globals,
                    result,
                );
            }
            for sub_stmt in equations {
                check_statement_references(sub_stmt, file_path, &local_defined, globals, result);
            }
        }
        crate::ir::ast::Statement::While(block) => {
            check_expression_references(&block.cond, file_path, defined, globals, result);
            for sub_stmt in &block.stmts {
                check_statement_references(sub_stmt, file_path, defined, globals, result);
            }
        }
        crate::ir::ast::Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_stmt in &block.stmts {
                    check_statement_references(sub_stmt, file_path, defined, globals, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement_references(sub_stmt, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Statement::When(blocks) => {
            for block in blocks {
                check_expression_references(&block.cond, file_path, defined, globals, result);
                for sub_stmt in &block.stmts {
                    check_statement_references(sub_stmt, file_path, defined, globals, result);
                }
            }
        }
        crate::ir::ast::Statement::Return { .. } | crate::ir::ast::Statement::Break { .. } => {}
    }
}

fn check_expression_references(
    expr: &Expression,
    file_path: &str,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            check_comp_ref_references(comp_ref, file_path, defined, globals, result);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            // Function name might be external, don't report as undefined
            // But check subscripts if any
            for part in &comp.parts {
                if let Some(subs) = &part.subs {
                    for sub in subs {
                        if let crate::ir::ast::Subscript::Expression(sub_expr) = sub {
                            check_expression_references(
                                sub_expr, file_path, defined, globals, result,
                            );
                        }
                    }
                }
            }
            for arg in args {
                check_expression_references(arg, file_path, defined, globals, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression_references(lhs, file_path, defined, globals, result);
            check_expression_references(rhs, file_path, defined, globals, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression_references(rhs, file_path, defined, globals, result);
        }
        Expression::Array { elements } => {
            for elem in elements {
                check_expression_references(elem, file_path, defined, globals, result);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                check_expression_references(elem, file_path, defined, globals, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression_references(cond, file_path, defined, globals, result);
                check_expression_references(then_expr, file_path, defined, globals, result);
            }
            check_expression_references(else_branch, file_path, defined, globals, result);
        }
        Expression::Range { start, step, end } => {
            check_expression_references(start, file_path, defined, globals, result);
            if let Some(s) = step {
                check_expression_references(s, file_path, defined, globals, result);
            }
            check_expression_references(end, file_path, defined, globals, result);
        }
        Expression::Parenthesized { inner } => {
            check_expression_references(inner, file_path, defined, globals, result);
        }
        Expression::ArrayComprehension { expr, indices } => {
            check_expression_references(expr, file_path, defined, globals, result);
            for idx in indices {
                check_expression_references(&idx.range, file_path, defined, globals, result);
            }
        }
    }
}

fn check_comp_ref_references(
    comp_ref: &crate::ir::ast::ComponentReference,
    file_path: &str,
    defined: &HashMap<String, DefinedSymbol>,
    globals: &HashSet<String>,
    result: &mut LintResult,
) {
    if let Some(first) = comp_ref.parts.first() {
        let name = &first.ident.text;

        // Check if defined locally or globally
        if !defined.contains_key(name) && !globals.contains(name) {
            result.messages.push(
                LintMessage::new(
                    "undefined-reference",
                    LintLevel::Error,
                    format!("Undefined variable '{}'", name),
                    file_path,
                    first.ident.location.start_line,
                    first.ident.location.start_column,
                )
                .with_suggestion("Check for typos or ensure the variable is declared"),
            );
        }

        // Check subscripts
        if let Some(subs) = &first.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(sub_expr) = sub {
                    check_expression_references(sub_expr, file_path, defined, globals, result);
                }
            }
        }
    }

    // Check remaining parts' subscripts
    for part in comp_ref.parts.iter().skip(1) {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let crate::ir::ast::Subscript::Expression(sub_expr) = sub {
                    check_expression_references(sub_expr, file_path, defined, globals, result);
                }
            }
        }
    }
}
