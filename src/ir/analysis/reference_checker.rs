//! Reference checking for Modelica code.
//!
//! This module provides unified reference checking functionality used by
//! both the linter and LSP diagnostics. It checks for undefined variable
//! references while properly handling scoped constructs like for-loops.

use std::collections::{HashMap, HashSet};

use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::analysis::symbols::{DefinedSymbol, add_loop_indices_to_defined};
use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, Statement, Subscript,
};

/// A reference error found during checking.
#[derive(Clone, Debug)]
pub struct ReferenceError {
    /// The undefined variable name
    pub name: String,
    /// Source line number (1-based)
    pub line: u32,
    /// Source column number (1-based)
    pub col: u32,
    /// Error message
    pub message: String,
}

impl ReferenceError {
    fn undefined_variable(name: &str, line: u32, col: u32) -> Self {
        Self {
            name: name.to_string(),
            line,
            col,
            message: format!("Undefined variable '{}'", name),
        }
    }
}

/// Result of reference checking.
#[derive(Clone, Debug, Default)]
pub struct ReferenceCheckResult {
    /// Reference errors found
    pub errors: Vec<ReferenceError>,
    /// All symbols that were used/referenced
    pub used_symbols: HashSet<String>,
}

/// Check a class for undefined references.
///
/// This function checks all equations, statements, and component start
/// expressions for references to undefined variables.
///
/// # Arguments
/// * `class` - The class definition to check
/// * `defined` - Map of locally defined symbols
/// * `scope` - Symbol table for global/parent scope resolution
///
/// # Returns
/// A `ReferenceCheckResult` containing any errors and the set of used symbols.
pub fn check_class_references(
    class: &ClassDefinition,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
) -> ReferenceCheckResult {
    let mut result = ReferenceCheckResult::default();

    // Check equations
    for eq in &class.equations {
        check_equation(eq, defined, scope, &mut result);
    }

    // Check initial equations
    for eq in &class.initial_equations {
        check_equation(eq, defined, scope, &mut result);
    }

    // Check algorithms
    for algo in &class.algorithms {
        for stmt in algo {
            check_statement(stmt, defined, scope, &mut result);
        }
    }

    // Check initial algorithms
    for algo in &class.initial_algorithms {
        for stmt in algo {
            check_statement(stmt, defined, scope, &mut result);
        }
    }

    // Check component start expressions
    for comp in class.components.values() {
        check_expression(&comp.start, defined, scope, &mut result);
    }

    result
}

fn check_equation(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    result: &mut ReferenceCheckResult,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            check_expression(lhs, defined, scope, result);
            check_expression(rhs, defined, scope, result);
        }
        Equation::Connect { lhs, rhs } => {
            check_component_ref(lhs, defined, scope, result);
            check_component_ref(rhs, defined, scope, result);
        }
        Equation::For { indices, equations } => {
            // Add loop indices as locally defined
            let mut local_defined = defined.clone();
            add_loop_indices_to_defined(indices, &mut local_defined);

            // Check range expressions
            for index in indices {
                check_expression(&index.range, &local_defined, scope, result);
            }

            // Check nested equations with extended scope
            for sub_eq in equations {
                check_equation(sub_eq, &local_defined, scope, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_expression(&block.cond, defined, scope, result);
                for sub_eq in &block.eqs {
                    check_equation(sub_eq, defined, scope, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression(&block.cond, defined, scope, result);
                for sub_eq in &block.eqs {
                    check_equation(sub_eq, defined, scope, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation(sub_eq, defined, scope, result);
                }
            }
        }
        Equation::FunctionCall { comp: _, args } => {
            // Don't check function name - it might be external
            for arg in args {
                check_expression(arg, defined, scope, result);
            }
        }
    }
}

fn check_statement(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    result: &mut ReferenceCheckResult,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            check_component_ref(comp, defined, scope, result);
            check_expression(value, defined, scope, result);
        }
        Statement::FunctionCall { comp: _, args } => {
            // Don't check function name - it might be external
            for arg in args {
                check_expression(arg, defined, scope, result);
            }
        }
        Statement::For { indices, equations } => {
            // Add loop indices as locally defined
            let mut local_defined = defined.clone();
            add_loop_indices_to_defined(indices, &mut local_defined);

            // Check range expressions
            for index in indices {
                check_expression(&index.range, &local_defined, scope, result);
            }

            // Check nested statements with extended scope
            for sub_stmt in equations {
                check_statement(sub_stmt, &local_defined, scope, result);
            }
        }
        Statement::While(block) => {
            check_expression(&block.cond, defined, scope, result);
            for sub_stmt in &block.stmts {
                check_statement(sub_stmt, defined, scope, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression(&block.cond, defined, scope, result);
                for sub_stmt in &block.stmts {
                    check_statement(sub_stmt, defined, scope, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement(sub_stmt, defined, scope, result);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_expression(&block.cond, defined, scope, result);
                for sub_stmt in &block.stmts {
                    check_statement(sub_stmt, defined, scope, result);
                }
            }
        }
        Statement::Return { .. } | Statement::Break { .. } => {}
    }
}

fn check_expression(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    result: &mut ReferenceCheckResult,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            check_component_ref(comp_ref, defined, scope, result);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            // Function name might be external, but check subscripts
            for part in &comp.parts {
                if let Some(subs) = &part.subs {
                    for sub in subs {
                        if let Subscript::Expression(sub_expr) = sub {
                            check_expression(sub_expr, defined, scope, result);
                        }
                    }
                }
            }
            for arg in args {
                check_expression(arg, defined, scope, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression(lhs, defined, scope, result);
            check_expression(rhs, defined, scope, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression(rhs, defined, scope, result);
        }
        Expression::Array { elements } => {
            for elem in elements {
                check_expression(elem, defined, scope, result);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                check_expression(elem, defined, scope, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression(cond, defined, scope, result);
                check_expression(then_expr, defined, scope, result);
            }
            check_expression(else_branch, defined, scope, result);
        }
        Expression::Range { start, step, end } => {
            check_expression(start, defined, scope, result);
            if let Some(s) = step {
                check_expression(s, defined, scope, result);
            }
            check_expression(end, defined, scope, result);
        }
        Expression::Parenthesized { inner } => {
            check_expression(inner, defined, scope, result);
        }
        Expression::ArrayComprehension { expr, indices } => {
            // Array comprehension indices are locally defined
            let mut local_defined = defined.clone();
            add_loop_indices_to_defined(indices, &mut local_defined);

            check_expression(expr, &local_defined, scope, result);
            for idx in indices {
                check_expression(&idx.range, &local_defined, scope, result);
            }
        }
    }
}

fn check_component_ref(
    comp_ref: &ComponentReference,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    result: &mut ReferenceCheckResult,
) {
    if let Some(first) = comp_ref.parts.first() {
        let name = &first.ident.text;

        // Track as used
        result.used_symbols.insert(name.clone());

        // Check if defined locally or globally
        if !defined.contains_key(name) && !scope.contains(name) {
            result.errors.push(ReferenceError::undefined_variable(
                name,
                first.ident.location.start_line,
                first.ident.location.start_column,
            ));
        }

        // Check subscripts
        if let Some(subs) = &first.subs {
            for sub in subs {
                if let Subscript::Expression(sub_expr) = sub {
                    check_expression(sub_expr, defined, scope, result);
                }
            }
        }
    }

    // Check remaining parts' subscripts
    for part in comp_ref.parts.iter().skip(1) {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let Subscript::Expression(sub_expr) = sub {
                    check_expression(sub_expr, defined, scope, result);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::analysis::symbols::collect_defined_symbols;
    use crate::modelica_grammar::ModelicaGrammar;
    use crate::modelica_parser::parse;

    fn parse_test_code(code: &str) -> crate::ir::ast::StoredDefinition {
        let mut grammar = ModelicaGrammar::new();
        parse(code, "test.mo", &mut grammar).expect("Failed to parse test code");
        grammar.modelica.expect("No AST produced")
    }

    #[test]
    fn test_undefined_reference() {
        let code = r#"
model Test
  Real x;
equation
  x = y + 1.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let defined = collect_defined_symbols(class);
        let scope = SymbolTable::new();

        let result = check_class_references(class, &defined, &scope);

        assert_eq!(result.errors.len(), 1);
        assert_eq!(result.errors[0].name, "y");
    }

    #[test]
    fn test_for_loop_index() {
        let code = r#"
model Test
  Real x[10];
equation
  for i in 1:10 loop
    x[i] = i * 2.0;
  end for;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let defined = collect_defined_symbols(class);
        let scope = SymbolTable::new();

        let result = check_class_references(class, &defined, &scope);

        // No errors - 'i' should be recognized as a loop index
        assert!(
            result.errors.is_empty(),
            "Expected no errors, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_used_symbols_tracking() {
        let code = r#"
model Test
  Real x;
  Real y;
equation
  x = y + 1.0;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let defined = collect_defined_symbols(class);
        let scope = SymbolTable::new();

        let result = check_class_references(class, &defined, &scope);

        assert!(result.used_symbols.contains("x"));
        assert!(result.used_symbols.contains("y"));
    }

    #[test]
    fn test_array_comprehension_index() {
        let code = r#"
model Test
  Real x[10] = {i * 2 for i in 1:10};
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let defined = collect_defined_symbols(class);
        let scope = SymbolTable::new();

        let result = check_class_references(class, &defined, &scope);

        // No errors - 'i' should be recognized as a comprehension index
        assert!(
            result.errors.is_empty(),
            "Expected no errors, got: {:?}",
            result.errors
        );
    }
}
