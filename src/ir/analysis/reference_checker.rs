//! Reference checking for Modelica code.
//!
//! This module provides unified reference checking functionality used by
//! both the linter, LSP diagnostics, and the compiler's VarValidator. It checks
//! for undefined variable references while properly handling scoped constructs
//! like for-loops and array comprehensions.
//!
//! ## Configuration
//!
//! Use [`ReferenceCheckConfig`] to customize reference checking behavior:
//! - `imported_packages`: Package roots that should be considered valid (e.g., "Modelica")
//! - `additional_globals`: Extra global symbols beyond what's in the SymbolTable

use std::collections::{HashMap, HashSet};

use crate::ir::analysis::symbol_table::SymbolTable;
use crate::ir::analysis::symbols::{DefinedSymbol, add_loop_indices_to_defined};
use crate::ir::ast::{
    ClassDefinition, ComponentReference, Equation, Expression, Import, Statement, Subscript,
};

/// Configuration for reference checking.
///
/// This allows customizing how reference checking behaves, particularly for
/// handling imports and additional global symbols.
#[derive(Clone, Debug, Default)]
pub struct ReferenceCheckConfig {
    /// Imported package root names (e.g., "Modelica" from "import Modelica.Math.*;")
    /// References starting with these names are considered valid.
    pub imported_packages: HashSet<String>,
    /// Additional global symbols that should be considered valid.
    /// Useful for peer class names, external function names, etc.
    pub additional_globals: HashSet<String>,
}

impl ReferenceCheckConfig {
    /// Create a new empty configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a configuration from a class's imports.
    ///
    /// Extracts the root package names from all imports in the class.
    pub fn from_imports(imports: &[Import]) -> Self {
        Self {
            imported_packages: collect_imported_packages(imports),
            additional_globals: HashSet::new(),
        }
    }

    /// Add imported package roots.
    pub fn with_imported_packages(mut self, packages: HashSet<String>) -> Self {
        self.imported_packages = packages;
        self
    }

    /// Add additional global symbols.
    pub fn with_additional_globals(mut self, globals: HashSet<String>) -> Self {
        self.additional_globals = globals;
        self
    }
}

/// Collect the root package names from imports.
///
/// For example:
/// - `import Modelica;` -> "Modelica"
/// - `import Modelica.Math.*;` -> "Modelica"
/// - `import SI = Modelica.Units.SI;` -> "Modelica" (the actual package root)
pub fn collect_imported_packages(imports: &[Import]) -> HashSet<String> {
    let mut packages = HashSet::new();

    for import in imports {
        match import {
            Import::Qualified { path, .. } => {
                // import A.B.C; -> root is "A"
                if let Some(first) = path.name.first() {
                    packages.insert(first.text.clone());
                }
            }
            Import::Renamed { path, .. } => {
                // import D = A.B.C; -> root is "A"
                if let Some(first) = path.name.first() {
                    packages.insert(first.text.clone());
                }
            }
            Import::Unqualified { path, .. } => {
                // import A.B.*; -> root is "A"
                if let Some(first) = path.name.first() {
                    packages.insert(first.text.clone());
                }
            }
            Import::Selective { path, .. } => {
                // import A.B.{C, D}; -> root is "A"
                if let Some(first) = path.name.first() {
                    packages.insert(first.text.clone());
                }
            }
        }
    }

    packages
}

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
    check_class_references_with_config(class, defined, scope, &ReferenceCheckConfig::default())
}

/// Check a class for undefined references with custom configuration.
///
/// This function checks all equations, statements, and component start
/// expressions for references to undefined variables, using the provided
/// configuration for import handling and additional globals.
///
/// # Arguments
/// * `class` - The class definition to check
/// * `defined` - Map of locally defined symbols
/// * `scope` - Symbol table for global/parent scope resolution
/// * `config` - Configuration for reference checking behavior
///
/// # Returns
/// A `ReferenceCheckResult` containing any errors and the set of used symbols.
pub fn check_class_references_with_config(
    class: &ClassDefinition,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    config: &ReferenceCheckConfig,
) -> ReferenceCheckResult {
    let mut result = ReferenceCheckResult::default();

    // Check all equations (regular + initial)
    for eq in class.iter_all_equations() {
        check_equation(eq, defined, scope, config, &mut result);
    }

    // Check all statements (algorithms + initial algorithms)
    for stmt in class.iter_all_statements() {
        check_statement(stmt, defined, scope, config, &mut result);
    }

    // Check component start expressions
    for (_, comp) in class.iter_components() {
        check_expression(&comp.start, defined, scope, config, &mut result);
    }

    result
}

fn check_equation(
    eq: &Equation,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    config: &ReferenceCheckConfig,
    result: &mut ReferenceCheckResult,
) {
    match eq {
        Equation::Empty => {}
        Equation::Simple { lhs, rhs } => {
            check_expression(lhs, defined, scope, config, result);
            check_expression(rhs, defined, scope, config, result);
        }
        Equation::Connect { lhs, rhs } => {
            check_component_ref(lhs, defined, scope, config, result);
            check_component_ref(rhs, defined, scope, config, result);
        }
        Equation::For { indices, equations } => {
            // Add loop indices as locally defined
            let mut local_defined = defined.clone();
            add_loop_indices_to_defined(indices, &mut local_defined);

            // Check range expressions
            for index in indices {
                check_expression(&index.range, &local_defined, scope, config, result);
            }

            // Check nested equations with extended scope
            for sub_eq in equations {
                check_equation(sub_eq, &local_defined, scope, config, result);
            }
        }
        Equation::When(blocks) => {
            for block in blocks {
                check_expression(&block.cond, defined, scope, config, result);
                for sub_eq in &block.eqs {
                    check_equation(sub_eq, defined, scope, config, result);
                }
            }
        }
        Equation::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression(&block.cond, defined, scope, config, result);
                for sub_eq in &block.eqs {
                    check_equation(sub_eq, defined, scope, config, result);
                }
            }
            if let Some(else_eqs) = else_block {
                for sub_eq in else_eqs {
                    check_equation(sub_eq, defined, scope, config, result);
                }
            }
        }
        Equation::FunctionCall { comp: _, args } => {
            // Don't check function name - it might be external
            for arg in args {
                check_expression(arg, defined, scope, config, result);
            }
        }
    }
}

fn check_statement(
    stmt: &Statement,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    config: &ReferenceCheckConfig,
    result: &mut ReferenceCheckResult,
) {
    match stmt {
        Statement::Empty => {}
        Statement::Assignment { comp, value } => {
            check_component_ref(comp, defined, scope, config, result);
            check_expression(value, defined, scope, config, result);
        }
        Statement::FunctionCall { comp: _, args } => {
            // Don't check function name - it might be external
            for arg in args {
                check_expression(arg, defined, scope, config, result);
            }
        }
        Statement::For { indices, equations } => {
            // Add loop indices as locally defined
            let mut local_defined = defined.clone();
            add_loop_indices_to_defined(indices, &mut local_defined);

            // Check range expressions
            for index in indices {
                check_expression(&index.range, &local_defined, scope, config, result);
            }

            // Check nested statements with extended scope
            for sub_stmt in equations {
                check_statement(sub_stmt, &local_defined, scope, config, result);
            }
        }
        Statement::While(block) => {
            check_expression(&block.cond, defined, scope, config, result);
            for sub_stmt in &block.stmts {
                check_statement(sub_stmt, defined, scope, config, result);
            }
        }
        Statement::If {
            cond_blocks,
            else_block,
        } => {
            for block in cond_blocks {
                check_expression(&block.cond, defined, scope, config, result);
                for sub_stmt in &block.stmts {
                    check_statement(sub_stmt, defined, scope, config, result);
                }
            }
            if let Some(else_stmts) = else_block {
                for sub_stmt in else_stmts {
                    check_statement(sub_stmt, defined, scope, config, result);
                }
            }
        }
        Statement::When(blocks) => {
            for block in blocks {
                check_expression(&block.cond, defined, scope, config, result);
                for sub_stmt in &block.stmts {
                    check_statement(sub_stmt, defined, scope, config, result);
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
    config: &ReferenceCheckConfig,
    result: &mut ReferenceCheckResult,
) {
    match expr {
        Expression::Empty => {}
        Expression::ComponentReference(comp_ref) => {
            check_component_ref(comp_ref, defined, scope, config, result);
        }
        Expression::Terminal { .. } => {}
        Expression::FunctionCall { comp, args } => {
            // Function name might be external, but check subscripts
            for part in &comp.parts {
                if let Some(subs) = &part.subs {
                    for sub in subs {
                        if let Subscript::Expression(sub_expr) = sub {
                            check_expression(sub_expr, defined, scope, config, result);
                        }
                    }
                }
            }
            for arg in args {
                check_expression(arg, defined, scope, config, result);
            }
        }
        Expression::Binary { lhs, rhs, .. } => {
            check_expression(lhs, defined, scope, config, result);
            check_expression(rhs, defined, scope, config, result);
        }
        Expression::Unary { rhs, .. } => {
            check_expression(rhs, defined, scope, config, result);
        }
        Expression::Array { elements } => {
            for elem in elements {
                check_expression(elem, defined, scope, config, result);
            }
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                check_expression(elem, defined, scope, config, result);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                check_expression(cond, defined, scope, config, result);
                check_expression(then_expr, defined, scope, config, result);
            }
            check_expression(else_branch, defined, scope, config, result);
        }
        Expression::Range { start, step, end } => {
            check_expression(start, defined, scope, config, result);
            if let Some(s) = step {
                check_expression(s, defined, scope, config, result);
            }
            check_expression(end, defined, scope, config, result);
        }
        Expression::Parenthesized { inner } => {
            check_expression(inner, defined, scope, config, result);
        }
        Expression::ArrayComprehension { expr, indices } => {
            // Array comprehension indices are locally defined
            let mut local_defined = defined.clone();
            add_loop_indices_to_defined(indices, &mut local_defined);

            check_expression(expr, &local_defined, scope, config, result);
            for idx in indices {
                check_expression(&idx.range, &local_defined, scope, config, result);
            }
        }
    }
}

fn check_component_ref(
    comp_ref: &ComponentReference,
    defined: &HashMap<String, DefinedSymbol>,
    scope: &SymbolTable,
    config: &ReferenceCheckConfig,
    result: &mut ReferenceCheckResult,
) {
    if let Some(first) = comp_ref.parts.first() {
        let name = &first.ident.text;

        // Track as used
        result.used_symbols.insert(name.clone());

        // Check if defined locally, globally, in imported packages, or in additional globals
        let is_defined = defined.contains_key(name)
            || scope.contains(name)
            || config.imported_packages.contains(name)
            || config.additional_globals.contains(name);

        if !is_defined {
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
                    check_expression(sub_expr, defined, scope, config, result);
                }
            }
        }
    }

    // Check remaining parts' subscripts
    for part in comp_ref.parts.iter().skip(1) {
        if let Some(subs) = &part.subs {
            for sub in subs {
                if let Subscript::Expression(sub_expr) = sub {
                    check_expression(sub_expr, defined, scope, config, result);
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

    #[test]
    fn test_imported_package_reference() {
        let code = r#"
model Test
  Real x;
equation
  x = Modelica.Constants.pi;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");

        let defined = collect_defined_symbols(class);
        let scope = SymbolTable::new();

        // Without config, "Modelica" should be undefined
        let result = check_class_references(class, &defined, &scope);
        assert_eq!(result.errors.len(), 1);
        assert_eq!(result.errors[0].name, "Modelica");

        // With config including "Modelica" as imported package, no error
        let config = ReferenceCheckConfig::new()
            .with_imported_packages(["Modelica".to_string()].into_iter().collect());
        let result = check_class_references_with_config(class, &defined, &scope, &config);
        assert!(
            result.errors.is_empty(),
            "Expected no errors with imported package, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_additional_globals_reference() {
        // Test with a variable reference (function calls don't check function names)
        let code = r#"
model Test
  Real x;
equation
  x = PeerClass.value;
end Test;
"#;
        let ast = parse_test_code(code);
        let class = ast.class_list.get("Test").expect("Test class not found");
        let defined = collect_defined_symbols(class);
        let scope = SymbolTable::new();

        // Without config, "PeerClass" should be undefined
        let result = check_class_references(class, &defined, &scope);
        assert_eq!(result.errors.len(), 1);
        assert_eq!(result.errors[0].name, "PeerClass");

        // With config including "PeerClass" as additional global, no error
        let config = ReferenceCheckConfig::new()
            .with_additional_globals(["PeerClass".to_string()].into_iter().collect());
        let result = check_class_references_with_config(class, &defined, &scope, &config);
        assert!(
            result.errors.is_empty(),
            "Expected no errors with additional global, got: {:?}",
            result.errors
        );
    }
}
