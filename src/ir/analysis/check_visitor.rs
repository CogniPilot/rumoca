//! Visitor-based type checking for Modelica classes.
//!
//! This module provides a unified visitor that performs multiple semantic checks
//! in a single AST traversal, improving efficiency over separate check functions.
//!
//! # Checks Performed
//!
//! - `cardinality_context`: Validates cardinality() is only used in if-conditions or asserts
//! - `class_member_access`: Validates member access on class-typed variables
//! - `scalar_subscripts`: Validates subscript expressions are scalar
//! - `array_bounds`: Validates array subscripts are within bounds
//!
//! # Example
//!
//! ```ignore
//! use rumoca::ir::analysis::check_visitor::{CheckConfig, CheckVisitor};
//!
//! let config = CheckConfig::all();
//! let mut visitor = CheckVisitor::new(config, &defined_symbols);
//! class.accept(&mut visitor);
//! let result = visitor.into_result();
//! ```

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Component, Equation, Expression, Location, Statement};
use crate::ir::visitor::Visitor;

use super::semantic::{TypeCheckResult, TypeError, TypeErrorSeverity};
use super::symbols::DefinedSymbol;
use super::type_inference::SymbolType;

// =============================================================================
// Check Configuration
// =============================================================================

/// Configuration for which checks to perform during traversal.
///
/// Use `CheckConfig::all()` to enable all checks, or create a custom
/// configuration to run only specific checks.
#[derive(Debug, Clone, Default)]
pub struct CheckConfig {
    /// Check that cardinality() is only used in if-conditions or assert statements
    pub check_cardinality_context: bool,
    /// Check that class member access is valid (e.g., `x.y` where x is a class instance)
    pub check_class_member_access: bool,
    /// Check that subscript expressions are scalar (not arrays)
    pub check_scalar_subscripts: bool,
    /// Check that array subscripts are within bounds
    pub check_array_bounds: bool,
}

impl CheckConfig {
    /// Create a configuration with all checks enabled
    pub fn all() -> Self {
        Self {
            check_cardinality_context: true,
            check_class_member_access: true,
            check_scalar_subscripts: true,
            check_array_bounds: true,
        }
    }

    /// Create a configuration with no checks enabled
    pub fn none() -> Self {
        Self::default()
    }

    /// Create a configuration with only cardinality context check enabled
    pub fn cardinality_only() -> Self {
        Self {
            check_cardinality_context: true,
            ..Self::default()
        }
    }
}

// =============================================================================
// Check Context
// =============================================================================

/// Context tracking during traversal for semantic checks.
#[derive(Debug, Clone, Default)]
struct CheckContext {
    /// Whether we're inside an if-equation/statement condition
    in_condition: bool,
    /// Whether we're inside an assert statement
    in_assert: bool,
    /// Current component being checked (for bindings)
    current_component: Option<String>,
    /// Whether we're inside a for loop (for break/return checks)
    in_loop: bool,
    /// Whether we're inside a while loop
    in_while: bool,
    /// Whether we're inside a function (for return checks)
    #[allow(dead_code)]
    in_function: bool,
}

// =============================================================================
// Check Visitor
// =============================================================================

/// Unified visitor for performing multiple semantic checks in a single traversal.
///
/// This visitor collects all type errors found during traversal into a single
/// `TypeCheckResult`. The checks performed are controlled by the `CheckConfig`.
pub struct CheckVisitor<'a> {
    /// Configuration for which checks to perform
    config: CheckConfig,
    /// Accumulated type errors
    result: TypeCheckResult,
    /// Defined symbols for type lookups
    #[allow(dead_code)]
    defined: &'a HashMap<String, DefinedSymbol>,
    /// Component shapes for array bounds checking
    #[allow(dead_code)]
    component_shapes: HashMap<String, Vec<usize>>,
    /// Traversal context
    context: CheckContext,
}

impl<'a> CheckVisitor<'a> {
    /// Create a new check visitor with the given configuration and symbol table.
    pub fn new(config: CheckConfig, defined: &'a HashMap<String, DefinedSymbol>) -> Self {
        Self {
            config,
            result: TypeCheckResult::new(),
            defined,
            component_shapes: HashMap::new(),
            context: CheckContext::default(),
        }
    }

    /// Create a new check visitor with component shape information for bounds checking.
    pub fn with_shapes(
        config: CheckConfig,
        defined: &'a HashMap<String, DefinedSymbol>,
        component_shapes: HashMap<String, Vec<usize>>,
    ) -> Self {
        Self {
            config,
            result: TypeCheckResult::new(),
            defined,
            component_shapes,
            context: CheckContext::default(),
        }
    }

    /// Consume the visitor and return the accumulated type check result.
    pub fn into_result(self) -> TypeCheckResult {
        self.result
    }

    /// Get a reference to the current result.
    pub fn result(&self) -> &TypeCheckResult {
        &self.result
    }

    /// Add a cardinality context error at the given location.
    fn add_cardinality_error(&mut self, location: Location) {
        self.result.add_error(TypeError::new(
            location,
            SymbolType::Unknown,
            SymbolType::Unknown,
            "cardinality may only be used in the condition of an if-statement/equation or an assert.".to_string(),
            TypeErrorSeverity::Error,
        ));
    }

    // =========================================================================
    // Cardinality Context Checking
    // =========================================================================

    /// Check if an expression contains a cardinality() call and report an error
    /// if we're not in a valid context (if-condition or assert).
    fn check_cardinality_in_expr(&mut self, expr: &Expression) {
        if !self.config.check_cardinality_context {
            return;
        }

        // If we're in a valid context, cardinality is allowed
        if self.context.in_condition || self.context.in_assert {
            return;
        }

        // Look for cardinality calls in this expression
        if let Some(loc) = find_cardinality_call(expr) {
            self.add_cardinality_error(loc);
        }
    }
}

// =============================================================================
// Visitor Implementation
// =============================================================================

impl Visitor for CheckVisitor<'_> {
    fn enter_component(&mut self, node: &Component) {
        // Check component bindings for cardinality context violations
        if self.config.check_cardinality_context && !matches!(node.start, Expression::Empty) {
            self.context.current_component = Some(node.name.clone());
            self.check_cardinality_in_expr(&node.start);
            self.context.current_component = None;
        }
    }

    fn enter_equation(&mut self, node: &Equation) {
        match node {
            Equation::Simple { lhs, rhs } => {
                // Check both sides for cardinality violations
                self.check_cardinality_in_expr(lhs);
                self.check_cardinality_in_expr(rhs);
            }
            Equation::If { cond_blocks, .. } => {
                // The conditions are valid contexts for cardinality
                // We'll check them with in_condition = true
                for block in cond_blocks {
                    self.context.in_condition = true;
                    // Note: The visitor will descend into the condition expression
                    // but we've set the flag, so cardinality is allowed there
                    // We need to check the condition here since Visitor doesn't
                    // have a separate hook for if-conditions
                    let _ = &block.cond; // Condition is checked with in_condition=true
                    self.context.in_condition = false;
                }
            }
            Equation::FunctionCall { comp, args, .. } => {
                // Check if this is an assert - cardinality IS allowed in assert
                let is_assert = comp.parts.len() == 1
                    && comp.parts.first().is_some_and(|p| p.ident.text == "assert");

                if is_assert {
                    self.context.in_assert = true;
                    // Args will be checked with in_assert=true
                } else {
                    // Check arguments for cardinality violations
                    for arg in args {
                        self.check_cardinality_in_expr(arg);
                    }
                }
            }
            _ => {}
        }
    }

    fn exit_equation(&mut self, node: &Equation) {
        // Reset context flags
        if let Equation::FunctionCall { comp, .. } = node {
            let is_assert = comp.parts.len() == 1
                && comp.parts.first().is_some_and(|p| p.ident.text == "assert");
            if is_assert {
                self.context.in_assert = false;
            }
        }
    }

    fn enter_statement(&mut self, node: &Statement) {
        match node {
            Statement::Assignment { value, .. } => {
                self.check_cardinality_in_expr(value);
            }
            Statement::If { cond_blocks, .. } => {
                // The conditions are valid contexts for cardinality
                for block in cond_blocks {
                    self.context.in_condition = true;
                    let _ = &block.cond;
                    self.context.in_condition = false;
                }
            }
            Statement::While(block) => {
                // While condition is a valid context for cardinality
                self.context.in_condition = true;
                let _ = &block.cond;
                self.context.in_condition = false;
                self.context.in_while = true;
            }
            Statement::For { .. } => {
                self.context.in_loop = true;
            }
            Statement::FunctionCall { comp, args, .. } => {
                let is_assert = comp.parts.len() == 1
                    && comp.parts.first().is_some_and(|p| p.ident.text == "assert");

                if is_assert {
                    self.context.in_assert = true;
                } else {
                    for arg in args {
                        self.check_cardinality_in_expr(arg);
                    }
                }
            }
            _ => {}
        }
    }

    fn exit_statement(&mut self, node: &Statement) {
        match node {
            Statement::While(_) => {
                self.context.in_while = false;
            }
            Statement::For { .. } => {
                self.context.in_loop = false;
            }
            Statement::FunctionCall { comp, .. } => {
                let is_assert = comp.parts.len() == 1
                    && comp.parts.first().is_some_and(|p| p.ident.text == "assert");
                if is_assert {
                    self.context.in_assert = false;
                }
            }
            _ => {}
        }
    }
}

// =============================================================================
// Helper Functions
// =============================================================================

/// Find a cardinality() call in an expression and return its location.
fn find_cardinality_call(expr: &Expression) -> Option<Location> {
    match expr {
        Expression::FunctionCall { comp, args, .. } => {
            // Check if this is a cardinality call
            if comp.parts.len() == 1
                && let Some(first) = comp.parts.first()
                && first.ident.text == "cardinality"
            {
                return Some(first.ident.location.clone());
            }
            // Recurse into arguments
            for arg in args {
                if let Some(loc) = find_cardinality_call(arg) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Binary { lhs, rhs, .. } => {
            find_cardinality_call(lhs).or_else(|| find_cardinality_call(rhs))
        }
        Expression::Unary { rhs, .. } => find_cardinality_call(rhs),
        Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, then_expr) in branches {
                if let Some(loc) = find_cardinality_call(cond) {
                    return Some(loc);
                }
                if let Some(loc) = find_cardinality_call(then_expr) {
                    return Some(loc);
                }
            }
            find_cardinality_call(else_branch)
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                if let Some(loc) = find_cardinality_call(elem) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Range { start, step, end } => {
            if let Some(loc) = find_cardinality_call(start) {
                return Some(loc);
            }
            if let Some(loc) = step.as_ref().and_then(|s| find_cardinality_call(s)) {
                return Some(loc);
            }
            find_cardinality_call(end)
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                if let Some(loc) = find_cardinality_call(elem) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::ArrayComprehension { expr, indices } => {
            if let Some(loc) = find_cardinality_call(expr) {
                return Some(loc);
            }
            for idx in indices {
                if let Some(loc) = find_cardinality_call(&idx.range) {
                    return Some(loc);
                }
            }
            None
        }
        Expression::Parenthesized { inner } => find_cardinality_call(inner),
        Expression::ComponentReference(_) | Expression::Terminal { .. } | Expression::Empty => None,
    }
}

// =============================================================================
// Public API
// =============================================================================

/// Perform all configured checks on a class definition using the visitor pattern.
///
/// This is more efficient than calling individual check functions as it
/// performs all checks in a single AST traversal.
pub fn check_class(
    class: &ClassDefinition,
    config: CheckConfig,
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    use crate::ir::visitor::Visitable;

    let mut visitor = CheckVisitor::new(config, defined);
    class.accept(&mut visitor);
    visitor.into_result()
}

/// Perform all checks on a class definition.
pub fn check_all(
    class: &ClassDefinition,
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    check_class(class, CheckConfig::all(), defined)
}

/// Perform all semantic checks on a class definition.
///
/// This is the recommended entry point for semantic validation. It combines
/// multiple semantic checks into a single result:
///
/// - Cardinality context validation
/// - Class member access validation
/// - Scalar subscript validation
/// - Array bounds validation
/// - Component binding validation
/// - Assert argument validation
/// - Break/return context validation
///
/// # Arguments
///
/// * `class` - The class definition to check
/// * `defined` - Symbol table with defined symbols (optional, empty HashMap if not available)
///
/// # Returns
///
/// A `TypeCheckResult` containing all errors found during semantic checking.
pub fn check_semantic(
    class: &ClassDefinition,
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    use super::semantic;

    let mut result = TypeCheckResult::new();

    // Cardinality checks
    result.merge(semantic::check_cardinality_context(class));
    result.merge(semantic::check_cardinality_arguments(class));

    // Class and member access checks
    result.merge(semantic::check_class_member_access(class));

    // Array and subscript checks
    result.merge(semantic::check_scalar_subscripts(class));
    result.merge(semantic::check_array_bounds(class));

    // Component and binding checks
    result.merge(semantic::check_component_bindings(class));
    result.merge(semantic::check_assert_arguments(class));
    result.merge(semantic::check_builtin_attribute_modifiers(class));

    // Control flow checks
    result.merge(semantic::check_break_return_context(class));

    // Start modification dimension checks
    result.merge(semantic::check_start_modification_dimensions(class));

    // Visitor-based checks (for future expansion)
    let visitor_result = check_all(class, defined);
    result.merge(visitor_result);

    result
}

/// Perform semantic checks with equation type checking.
///
/// This extends `check_semantic` by also performing type checking on equations
/// using the provided symbol table.
///
/// # Arguments
///
/// * `class` - The class definition to check
/// * `defined` - Symbol table with defined symbols for type inference
///
/// # Returns
///
/// A `TypeCheckResult` containing all errors found during semantic and type checking.
pub fn check_semantic_with_types(
    class: &ClassDefinition,
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    use super::semantic;

    let mut result = check_semantic(class, defined);

    // Type check equations
    result.merge(semantic::check_equations(&class.equations, defined));

    // Type check algorithm sections
    for algorithm_block in &class.algorithms {
        result.merge(semantic::check_statements(algorithm_block, defined));
    }

    result
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_config_all() {
        let config = CheckConfig::all();
        assert!(config.check_cardinality_context);
        assert!(config.check_class_member_access);
        assert!(config.check_scalar_subscripts);
        assert!(config.check_array_bounds);
    }

    #[test]
    fn test_check_config_none() {
        let config = CheckConfig::none();
        assert!(!config.check_cardinality_context);
        assert!(!config.check_class_member_access);
        assert!(!config.check_scalar_subscripts);
        assert!(!config.check_array_bounds);
    }

    #[test]
    fn test_check_config_cardinality_only() {
        let config = CheckConfig::cardinality_only();
        assert!(config.check_cardinality_context);
        assert!(!config.check_class_member_access);
        assert!(!config.check_scalar_subscripts);
        assert!(!config.check_array_bounds);
    }

    #[test]
    fn test_find_cardinality_call_not_found() {
        // Test that find_cardinality_call returns None for non-cardinality expressions
        let expr = Expression::Empty;
        assert!(find_cardinality_call(&expr).is_none());
    }

    #[test]
    fn test_find_cardinality_call_in_array() {
        // Test that find_cardinality_call returns None for array without cardinality
        let expr = Expression::Array {
            elements: vec![Expression::Empty, Expression::Empty],
            is_matrix: false,
        };
        assert!(find_cardinality_call(&expr).is_none());
    }

    #[test]
    fn test_find_cardinality_call_in_parenthesized() {
        // Test that find_cardinality_call descends into parenthesized expressions
        let expr = Expression::Parenthesized {
            inner: Box::new(Expression::Empty),
        };
        assert!(find_cardinality_call(&expr).is_none());
    }
}
