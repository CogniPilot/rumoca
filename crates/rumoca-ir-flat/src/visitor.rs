//! Expression visitor trait for traversing flat expression trees.
//!
//! This module provides a reusable visitor pattern for Expression,
//! reducing code duplication across traversal operations.
//!
//! # Usage
//!
//! ```ignore
//! use rumoca_ir_flat as flat;
//! use rumoca_ir_flat::{Expression, ExpressionVisitor, VarName};
//!
//! struct FunctionCollector {
//!     functions: Vec<String>,
//! }
//!
//! impl ExpressionVisitor for FunctionCollector {
//!     fn visit_function_call(&mut self, name: &VarName, args: &[Expression]) {
//!         self.functions.push(name.to_string());
//!         self.walk_function_call(name, args); // Continue into args
//!     }
//! }
//!
//! let mut collector = FunctionCollector { functions: vec![] };
//! collector.visit_expression(&expr);
//! ```

use crate::{
    BuiltinFunction, ComponentReference, ComprehensionIndex, Expression, ForIndex, Literal,
    OpBinary, Statement, StatementBlock, Subscript, VarName,
};

/// Trait for visiting Expression trees without modification.
///
/// Implementors override specific `visit_*` methods to customize behavior.
/// Default implementations call `walk_*` methods to continue traversal.
///
/// # Pattern
///
/// ```ignore
/// fn visit_xyz(&mut self, ...) {
///     // Custom logic before visiting children
///     self.walk_xyz(...);  // Visit children
///     // Custom logic after visiting children
/// }
/// ```
pub trait ExpressionVisitor {
    /// Visit any expression. Override for general expression handling.
    fn visit_expression(&mut self, expr: &Expression) {
        self.walk_expression(expr);
    }

    /// Walk all children of an expression.
    fn walk_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary { op, lhs, rhs } => {
                self.visit_binary(op, lhs, rhs);
            }
            Expression::Unary { rhs, .. } => {
                self.visit_expression(rhs);
            }
            Expression::VarRef { name, subscripts } => {
                self.visit_var_ref(name, subscripts);
            }
            Expression::BuiltinCall { function, args } => {
                self.visit_builtin_call(function, args);
            }
            Expression::FunctionCall { name, args, .. } => {
                self.visit_function_call(name, args);
            }
            Expression::Literal(lit) => {
                self.visit_literal(lit);
            }
            Expression::If {
                branches,
                else_branch,
            } => {
                self.visit_if(branches, else_branch);
            }
            Expression::Array {
                elements,
                is_matrix,
            } => {
                self.visit_array(elements, *is_matrix);
            }
            Expression::Tuple { elements } => {
                self.visit_tuple(elements);
            }
            Expression::Range { start, step, end } => {
                self.visit_range(start, step.as_deref(), end);
            }
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                self.visit_array_comprehension(expr, indices, filter.as_deref());
            }
            Expression::Index { base, subscripts } => {
                self.visit_index(base, subscripts);
            }
            Expression::FieldAccess { base, field } => {
                self.visit_field_access(base, field);
            }
            Expression::Empty => {}
        }
    }

    /// Visit a field access expression.
    fn visit_field_access(&mut self, base: &Expression, _field: &str) {
        self.visit_expression(base);
    }

    /// Visit a binary expression.
    fn visit_binary(&mut self, op: &OpBinary, lhs: &Expression, rhs: &Expression) {
        self.walk_binary(op, lhs, rhs);
    }

    /// Walk children of a binary expression.
    fn walk_binary(&mut self, _op: &OpBinary, lhs: &Expression, rhs: &Expression) {
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }

    /// Visit a variable reference.
    fn visit_var_ref(&mut self, name: &VarName, subscripts: &[Subscript]) {
        self.walk_var_ref(name, subscripts);
    }

    /// Walk children of a variable reference (subscript expressions).
    fn walk_var_ref(&mut self, _name: &VarName, subscripts: &[Subscript]) {
        for sub in subscripts {
            self.visit_subscript(sub);
        }
    }

    /// Visit a subscript.
    fn visit_subscript(&mut self, sub: &Subscript) {
        self.walk_subscript(sub);
    }

    /// Walk children of a subscript.
    fn walk_subscript(&mut self, sub: &Subscript) {
        if let Subscript::Expr(expr) = sub {
            self.visit_expression(expr);
        }
    }

    /// Visit a builtin function call (der, sin, cos, etc.).
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        self.walk_builtin_call(function, args);
    }

    /// Walk children of a builtin call.
    fn walk_builtin_call(&mut self, _function: &BuiltinFunction, args: &[Expression]) {
        for arg in args {
            self.visit_expression(arg);
        }
    }

    /// Visit a user-defined function call.
    fn visit_function_call(&mut self, name: &VarName, args: &[Expression]) {
        self.walk_function_call(name, args);
    }

    /// Walk children of a function call.
    fn walk_function_call(&mut self, _name: &VarName, args: &[Expression]) {
        for arg in args {
            self.visit_expression(arg);
        }
    }

    /// Visit a literal.
    fn visit_literal(&mut self, _lit: &Literal) {
        // No children to walk
    }

    /// Visit an if-expression.
    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        self.walk_if(branches, else_branch);
    }

    /// Walk children of an if-expression.
    fn walk_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        for (cond, then_expr) in branches {
            self.visit_expression(cond);
            self.visit_expression(then_expr);
        }
        self.visit_expression(else_branch);
    }

    /// Visit an array.
    fn visit_array(&mut self, elements: &[Expression], is_matrix: bool) {
        self.walk_array(elements, is_matrix);
    }

    /// Walk children of an array.
    fn walk_array(&mut self, elements: &[Expression], _is_matrix: bool) {
        for elem in elements {
            self.visit_expression(elem);
        }
    }

    /// Visit a tuple.
    fn visit_tuple(&mut self, elements: &[Expression]) {
        self.walk_tuple(elements);
    }

    /// Walk children of a tuple.
    fn walk_tuple(&mut self, elements: &[Expression]) {
        for elem in elements {
            self.visit_expression(elem);
        }
    }

    /// Visit a range expression.
    fn visit_range(&mut self, start: &Expression, step: Option<&Expression>, end: &Expression) {
        self.walk_range(start, step, end);
    }

    /// Walk children of a range expression.
    fn walk_range(&mut self, start: &Expression, step: Option<&Expression>, end: &Expression) {
        self.visit_expression(start);
        if let Some(s) = step {
            self.visit_expression(s);
        }
        self.visit_expression(end);
    }

    /// Visit an array-comprehension expression.
    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
    ) {
        self.walk_array_comprehension(expr, indices, filter);
    }

    /// Walk children of an array-comprehension expression.
    fn walk_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
    ) {
        for index in indices {
            self.visit_expression(&index.range);
        }
        self.visit_expression(expr);
        if let Some(condition) = filter {
            self.visit_expression(condition);
        }
    }

    /// Visit an index expression.
    fn visit_index(&mut self, base: &Expression, subscripts: &[Subscript]) {
        self.walk_index(base, subscripts);
    }

    /// Walk children of an index expression.
    fn walk_index(&mut self, base: &Expression, subscripts: &[Subscript]) {
        self.visit_expression(base);
        for sub in subscripts {
            self.visit_subscript(sub);
        }
    }
}

/// Visitor for flat algorithm statements.
pub trait StatementVisitor: ExpressionVisitor {
    fn visit_statement(&mut self, stmt: &Statement) {
        self.walk_statement(stmt);
    }

    fn walk_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Empty | Statement::Return | Statement::Break => {}
            Statement::Assignment { comp, value } => self.visit_assignment(comp, value),
            Statement::For { indices, equations } => self.visit_for_statement(indices, equations),
            Statement::While(block) => self.visit_statement_block(block),
            Statement::If {
                cond_blocks,
                else_block,
            } => self.visit_if_statement(cond_blocks, else_block.as_deref()),
            Statement::When(blocks) => self.visit_when_statement(blocks),
            Statement::FunctionCall {
                comp,
                args,
                outputs,
            } => self.visit_statement_function_call(comp, args, outputs),
            Statement::Reinit { variable, value } => self.visit_reinit(variable, value),
            Statement::Assert {
                condition,
                message,
                level,
            } => self.visit_assert(condition, message, level.as_ref()),
        }
    }

    fn visit_assignment(&mut self, comp: &ComponentReference, value: &Expression) {
        self.visit_component_reference(comp);
        self.visit_expression(value);
    }

    fn visit_for_statement(&mut self, indices: &[ForIndex], statements: &[Statement]) {
        for index in indices {
            self.visit_expression(&index.range);
        }
        for stmt in statements {
            self.visit_statement(stmt);
        }
    }

    fn visit_if_statement(
        &mut self,
        cond_blocks: &[StatementBlock],
        else_block: Option<&[Statement]>,
    ) {
        for block in cond_blocks {
            self.visit_statement_block(block);
        }
        if let Some(else_statements) = else_block {
            for stmt in else_statements {
                self.visit_statement(stmt);
            }
        }
    }

    fn visit_when_statement(&mut self, blocks: &[StatementBlock]) {
        for block in blocks {
            self.visit_statement_block(block);
        }
    }

    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        outputs: &[Expression],
    ) {
        self.visit_component_reference(comp);
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs {
            self.visit_expression(output);
        }
    }

    fn visit_reinit(&mut self, variable: &ComponentReference, value: &Expression) {
        self.visit_component_reference(variable);
        self.visit_expression(value);
    }

    fn visit_assert(
        &mut self,
        condition: &Expression,
        message: &Expression,
        level: Option<&Expression>,
    ) {
        self.visit_expression(condition);
        self.visit_expression(message);
        if let Some(level_expr) = level {
            self.visit_expression(level_expr);
        }
    }

    fn visit_component_reference(&mut self, comp: &ComponentReference) {
        for part in &comp.parts {
            for subscript in &part.subs {
                self.visit_subscript(subscript);
            }
        }
    }

    fn visit_statement_block(&mut self, block: &StatementBlock) {
        self.visit_expression(&block.cond);
        for stmt in &block.stmts {
            self.visit_statement(stmt);
        }
    }
}

// =============================================================================
// Common visitor implementations
// =============================================================================

/// Collector for function call names.
///
/// Usage:
/// ```ignore
/// let mut collector = FunctionCallCollector::new();
/// collector.visit_expression(&expr);
/// let function_names = collector.into_names();
/// ```
pub struct FunctionCallCollector {
    names: std::collections::HashSet<String>,
}

impl FunctionCallCollector {
    /// Create a new function call collector.
    pub fn new() -> Self {
        Self {
            names: std::collections::HashSet::new(),
        }
    }

    /// Consume the collector and return the collected function names.
    pub fn into_names(self) -> std::collections::HashSet<String> {
        self.names
    }

    /// Get a reference to the collected names.
    pub fn names(&self) -> &std::collections::HashSet<String> {
        &self.names
    }
}

impl Default for FunctionCallCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for FunctionCallCollector {
    fn visit_function_call(&mut self, name: &VarName, args: &[Expression]) {
        self.names.insert(name.to_string());
        self.walk_function_call(name, args);
    }
}

/// Collector for algorithm output variables from statement trees.
pub struct AlgorithmOutputCollector {
    outputs: indexmap::IndexSet<VarName>,
}

impl AlgorithmOutputCollector {
    /// Create a new algorithm output collector.
    pub fn new() -> Self {
        Self {
            outputs: indexmap::IndexSet::new(),
        }
    }

    /// Consume the collector and return collected outputs in first-seen order.
    pub fn into_outputs(self) -> Vec<VarName> {
        self.outputs.into_iter().collect()
    }

    /// Get a reference to collected outputs.
    pub fn outputs(&self) -> &indexmap::IndexSet<VarName> {
        &self.outputs
    }
}

impl Default for AlgorithmOutputCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for AlgorithmOutputCollector {}

impl StatementVisitor for AlgorithmOutputCollector {
    fn visit_assignment(&mut self, comp: &ComponentReference, value: &Expression) {
        self.outputs
            .insert(crate::component_ref_to_base_var_name(comp));
        self.visit_expression(value);
    }

    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        outputs: &[Expression],
    ) {
        self.visit_component_reference(comp);
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs {
            if let Expression::VarRef { name, .. } = output {
                self.outputs.insert(name.clone());
            }
            self.visit_expression(output);
        }
    }

    fn visit_reinit(&mut self, variable: &ComponentReference, value: &Expression) {
        self.outputs
            .insert(crate::component_ref_to_base_var_name(variable));
        self.visit_expression(value);
    }
}

/// Collector for state variables (variables passed to der()).
///
/// Usage:
/// ```ignore
/// let mut collector = StateVariableCollector::new();
/// collector.visit_expression(&expr);
/// let states = collector.into_states();
/// ```
pub struct StateVariableCollector {
    states: std::collections::HashSet<VarName>,
}

impl StateVariableCollector {
    /// Create a new state variable collector.
    pub fn new() -> Self {
        Self {
            states: std::collections::HashSet::new(),
        }
    }

    /// Consume the collector and return the collected state variables.
    pub fn into_states(self) -> std::collections::HashSet<VarName> {
        self.states
    }

    /// Get a reference to the collected states.
    pub fn states(&self) -> &std::collections::HashSet<VarName> {
        &self.states
    }
}

impl Default for StateVariableCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for StateVariableCollector {
    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && let Some(Expression::VarRef { name, .. }) = args.first()
        {
            // Strip trailing array subscripts: der(x[1]) → register "x"
            // since flat variables are keyed by base name (MLS §8.4).
            // Only strip subscripts at the end — mid-path subscripts like
            // rmsvM[1].mean.x are component array indices, not variable subscripts.
            let base = if name.0.ends_with(']') {
                name.0
                    .rfind('[')
                    .map_or_else(|| name.clone(), |i| VarName(name.0[..i].to_string()))
            } else {
                name.clone()
            };
            self.states.insert(base);
        }
        self.walk_builtin_call(function, args);
    }
}

/// Checker for whether an expression contains der() calls.
///
/// This visitor short-circuits as soon as it finds a der() call,
/// avoiding unnecessary traversal of the rest of the expression tree.
///
/// Usage:
/// ```ignore
/// let contains = ContainsDerChecker::check(&expr);
/// ```
pub struct ContainsDerChecker {
    found: bool,
}

impl ContainsDerChecker {
    /// Check if an expression contains any der() calls.
    pub fn check(expr: &Expression) -> bool {
        let mut checker = Self { found: false };
        checker.visit_expression(expr);
        checker.found
    }
}

impl ExpressionVisitor for ContainsDerChecker {
    fn visit_expression(&mut self, expr: &Expression) {
        // Short-circuit if we already found der()
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            self.found = true;
        }
        // Only continue if not found yet
        if !self.found {
            self.walk_builtin_call(function, args);
        }
    }
}

/// Checker for der() calls applied to state variables.
///
/// This visitor determines if an expression contains der(x) where x is a state
/// variable. Unlike ContainsDerChecker, this distinguishes between:
/// - `der(state_var)` → true (ODE equation defining state evolution)
/// - `y = der(input_var)` → false (algebraic equation using derivative)
///
/// Usage:
/// ```ignore
/// let state_vars: HashSet<VarName> = ...;
/// let is_ode = ContainsDerOfStateChecker::check(&expr, &state_vars);
/// ```
pub struct ContainsDerOfStateChecker<'a> {
    state_vars: &'a std::collections::HashSet<VarName>,
    found: bool,
}

impl<'a> ContainsDerOfStateChecker<'a> {
    /// Check if an expression contains der() applied to a state variable.
    pub fn check(expr: &Expression, state_vars: &'a std::collections::HashSet<VarName>) -> bool {
        let mut checker = Self {
            state_vars,
            found: false,
        };
        checker.visit_expression(expr);
        checker.found
    }
}

impl ExpressionVisitor for ContainsDerOfStateChecker<'_> {
    fn visit_expression(&mut self, expr: &Expression) {
        // Short-circuit if we already found der(state)
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && let Some(Expression::VarRef { name, .. }) = args.first()
        {
            // Strip trailing array subscripts to match base name (MLS §8.4)
            let base = if name.0.ends_with(']') {
                name.0
                    .rfind('[')
                    .map_or_else(|| name.clone(), |i| VarName(name.0[..i].to_string()))
            } else {
                name.clone()
            };
            if self.state_vars.contains(&base) {
                self.found = true;
            }
        }
        // Continue searching if not found yet
        if !self.found {
            self.walk_builtin_call(function, args);
        }
    }
}

/// Collector for all variable references in an expression.
///
/// Usage:
/// ```ignore
/// let mut collector = VarRefCollector::new();
/// collector.visit_expression(&expr);
/// let vars = collector.into_vars();
/// ```
pub struct VarRefCollector {
    vars: std::collections::HashSet<VarName>,
}

impl VarRefCollector {
    /// Create a new variable reference collector.
    pub fn new() -> Self {
        Self {
            vars: std::collections::HashSet::new(),
        }
    }

    /// Consume the collector and return the collected variable names.
    pub fn into_vars(self) -> std::collections::HashSet<VarName> {
        self.vars
    }

    /// Get a reference to the collected variables.
    pub fn vars(&self) -> &std::collections::HashSet<VarName> {
        &self.vars
    }
}

impl Default for VarRefCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for VarRefCollector {
    fn visit_var_ref(&mut self, name: &VarName, subscripts: &[Subscript]) {
        self.vars.insert(name.clone());
        self.walk_var_ref(name, subscripts);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name),
            subscripts: vec![],
        }
    }

    fn make_func_call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::FunctionCall {
            name: VarName::new(name),
            args,
            is_constructor: false,
        }
    }

    fn make_der(var_name: &str) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![make_var(var_name)],
        }
    }

    #[test]
    fn test_function_call_collector() {
        // f(g(x), h(y))
        let expr = make_func_call(
            "f",
            vec![
                make_func_call("g", vec![make_var("x")]),
                make_func_call("h", vec![make_var("y")]),
            ],
        );

        let mut collector = FunctionCallCollector::new();
        collector.visit_expression(&expr);
        let names = collector.into_names();

        assert_eq!(names.len(), 3);
        assert!(names.contains("f"));
        assert!(names.contains("g"));
        assert!(names.contains("h"));
    }

    #[test]
    fn test_state_variable_collector() {
        // der(x) + der(y) - z
        let expr = Expression::Binary {
            op: OpBinary::Sub(crate::Token::default()),
            lhs: Box::new(Expression::Binary {
                op: OpBinary::Add(crate::Token::default()),
                lhs: Box::new(make_der("x")),
                rhs: Box::new(make_der("y")),
            }),
            rhs: Box::new(make_var("z")),
        };

        let mut collector = StateVariableCollector::new();
        collector.visit_expression(&expr);
        let states = collector.into_states();

        assert_eq!(states.len(), 2);
        assert!(states.contains(&VarName::new("x")));
        assert!(states.contains(&VarName::new("y")));
        assert!(!states.contains(&VarName::new("z")));
    }

    #[test]
    fn test_contains_der_checker() {
        // Expression with der(): der(x) + y
        let with_der = Expression::Binary {
            op: OpBinary::Add(crate::Token::default()),
            lhs: Box::new(make_der("x")),
            rhs: Box::new(make_var("y")),
        };
        assert!(ContainsDerChecker::check(&with_der));

        // Expression without der(): x + y
        let without_der = Expression::Binary {
            op: OpBinary::Add(crate::Token::default()),
            lhs: Box::new(make_var("x")),
            rhs: Box::new(make_var("y")),
        };
        assert!(!ContainsDerChecker::check(&without_der));

        // Nested der(): f(der(x))
        let nested_der = make_func_call("f", vec![make_der("x")]);
        assert!(ContainsDerChecker::check(&nested_der));
    }

    #[test]
    fn test_var_ref_collector() {
        // x + y * z
        let expr = Expression::Binary {
            op: OpBinary::Add(crate::Token::default()),
            lhs: Box::new(make_var("x")),
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Mul(crate::Token::default()),
                lhs: Box::new(make_var("y")),
                rhs: Box::new(make_var("z")),
            }),
        };

        let mut collector = VarRefCollector::new();
        collector.visit_expression(&expr);
        let vars = collector.into_vars();

        assert_eq!(vars.len(), 3);
        assert!(vars.contains(&VarName::new("x")));
        assert!(vars.contains(&VarName::new("y")));
        assert!(vars.contains(&VarName::new("z")));
    }

    #[test]
    fn test_algorithm_output_collector() {
        let stmts = vec![Statement::For {
            indices: vec![ForIndex {
                ident: "i".to_string(),
                range: Expression::Literal(Literal::Integer(1)),
            }],
            equations: vec![
                Statement::Assignment {
                    comp: ComponentReference {
                        local: false,
                        parts: vec![crate::ComponentRefPart {
                            ident: "x".to_string(),
                            subs: vec![Subscript::Index(1)],
                        }],
                        def_id: None,
                    },
                    value: make_var("u"),
                },
                Statement::Reinit {
                    variable: ComponentReference {
                        local: false,
                        parts: vec![crate::ComponentRefPart {
                            ident: "y".to_string(),
                            subs: vec![],
                        }],
                        def_id: None,
                    },
                    value: make_var("v"),
                },
            ],
        }];

        let mut collector = AlgorithmOutputCollector::new();
        for stmt in &stmts {
            collector.visit_statement(stmt);
        }
        let outputs = collector.into_outputs();

        assert!(outputs.contains(&VarName::new("x")));
        assert!(outputs.contains(&VarName::new("y")));
    }
}
