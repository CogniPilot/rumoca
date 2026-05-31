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
    BuiltinFunction, ComponentReference, Expression, ForIndex, Statement, StatementBlock,
    Subscript, VarName,
};
use indexmap::IndexSet;

use rumoca_core::{ExpressionVisitor, FallibleExpressionVisitor};

pub enum StatementScope<'a> {
    ForStatement(&'a [ForIndex]),
}

/// Visitor for flat algorithm statements.
pub trait StatementVisitor: ExpressionVisitor {
    fn visit_statement(&mut self, stmt: &Statement) {
        self.walk_statement(stmt);
    }

    fn walk_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => {}
            Statement::Assignment { comp, value, .. } => self.visit_assignment(comp, value),
            Statement::For {
                indices, equations, ..
            } => self.visit_for_statement(indices, equations),
            Statement::While { block, .. } => self.visit_statement_block(block),
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.visit_if_statement(cond_blocks, else_block.as_deref()),
            Statement::When { blocks, .. } => self.visit_when_statement(blocks),
            Statement::FunctionCall {
                comp,
                args,
                outputs,
                ..
            } => self.visit_statement_function_call(comp, args, outputs),
            Statement::Reinit {
                variable, value, ..
            } => self.visit_reinit(variable, value),
            Statement::Assert {
                condition,
                message,
                level,
                ..
            } => self.visit_assert(condition, message, level.as_deref()),
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
        StatementVisitor::enter_scope(self, StatementScope::ForStatement(indices));
        for stmt in statements {
            self.visit_statement(stmt);
        }
        StatementVisitor::exit_scope(self, StatementScope::ForStatement(indices));
    }

    fn enter_scope(&mut self, _scope: StatementScope<'_>) {}

    fn exit_scope(&mut self, _scope: StatementScope<'_>) {}

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
        outputs: &[ComponentReference],
    ) {
        self.visit_component_reference(comp);
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs {
            self.visit_component_reference(output);
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

/// Fallible visitor for flat algorithm statements.
pub trait FallibleStatementVisitor: FallibleExpressionVisitor {
    fn visit_statement(&mut self, stmt: &Statement) -> Result<(), Self::Error> {
        self.walk_statement(stmt)
    }

    fn walk_statement(&mut self, stmt: &Statement) -> Result<(), Self::Error> {
        match stmt {
            Statement::Empty { .. } | Statement::Return { .. } | Statement::Break { .. } => Ok(()),
            Statement::Assignment { comp, value, .. } => self.visit_assignment(comp, value),
            Statement::For {
                indices, equations, ..
            } => self.visit_for_statement(indices, equations),
            Statement::While { block, .. } => self.visit_statement_block(block),
            Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.visit_if_statement(cond_blocks, else_block.as_deref()),
            Statement::When { blocks, .. } => self.visit_when_statement(blocks),
            Statement::FunctionCall {
                comp,
                args,
                outputs,
                ..
            } => self.visit_statement_function_call(comp, args, outputs),
            Statement::Reinit {
                variable, value, ..
            } => self.visit_reinit(variable, value),
            Statement::Assert {
                condition,
                message,
                level,
                ..
            } => self.visit_assert(condition, message, level.as_deref()),
        }
    }

    fn visit_assignment(
        &mut self,
        comp: &ComponentReference,
        value: &Expression,
    ) -> Result<(), Self::Error> {
        self.visit_component_reference(comp)?;
        self.visit_expression(value)
    }

    fn visit_for_statement(
        &mut self,
        indices: &[ForIndex],
        statements: &[Statement],
    ) -> Result<(), Self::Error> {
        for index in indices {
            self.visit_expression(&index.range)?;
        }
        FallibleStatementVisitor::enter_scope(self, StatementScope::ForStatement(indices))?;
        let body_result = (|| {
            for stmt in statements {
                self.visit_statement(stmt)?;
            }
            Ok(())
        })();
        let exit_result =
            FallibleStatementVisitor::exit_scope(self, StatementScope::ForStatement(indices));
        body_result?;
        exit_result
    }

    fn enter_scope(&mut self, _scope: StatementScope<'_>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn exit_scope(&mut self, _scope: StatementScope<'_>) -> Result<(), Self::Error> {
        Ok(())
    }

    fn visit_if_statement(
        &mut self,
        cond_blocks: &[StatementBlock],
        else_block: Option<&[Statement]>,
    ) -> Result<(), Self::Error> {
        for block in cond_blocks {
            self.visit_statement_block(block)?;
        }
        if let Some(else_statements) = else_block {
            for stmt in else_statements {
                self.visit_statement(stmt)?;
            }
        }
        Ok(())
    }

    fn visit_when_statement(&mut self, blocks: &[StatementBlock]) -> Result<(), Self::Error> {
        for block in blocks {
            self.visit_statement_block(block)?;
        }
        Ok(())
    }

    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        outputs: &[ComponentReference],
    ) -> Result<(), Self::Error> {
        self.visit_component_reference(comp)?;
        for arg in args {
            self.visit_expression(arg)?;
        }
        for output in outputs {
            self.visit_component_reference(output)?;
        }
        Ok(())
    }

    fn visit_reinit(
        &mut self,
        variable: &ComponentReference,
        value: &Expression,
    ) -> Result<(), Self::Error> {
        self.visit_component_reference(variable)?;
        self.visit_expression(value)
    }

    fn visit_assert(
        &mut self,
        condition: &Expression,
        message: &Expression,
        level: Option<&Expression>,
    ) -> Result<(), Self::Error> {
        self.visit_expression(condition)?;
        self.visit_expression(message)?;
        if let Some(level_expr) = level {
            self.visit_expression(level_expr)?;
        }
        Ok(())
    }

    fn visit_component_reference(&mut self, comp: &ComponentReference) -> Result<(), Self::Error> {
        for part in &comp.parts {
            for subscript in &part.subs {
                self.visit_subscript(subscript)?;
            }
        }
        Ok(())
    }

    fn visit_statement_block(&mut self, block: &StatementBlock) -> Result<(), Self::Error> {
        self.visit_expression(&block.cond)?;
        for stmt in &block.stmts {
            self.visit_statement(stmt)?;
        }
        Ok(())
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
    names: IndexSet<String>,
}

impl FunctionCallCollector {
    /// Create a new function call collector.
    pub fn new() -> Self {
        Self {
            names: IndexSet::new(),
        }
    }

    /// Consume the collector and return the collected function names in first-seen order.
    pub fn into_names(self) -> IndexSet<String> {
        self.names
    }

    /// Get a reference to the collected names.
    pub fn names(&self) -> &IndexSet<String> {
        &self.names
    }
}

impl Default for FunctionCallCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for FunctionCallCollector {
    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[Expression],
        is_constructor: bool,
    ) {
        self.names.insert(name.to_string());
        self.walk_function_call(name, args, is_constructor);
    }
}

/// Collector for algorithm output variables from statement trees.
pub struct AlgorithmOutputCollector {
    outputs: Vec<rumoca_core::Reference>,
}

impl AlgorithmOutputCollector {
    /// Create a new algorithm output collector.
    pub fn new() -> Self {
        Self {
            outputs: Vec::new(),
        }
    }

    /// Consume the collector and return collected outputs in first-seen order.
    pub fn into_outputs(self) -> Vec<rumoca_core::Reference> {
        self.outputs
    }

    /// Get a reference to collected outputs.
    pub fn outputs(&self) -> &[rumoca_core::Reference] {
        &self.outputs
    }

    fn insert_output(&mut self, output: rumoca_core::Reference) {
        if !self.outputs.contains(&output) {
            self.outputs.push(output);
        }
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
        self.insert_output(rumoca_core::component_ref_to_base_reference(comp));
        self.visit_expression(value);
    }

    fn visit_statement_function_call(
        &mut self,
        comp: &ComponentReference,
        args: &[Expression],
        outputs: &[ComponentReference],
    ) {
        self.visit_component_reference(comp);
        for arg in args {
            self.visit_expression(arg);
        }
        for output in outputs {
            self.insert_output(rumoca_core::component_ref_to_base_reference(output));
            self.visit_component_reference(output);
        }
    }

    fn visit_reinit(&mut self, variable: &ComponentReference, value: &Expression) {
        self.insert_output(rumoca_core::component_ref_to_base_reference(variable));
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
    states: IndexSet<VarName>,
}

impl StateVariableCollector {
    /// Create a new state variable collector.
    pub fn new() -> Self {
        Self {
            states: IndexSet::new(),
        }
    }

    /// Consume the collector and return the collected state variables in first-seen order.
    pub fn into_states(self) -> IndexSet<VarName> {
        self.states
    }

    /// Get a reference to the collected states.
    pub fn states(&self) -> &IndexSet<VarName> {
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
            let base = rumoca_core::strip_trailing_subscript_suffix(name.as_str())
                .map_or_else(|| name.var_name().clone(), VarName::new);
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
/// let state_vars: IndexSet<VarName> = ...;
/// let is_ode = ContainsDerOfStateChecker::check(&expr, &state_vars);
/// ```
pub struct ContainsDerOfStateChecker<'a> {
    state_vars: &'a IndexSet<VarName>,
    found: bool,
}

impl<'a> ContainsDerOfStateChecker<'a> {
    /// Check if an expression contains der() applied to a state variable.
    pub fn check(expr: &Expression, state_vars: &'a IndexSet<VarName>) -> bool {
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
            let base = rumoca_core::strip_trailing_subscript_suffix(name.as_str())
                .map_or_else(|| name.var_name().clone(), VarName::new);
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
    vars: IndexSet<VarName>,
}

impl VarRefCollector {
    /// Create a new variable reference collector.
    pub fn new() -> Self {
        Self {
            vars: IndexSet::new(),
        }
    }

    /// Consume the collector and return the collected variable names in first-seen order.
    pub fn into_vars(self) -> IndexSet<VarName> {
        self.vars
    }

    /// Get a reference to the collected variables.
    pub fn vars(&self) -> &IndexSet<VarName> {
        &self.vars
    }
}

impl Default for VarRefCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for VarRefCollector {
    fn visit_var_ref(&mut self, name: &rumoca_core::Reference, subscripts: &[Subscript]) {
        self.vars.insert(name.var_name().clone());
        self.walk_var_ref(name, subscripts);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Reference;
    use rumoca_core::{ComponentRefPart, Literal, OpBinary};

    fn make_var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::from(name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_func_call(name: &str, args: Vec<Expression>) -> Expression {
        Expression::FunctionCall {
            name: Reference::from(name),
            args,
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn make_der(var_name: &str) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![make_var(var_name)],
            span: rumoca_core::Span::DUMMY,
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
            op: OpBinary::Sub,
            lhs: Box::new(Expression::Binary {
                op: OpBinary::Add,
                lhs: Box::new(make_der("x")),
                rhs: Box::new(make_der("y")),
                span: rumoca_core::Span::DUMMY,
            }),
            rhs: Box::new(make_var("z")),
            span: rumoca_core::Span::DUMMY,
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
            op: OpBinary::Add,
            lhs: Box::new(make_der("x")),
            rhs: Box::new(make_var("y")),
            span: rumoca_core::Span::DUMMY,
        };
        assert!(ContainsDerChecker::check(&with_der));

        // Expression without der(): x + y
        let without_der = Expression::Binary {
            op: OpBinary::Add,
            lhs: Box::new(make_var("x")),
            rhs: Box::new(make_var("y")),
            span: rumoca_core::Span::DUMMY,
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
            op: OpBinary::Add,
            lhs: Box::new(make_var("x")),
            rhs: Box::new(Expression::Binary {
                op: OpBinary::Mul,
                lhs: Box::new(make_var("y")),
                rhs: Box::new(make_var("z")),
                span: rumoca_core::Span::DUMMY,
            }),
            span: rumoca_core::Span::DUMMY,
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
                range: Expression::Literal {
                    value: Literal::Integer(1),
                    span: rumoca_core::Span::DUMMY,
                },
            }],
            equations: vec![
                Statement::Assignment {
                    comp: ComponentReference {
                        local: false,
                        span: rumoca_core::Span::DUMMY,
                        parts: vec![ComponentRefPart {
                            ident: "x".to_string(),
                            span: rumoca_core::Span::DUMMY,
                            subs: vec![Subscript::generated_index(1, rumoca_core::Span::DUMMY)],
                        }],
                        def_id: None,
                    },
                    value: make_var("u"),
                    span: rumoca_core::Span::DUMMY,
                },
                Statement::Reinit {
                    variable: ComponentReference {
                        local: false,
                        span: rumoca_core::Span::DUMMY,
                        parts: vec![ComponentRefPart {
                            ident: "y".to_string(),
                            span: rumoca_core::Span::DUMMY,
                            subs: vec![],
                        }],
                        def_id: None,
                    },
                    value: make_var("v"),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        }];

        let mut collector = AlgorithmOutputCollector::new();
        for stmt in &stmts {
            collector.visit_statement(stmt);
        }
        let outputs = collector.into_outputs();

        assert!(outputs.iter().any(|output| output.as_str() == "x"));
        assert!(outputs.iter().any(|output| output.as_str() == "y"));
        assert!(outputs.iter().all(rumoca_core::Reference::has_structure));
    }
}
