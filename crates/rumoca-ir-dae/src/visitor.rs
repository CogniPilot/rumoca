//! Visitor traits for traversing DAE expression and statement trees.
//!
//! These visitors centralize traversal logic so analysis passes can focus on
//! semantics instead of hand-written recursion.

use crate::{
    BuiltinFunction, ComponentReference, ComprehensionIndex, Expression, ForIndex, Statement,
    StatementBlock, Subscript, VarName,
};
use indexmap::IndexSet;

/// Visitor for DAE expressions.
pub trait ExpressionVisitor {
    fn visit_expression(&mut self, expr: &Expression) {
        self.walk_expression(expr);
    }

    fn walk_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary { op, lhs, rhs } => self.visit_binary(op, lhs, rhs),
            Expression::Unary { rhs, .. } => self.visit_expression(rhs),
            Expression::VarRef { name, subscripts } => self.visit_var_ref(name, subscripts),
            Expression::BuiltinCall { function, args } => self.visit_builtin_call(function, args),
            Expression::FunctionCall {
                name,
                args,
                is_constructor,
            } => self.visit_function_call(name, args, *is_constructor),
            Expression::Literal(lit) => self.visit_literal(lit),
            Expression::If {
                branches,
                else_branch,
            } => self.visit_if(branches, else_branch),
            Expression::Array {
                elements,
                is_matrix,
            } => self.visit_array(elements, *is_matrix),
            Expression::Tuple { elements } => self.visit_tuple(elements),
            Expression::Range { start, step, end } => self.visit_range(start, step.as_deref(), end),
            Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => self.visit_array_comprehension(expr, indices, filter.as_deref()),
            Expression::Index { base, subscripts } => self.visit_index(base, subscripts),
            Expression::FieldAccess { base, field } => self.visit_field_access(base, field),
            Expression::Empty => {}
        }
    }

    fn visit_binary(&mut self, _op: &rumoca_ir_core::OpBinary, lhs: &Expression, rhs: &Expression) {
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }

    fn visit_var_ref(&mut self, _name: &VarName, subscripts: &[Subscript]) {
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_subscript(&mut self, subscript: &Subscript) {
        if let Subscript::Expr(expr) = subscript {
            self.visit_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, _function: &BuiltinFunction, args: &[Expression]) {
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(&mut self, _name: &VarName, args: &[Expression], _is_constructor: bool) {
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_literal(&mut self, _lit: &crate::Literal) {}

    fn visit_if(&mut self, branches: &[(Expression, Expression)], else_branch: &Expression) {
        for (cond, then_expr) in branches {
            self.visit_expression(cond);
            self.visit_expression(then_expr);
        }
        self.visit_expression(else_branch);
    }

    fn visit_array(&mut self, elements: &[Expression], _is_matrix: bool) {
        for elem in elements {
            self.visit_expression(elem);
        }
    }

    fn visit_tuple(&mut self, elements: &[Expression]) {
        for elem in elements {
            self.visit_expression(elem);
        }
    }

    fn visit_range(&mut self, start: &Expression, step: Option<&Expression>, end: &Expression) {
        self.visit_expression(start);
        if let Some(step_expr) = step {
            self.visit_expression(step_expr);
        }
        self.visit_expression(end);
    }

    fn visit_array_comprehension(
        &mut self,
        expr: &Expression,
        indices: &[ComprehensionIndex],
        filter: Option<&Expression>,
    ) {
        self.visit_expression(expr);
        for index in indices {
            self.visit_expression(&index.range);
        }
        if let Some(filter_expr) = filter {
            self.visit_expression(filter_expr);
        }
    }

    fn visit_index(&mut self, base: &Expression, subscripts: &[Subscript]) {
        self.visit_expression(base);
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_field_access(&mut self, base: &Expression, _field: &str) {
        self.visit_expression(base);
    }
}

/// Visitor for DAE statements.
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

/// Collects user-defined state variables referenced by `der(...)`.
pub struct StateVariableCollector {
    states: std::collections::HashSet<VarName>,
}

impl StateVariableCollector {
    pub fn new() -> Self {
        Self {
            states: std::collections::HashSet::new(),
        }
    }

    pub fn into_states(self) -> std::collections::HashSet<VarName> {
        self.states
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
            self.states.insert(name.clone());
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

/// Collects all variable references in expression trees.
pub struct VarRefCollector {
    vars: std::collections::HashSet<VarName>,
}

impl VarRefCollector {
    pub fn new() -> Self {
        Self {
            vars: std::collections::HashSet::new(),
        }
    }

    pub fn into_vars(self) -> std::collections::HashSet<VarName> {
        self.vars
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
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

/// Collects variable references with explicit subscript payloads.
pub struct VarRefWithSubscriptsCollector {
    refs: Vec<(VarName, Vec<Subscript>)>,
}

impl VarRefWithSubscriptsCollector {
    pub fn new() -> Self {
        Self { refs: Vec::new() }
    }

    pub fn into_refs(self) -> Vec<(VarName, Vec<Subscript>)> {
        self.refs
    }
}

impl Default for VarRefWithSubscriptsCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for VarRefWithSubscriptsCollector {
    fn visit_var_ref(&mut self, name: &VarName, subscripts: &[Subscript]) {
        self.refs.push((name.clone(), subscripts.to_vec()));
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

/// Short-circuit checker for any `der(...)` occurrence.
pub struct ContainsDerChecker {
    found: bool,
}

impl ContainsDerChecker {
    pub fn check(expr: &Expression) -> bool {
        let mut checker = Self { found: false };
        checker.visit_expression(expr);
        checker.found
    }
}

impl ExpressionVisitor for ContainsDerChecker {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

/// Short-circuit checker for `der(state_var)` occurrences.
pub struct ContainsDerOfStateChecker<'a> {
    state_vars: &'a std::collections::HashSet<VarName>,
    found: bool,
}

impl<'a> ContainsDerOfStateChecker<'a> {
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
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && let Some(Expression::VarRef { name, .. }) = args.first()
            && self.state_vars.contains(name)
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

/// Short-circuit checker for implicit `sample(expr)` calls (single-arg only).
pub struct ImplicitSampleChecker {
    found: bool,
}

impl ImplicitSampleChecker {
    pub fn check(expr: &Expression) -> bool {
        let mut checker = Self { found: false };
        checker.visit_expression(expr);
        checker.found
    }
}

impl ExpressionVisitor for ImplicitSampleChecker {
    fn visit_expression(&mut self, expr: &Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if matches!(function, BuiltinFunction::Sample) && args.len() == 1 {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(&mut self, name: &VarName, args: &[Expression], _is_constructor: bool) {
        let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
        if short == "sample" && args.len() == 1 {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

/// Collects algorithm outputs from statement trees.
pub struct AlgorithmOutputCollector {
    outputs: IndexSet<VarName>,
}

impl AlgorithmOutputCollector {
    pub fn new() -> Self {
        Self {
            outputs: IndexSet::new(),
        }
    }

    pub fn into_outputs(self) -> Vec<VarName> {
        self.outputs.into_iter().collect()
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
            .insert(crate::types::component_ref_to_base_var_name(comp));
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
            .insert(crate::types::component_ref_to_base_var_name(variable));
        self.visit_expression(value);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: VarName::new(name),
            subscripts: vec![],
        }
    }

    #[test]
    fn implicit_sample_checker_detects_single_arg_sample_call() {
        let expr = Expression::FunctionCall {
            name: VarName::new("Modelica.sample"),
            args: vec![var("x")],
            is_constructor: false,
        };
        assert!(ImplicitSampleChecker::check(&expr));

        let non_implicit = Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("x"), var("dt")],
        };
        assert!(!ImplicitSampleChecker::check(&non_implicit));
    }

    #[test]
    fn var_ref_with_subscripts_collector_recurses_into_subscript_exprs() {
        let expr = Expression::VarRef {
            name: VarName::new("x"),
            subscripts: vec![Subscript::Expr(Box::new(var("i")))],
        };

        let mut collector = VarRefWithSubscriptsCollector::new();
        collector.visit_expression(&expr);
        let refs = collector.into_refs();
        let names: std::collections::HashSet<_> = refs.into_iter().map(|(name, _)| name).collect();

        assert!(names.contains(&VarName::new("x")));
        assert!(names.contains(&VarName::new("i")));
    }

    #[test]
    fn algorithm_output_collector_tracks_nested_assignments() {
        let statements = vec![Statement::For {
            indices: vec![ForIndex {
                ident: "k".to_string(),
                range: Expression::Literal(crate::Literal::Integer(1)),
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
                    value: var("u"),
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
                    value: var("v"),
                },
            ],
        }];

        let mut collector = AlgorithmOutputCollector::new();
        for statement in &statements {
            collector.visit_statement(statement);
        }
        let outputs = collector.into_outputs();

        assert!(outputs.contains(&VarName::new("x")));
        assert!(outputs.contains(&VarName::new("y")));
    }
}
