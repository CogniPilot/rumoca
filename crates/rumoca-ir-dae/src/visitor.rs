//! Visitor traits for traversing DAE expression and statement trees.
//!
//! These visitors centralize traversal logic so analysis passes can focus on
//! semantics instead of hand-written recursion.

use indexmap::IndexSet;
use rumoca_core::{
    BuiltinFunction, ComponentReference, Expression, ExpressionRewriter, ForIndex, Reference,
    Statement, StatementBlock, Subscript, VarName,
};

use rumoca_core::ExpressionVisitor;

/// Visitor for top-level DAE model structure.
///
/// DAE owns this traversal because variable and equation partitions are part
/// of the IR schema. Passes can override only the slots they care about.
pub trait DaeVisitor {
    fn visit_dae(&mut self, dae: &crate::Dae) {
        self.visit_variables(&dae.variables);
        self.visit_equations(&dae.continuous.equations);
        self.visit_equations(&dae.initialization.equations);
        self.visit_equations(&dae.discrete.real_updates);
        self.visit_equations(&dae.discrete.valued_updates);
        self.visit_equations(&dae.conditions.equations);
        self.visit_expressions(&dae.conditions.relations);
        self.visit_expressions(&dae.events.synthetic_root_conditions);
        self.visit_event_actions(&dae.events.event_actions);
        self.visit_expressions(&dae.clocks.constructor_exprs);
        self.visit_expressions(&dae.clocks.triggered_conditions);
    }

    fn visit_variables(&mut self, variables: &crate::DaeVariables) {
        self.visit_variable_partition(crate::DaeVariablePartition::State, &variables.states);
        self.visit_variable_partition(
            crate::DaeVariablePartition::Algebraic,
            &variables.algebraics,
        );
        self.visit_variable_partition(crate::DaeVariablePartition::Input, &variables.inputs);
        self.visit_variable_partition(crate::DaeVariablePartition::Output, &variables.outputs);
        self.visit_variable_partition(
            crate::DaeVariablePartition::Parameter,
            &variables.parameters,
        );
        self.visit_variable_partition(crate::DaeVariablePartition::Constant, &variables.constants);
        self.visit_variable_partition(
            crate::DaeVariablePartition::DiscreteReal,
            &variables.discrete_reals,
        );
        self.visit_variable_partition(
            crate::DaeVariablePartition::DiscreteValued,
            &variables.discrete_valued,
        );
    }

    fn visit_variable_partition(
        &mut self,
        partition: crate::DaeVariablePartition,
        variables: &indexmap::IndexMap<VarName, crate::Variable>,
    ) {
        for (name, variable) in variables {
            self.visit_variable(partition, name, variable);
        }
    }

    fn visit_variable(
        &mut self,
        _partition: crate::DaeVariablePartition,
        _name: &VarName,
        _variable: &crate::Variable,
    ) {
    }

    fn visit_equations(&mut self, equations: &[crate::Equation]) {
        for equation in equations {
            self.visit_equation(equation);
        }
    }

    fn visit_equation(&mut self, equation: &crate::Equation) {
        self.visit_expression(&equation.rhs);
    }

    fn visit_expressions(&mut self, expressions: &[Expression]) {
        for expression in expressions {
            self.visit_expression(expression);
        }
    }

    fn visit_event_actions(&mut self, actions: &[crate::DaeEventAction]) {
        for action in actions {
            self.visit_expression(&action.condition);
        }
    }

    fn visit_expression(&mut self, _expr: &Expression) {}
}

/// Mutable visitor for DAE variable declarations.
pub trait DaeVariableMutVisitor {
    fn visit_variables_mut(&mut self, variables: &mut crate::DaeVariables) {
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::State,
            &mut variables.states,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::Algebraic,
            &mut variables.algebraics,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::Input,
            &mut variables.inputs,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::Output,
            &mut variables.outputs,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::Parameter,
            &mut variables.parameters,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::Constant,
            &mut variables.constants,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::DiscreteReal,
            &mut variables.discrete_reals,
        );
        self.visit_variable_partition_mut(
            crate::DaeVariablePartition::DiscreteValued,
            &mut variables.discrete_valued,
        );
    }

    fn visit_variable_partition_mut(
        &mut self,
        partition: crate::DaeVariablePartition,
        variables: &mut indexmap::IndexMap<VarName, crate::Variable>,
    ) {
        for (name, variable) in variables {
            self.visit_variable_mut(partition, name, variable);
        }
    }

    fn visit_variable_mut(
        &mut self,
        _partition: crate::DaeVariablePartition,
        _name: &VarName,
        _variable: &mut crate::Variable,
    ) {
    }
}

/// Rewrites every expression-bearing DAE partition.
///
/// DAE owns the traversal because the partition list is part of the IR schema.
/// Transformation passes override `rewrite_equation` when they need equation
/// specific behavior, such as converting a substituted LHS into an implicit
/// residual.
pub trait DaeExpressionRewriter: ExpressionRewriter {
    fn rewrite_dae(&mut self, dae: &mut crate::Dae) {
        self.rewrite_equations(&mut dae.continuous.equations);
        self.rewrite_equations(&mut dae.initialization.equations);
        self.rewrite_equations(&mut dae.discrete.real_updates);
        self.rewrite_equations(&mut dae.discrete.valued_updates);
        self.rewrite_equations(&mut dae.conditions.equations);
        self.rewrite_expression_slots(&mut dae.conditions.relations);
        self.rewrite_expression_slots(&mut dae.events.synthetic_root_conditions);
        self.rewrite_event_actions(&mut dae.events.event_actions);
        self.rewrite_expression_slots(&mut dae.clocks.constructor_exprs);
        self.rewrite_expression_slots(&mut dae.clocks.triggered_conditions);
    }

    fn rewrite_equations(&mut self, equations: &mut [crate::Equation]) {
        for equation in equations {
            self.rewrite_equation(equation);
        }
    }

    fn rewrite_equation(&mut self, equation: &mut crate::Equation) {
        equation.rhs = self.rewrite_expression(&equation.rhs);
    }

    fn rewrite_expression_slots(&mut self, expressions: &mut [Expression]) {
        for expression in expressions {
            *expression = self.rewrite_expression(expression);
        }
    }

    fn rewrite_event_actions(&mut self, actions: &mut [crate::DaeEventAction]) {
        for action in actions {
            action.condition = self.rewrite_expression(&action.condition);
        }
    }
}

pub enum StatementScope<'a> {
    ForStatement(&'a [ForIndex]),
}

/// Visitor for DAE statements.
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

/// Collects user-defined state variables referenced by `der(...)`.
pub struct StateVariableCollector {
    states: IndexSet<VarName>,
}

impl StateVariableCollector {
    pub fn new() -> Self {
        Self {
            states: IndexSet::new(),
        }
    }

    pub fn into_states(self) -> IndexSet<VarName> {
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
            self.states.insert(name.var_name().clone());
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

/// Collects all variable references in expression trees.
pub struct VarRefCollector {
    vars: IndexSet<VarName>,
}

impl VarRefCollector {
    pub fn new() -> Self {
        Self {
            vars: IndexSet::new(),
        }
    }

    pub fn into_vars(self) -> IndexSet<VarName> {
        self.vars
    }
}

impl Default for VarRefCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for VarRefCollector {
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.vars.insert(name.var_name().clone());
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
    fn visit_var_ref(&mut self, name: &Reference, subscripts: &[Subscript]) {
        self.refs
            .push((name.var_name().clone(), subscripts.to_vec()));
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
    state_vars: &'a IndexSet<VarName>,
    found: bool,
}

impl<'a> ContainsDerOfStateChecker<'a> {
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
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(&mut self, function: &BuiltinFunction, args: &[Expression]) {
        if *function == BuiltinFunction::Der
            && let Some(Expression::VarRef { name, .. }) = args.first()
            && self.state_vars.contains(name.var_name())
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

    fn visit_function_call(
        &mut self,
        name: &Reference,
        args: &[Expression],
        _is_constructor: bool,
    ) {
        let short = name.last_segment();
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
    outputs: Vec<Reference>,
}

impl AlgorithmOutputCollector {
    pub fn new() -> Self {
        Self {
            outputs: Vec::new(),
        }
    }

    pub fn into_outputs(self) -> Vec<Reference> {
        self.outputs
    }

    fn insert_output(&mut self, output: Reference) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::{ComponentRefPart, Literal, Reference};

    fn var(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::from(name),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn implicit_sample_checker_detects_single_arg_sample_call() {
        let expr = Expression::FunctionCall {
            name: Reference::from("Modelica.sample"),
            args: vec![var("x")],
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        };
        assert!(ImplicitSampleChecker::check(&expr));

        let non_implicit = Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args: vec![var("x"), var("dt")],
            span: rumoca_core::Span::DUMMY,
        };
        assert!(!ImplicitSampleChecker::check(&non_implicit));
    }

    #[test]
    fn var_ref_with_subscripts_collector_recurses_into_subscript_exprs() {
        let expr = Expression::VarRef {
            name: Reference::from("x"),
            subscripts: vec![Subscript::generated_expr(Box::new(var("i")))],
            span: rumoca_core::Span::DUMMY,
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
                    value: var("u"),
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
                    value: var("v"),
                    span: rumoca_core::Span::DUMMY,
                },
            ],
            span: rumoca_core::Span::DUMMY,
        }];

        let mut collector = AlgorithmOutputCollector::new();
        for statement in &statements {
            collector.visit_statement(statement);
        }
        let outputs = collector.into_outputs();

        assert!(outputs.iter().any(|output| output.as_str() == "x"));
        assert!(outputs.iter().any(|output| output.as_str() == "y"));
        assert!(outputs.iter().all(rumoca_core::Reference::has_structure));
    }
}
