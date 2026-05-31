//! Canonical DAE condition lowering (MLS Appendix B, B.1d).
//!
//! Builds `relation` + `f_c` from model conditions so solver backends can rely
//! on one canonical root-condition surface.

use rumoca_core::{ExpressionRewriter, ExpressionVisitor, Span};
use rumoca_ir_dae as dae;

const DEFAULT_CONDITION_VAR_NAME: &str = "c";
const GENERATED_CONDITION_VAR_PREFIX: &str = "__rumoca_c";

#[derive(Debug, Clone)]
struct ConditionCandidate {
    expr: rumoca_core::Expression,
    span: Span,
    source: String,
}

pub(crate) fn populate_canonical_conditions(dae_model: &mut dae::Dae) {
    let mut candidates = Vec::new();

    // DAE buckets are the semantic source of truth here. Looking back at flat
    // equations would collect source occurrences instead of the expressions
    // the solver will actually see.
    for eq in &dae_model.continuous.equations {
        collect_if_condition_candidates(
            &eq.rhs,
            eq.span,
            eq.origin.clone(),
            false,
            &mut candidates,
        );
    }

    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
    {
        collect_if_condition_candidates(
            &eq.rhs,
            eq.span,
            eq.origin.clone(),
            false,
            &mut candidates,
        );
    }

    let event_candidates: Vec<&ConditionCandidate> = candidates
        .iter()
        .filter(|candidate| expr_can_vary_during_simulation(&candidate.expr, dae_model))
        .collect();
    dae_model.conditions.relations = event_candidates
        .iter()
        .map(|candidate| candidate.expr.clone())
        .collect();
    let Some(condition_name) = canonical_condition_variable_name(dae_model) else {
        return;
    };
    dae_model.conditions.equations = event_candidates
        .iter()
        .enumerate()
        .map(|(idx, candidate)| build_condition_equation(candidate, idx + 1, &condition_name))
        .collect();
}

pub(crate) fn finalize_canonical_condition_variables(dae_model: &mut dae::Dae) {
    let Some(requested_name) = condition_variable_name_from_fc(dae_model) else {
        return;
    };
    let condition_name = if dae_variable_name_exists(dae_model, &requested_name) {
        let Some(name) = generated_condition_variable_name(dae_model) else {
            return;
        };
        name
    } else {
        requested_name
    };
    rename_condition_equations(dae_model, &condition_name);
    declare_condition_variable(dae_model, &condition_name);
    declare_condition_pre_parameter(dae_model, &condition_name);
    rewrite_continuous_event_conditions(dae_model, &condition_name);
}

fn build_condition_equation(
    candidate: &ConditionCandidate,
    condition_index: usize,
    condition_name: &str,
) -> dae::Equation {
    dae::Equation::explicit(
        rumoca_core::VarName::new(format!("{condition_name}[{condition_index}]")),
        candidate.expr.clone(),
        candidate.span,
        format!("condition equation from {}", candidate.source),
    )
}

fn declare_condition_variable(dae_model: &mut dae::Dae, condition_name: &str) {
    // MLS Appendix B: event-generating relations are represented by Boolean
    // condition variables c(t_e) that are updated only at event instants.
    dae_model.variables.discrete_valued.insert(
        rumoca_core::VarName::new(condition_name),
        dae::Variable {
            name: rumoca_core::VarName::new(condition_name),
            dims: vec![dae_model.conditions.relations.len() as i64],
            start: Some(rumoca_core::Expression::Array {
                elements: vec![
                    rumoca_core::Expression::Literal {
                        value: rumoca_core::Literal::Boolean(false),
                        span: rumoca_core::Span::DUMMY
                    };
                    dae_model.conditions.relations.len()
                ],
                is_matrix: false,
                span: rumoca_core::Span::DUMMY,
            }),
            ..Default::default()
        },
    );
}

fn declare_condition_pre_parameter(dae_model: &mut dae::Dae, condition_name: &str) {
    let condition_key = rumoca_core::VarName::new(condition_name);
    let Some(condition_var) = dae_model.variables.discrete_valued.get(&condition_key) else {
        return;
    };
    let pre_name = rumoca_core::VarName::new(format!("__pre__.{condition_name}"));
    dae_model
        .variables
        .parameters
        .entry(pre_name.clone())
        .or_insert_with(|| dae::Variable {
            name: pre_name,
            component_ref: None,
            source_span: condition_var.source_span,
            dims: condition_var.dims.clone(),
            start: condition_var.start.clone(),
            start_span: condition_var.start_attribute_span(),
            fixed: Some(true),
            min: None,
            min_span: None,
            max: None,
            max_span: None,
            nominal: None,
            nominal_span: None,
            unit: None,
            state_select: rumoca_core::StateSelect::Default,
            description: Some(format!("pre() of {condition_name}")),
            is_tunable: false,
        });
}

fn canonical_condition_variable_name(dae_model: &dae::Dae) -> Option<String> {
    if !dae_variable_name_exists(dae_model, DEFAULT_CONDITION_VAR_NAME) {
        return Some(DEFAULT_CONDITION_VAR_NAME.to_string());
    }
    generated_condition_variable_name(dae_model)
}

fn generated_condition_variable_name(dae_model: &dae::Dae) -> Option<String> {
    // Loop is bounded by u32::MAX. Exhausting all names would require a model
    // with more than 4 billion condition variables, which is impossible in
    // practice (f_c count is bounded by equation count). Returns None rather
    // than panicking so callers can emit a diagnostic instead of crashing.
    for idx in 0..=u32::MAX {
        let name = if idx == 0 {
            GENERATED_CONDITION_VAR_PREFIX.to_string()
        } else {
            format!("{GENERATED_CONDITION_VAR_PREFIX}_{idx}")
        };
        if !dae_variable_name_exists(dae_model, &name) {
            return Some(name);
        }
    }
    None
}

fn rename_condition_equations(dae_model: &mut dae::Dae, condition_name: &str) {
    for (idx, eq) in dae_model.conditions.equations.iter_mut().enumerate() {
        eq.lhs = Some(rumoca_core::VarName::new(format!(
            "{condition_name}[{}]",
            idx.saturating_add(1)
        )));
    }
}

fn dae_variable_name_exists(dae_model: &dae::Dae, name: &str) -> bool {
    let key = rumoca_core::VarName::new(name);
    partition_has_conflicting_name(&dae_model.variables.states, &key)
        || partition_has_conflicting_name(&dae_model.variables.algebraics, &key)
        || partition_has_conflicting_name(&dae_model.variables.outputs, &key)
        || partition_has_conflicting_name(&dae_model.variables.inputs, &key)
        || partition_has_conflicting_name(&dae_model.variables.parameters, &key)
        || partition_has_conflicting_name(&dae_model.variables.constants, &key)
        || partition_has_conflicting_name(&dae_model.variables.discrete_reals, &key)
        || partition_has_conflicting_name(&dae_model.variables.discrete_valued, &key)
}

fn partition_has_conflicting_name(
    partition: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>,
    name: &rumoca_core::VarName,
) -> bool {
    partition
        .keys()
        .any(|candidate| variable_name_conflicts(candidate, name))
}

fn variable_name_conflicts(candidate: &rumoca_core::VarName, name: &rumoca_core::VarName) -> bool {
    if candidate == name {
        return true;
    }
    let prefix = format!("{}.", name.as_str());
    candidate.as_str().starts_with(&prefix)
}

fn condition_variable_name_from_fc(dae_model: &dae::Dae) -> Option<String> {
    let lhs = dae_model.conditions.equations.first()?.lhs.as_ref()?;
    Some(dae::component_base_name(lhs.as_str()).unwrap_or_else(|| lhs.as_str().to_string()))
}

fn insert_condition_candidate(out: &mut Vec<ConditionCandidate>, candidate: ConditionCandidate) {
    // Multiple equations may intentionally share one Modelica relation, e.g. a
    // contact predicate reused for all force components. Intern the relation
    // once so Appendix B condition variables represent semantic relation
    // surfaces rather than source-token occurrences.
    if out.iter().any(|existing| {
        rumoca_core::expressions_semantically_equal(&existing.expr, &candidate.expr)
    }) {
        return;
    }
    out.push(candidate);
}

fn expr_can_vary_during_simulation(expr: &rumoca_core::Expression, dae_model: &dae::Dae) -> bool {
    struct SimulationVarianceChecker<'a> {
        dae_model: &'a dae::Dae,
        varies: bool,
    }

    impl ExpressionVisitor for SimulationVarianceChecker<'_> {
        fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
            if !self.varies {
                self.walk_expression(expr);
            }
        }

        fn visit_var_ref(
            &mut self,
            name: &rumoca_core::Reference,
            subscripts: &[rumoca_core::Subscript],
        ) {
            if name.as_str() == "time"
                || var_ref_can_vary_during_simulation(name.var_name(), self.dae_model)
            {
                self.varies = true;
                return;
            }
            for subscript in subscripts {
                self.visit_subscript(subscript);
            }
        }
    }

    let mut checker = SimulationVarianceChecker {
        dae_model,
        varies: false,
    };
    checker.visit_expression(expr);
    checker.varies
}

fn var_ref_can_vary_during_simulation(name: &rumoca_core::VarName, dae_model: &dae::Dae) -> bool {
    if name.as_str().starts_with("__pre__.") {
        return true;
    }
    if is_time_invariant_var_ref(name, dae_model) {
        return false;
    }
    if is_time_varying_var_ref(name, dae_model) {
        return true;
    }
    !dae_model
        .symbols
        .enum_literal_ordinals
        .contains_key(name.as_str())
}

fn is_time_invariant_var_ref(name: &rumoca_core::VarName, dae_model: &dae::Dae) -> bool {
    dae_model.variables.parameters.contains_key(name)
        || dae_model.variables.constants.contains_key(name)
        || dae::component_base_name(name.as_str()).is_some_and(|base| {
            let base = rumoca_core::VarName::new(base);
            dae_model.variables.parameters.contains_key(&base)
                || dae_model.variables.constants.contains_key(&base)
        })
}

fn is_time_varying_var_ref(name: &rumoca_core::VarName, dae_model: &dae::Dae) -> bool {
    dae_model.variables.states.contains_key(name)
        || dae_model.variables.algebraics.contains_key(name)
        || dae_model.variables.outputs.contains_key(name)
        || dae_model.variables.inputs.contains_key(name)
        || dae_model.variables.discrete_reals.contains_key(name)
        || dae_model.variables.discrete_valued.contains_key(name)
        || dae::component_base_name(name.as_str()).is_some_and(|base| {
            let base = rumoca_core::VarName::new(base);
            dae_model.variables.states.contains_key(&base)
                || dae_model.variables.algebraics.contains_key(&base)
                || dae_model.variables.outputs.contains_key(&base)
                || dae_model.variables.inputs.contains_key(&base)
                || dae_model.variables.discrete_reals.contains_key(&base)
                || dae_model.variables.discrete_valued.contains_key(&base)
        })
}

fn rewrite_continuous_event_conditions(dae_model: &mut dae::Dae, condition_name: &str) {
    if dae_model.conditions.relations.is_empty() {
        return;
    }

    for eq in &mut dae_model.continuous.equations {
        let mut rewriter = ConditionRewriter {
            relations: &dae_model.conditions.relations,
            suppress_events: false,
            condition_name,
        };
        eq.rhs = rewriter.rewrite_expression(&eq.rhs);
    }
}

fn is_event_suppressed_wrapper(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::NoEvent,
            ..
        }
    )
}

fn is_relation_extracting_event_wrapper(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Edge | rumoca_core::BuiltinFunction::Change,
            ..
        }
    )
}

fn is_non_relation_condition(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            ..
        }
    )
}

fn c_var_ref(condition_name: &str, condition_index: usize, span: Span) -> rumoca_core::Expression {
    rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::new(condition_name),
        subscripts: vec![rumoca_core::Subscript::generated_index(
            condition_index as i64,
            span,
        )],
        span,
    }
}

struct ConditionRewriter<'a> {
    relations: &'a [rumoca_core::Expression],
    suppress_events: bool,
    condition_name: &'a str,
}

impl ExpressionRewriter for ConditionRewriter<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        match expr {
            rumoca_core::Expression::Binary { op, span, .. } => {
                if !self.suppress_events
                    && op.is_relational()
                    && let Some(condition_index) = matching_relation_index(expr, self.relations)
                {
                    return c_var_ref(self.condition_name, condition_index, *span);
                }
                self.walk_expression(expr)
            }
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => {
                let suppressed = self.suppress_events
                    || matches!(function, rumoca_core::BuiltinFunction::NoEvent);
                let mut arg_rewriter = ConditionRewriter {
                    relations: self.relations,
                    suppress_events: suppressed,
                    condition_name: self.condition_name,
                };
                rumoca_core::Expression::BuiltinCall {
                    function: *function,
                    args: arg_rewriter.rewrite_expressions(args),
                    span: *span,
                }
            }
            _ => self.walk_expression(expr),
        }
    }
}

fn matching_relation_index(
    expr: &rumoca_core::Expression,
    relations: &[rumoca_core::Expression],
) -> Option<usize> {
    relations
        .iter()
        .position(|relation| rumoca_core::expressions_semantically_equal(relation, expr))
        .map(|idx| idx + 1)
}

fn collect_if_condition_candidates(
    expr: &rumoca_core::Expression,
    span: Span,
    source: String,
    suppress_events: bool,
    out: &mut Vec<ConditionCandidate>,
) {
    ConditionCandidateCollector {
        current_span: span,
        source,
        suppress_events,
        out,
    }
    .visit_expression(expr);
}

struct ConditionCandidateCollector<'a> {
    current_span: Span,
    source: String,
    suppress_events: bool,
    out: &'a mut Vec<ConditionCandidate>,
}

impl ConditionCandidateCollector<'_> {
    fn insert_candidate(&mut self, expr: rumoca_core::Expression) {
        if self.suppress_events {
            return;
        }
        insert_condition_candidate(
            self.out,
            ConditionCandidate {
                expr,
                span: self.current_span,
                source: self.source.clone(),
            },
        );
    }

    fn visit_with_suppression(&mut self, expr: &rumoca_core::Expression, suppress: bool) {
        let previous = self.suppress_events;
        self.suppress_events = suppress;
        self.visit_expression(expr);
        self.suppress_events = previous;
    }
}

impl ExpressionVisitor for ConditionCandidateCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        let previous_span = self.current_span;
        self.current_span = expr.span().unwrap_or(previous_span);
        if let rumoca_core::Expression::Binary { op, .. } = expr
            && op.is_relational()
        {
            self.insert_candidate(expr.clone());
        }
        self.walk_expression(expr);
        self.current_span = previous_span;
    }

    fn visit_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
    ) {
        for (condition, value) in branches {
            let cond_suppressed = self.suppress_events || is_event_suppressed_wrapper(condition);
            // MLS Appendix B B.1d: canonical conditions live on relation(v), so
            // event combinators like edge/change contribute their underlying
            // relational guard via the builtin walk below, not as wrapper roots.
            if !cond_suppressed
                && !is_relation_extracting_event_wrapper(condition)
                && !is_non_relation_condition(condition)
            {
                self.insert_candidate(condition.clone());
            }
            self.visit_with_suppression(condition, cond_suppressed);
            self.visit_expression(value);
        }
        self.visit_expression(else_branch);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        let suppressed =
            self.suppress_events || matches!(function, rumoca_core::BuiltinFunction::NoEvent);
        if !suppressed
            && matches!(
                function,
                rumoca_core::BuiltinFunction::Edge | rumoca_core::BuiltinFunction::Change
            )
            && let Some(arg) = args.first()
            && arg.contains_relational_operator()
        {
            self.insert_candidate(arg.clone());
        }
        for arg in args {
            self.visit_with_suppression(arg, suppressed);
        }
    }
}
