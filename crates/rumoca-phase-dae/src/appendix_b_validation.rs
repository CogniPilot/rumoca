//! Appendix B invariant checks on solver-facing DAE.

use std::collections::{HashMap, HashSet};

use rumoca_core::{ExpressionVisitor, FallibleExpressionVisitor};
use rumoca_ir_dae as dae;

use crate::{
    ToDaeError, dae_to_flat_var_name, flat_to_dae_var_name, path_utils::subscript_fallback_chain,
};

pub(super) fn validate_appendix_b_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    validate_runtime_contract_invariants(dae_model)?;
    validate_condition_partition(dae_model)?;
    validate_discrete_real_solved_form(dae_model)?;
    validate_discrete_valued_solved_form(dae_model)?;
    validate_strict_solver_expression_invariants(dae_model)?;
    validate_runtime_metadata_invariants(dae_model)?;
    validate_no_source_temporal_operator_survives(dae_model)?;
    validate_no_flow_action_expression_survives(dae_model)?;
    Ok(())
}

/// SPEC_0007 Stage 3 Contract: source temporal operators are lowered into
/// Appendix B variables, condition relations, schedules, and ordinary equations
/// before solver-facing DAE leaves `phase-dae`.
fn validate_no_source_temporal_operator_survives(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    for (context, expr, fallback_span) in solver_facing_dae_expressions(dae_model) {
        if let Some(found) = find_source_temporal_operator(expr) {
            return Err(ToDaeError::source_temporal_operator_survived_dae_boundary(
                format!(
                    "`{}` survived into {context}. SPEC_0007 Stage 3 Contract: \
                     DAE temporal lowering must convert source temporal \
                     operators before the DAE stage exits.",
                    found.operator,
                ),
                found.span.unwrap_or(fallback_span),
            ));
        }
    }
    Ok(())
}

/// SPEC_0007 Stage 3 Contract: runtime flow actions are not expression graph
/// calls. `reinit` is lowered into guarded discrete updates; `assert` and
/// `terminate` remain event metadata. Guard/value expressions remain pure.
fn validate_no_flow_action_expression_survives(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    for (context, expr, span) in solver_facing_dae_expressions(dae_model) {
        if let Some(found) = find_flow_action_expression(expr) {
            return Err(ToDaeError::strict_solver_dae_violation(
                format!(
                    "`{}` survived into {context}. SPEC_0007 Stage 3 Contract: \
                     reinit must lower to guarded discrete updates, while \
                     assert and terminate must be represented as DAE event \
                     actions, not value-expression calls.",
                    found.action,
                ),
                found.span.unwrap_or(span),
            ));
        }
    }
    Ok(())
}

fn solver_facing_dae_expressions(
    dae_model: &dae::Dae,
) -> Vec<(String, &rumoca_core::Expression, rumoca_core::Span)> {
    let mut exprs = Vec::new();
    push_partition_exprs(&mut exprs, "f_x", &dae_model.continuous.equations);
    push_partition_exprs(
        &mut exprs,
        "initial_equations",
        &dae_model.initialization.equations,
    );
    push_partition_exprs(&mut exprs, "f_z", &dae_model.discrete.real_updates);
    push_partition_exprs(&mut exprs, "f_m", &dae_model.discrete.valued_updates);
    push_partition_exprs(&mut exprs, "f_c", &dae_model.conditions.equations);
    exprs.extend(
        dae_model
            .conditions
            .relations
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                (
                    format!("relation[{idx}]"),
                    expr,
                    expr.span().unwrap_or(rumoca_core::Span::DUMMY),
                )
            }),
    );
    for (idx, action) in dae_model.events.event_actions.iter().enumerate() {
        exprs.push((
            format!("event_actions[{idx}].condition"),
            &action.condition,
            action.span,
        ));
    }
    exprs.extend(
        dae_model
            .clocks
            .triggered_conditions
            .iter()
            .enumerate()
            .map(|(idx, expr)| {
                (
                    format!("clocks.triggered_conditions[{idx}]"),
                    expr,
                    expr.span().unwrap_or(rumoca_core::Span::DUMMY),
                )
            }),
    );
    exprs
}

fn push_partition_exprs<'a>(
    exprs: &mut Vec<(String, &'a rumoca_core::Expression, rumoca_core::Span)>,
    partition: &str,
    equations: &'a [dae::Equation],
) {
    exprs.extend(equations.iter().enumerate().map(|(idx, equation)| {
        (
            format!("{partition}[{idx}] (origin='{}')", equation.origin),
            &equation.rhs,
            equation.span,
        )
    }));
}

fn find_flow_action_expression(
    expr: &rumoca_core::Expression,
) -> Option<FlowActionExpressionOccurrence> {
    let mut checker = FlowActionExpressionChecker { found: None };
    checker.visit_expression(expr);
    checker.found
}

#[derive(Clone, Copy, Debug)]
struct FlowActionExpressionOccurrence {
    action: &'static str,
    span: Option<rumoca_core::Span>,
}

struct FlowActionExpressionChecker {
    found: Option<FlowActionExpressionOccurrence>,
}

impl ExpressionVisitor for FlowActionExpressionChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found.is_some() {
            return;
        }
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Reinit,
                span,
                ..
            } => {
                self.found = Some(FlowActionExpressionOccurrence {
                    action: "reinit",
                    span: Some(*span),
                });
                return;
            }
            rumoca_core::Expression::FunctionCall { name, span, .. } => {
                if let Some(action) =
                    rumoca_core::runtime_flow_action_function_short_name(name.as_str())
                {
                    self.found = Some(FlowActionExpressionOccurrence {
                        action,
                        span: Some(*span),
                    });
                    return;
                }
            }
            _ => {}
        }
        self.walk_expression(expr);
    }
}

fn find_source_temporal_operator(
    expr: &rumoca_core::Expression,
) -> Option<SourceTemporalOperatorOccurrence> {
    let mut checker = SourceTemporalOperatorChecker { found: None };
    checker.visit_expression(expr);
    checker.found
}

#[derive(Clone, Copy, Debug)]
struct SourceTemporalOperatorOccurrence {
    operator: &'static str,
    span: Option<rumoca_core::Span>,
}

struct SourceTemporalOperatorChecker {
    found: Option<SourceTemporalOperatorOccurrence>,
}

impl ExpressionVisitor for SourceTemporalOperatorChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found.is_some() {
            return;
        }
        match expr {
            rumoca_core::Expression::BuiltinCall { function, span, .. } => {
                if let Some(operator) = rumoca_core::source_temporal_builtin_name(*function) {
                    self.found = Some(SourceTemporalOperatorOccurrence {
                        operator,
                        span: Some(*span),
                    });
                    return;
                }
            }
            rumoca_core::Expression::FunctionCall { name, span, .. } => {
                if let Some(operator) =
                    rumoca_core::source_temporal_function_short_name(name.as_str())
                {
                    self.found = Some(SourceTemporalOperatorOccurrence {
                        operator,
                        span: Some(*span),
                    });
                    return;
                }
            }
            _ => {}
        }
        self.walk_expression(expr);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if let Some(operator) = rumoca_core::source_temporal_builtin_name(*function) {
            self.found = Some(SourceTemporalOperatorOccurrence {
                operator,
                span: None,
            });
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        if let Some(operator) = rumoca_core::source_temporal_function_short_name(name.as_str()) {
            self.found = Some(SourceTemporalOperatorOccurrence {
                operator,
                span: None,
            });
            return;
        }
        self.walk_function_call(name, args, is_constructor);
    }
}

fn validate_strict_solver_expression_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    for (index, equation) in dae_model.continuous.equations.iter().enumerate() {
        validate_solver_expression(
            &equation.rhs,
            &format!("f_x[{index}] (origin='{}')", equation.origin),
            equation.span,
        )?;
    }

    for (index, equation) in dae_model.initialization.equations.iter().enumerate() {
        validate_solver_expression(
            &equation.rhs,
            &format!("initial_equations[{index}] (origin='{}')", equation.origin),
            equation.span,
        )?;
    }

    Ok(())
}

fn validate_runtime_contract_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    let mut seen_partition_for_name: HashMap<&rumoca_core::VarName, &'static str> = HashMap::new();
    register_partition_names(
        &mut seen_partition_for_name,
        "x",
        dae_model.variables.states.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "y",
        dae_model.variables.algebraics.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "u",
        dae_model.variables.inputs.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "w",
        dae_model.variables.outputs.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "p",
        dae_model.variables.parameters.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "constants",
        dae_model.variables.constants.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "z",
        dae_model.variables.discrete_reals.keys(),
    )?;
    register_partition_names(
        &mut seen_partition_for_name,
        "m",
        dae_model.variables.discrete_valued.keys(),
    )?;
    Ok(())
}

fn register_partition_names<'a, I>(
    seen_partition_for_name: &mut HashMap<&'a rumoca_core::VarName, &'static str>,
    partition: &'static str,
    names: I,
) -> Result<(), ToDaeError>
where
    I: Iterator<Item = &'a rumoca_core::VarName>,
{
    for name in names {
        if let Some(previous_partition) = seen_partition_for_name.insert(name, partition) {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "variable `{name}` appears in multiple partitions: `{previous_partition}` and `{partition}`"
            )));
        }
    }
    Ok(())
}

fn validate_solver_expression(
    expr: &rumoca_core::Expression,
    context: &str,
    span: rumoca_core::Span,
) -> Result<(), ToDaeError> {
    let mut validator = StrictSolverExpressionValidator {
        context,
        fallback_span: span,
    };
    FallibleExpressionVisitor::visit_expression(&mut validator, expr)
}

struct StrictSolverExpressionValidator<'a> {
    context: &'a str,
    fallback_span: rumoca_core::Span,
}

impl FallibleExpressionVisitor for StrictSolverExpressionValidator<'_> {
    type Error = ToDaeError;

    fn visit_expression(&mut self, expr: &rumoca_core::Expression) -> Result<(), Self::Error> {
        if let Some(construct) = forbidden_synchronous_construct(expr) {
            return Err(ToDaeError::strict_solver_dae_violation(
                format!(
                    "{} contains unlowered synchronous construct `{construct}`",
                    self.context
                ),
                expr.span().unwrap_or(self.fallback_span),
            ));
        }
        self.walk_expression(expr)
    }
}

fn forbidden_synchronous_construct(expr: &rumoca_core::Expression) -> Option<&'static str> {
    match expr {
        rumoca_core::Expression::BuiltinCall { function, .. } => {
            if *function == rumoca_core::BuiltinFunction::Sample {
                return Some("sample");
            }
            None
        }
        rumoca_core::Expression::FunctionCall { name, .. } => {
            let short = name.last_segment();
            forbidden_sync_name(short)
        }
        _ => None,
    }
}

fn forbidden_sync_name(name: &str) -> Option<&'static str> {
    match name {
        "sample" => Some("sample"),
        "hold" => Some("hold"),
        "Clock" => Some("Clock"),
        "subSample" => Some("subSample"),
        "superSample" => Some("superSample"),
        "shiftSample" => Some("shiftSample"),
        "backSample" => Some("backSample"),
        "noClock" => Some("noClock"),
        "firstTick" => Some("firstTick"),
        "previous" => Some("previous"),
        _ => None,
    }
}

fn validate_condition_partition(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    if dae_model.conditions.equations.len() != dae_model.conditions.relations.len() {
        return Err(ToDaeError::condition_partition_violation(format!(
            "f_c count ({}) does not match relation count ({})",
            dae_model.conditions.equations.len(),
            dae_model.conditions.relations.len()
        )));
    }
    let condition_name = condition_partition_name(dae_model);

    for (index, (equation, relation)) in dae_model
        .conditions
        .equations
        .iter()
        .zip(dae_model.conditions.relations.iter())
        .enumerate()
    {
        let expected_lhs = rumoca_core::VarName::new(format!("{}[{}]", condition_name, index + 1));
        if equation.lhs.as_ref() != Some(&expected_lhs) {
            return Err(ToDaeError::condition_partition_violation(format!(
                "f_c[{index}] must define `{expected_lhs}` but found lhs={:?}",
                equation.lhs
            )));
        }

        if equation.rhs != *relation {
            return Err(ToDaeError::condition_partition_violation(format!(
                "f_c[{index}] rhs does not match relation[{index}]"
            )));
        }
    }

    Ok(())
}

fn condition_partition_name(dae_model: &dae::Dae) -> String {
    dae_model
        .conditions
        .equations
        .first()
        .and_then(|equation| equation.lhs.as_ref())
        .map(|lhs| dae::component_base_name(lhs.as_str()).unwrap_or_else(|| lhs.as_str().into()))
        .unwrap_or_else(|| "c".to_string())
}

fn validate_discrete_valued_solved_form(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    let mut assignments: HashMap<rumoca_core::VarName, &dae::Equation> = HashMap::new();
    for equation in &dae_model.discrete.valued_updates {
        let lhs = equation.lhs.as_ref().ok_or_else(|| {
            ToDaeError::discrete_solved_form_violation(
                format!(
                    "f_m equation is not in explicit assignment form (origin='{}', rhs={:?})",
                    equation.origin, equation.rhs
                ),
                equation.span,
            )
        })?;

        if !is_discrete_valued_name(dae_model, lhs) {
            return Err(ToDaeError::discrete_solved_form_violation(
                format!("f_m lhs `{lhs}` is not a discrete-valued variable"),
                equation.span,
            ));
        }

        if let Some(previous) = assignments.insert(lhs.clone(), equation) {
            return Err(ToDaeError::discrete_solved_form_violation(
                format!(
                    "duplicate f_m assignment target `{lhs}` (new origin='{}', prior origin='{}', new rhs={:?}, prior rhs={:?})",
                    equation.origin, previous.origin, equation.rhs, previous.rhs
                ),
                equation.span,
            ));
        }
    }

    let targets: HashSet<rumoca_core::VarName> = assignments.keys().cloned().collect();
    let mut deps: HashMap<rumoca_core::VarName, Vec<rumoca_core::VarName>> = HashMap::new();

    for (lhs, equation) in &assignments {
        let mut refs = HashSet::new();
        collect_current_var_refs_for_assignment(&equation.rhs, lhs, &mut refs);

        let mut lhs_deps = Vec::new();
        for var_ref in refs {
            let Some(dep_target) = resolve_assignment_target(&var_ref, &targets) else {
                continue;
            };
            if dep_target == *lhs {
                return Err(ToDaeError::discrete_solved_form_violation(
                    format!("f_m assignment `{lhs}` has direct self-dependency"),
                    equation.span,
                ));
            }
            lhs_deps.push(dep_target);
        }

        lhs_deps.sort_unstable_by(|lhs, rhs| lhs.as_str().cmp(rhs.as_str()));
        lhs_deps.dedup();
        deps.insert(lhs.clone(), lhs_deps);
    }

    if let Some(cycle) = find_cycle(&deps) {
        let cycle_str = cycle
            .iter()
            .map(rumoca_core::VarName::as_str)
            .collect::<Vec<_>>()
            .join(" -> ");
        let span = assignments
            .get(&cycle[0])
            .map(|equation| equation.span)
            .unwrap_or(rumoca_core::Span::DUMMY);
        return Err(ToDaeError::discrete_solved_form_violation(
            format!("f_m assignments have cyclic dependency: {cycle_str}"),
            span,
        ));
    }

    Ok(())
}

fn validate_discrete_real_solved_form(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    let mut assignments: HashMap<rumoca_core::VarName, &dae::Equation> = HashMap::new();
    for equation in &dae_model.discrete.real_updates {
        // f_z may contain residual-style lowered equations in current ToDae
        // output. Enforce partition integrity when an explicit target exists.
        let Some(lhs) = equation.lhs.as_ref() else {
            continue;
        };

        if !is_discrete_real_or_state_name(dae_model, lhs) {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "f_z lhs `{lhs}` is not an event-updated Real target (state or discrete Real)"
            )));
        }

        if let Some(previous) = assignments.insert(lhs.clone(), equation) {
            return Err(ToDaeError::runtime_contract_violation(format!(
                "duplicate f_z assignment target `{lhs}` (new origin='{}', prior origin='{}')",
                equation.origin, previous.origin
            )));
        }
    }
    Ok(())
}

fn is_discrete_valued_name(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    dae_model.variables.discrete_valued.contains_key(name)
        || subscript_fallback_chain(dae_to_flat_var_name(name).as_str())
            .into_iter()
            .any(|candidate| {
                dae_model
                    .variables
                    .discrete_valued
                    .contains_key(&flat_to_dae_var_name(&candidate))
            })
}

fn is_discrete_real_or_state_name(dae_model: &dae::Dae, name: &rumoca_core::VarName) -> bool {
    dae_model.variables.discrete_reals.contains_key(name)
        || dae_model.variables.states.contains_key(name)
        || subscript_fallback_chain(dae_to_flat_var_name(name).as_str())
            .into_iter()
            .any(|candidate| {
                let candidate = flat_to_dae_var_name(&candidate);
                dae_model.variables.discrete_reals.contains_key(&candidate)
                    || dae_model.variables.states.contains_key(&candidate)
            })
}

fn resolve_assignment_target(
    name: &rumoca_core::VarName,
    targets: &HashSet<rumoca_core::VarName>,
) -> Option<rumoca_core::VarName> {
    if targets.contains(name) {
        return Some(name.clone());
    }
    subscript_fallback_chain(dae_to_flat_var_name(name).as_str())
        .into_iter()
        .map(|candidate| flat_to_dae_var_name(&candidate))
        .find(|candidate| targets.contains(candidate))
}

fn collect_current_var_refs_for_assignment(
    expr: &rumoca_core::Expression,
    target: &rumoca_core::VarName,
    out: &mut HashSet<rumoca_core::VarName>,
) {
    CurrentVarRefCollector {
        target,
        allow_current_target: false,
        out,
    }
    .visit_expression(expr);
}

struct CurrentVarRefCollector<'a, 'out> {
    target: &'a rumoca_core::VarName,
    allow_current_target: bool,
    out: &'out mut HashSet<rumoca_core::VarName>,
}

impl CurrentVarRefCollector<'_, '_> {
    fn visit_with_current_target_allowed(
        &mut self,
        expr: &rumoca_core::Expression,
        allow_current_target: bool,
    ) {
        let previous = self.allow_current_target;
        self.allow_current_target = allow_current_target;
        self.visit_expression(expr);
        self.allow_current_target = previous;
    }
}

impl ExpressionVisitor for CurrentVarRefCollector<'_, '_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if !(self.allow_current_target && name.var_name() == self.target) {
            self.out.insert(name.var_name().clone());
        }
        self.walk_var_ref(name, subscripts);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if matches!(function, rumoca_core::BuiltinFunction::Pre) {
            return;
        }
        self.walk_builtin_call(function, args);
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
    ) {
        let short_name = name.last_segment();
        if function_call_uses_left_limit_value(short_name) {
            return;
        }
        self.walk_function_call(name, args, is_constructor);
    }

    fn visit_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
    ) {
        for (cond, value) in branches {
            self.visit_expression(cond);
            self.visit_with_current_target_allowed(value, is_initial_builtin_call(cond));
        }
        self.visit_expression(else_branch);
    }
}

fn function_call_uses_left_limit_value(name: &str) -> bool {
    matches!(
        name,
        // MLS §16.5.1: previous(..) and hold(..) read stored clocked values
        // from the associated clock history instead of creating a same-tick
        // current-value dependency in the discrete solved form.
        "previous" | "hold"
    )
}

fn is_initial_builtin_call(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Initial,
            args, ..
        } if args.is_empty()
    )
}

fn find_cycle(
    deps: &HashMap<rumoca_core::VarName, Vec<rumoca_core::VarName>>,
) -> Option<Vec<rumoca_core::VarName>> {
    let mut visiting = HashSet::new();
    let mut visited = HashSet::new();
    let mut stack = Vec::new();

    for target in deps.keys() {
        if let Some(cycle) = dfs_cycle(target, deps, &mut visiting, &mut visited, &mut stack) {
            return Some(cycle);
        }
    }

    None
}

fn dfs_cycle(
    node: &rumoca_core::VarName,
    deps: &HashMap<rumoca_core::VarName, Vec<rumoca_core::VarName>>,
    visiting: &mut HashSet<rumoca_core::VarName>,
    visited: &mut HashSet<rumoca_core::VarName>,
    stack: &mut Vec<rumoca_core::VarName>,
) -> Option<Vec<rumoca_core::VarName>> {
    if visited.contains(node) {
        return None;
    }
    if let Some(pos) = stack.iter().position(|entry| entry == node) {
        let mut cycle = stack[pos..].to_vec();
        cycle.push(node.clone());
        return Some(cycle);
    }
    if !visiting.insert(node.clone()) {
        return None;
    }

    stack.push(node.clone());
    if let Some(children) = deps.get(node) {
        for child in children {
            if let Some(cycle) = dfs_cycle(child, deps, visiting, visited, stack) {
                return Some(cycle);
            }
        }
    }
    stack.pop();
    visiting.remove(node);
    visited.insert(node.clone());
    None
}

fn validate_runtime_metadata_invariants(dae_model: &dae::Dae) -> Result<(), ToDaeError> {
    for schedule in &dae_model.clocks.schedules {
        if !schedule.period_seconds.is_finite() || schedule.period_seconds <= 0.0 {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "clock period must be finite and positive, got {}",
                schedule.period_seconds
            )));
        }
        if !schedule.phase_seconds.is_finite() {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "clock phase must be finite, got {}",
                schedule.phase_seconds
            )));
        }
    }
    for (name, interval) in &dae_model.clocks.intervals {
        if !interval.is_finite() || *interval <= 0.0 {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "clock interval for `{name}` must be finite and positive, got {interval}",
            )));
        }
        let key = rumoca_core::VarName::new(name);
        if !dae_model.variables.discrete_reals.contains_key(&key)
            && !dae_model.variables.discrete_valued.contains_key(&key)
            && !dae_model.variables.algebraics.contains_key(&key)
            && !dae_model.variables.outputs.contains_key(&key)
            && !dae_model.variables.inputs.contains_key(&key)
            && !dae_model.variables.states.contains_key(&key)
        {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "clock interval key `{name}` must reference a runtime variable in DAE",
            )));
        }
    }

    for pair in dae_model.events.scheduled_time_events.windows(2) {
        if pair[1] <= pair[0] {
            return Err(ToDaeError::runtime_metadata_violation(format!(
                "scheduled_time_events must be strictly increasing; got [{}, {}]",
                pair[0], pair[1]
            )));
        }
    }

    let mut seen_roots: Vec<&rumoca_core::Expression> = Vec::new();
    for root in &dae_model.events.synthetic_root_conditions {
        if seen_roots.contains(&root) {
            return Err(ToDaeError::runtime_metadata_violation(
                "synthetic_root_conditions contain duplicates",
            ));
        }
        seen_roots.push(root);
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn bool_var(name: &str) -> dae::Variable {
        dae::Variable {
            name: rumoca_core::VarName::new(name),
            ..Default::default()
        }
    }

    fn var_ref(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(name).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn pre_call(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::BuiltinCall {
            function: rumoca_core::BuiltinFunction::Pre,
            args: vec![var_ref(name)],
            span: rumoca_core::Span::DUMMY,
        }
    }

    /// Construct a reference to the `__pre__.*` parameter slot for `name`.
    ///
    /// SPEC_0007 Stage 3 Contract: `pre()` is eliminated at the DAE boundary,
    /// so test DAEs that exercise event-time previous-value semantics must
    /// reference the lowered `__pre__.*` parameter, not a `BuiltinCall { Pre }`.
    fn pre_var(name: &str) -> rumoca_core::Expression {
        rumoca_core::Expression::VarRef {
            name: rumoca_core::VarName::new(format!("__pre__.{}", name)).into(),
            subscripts: vec![],
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn lit(value: f64) -> rumoca_core::Expression {
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn call(name: &str, args: Vec<rumoca_core::Expression>) -> rumoca_core::Expression {
        rumoca_core::Expression::FunctionCall {
            name: rumoca_core::VarName::new(name).into(),
            args,
            is_constructor: false,
            span: rumoca_core::Span::DUMMY,
        }
    }

    fn sub(lhs: rumoca_core::Expression, rhs: rumoca_core::Expression) -> rumoca_core::Expression {
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: rumoca_core::Span::DUMMY,
        }
    }

    #[test]
    fn condition_partition_mismatch_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model.conditions.relations.push(var_ref("u"));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("missing f_c entry must fail condition partition validation");
        assert!(matches!(
            err,
            ToDaeError::ConditionPartitionViolation { .. }
        ));
    }

    #[test]
    fn runtime_contract_rejects_partition_overlap() {
        let mut dae_model = dae::Dae::default();
        let shared = rumoca_core::VarName::new("x");
        dae_model
            .variables
            .states
            .insert(shared.clone(), bool_var("x"));
        dae_model.variables.algebraics.insert(shared, bool_var("x"));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("overlapping partition variable must fail runtime contract validation");
        assert!(matches!(err, ToDaeError::RuntimeContractViolation { .. }));
    }

    #[test]
    fn runtime_contract_rejects_non_assignment_fz_equation() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .algebraics
            .insert(rumoca_core::VarName::new("a"), bool_var("a"));
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("a"),
                lit(1.0),
                rumoca_core::Span::DUMMY,
                "test explicit f_z non-discrete-real target",
            ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("non-discrete-real f_z target must fail runtime contract validation");
        assert!(matches!(err, ToDaeError::RuntimeContractViolation { .. }));
    }

    #[test]
    fn fm_requires_explicit_assignments() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("m"), bool_var("m"));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::residual(
                sub(var_ref("m"), pre_var("m")),
                rumoca_core::Span::DUMMY,
                "test residual f_m",
            ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("residual f_m equation must fail solved-form validation");
        assert!(matches!(
            err,
            ToDaeError::DiscreteSolvedFormViolation { .. }
        ));
    }

    #[test]
    fn fm_cycle_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), bool_var("a"));
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("b"), bool_var("b"));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("a"),
                var_ref("b"),
                rumoca_core::Span::DUMMY,
                "a = b",
            ));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("b"),
                var_ref("a"),
                rumoca_core::Span::DUMMY,
                "b = a",
            ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("cyclic f_m dependencies must fail validation");
        assert!(matches!(
            err,
            ToDaeError::DiscreteSolvedFormViolation { .. }
        ));
    }

    #[test]
    fn fm_acyclic_with_pre_is_accepted() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), bool_var("a"));
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("b"), bool_var("b"));
        dae_model.conditions.relations.push(var_ref("a"));
        dae_model.conditions.equations.push(dae::Equation::explicit(
            rumoca_core::VarName::new("c[1]"),
            var_ref("a"),
            rumoca_core::Span::DUMMY,
            "condition",
        ));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("a"),
                pre_var("a"),
                rumoca_core::Span::DUMMY,
                "a = pre(a)",
            ));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("b"),
                var_ref("a"),
                rumoca_core::Span::DUMMY,
                "b = a",
            ));

        validate_appendix_b_invariants(&dae_model)
            .expect("acyclic f_m with pre() dependency should pass validation");
    }

    #[test]
    fn fm_cycle_through_hold_is_accepted() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), bool_var("a"));
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("b"), bool_var("b"));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("a"),
                call("hold", vec![var_ref("b")]),
                rumoca_core::Span::DUMMY,
                "a = hold(b)",
            ));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("b"),
                var_ref("a"),
                rumoca_core::Span::DUMMY,
                "b = a",
            ));

        validate_appendix_b_invariants(&dae_model).expect(
            "hold(..) should break current-value cycle detection in f_m solved-form validation",
        );
    }

    #[test]
    fn fm_initial_branch_current_self_ref_is_accepted() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("a"), bool_var("a"));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("a"),
                rumoca_core::Expression::If {
                    branches: vec![(
                        rumoca_core::Expression::BuiltinCall {
                            function: rumoca_core::BuiltinFunction::Initial,
                            args: vec![],
                            span: rumoca_core::Span::DUMMY,
                        },
                        var_ref("a"),
                    )],
                    else_branch: Box::new(pre_var("a")),
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Span::DUMMY,
                "a = if initial() then a else pre(a)",
            ));

        validate_appendix_b_invariants(&dae_model)
            .expect("MLS §8.6 initial-branch self reference should pass validation");
    }

    #[test]
    fn strict_solver_dae_rejects_sample_in_fx() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.equations.push(dae::Equation::residual(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Sample,
                args: vec![lit(0.0), lit(0.1)],
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            "sample in f_x",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("sample() in f_x must fail strict solver DAE validation");
        assert!(matches!(err, ToDaeError::StrictSolverDaeViolation { .. }));
    }

    #[test]
    fn strict_solver_dae_rejects_clock_in_initial_equation() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .initialization
            .equations
            .push(dae::Equation::residual(
                call("Clock", vec![lit(0.1)]),
                rumoca_core::Span::DUMMY,
                "clock in initial equation",
            ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("Clock() in initial equation must fail strict solver DAE validation");
        assert!(matches!(err, ToDaeError::StrictSolverDaeViolation { .. }));
    }

    #[test]
    fn strict_solver_dae_allows_clock_constructor_metadata() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .clocks
            .constructor_exprs
            .push(call("Clock", vec![lit(0.1)]));

        validate_appendix_b_invariants(&dae_model).expect(
            "clock constructor metadata should not violate strict solver expression checks",
        );
    }

    #[test]
    fn source_temporal_operator_surviving_dae_boundary_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.equations.push(dae::Equation::residual(
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::Edge,
                args: vec![var_ref("x")],
                span: rumoca_core::Span::DUMMY,
            },
            rumoca_core::Span::DUMMY,
            "raw edge in f_x",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("raw source temporal operator must fail the DAE boundary validation gate");
        assert!(matches!(
            err,
            ToDaeError::SourceTemporalOperatorSurvivedDaeBoundary { .. }
        ));
    }

    #[test]
    fn pre_operator_surviving_var_ref_subscript_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .variables
            .discrete_valued
            .insert(rumoca_core::VarName::new("target"), bool_var("target"));
        dae_model
            .discrete
            .valued_updates
            .push(dae::Equation::explicit(
                rumoca_core::VarName::new("target"),
                rumoca_core::Expression::VarRef {
                    name: rumoca_core::VarName::new("table").into(),
                    subscripts: vec![rumoca_core::Subscript::generated_expr(Box::new(pre_call(
                        "selector",
                    )))],
                    span: rumoca_core::Span::DUMMY,
                },
                rumoca_core::Span::DUMMY,
                "target = table[pre(selector)]",
            ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("raw pre() in a subscript must fail the DAE boundary validation gate");
        assert!(matches!(
            err,
            ToDaeError::SourceTemporalOperatorSurvivedDaeBoundary { .. }
        ));
    }

    #[test]
    fn source_temporal_operator_surviving_triggered_clock_condition_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model.clocks.triggered_conditions.push(pre_call("gate"));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("raw pre() in triggered clock condition must fail validation");
        assert!(matches!(
            err,
            ToDaeError::SourceTemporalOperatorSurvivedDaeBoundary { .. }
        ));
    }

    #[test]
    fn flow_action_call_surviving_expression_graph_is_rejected() {
        let mut dae_model = dae::Dae::default();
        dae_model.continuous.equations.push(dae::Equation::residual(
            call("assert", vec![var_ref("ok"), lit(0.0)]),
            rumoca_core::Span::DUMMY,
            "raw assert in f_x",
        ));

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("assert() in a DAE value expression must fail validation");

        assert!(matches!(err, ToDaeError::StrictSolverDaeViolation { .. }));
    }

    #[test]
    fn event_action_metadata_is_accepted_for_assert_and_terminate_only() {
        let mut dae_model = dae::Dae::default();
        dae_model.events.event_actions.push(dae::DaeEventAction {
            condition: var_ref("failed"),
            kind: dae::DaeEventActionKind::Assert {
                message: "failed".to_string(),
            },
            span: rumoca_core::Span::DUMMY,
            origin: "assert action".to_string(),
        });
        dae_model.events.event_actions.push(dae::DaeEventAction {
            condition: var_ref("done"),
            kind: dae::DaeEventActionKind::Terminate {
                message: "done".to_string(),
            },
            span: rumoca_core::Span::DUMMY,
            origin: "terminate action".to_string(),
        });

        validate_appendix_b_invariants(&dae_model)
            .expect("assert and terminate belong in DAE event action metadata");
    }

    #[test]
    fn runtime_metadata_rejects_clock_interval_for_unknown_variable() {
        let mut dae_model = dae::Dae::default();
        dae_model
            .clocks
            .intervals
            .insert("unknownVar".to_string(), 0.1);

        let err = validate_appendix_b_invariants(&dae_model)
            .expect_err("clock interval key without matching discrete variable must fail");
        assert!(matches!(err, ToDaeError::RuntimeMetadataViolation { .. }));
    }

    #[test]
    fn runtime_metadata_accepts_clock_interval_for_algebraic_variable() {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.algebraics.insert(
            rumoca_core::VarName::new("clockedAlg"),
            dae::Variable::new(rumoca_core::VarName::new("clockedAlg")),
        );
        dae_model
            .clocks
            .intervals
            .insert("clockedAlg".to_string(), 0.1);

        validate_appendix_b_invariants(&dae_model)
            .expect("clock interval metadata should accept algebraic clocked variables");
    }
}
