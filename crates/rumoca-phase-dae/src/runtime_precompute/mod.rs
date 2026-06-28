//! Precompute runtime metadata on DAE so solver backends can stay thin.

use super::ToDaeError;
use crate::condition_activation;
use rumoca_core::timing::{maybe_elapsed_seconds, maybe_start_timer_if};
use rumoca_core::{ExpressionRewriter, ExpressionVisitor};
use rumoca_ir_dae as dae;
use std::collections::{HashMap, HashSet};

mod clock;
mod expression_identity;

fn runtime_precompute_profile_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(
            target: "rumoca_phase_dae::runtime_precompute",
            tracing::Level::DEBUG
        )
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

fn log_runtime_precompute_profile(label: &str, start: rumoca_core::timing::OptionalTimer) {
    if start.is_none() {
        return;
    }
    let elapsed = maybe_elapsed_seconds(start);

    #[cfg(feature = "tracing")]
    tracing::debug!(
        target: "rumoca_phase_dae::runtime_precompute",
        phase = label,
        elapsed_seconds = elapsed,
        "ToDae runtime_precompute subphase"
    );

    #[cfg(not(feature = "tracing"))]
    let _ = (label, elapsed);
}

pub(crate) fn populate_runtime_precompute(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    let profile = runtime_precompute_profile_enabled();
    let compile_time_scalars_start = maybe_start_timer_if(profile);
    let compile_time_scalars = collect_compile_time_scalars(dae_model);
    log_runtime_precompute_profile("compile_time_scalars", compile_time_scalars_start);

    let synthetic_root_start = maybe_start_timer_if(profile);
    let mut synthetic_roots = expression_identity::UniqueExpressions::default();
    for eq in &dae_model.continuous.equations {
        clock::collect_synthetic_root_conditions_expr(
            &eq.rhs,
            false,
            &compile_time_scalars,
            &mut synthetic_roots,
        );
    }
    let mut synthetic_roots = synthetic_roots.into_vec();
    log_runtime_precompute_profile("synthetic_roots", synthetic_root_start);

    let time_event_start = maybe_start_timer_if(profile);
    let mut seen_time_events = HashSet::new();
    let mut scheduled_time_events = Vec::new();
    for relation in &dae_model.conditions.relations {
        maybe_push_time_event_condition(
            relation,
            false,
            &compile_time_scalars,
            &mut seen_time_events,
            &mut scheduled_time_events,
        );
    }
    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
    {
        collect_time_discontinuity_events_expr(
            &eq.rhs,
            false,
            &compile_time_scalars,
            &mut seen_time_events,
            &mut scheduled_time_events,
        );
    }
    scheduled_time_events.sort_by(f64::total_cmp);
    scheduled_time_events.dedup_by(|a, b| (*a - *b).abs() <= 1e-12 * (1.0 + a.abs().max(b.abs())));
    log_runtime_precompute_profile("scheduled_time_events", time_event_start);

    let clock_metadata_start = maybe_start_timer_if(profile);
    let clock_compile_time_scalars =
        collect_clock_compile_time_scalars(dae_model, &compile_time_scalars);
    let (
        clock_constructor_exprs,
        clock_schedules,
        clock_intervals,
        clock_timings,
        triggered_clock_conditions,
    ) = clock::compute_clock_runtime_metadata(dae_model, &clock_compile_time_scalars)?;
    log_runtime_precompute_profile("clock_metadata", clock_metadata_start);

    let prune_start = maybe_start_timer_if(profile);
    prune_unreferenced_condition_memory(dae_model, &compile_time_scalars)?;
    remove_relation_duplicate_synthetic_roots(dae_model, &mut synthetic_roots);
    log_runtime_precompute_profile("prune_condition_memory", prune_start);
    dae_model.events.synthetic_root_conditions = synthetic_roots;
    dae_model.events.scheduled_time_events = scheduled_time_events;
    dae_model.clocks.constructor_exprs = clock_constructor_exprs;
    dae_model.clocks.schedules = clock_schedules;
    dae_model.clocks.intervals = clock_intervals;
    dae_model.clocks.timings = clock_timings;
    dae_model.clocks.triggered_conditions = triggered_clock_conditions;
    Ok(())
}

fn remove_relation_duplicate_synthetic_roots(
    dae_model: &dae::Dae,
    synthetic_roots: &mut Vec<rumoca_core::Expression>,
) {
    if dae_model.conditions.relations.is_empty() || synthetic_roots.is_empty() {
        return;
    }

    let relation_roots = dae_model
        .conditions
        .relations
        .iter()
        .filter_map(clock::relation_root_expression)
        .collect::<Vec<_>>();
    synthetic_roots.retain(|synthetic_root| {
        !relation_roots.iter().any(|relation_root| {
            rumoca_core::expressions_semantically_equal(synthetic_root, relation_root)
        })
    });
}

fn prune_unreferenced_condition_memory(
    dae_model: &mut dae::Dae,
    constants: &HashMap<String, f64>,
) -> Result<(), ToDaeError> {
    if dae_model.conditions.relations.is_empty() || dae_model.conditions.equations.is_empty() {
        return Ok(());
    }

    let old_relations = std::mem::take(&mut dae_model.conditions.relations);
    let old_conditions = std::mem::take(&mut dae_model.conditions.equations);
    let condition_name = condition_variable_name_from_equation(
        old_conditions
            .first()
            .ok_or_else(|| runtime_contract("condition relation set has no equations", None))?,
    )?;
    let mut kept_relations = Vec::with_capacity(old_relations.len());
    let mut kept_conditions = Vec::with_capacity(old_conditions.len());
    let mut replacements = Vec::with_capacity(old_relations.len());

    // Collect every referenced condition-memory index in a SINGLE walk of the
    // model. The previous code re-walked the whole model once per relation,
    // which is O(relations * equations) = O(n^2) on large array models (one
    // relation per unrolled cell, every cell equation walked for each).
    let referenced = collect_referenced_condition_indices(dae_model, &condition_name);

    for (old_index, (relation, equation)) in
        old_relations.into_iter().zip(old_conditions).enumerate()
    {
        if !referenced.contains(&(old_index + 1))
            && condition_memory_can_be_direct(&relation, constants)
        {
            replacements.push(ConditionMemoryReplacement::Direct(relation));
            continue;
        }

        let condition_index = kept_relations.len() + 1;
        kept_conditions.push(dae::Equation::explicit(
            generated_condition_reference(&condition_name, condition_index, equation.span)?,
            relation.clone(),
            equation.span,
            equation.origin,
        ));
        replacements.push(ConditionMemoryReplacement::Memory(condition_index));
        kept_relations.push(relation);
    }

    dae_model.conditions.relations = kept_relations;
    dae_model.conditions.equations = kept_conditions;
    rewrite_condition_memory_references(dae_model, &condition_name, &replacements)?;
    Ok(())
}

fn condition_memory_can_be_direct(
    relation: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> bool {
    extract_time_event_instant(relation, constants).is_some()
        || is_logical_condition_memory(relation)
}

fn is_logical_condition_memory(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::And | rumoca_core::OpBinary::Or,
            ..
        } | rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Not,
            ..
        }
    )
}

fn condition_variable_name_from_equation(equation: &dae::Equation) -> Result<String, ToDaeError> {
    let Some(lhs) = equation.lhs.as_ref() else {
        return Err(runtime_contract(
            "condition equation is missing generated condition lhs",
            Some(equation.span),
        ));
    };
    let Some(component_ref) = lhs.component_ref() else {
        return Err(runtime_contract(
            format!(
                "condition equation lhs `{}` has no structured component reference",
                lhs.as_str()
            ),
            Some(equation.span),
        ));
    };
    if component_ref.parts.len() != 1 {
        return Err(runtime_contract(
            format!(
                "condition equation lhs `{}` has invalid generated component reference shape",
                lhs.as_str()
            ),
            Some(equation.span),
        ));
    }
    Ok(component_ref.parts[0].ident.clone())
}

fn generated_condition_reference(
    condition_name: &str,
    condition_index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Reference, ToDaeError> {
    Ok(rumoca_core::Reference::generated_component(
        condition_name,
        vec![generated_index_subscript(
            condition_index_i64(
                condition_index,
                span,
                "runtime condition equation memory subscript",
            )?,
            span,
            "runtime condition equation memory subscript",
        )?],
        span,
    ))
}

fn runtime_contract(detail: impl Into<String>, span: Option<rumoca_core::Span>) -> ToDaeError {
    match span {
        Some(span) => ToDaeError::runtime_contract_violation_at(detail, span),
        None => ToDaeError::runtime_contract_violation(detail),
    }
}

/// Walk the model once and collect every condition-memory index referenced by
/// any equation, update, initialization, or event condition. Replaces the prior
/// per-index re-walk so pruning is O(n) in the model size instead of O(n^2).
fn collect_referenced_condition_indices(
    dae_model: &dae::Dae,
    condition_name: &str,
) -> std::collections::HashSet<usize> {
    let mut collector = ConditionMemoryUseCollector {
        condition_name,
        pre_condition_name: format!("__pre__.{condition_name}"),
        referenced: std::collections::HashSet::new(),
    };
    for expr in dae_model
        .continuous
        .equations
        .iter()
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.initialization.equations.iter())
        .map(|equation| &equation.rhs)
        .chain(
            dae_model
                .events
                .event_actions
                .iter()
                .map(|action| &action.condition),
        )
    {
        collector.visit_expression(expr);
    }
    collector.referenced
}

struct ConditionMemoryUseCollector<'a> {
    condition_name: &'a str,
    pre_condition_name: String,
    referenced: std::collections::HashSet<usize>,
}

impl ExpressionVisitor for ConditionMemoryUseCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        if let Some((_, index)) = condition_memory_index(
            name.as_str(),
            subscripts,
            self.condition_name,
            &self.pre_condition_name,
        ) {
            self.referenced.insert(index);
            return;
        }
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }
}

#[derive(Clone)]
enum ConditionMemoryReplacement {
    Memory(usize),
    Direct(rumoca_core::Expression),
}

fn rewrite_condition_memory_references(
    dae_model: &mut dae::Dae,
    condition_name: &str,
    replacements: &[ConditionMemoryReplacement],
) -> Result<(), ToDaeError> {
    if replacements.is_empty() {
        return Ok(());
    }

    let mut rewriter = ConditionMemoryReindexer {
        condition_name,
        pre_condition_name: format!("__pre__.{condition_name}"),
        replacements,
        error: None,
    };
    rewrite_equation_rhs(&mut dae_model.continuous.equations, &mut rewriter);
    rewrite_equation_rhs(&mut dae_model.discrete.real_updates, &mut rewriter);
    rewrite_equation_rhs(&mut dae_model.discrete.valued_updates, &mut rewriter);
    rewrite_equation_rhs(&mut dae_model.initialization.equations, &mut rewriter);
    rewrite_equation_rhs(&mut dae_model.conditions.equations, &mut rewriter);
    for relation in &mut dae_model.conditions.relations {
        *relation = rewriter.rewrite_expression(relation);
    }
    for action in &mut dae_model.events.event_actions {
        action.condition = rewriter.rewrite_expression(&action.condition);
    }
    match rewriter.error {
        Some(error) => Err(error),
        None => Ok(()),
    }
}

fn rewrite_equation_rhs(
    equations: &mut [dae::Equation],
    rewriter: &mut ConditionMemoryReindexer<'_>,
) {
    for equation in equations {
        equation.rhs = rewriter.rewrite_expression(&equation.rhs);
    }
}

struct ConditionMemoryReindexer<'a> {
    condition_name: &'a str,
    pre_condition_name: String,
    replacements: &'a [ConditionMemoryReplacement],
    error: Option<ToDaeError>,
}

impl ExpressionRewriter for ConditionMemoryReindexer<'_> {
    fn rewrite_expression(&mut self, expr: &rumoca_core::Expression) -> rumoca_core::Expression {
        if self.error.is_some() {
            return expr.clone();
        }
        if let rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } = expr
            && let Some((is_pre, old_index)) = condition_memory_index(
                name.as_str(),
                subscripts,
                self.condition_name,
                &self.pre_condition_name,
            )
            && let Some(replacement) = old_index
                .checked_sub(1)
                .and_then(|index| self.replacements.get(index))
        {
            match replacement_expr(replacement, self.condition_name, is_pre, *span) {
                Ok(expr) => return expr,
                Err(error) => {
                    self.error = Some(error);
                    return expr.clone();
                }
            }
        }
        self.walk_expression(expr)
    }
}

fn replacement_expr(
    replacement: &ConditionMemoryReplacement,
    condition_name: &str,
    is_pre: bool,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, ToDaeError> {
    match replacement {
        ConditionMemoryReplacement::Memory(index) => condition_memory_ref(
            if is_pre {
                format!("__pre__.{condition_name}")
            } else {
                condition_name.to_string()
            },
            *index,
            span,
        ),
        ConditionMemoryReplacement::Direct(expr) => Ok(expr.clone()),
    }
}

fn condition_memory_ref(
    name: String,
    index: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, ToDaeError> {
    Ok(rumoca_core::Expression::VarRef {
        name: rumoca_core::Reference::generated(name),
        subscripts: vec![generated_index_subscript(
            condition_index_i64(
                index,
                span,
                "runtime condition memory replacement subscript",
            )?,
            span,
            "runtime condition memory replacement subscript",
        )?],
        span,
    })
}

fn condition_index_i64(
    index: usize,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<i64, ToDaeError> {
    i64::try_from(index)
        .map_err(|_| runtime_contract(format!("{context} {index} exceeds i64 range"), Some(span)))
}

fn generated_index_subscript(
    index: i64,
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Subscript, ToDaeError> {
    rumoca_core::Subscript::try_generated_index(index, span, context).map_err(|err| {
        if span.is_dummy() {
            ToDaeError::runtime_metadata_violation(err.to_string())
        } else {
            ToDaeError::runtime_metadata_violation_at(err.to_string(), span)
        }
    })
}

// `pre_condition_name` is `format!("__pre__.{condition_name}")`, precomputed by
// the caller. It is hoisted out because this is called once per visited
// var-ref; allocating it here turned a model walk into per-var-ref string
// formatting (a measurable hot spot on large array models).
fn condition_memory_index(
    name: &str,
    subscripts: &[rumoca_core::Subscript],
    condition_name: &str,
    pre_condition_name: &str,
) -> Option<(bool, usize)> {
    if name == condition_name {
        return one_based_static_index(subscripts).map(|index| (false, index));
    }
    if name == pre_condition_name {
        return one_based_static_index(subscripts).map(|index| (true, index));
    }
    if !subscripts.is_empty() {
        return None;
    }

    let scalar = rumoca_core::parse_scalar_name(name)?;
    if scalar.indices.len() != 1 {
        return None;
    }
    let index = usize::try_from(scalar.indices[0]).ok()?;
    if scalar.base == condition_name {
        return Some((false, index));
    }
    (scalar.base == pre_condition_name).then_some((true, index))
}

fn one_based_static_index(subscripts: &[rumoca_core::Subscript]) -> Option<usize> {
    let [rumoca_core::Subscript::Index { value, .. }] = subscripts else {
        return None;
    };
    usize::try_from(*value).ok().filter(|index| *index > 0)
}
fn insert_compile_time_scalar(values: &mut HashMap<String, f64>, name: &str, value: f64) -> bool {
    if values
        .get(name)
        .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
    {
        values.insert(name.to_string(), value);
        return true;
    }
    false
}
fn collect_array_elements_scalar_entries(
    name: &rumoca_core::VarName,
    elements: &[rumoca_core::Expression],
    values: &HashMap<String, f64>,
) -> Vec<(String, f64)> {
    elements
        .iter()
        .enumerate()
        .filter_map(|(index, element)| {
            eval_scalar_const_expr(element, values).map(|value| {
                (
                    dae::format_subscript_key(name.as_str(), &[index + 1]),
                    value,
                )
            })
        })
        .collect()
}
fn collect_range_scalar_entries(
    name: &rumoca_core::VarName,
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    values: &HashMap<String, f64>,
) -> Vec<(String, f64)> {
    let Some(mut current) = eval_scalar_const_expr(start, values) else {
        return Vec::new();
    };
    let Some(end_value) = eval_scalar_const_expr(end, values) else {
        return Vec::new();
    };
    let step_value = match step {
        Some(expr) => {
            let Some(value) = eval_scalar_const_expr(expr, values) else {
                return Vec::new();
            };
            value
        }
        None => 1.0,
    };
    if step_value.abs() <= f64::EPSILON {
        return Vec::new();
    }

    let mut entries = Vec::new();
    let mut index = 1usize;
    if step_value > 0.0 {
        while current <= end_value + 1.0e-12 && index <= 10_000 {
            entries.push((dae::format_subscript_key(name.as_str(), &[index]), current));
            current += step_value;
            index += 1;
        }
    } else {
        while current >= end_value - 1.0e-12 && index <= 10_000 {
            entries.push((dae::format_subscript_key(name.as_str(), &[index]), current));
            current += step_value;
            index += 1;
        }
    }
    entries
}
fn collect_array_scalar_entries(
    name: &rumoca_core::VarName,
    start: &rumoca_core::Expression,
    values: &HashMap<String, f64>,
) -> Vec<(String, f64)> {
    match start {
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => {
            collect_array_elements_scalar_entries(name, elements, values)
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => collect_range_scalar_entries(name, start, step.as_deref(), end, values),
        _ => Vec::new(),
    }
}
fn collect_compile_time_scalars(dae_model: &dae::Dae) -> HashMap<String, f64> {
    let mut values = HashMap::new();
    for (literal, ordinal) in &dae_model.symbols.enum_literal_ordinals {
        values.insert(literal.clone(), *ordinal as f64);
    }
    let bindings: Vec<(&rumoca_core::VarName, &rumoca_core::Expression)> = dae_model
        .variables
        .parameters
        .iter()
        .chain(dae_model.variables.constants.iter())
        .chain(dae_model.variables.inputs.iter())
        .filter_map(|(name, variable)| variable.start.as_ref().map(|start| (name, start)))
        .collect();

    let max_passes = (bindings.len().max(1)).saturating_mul(2);
    for _ in 0..max_passes {
        let mut changed = false;
        for (name, start) in &bindings {
            if let Some(value) = eval_scalar_const_expr(start, &values) {
                changed |= insert_compile_time_scalar(&mut values, name.as_str(), value);
            }
            for (indexed_name, indexed_value) in collect_array_scalar_entries(name, start, &values)
            {
                changed |= insert_compile_time_scalar(&mut values, &indexed_name, indexed_value);
            }
        }
        if !changed {
            break;
        }
    }

    values
}

fn collect_clock_compile_time_scalars(
    dae_model: &dae::Dae,
    compile_time_scalars: &HashMap<String, f64>,
) -> HashMap<String, f64> {
    let mut values = compile_time_scalars.clone();
    for name in initial_time_parameter_names(dae_model) {
        values.insert(name, 0.0);
    }
    values
}

fn initial_time_parameter_names(dae_model: &dae::Dae) -> Vec<String> {
    let mut names = Vec::new();
    let mut seen = HashSet::new();
    for equation in &dae_model.initialization.equations {
        if let Some(name) = initial_time_parameter_name_from_equation(dae_model, equation)
            && seen.insert(name.clone())
        {
            names.push(name);
        }
    }
    names
}

fn initial_time_parameter_name_from_equation(
    dae_model: &dae::Dae,
    equation: &dae::Equation,
) -> Option<String> {
    if let Some(lhs) = &equation.lhs
        && expr_is_time_ref(&equation.rhs)
        && dae_model
            .variables
            .parameters
            .contains_key(&rumoca_core::VarName::new(lhs.as_str()))
    {
        return Some(lhs.as_str().to_string());
    }

    let rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = &equation.rhs
    else {
        return None;
    };

    match (
        initial_time_parameter_var_ref(dae_model, lhs),
        expr_is_time_ref(rhs),
        expr_is_time_ref(lhs),
        initial_time_parameter_var_ref(dae_model, rhs),
    ) {
        (Some(name), true, _, _) | (_, _, true, Some(name)) => Some(name),
        _ => None,
    }
}

fn initial_time_parameter_var_ref(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> Option<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty()
        || !dae_model
            .variables
            .parameters
            .contains_key(&rumoca_core::VarName::new(name.as_str()))
    {
        return None;
    }
    Some(name.as_str().to_string())
}

fn expr_is_time_ref(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        } if name.as_str() == "time" && subscripts.is_empty()
    )
}

fn eval_scalar_const_expr(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    rumoca_eval_dae::constant::eval_scalar_const_expr_with(expr, &|name, subscripts| {
        if subscripts.is_empty() {
            return constants
                .get(name.as_str())
                .copied()
                .map(rumoca_eval_dae::constant::ConstValue::Real);
        }
        clock::canonical_var_ref_key(name, subscripts, constants)
            .and_then(|key| constants.get(&key).copied())
            .map(rumoca_eval_dae::constant::ConstValue::Real)
    })
}
fn extract_time_event_instant(
    cond: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = cond else {
        return None;
    };
    if !matches!(
        op,
        rumoca_core::OpBinary::Lt
            | rumoca_core::OpBinary::Le
            | rumoca_core::OpBinary::Gt
            | rumoca_core::OpBinary::Ge
    ) {
        return None;
    }
    let (a_lhs, b_lhs) = affine_time_coefficients(lhs, constants)?;
    let (a_rhs, b_rhs) = affine_time_coefficients(rhs, constants)?;
    let a = a_lhs - a_rhs;
    if a.abs() <= 1e-14 {
        return None;
    }
    let b = b_lhs - b_rhs;
    let t = -b / a;
    t.is_finite().then_some(t)
}
fn affine_time_coefficients(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> Option<(f64, f64)> {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            if name.as_str() == "time" {
                return Some((1.0, 0.0));
            }
            eval_scalar_const_expr(expr, constants).map(|value| (0.0, value))
        }
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(value),
            ..
        } => Some((0.0, *value as f64)),
        rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(value),
            ..
        } => Some((0.0, *value)),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => affine_time_coefficients(rhs, constants).map(|(a, b)| (-a, -b)),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => {
            let (a_lhs, b_lhs) = affine_time_coefficients(lhs, constants)?;
            let (a_rhs, b_rhs) = affine_time_coefficients(rhs, constants)?;
            Some((a_lhs + a_rhs, b_lhs + b_rhs))
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => {
            let (a_lhs, b_lhs) = affine_time_coefficients(lhs, constants)?;
            let (a_rhs, b_rhs) = affine_time_coefficients(rhs, constants)?;
            Some((a_lhs - a_rhs, b_lhs - b_rhs))
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => {
            if let Some(scalar) = eval_scalar_const_expr(lhs, constants) {
                let (a, b) = affine_time_coefficients(rhs, constants)?;
                return Some((scalar * a, scalar * b));
            }
            if let Some(scalar) = eval_scalar_const_expr(rhs, constants) {
                let (a, b) = affine_time_coefficients(lhs, constants)?;
                return Some((scalar * a, scalar * b));
            }
            None
        }
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs,
            rhs,
            ..
        } => {
            let denominator = eval_scalar_const_expr(rhs, constants)?;
            if denominator.abs() <= f64::EPSILON {
                return None;
            }
            let (a, b) = affine_time_coefficients(lhs, constants)?;
            Some((a / denominator, b / denominator))
        }
        _ => eval_scalar_const_expr(expr, constants).map(|value| (0.0, value)),
    }
}
fn maybe_push_time_event_condition(
    expr: &rumoca_core::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    if suppress_events {
        return;
    }
    let Some(event_time) = extract_time_event_instant(expr, constants) else {
        return;
    };
    let key = format!("time::{event_time:.15e}");
    if seen.insert(key) {
        out.push(event_time);
    }
}
fn collect_time_discontinuity_events_expr(
    expr: &rumoca_core::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    let mut collector = TimeDiscontinuityEventCollector {
        suppress_events,
        constants,
        seen,
        out,
    };
    collector.visit_expression(expr);
}

struct TimeDiscontinuityEventCollector<'a> {
    suppress_events: bool,
    constants: &'a HashMap<String, f64>,
    seen: &'a mut HashSet<String>,
    out: &'a mut Vec<f64>,
}

impl TimeDiscontinuityEventCollector<'_> {
    fn visit_with_event_suppression(&mut self, exprs: &[rumoca_core::Expression]) {
        let previous = self.suppress_events;
        self.suppress_events = true;
        for expr in exprs {
            self.visit_expression(expr);
        }
        self.suppress_events = previous;
    }

    fn visit_expr_with_suppression(&mut self, expr: &rumoca_core::Expression, suppress: bool) {
        let previous = self.suppress_events;
        self.suppress_events = suppress;
        self.visit_expression(expr);
        self.suppress_events = previous;
    }
}

impl ExpressionVisitor for TimeDiscontinuityEventCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        match expr {
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => {
                let mut else_suppressed = self.suppress_events;
                for (cond, value) in branches {
                    let condition_activation = condition_activation::runtime_activation(cond);
                    let cond_suppressed = else_suppressed || condition_activation.is_some();
                    maybe_push_time_event_condition(
                        cond,
                        cond_suppressed,
                        self.constants,
                        self.seen,
                        self.out,
                    );
                    self.visit_expr_with_suppression(cond, cond_suppressed);
                    self.visit_expr_with_suppression(
                        value,
                        else_suppressed || matches!(condition_activation, Some(false)),
                    );
                    else_suppressed |= matches!(condition_activation, Some(true));
                }
                self.visit_expr_with_suppression(else_branch, else_suppressed);
            }
            rumoca_core::Expression::Binary { .. } => {
                maybe_push_time_event_condition(
                    expr,
                    self.suppress_events,
                    self.constants,
                    self.seen,
                    self.out,
                );
                self.walk_expression(expr);
            }
            rumoca_core::Expression::BuiltinCall {
                function:
                    rumoca_core::BuiltinFunction::NoEvent | rumoca_core::BuiltinFunction::Smooth,
                args,
                ..
            } => {
                self.visit_with_event_suppression(args);
            }
            _ => self.walk_expression(expr),
        }
    }
}

#[cfg(test)]
mod tests;
