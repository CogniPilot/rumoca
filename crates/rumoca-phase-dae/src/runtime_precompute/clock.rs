use super::ToDaeError;
use super::{eval_scalar_const_expr, extract_time_event_instant};
use indexmap::IndexMap;
use std::collections::{HashMap, HashSet};

use rumoca_ir_dae as dae;
use rumoca_ir_dae::{ExpressionVisitor, ImplicitSampleChecker, VarRefWithSubscriptsCollector};

type SourceMap<'a> = HashMap<String, Vec<&'a dae::Expression>>;
type ClockRuntimeMetadata = (
    Vec<dae::Expression>,
    Vec<dae::ClockSchedule>,
    IndexMap<String, f64>,
    Vec<dae::Expression>,
);

pub(super) fn compute_clock_runtime_metadata(
    dae_model: &dae::Dae,
    compile_time_scalars: &HashMap<String, f64>,
) -> Result<ClockRuntimeMetadata, ToDaeError> {
    let mut clock_constructor_exprs = Vec::new();
    for eq in dae_model.f_z.iter().chain(dae_model.f_m.iter()) {
        collect_clock_constructor_exprs(
            &eq.rhs,
            compile_time_scalars,
            &mut clock_constructor_exprs,
        );
    }
    dedupe_expressions_in_place(&mut clock_constructor_exprs);
    let clock_sources = build_clock_source_map(dae_model, compile_time_scalars);
    let mut clock_schedules = Vec::new();
    let mut unresolved_clock_exprs = Vec::new();
    let mut triggered_clock_conditions = Vec::new();
    let mut static_constructor_count = 0usize;
    for expr in &clock_constructor_exprs {
        if !requires_static_clock_schedule(expr) {
            continue;
        }
        static_constructor_count += 1;
        let Some((period, phase)) = infer_clock_timing_from_expr(
            expr,
            compile_time_scalars,
            &clock_sources,
            24,
            &mut HashSet::new(),
        ) else {
            if is_non_static_event_clock_constructor(
                expr,
                dae_model,
                compile_time_scalars,
                &clock_sources,
            ) {
                // Extract the condition expression from Clock(condition)
                if let dae::Expression::FunctionCall { args, .. } = expr {
                    if let Some(cond) = args.first() {
                        triggered_clock_conditions.push(cond.clone());
                    }
                }
                continue;
            }
            if is_non_static_inferred_clock_composition(
                expr,
                compile_time_scalars,
                &clock_sources,
            ) {
                continue;
            }
            unresolved_clock_exprs.push(format_unresolved_clock_expr(
                expr,
                dae_model,
                compile_time_scalars,
                &clock_sources,
            ));
            continue;
        };
        clock_schedules.push(dae::ClockSchedule {
            period_seconds: period,
            phase_seconds: phase,
        });
    }
    clock_schedules.sort_by(|lhs, rhs| {
        lhs.period_seconds
            .total_cmp(&rhs.period_seconds)
            .then(lhs.phase_seconds.total_cmp(&rhs.phase_seconds))
    });
    clock_schedules.dedup_by(|lhs, rhs| {
        (lhs.period_seconds - rhs.period_seconds).abs()
            <= 1e-12 * (1.0 + lhs.period_seconds.abs().max(rhs.period_seconds.abs()))
            && (lhs.phase_seconds - rhs.phase_seconds).abs()
                <= 1e-12 * (1.0 + lhs.phase_seconds.abs().max(rhs.phase_seconds.abs()))
    });
    if !unresolved_clock_exprs.is_empty() {
        let unresolved = unresolved_clock_exprs.len();
        let constructors = static_constructor_count;
        let examples = unresolved_clock_exprs
            .iter()
            .take(3)
            .cloned()
            .collect::<Vec<_>>()
            .join(" | ");
        return Err(ToDaeError::unresolved_clock_schedule(
            constructors,
            unresolved,
            examples,
        ));
    }
    let clock_intervals =
        infer_clock_intervals_by_variable(dae_model, compile_time_scalars, &clock_schedules);

    Ok((
        clock_constructor_exprs,
        clock_schedules,
        clock_intervals,
        triggered_clock_conditions,
    ))
}

fn unresolved_clock_debug_enabled() -> bool {
    std::env::var("RUMOCA_DAE_CLOCK_DEBUG").is_ok()
}

fn format_unresolved_clock_expr(
    expr: &dae::Expression,
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> String {
    if !unresolved_clock_debug_enabled() {
        return format!("{expr:?}");
    }
    let context = summarize_unresolved_clock_context(expr, dae_model, constants, sources);
    format!("{expr:?} [{context}]")
}

fn summarize_unresolved_clock_context(
    expr: &dae::Expression,
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> String {
    let refs = collect_unique_clock_var_refs(expr, constants);
    if refs.is_empty() {
        return "no_var_refs".to_string();
    }
    refs.into_iter()
        .take(6)
        .map(|(name, subscripts, key)| {
            let source = sources.get(&key).map(|exprs| {
                exprs
                    .iter()
                    .take(3)
                    .map(|expr| short_expr(expr, 120))
                    .collect::<Vec<_>>()
                    .join(" || ")
            });
            let source_count = sources.get(&key).map_or(0usize, Vec::len);
            let value = eval_clock_scalar_with_sources(
                &dae::Expression::VarRef {
                    name: name.clone(),
                    subscripts: subscripts.clone(),
                },
                constants,
                sources,
                24,
                &mut HashSet::new(),
            );
            let (kind, start) = dae_var_kind_and_start(dae_model, name.as_str());
            format!(
                "{}{{kind={},const={},start={},source={},value={}}}",
                key,
                kind,
                constants
                    .get(&key)
                    .map(|v| format!("{v:.6e}"))
                    .unwrap_or_else(|| "-".to_string()),
                start.unwrap_or_else(|| "-".to_string()),
                if source_count == 0 {
                    "-".to_string()
                } else {
                    format!("{source_count}:{}", source.unwrap_or_default())
                },
                value
                    .map(|v| format!("{v:.6e}"))
                    .unwrap_or_else(|| "?".to_string())
            )
        })
        .collect::<Vec<_>>()
        .join("; ")
}

fn short_expr(expr: &dae::Expression, max_len: usize) -> String {
    let rendered = format!("{expr:?}");
    if rendered.len() <= max_len {
        return rendered;
    }
    format!("{}...", &rendered[..max_len])
}

fn dae_var_kind_and_start(dae_model: &dae::Dae, name: &str) -> (&'static str, Option<String>) {
    let lookup = |kind: &'static str,
                  vars: &indexmap::IndexMap<dae::VarName, dae::Variable>|
     -> Option<(&'static str, Option<String>)> {
        vars.get(&dae::VarName::new(name))
            .map(|var| (kind, var.start.as_ref().map(|expr| short_expr(expr, 320))))
    };

    lookup("parameter", &dae_model.parameters)
        .or_else(|| lookup("constant", &dae_model.constants))
        .or_else(|| lookup("input", &dae_model.inputs))
        .or_else(|| lookup("discrete_real", &dae_model.discrete_reals))
        .or_else(|| lookup("discrete_valued", &dae_model.discrete_valued))
        .or_else(|| lookup("state", &dae_model.states))
        .or_else(|| lookup("algebraic", &dae_model.algebraics))
        .or_else(|| lookup("output", &dae_model.outputs))
        .unwrap_or(("missing", None))
}

fn collect_unique_clock_var_refs(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
) -> Vec<(dae::VarName, Vec<dae::Subscript>, String)> {
    let mut collector = VarRefWithSubscriptsCollector::new();
    collector.visit_expression(expr);
    let refs = collector.into_refs();
    let mut seen = HashSet::new();
    refs.into_iter()
        .filter_map(|(name, subscripts)| {
            let key = canonical_var_ref_key(&name, &subscripts, constants)
                .unwrap_or_else(|| name.to_string());
            if seen.insert(key.clone()) {
                Some((name, subscripts, key))
            } else {
                None
            }
        })
        .collect()
}

pub(super) fn dedupe_expressions_in_place(expressions: &mut Vec<dae::Expression>) {
    let mut seen = HashSet::new();
    expressions.retain(|expr| seen.insert(format!("{expr:?}")));
}

fn is_relational_condition(expr: &dae::Expression) -> bool {
    matches!(
        expr,
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(_)
                | rumoca_ir_core::OpBinary::Le(_)
                | rumoca_ir_core::OpBinary::Gt(_)
                | rumoca_ir_core::OpBinary::Ge(_),
            ..
        }
    )
}

pub(super) fn collect_synthetic_root_conditions_expr(
    expr: &dae::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    out: &mut Vec<dae::Expression>,
) {
    match expr {
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (cond, value) in branches {
                if !suppress_events
                    && is_relational_condition(cond)
                    && extract_time_event_instant(cond, constants).is_none()
                {
                    out.push(cond.clone());
                }
                collect_synthetic_root_conditions_expr(cond, suppress_events, constants, out);
                collect_synthetic_root_conditions_expr(value, suppress_events, constants, out);
            }
            collect_synthetic_root_conditions_expr(else_branch, suppress_events, constants, out);
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            if !suppress_events
                && is_relational_condition(expr)
                && extract_time_event_instant(expr, constants).is_none()
            {
                out.push(expr.clone());
            }
            collect_synthetic_root_conditions_expr(lhs, suppress_events, constants, out);
            collect_synthetic_root_conditions_expr(rhs, suppress_events, constants, out);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_synthetic_root_conditions_expr(rhs, suppress_events, constants, out);
        }
        dae::Expression::BuiltinCall { function, args } => {
            let suppressed = suppress_events
                || matches!(
                    function,
                    dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Smooth
                );
            if !suppressed
                && matches!(
                    function,
                    dae::BuiltinFunction::Abs | dae::BuiltinFunction::Sign
                )
                && let Some(arg) = args.first()
            {
                out.push(arg.clone());
            }
            for arg in args {
                collect_synthetic_root_conditions_expr(arg, suppressed, constants, out);
            }
        }
        dae::Expression::FunctionCall { args, .. } => {
            for arg in args {
                collect_synthetic_root_conditions_expr(arg, suppress_events, constants, out);
            }
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            for element in elements {
                collect_synthetic_root_conditions_expr(element, suppress_events, constants, out);
            }
        }
        dae::Expression::Range { start, step, end } => {
            collect_synthetic_root_conditions_expr(start, suppress_events, constants, out);
            if let Some(step_expr) = step {
                collect_synthetic_root_conditions_expr(step_expr, suppress_events, constants, out);
            }
            collect_synthetic_root_conditions_expr(end, suppress_events, constants, out);
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            collect_synthetic_root_conditions_expr(expr, suppress_events, constants, out);
            for idx in indices {
                collect_synthetic_root_conditions_expr(&idx.range, suppress_events, constants, out);
            }
            if let Some(filter_expr) = filter {
                collect_synthetic_root_conditions_expr(
                    filter_expr,
                    suppress_events,
                    constants,
                    out,
                );
            }
        }
        dae::Expression::Index { base, subscripts } => {
            collect_synthetic_root_conditions_expr(base, suppress_events, constants, out);
            for subscript in subscripts {
                if let dae::Subscript::Expr(expr) = subscript {
                    collect_synthetic_root_conditions_expr(expr, suppress_events, constants, out);
                }
            }
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_synthetic_root_conditions_expr(base, suppress_events, constants, out);
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

fn is_clock_constructor_function_name(short: &str) -> bool {
    matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample"
    )
}

fn requires_static_clock_schedule(expr: &dae::Expression) -> bool {
    let dae::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    match short {
        "Clock" => !args.is_empty(),
        "subSample" | "superSample" | "shiftSample" | "backSample" => true,
        _ => true,
    }
}

fn expression_is_event_clock_condition(expr: &dae::Expression, dae_model: &dae::Dae) -> bool {
    match expr {
        dae::Expression::Literal(dae::Literal::Boolean(_)) => true,
        dae::Expression::VarRef { name, .. } => {
            dae_model.discrete_valued.contains_key(name)
                || dae_model.enum_literal_ordinals.contains_key(name.as_str())
        }
        dae::Expression::Unary { op, rhs } => {
            matches!(op, rumoca_ir_core::OpUnary::Not(_))
                || expression_is_event_clock_condition(rhs, dae_model)
        }
        dae::Expression::Binary { op, lhs, rhs } => {
            matches!(
                op,
                rumoca_ir_core::OpBinary::And(_)
                    | rumoca_ir_core::OpBinary::Or(_)
                    | rumoca_ir_core::OpBinary::Lt(_)
                    | rumoca_ir_core::OpBinary::Le(_)
                    | rumoca_ir_core::OpBinary::Gt(_)
                    | rumoca_ir_core::OpBinary::Ge(_)
                    | rumoca_ir_core::OpBinary::Eq(_)
                    | rumoca_ir_core::OpBinary::Neq(_)
            ) || expression_is_event_clock_condition(lhs, dae_model)
                || expression_is_event_clock_condition(rhs, dae_model)
        }
        dae::Expression::BuiltinCall { function, args } => {
            matches!(function, dae::BuiltinFunction::Pre)
                && args
                    .first()
                    .is_some_and(|arg| expression_is_event_clock_condition(arg, dae_model))
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_is_event_clock_condition(cond, dae_model)
                    || expression_is_event_clock_condition(value, dae_model)
            }) || expression_is_event_clock_condition(else_branch, dae_model)
        }
        _ => false,
    }
}

fn is_non_static_event_clock_constructor(
    expr: &dae::Expression,
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    let dae::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short != "Clock" || args.is_empty() {
        return false;
    }
    infer_clock_constructor_timing(args, constants, sources, 24, &mut HashSet::new()).is_none()
        && expression_is_event_clock_condition(&args[0], dae_model)
}

fn is_non_static_inferred_clock_composition(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    let dae::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if !matches!(
        short,
        "subSample" | "superSample" | "shiftSample" | "backSample"
    ) {
        return false;
    }
    let Some(source_expr) = args.first() else {
        return false;
    };
    infer_clock_timing_from_expr(source_expr, constants, sources, 24, &mut HashSet::new()).is_none()
}

fn valid_positive_period(period: f64) -> Option<f64> {
    (period.is_finite() && period > 0.0).then_some(period)
}

fn eval_clock_scalar_child(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    eval_clock_scalar_with_sources(
        expr,
        constants,
        sources,
        remaining_depth.saturating_sub(1),
        visiting,
    )
}

fn eval_clock_scalar_from_var_ref(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    let key = canonical_var_ref_key(name, subscripts, constants)?;
    let visit_key = format!("scalar::{key}");
    if !visiting.insert(visit_key.clone()) {
        return None;
    }
    let inferred = sources.get(&key).and_then(|source_exprs| {
        source_exprs.iter().find_map(|source| {
            eval_clock_scalar_child(source, constants, sources, remaining_depth, visiting)
        })
    });
    visiting.remove(&visit_key);
    inferred
}

fn eval_clock_scalar_with_sources(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    if remaining_depth == 0 {
        return None;
    }
    if let Some(value) = eval_scalar_const_expr(expr, constants) {
        return Some(value);
    }

    match expr {
        dae::Expression::VarRef { name, subscripts } => eval_clock_scalar_from_var_ref(
            name,
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)
            .map(|value| -value),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        } => Some(
            eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?
                + eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?,
        ),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => Some(
            eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?
                - eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?,
        ),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(_),
            lhs,
            rhs,
        } => Some(
            eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?
                * eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?,
        ),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Div(_),
            lhs,
            rhs,
        } => {
            let denominator =
                eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?;
            if denominator.abs() <= f64::EPSILON {
                return None;
            }
            let numerator =
                eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?;
            Some(numerator / denominator)
        }
        _ => None,
    }
}

fn eval_positive_factor(
    expr: Option<&dae::Expression>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    let raw = eval_clock_scalar_with_sources(expr?, constants, sources, remaining_depth, visiting)?;
    let rounded = raw.round();
    (rounded.is_finite() && rounded > 0.0).then_some(rounded)
}

pub(super) fn canonical_var_ref_key(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    constants: &HashMap<String, f64>,
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut key = name.as_str().to_string();
    for subscript in subscripts {
        match subscript {
            dae::Subscript::Index(index) => key.push_str(&format!("[{index}]")),
            dae::Subscript::Expr(expr) => {
                let raw = eval_scalar_const_expr(expr, constants)?;
                let rounded = raw.round();
                if !rounded.is_finite() {
                    return None;
                }
                key.push_str(&format!("[{}]", rounded as i64));
            }
            dae::Subscript::Colon => return None,
        }
    }
    Some(key)
}

fn extract_assignment_from_simple_residual<'a>(
    expr: &'a dae::Expression,
    constants: &HashMap<String, f64>,
) -> Option<(String, &'a dae::Expression)> {
    let dae::Expression::Binary {
        op: rumoca_ir_core::OpBinary::Sub(_),
        lhs,
        rhs,
    } = expr
    else {
        return None;
    };

    let lhs_key = if let dae::Expression::VarRef { name, subscripts } = lhs.as_ref() {
        canonical_var_ref_key(name, subscripts, constants)
    } else {
        None
    };
    let rhs_key = if let dae::Expression::VarRef { name, subscripts } = rhs.as_ref() {
        canonical_var_ref_key(name, subscripts, constants)
    } else {
        None
    };

    match (lhs_key, rhs_key) {
        (Some(target), _) => Some((target, rhs.as_ref())),
        (None, Some(target)) => Some((target, lhs.as_ref())),
        (None, None) => None,
    }
}

fn collect_assignments_from_residual<'a>(
    expr: &'a dae::Expression,
    constants: &HashMap<String, f64>,
    out: &mut Vec<(String, &'a dae::Expression)>,
) {
    match expr {
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            for (_, value) in branches {
                collect_assignments_from_residual(value, constants, out);
            }
            collect_assignments_from_residual(else_branch, constants, out);
        }
        _ => {
            if let Some((target, source)) = extract_assignment_from_simple_residual(expr, constants)
            {
                out.push((target, source));
            }
        }
    }
}

fn collect_assignment_sources<'a>(
    eq: &'a dae::Equation,
    constants: &HashMap<String, f64>,
    out: &mut Vec<(String, &'a dae::Expression)>,
) {
    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(key) = canonical_var_ref_key(lhs, &[], constants)
    {
        out.push((key, &eq.rhs));
        return;
    }
    collect_assignments_from_residual(&eq.rhs, constants, out);
}

fn build_clock_source_map<'a>(
    dae_model: &'a dae::Dae,
    constants: &HashMap<String, f64>,
) -> SourceMap<'a> {
    let mut sources = HashMap::new();
    for eq in dae_model
        .f_z
        .iter()
        .chain(dae_model.f_m.iter())
        .chain(dae_model.f_x.iter())
    {
        let mut assignment_sources = Vec::new();
        collect_assignment_sources(eq, constants, &mut assignment_sources);
        for (target, source) in assignment_sources {
            sources.entry(target).or_insert_with(Vec::new).push(source);
        }
    }
    sources
}

fn infer_clock_intervals_by_variable(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    clock_schedules: &[dae::ClockSchedule],
) -> IndexMap<String, f64> {
    let sources = build_clock_source_map(dae_model, constants);
    let mut intervals = IndexMap::new();

    for name in dae_model
        .discrete_reals
        .keys()
        .chain(dae_model.discrete_valued.keys())
    {
        let expr = dae::Expression::VarRef {
            name: name.clone(),
            subscripts: Vec::new(),
        };
        if let Some((period, _phase)) =
            infer_clock_timing_from_expr(&expr, constants, &sources, 24, &mut HashSet::new())
            && period.is_finite()
            && period > 0.0
        {
            intervals.insert(name.as_str().to_string(), period);
        }
    }

    if let Some(fallback_period) = unique_static_clock_period(clock_schedules) {
        add_implicit_sample_fallback_intervals(
            dae_model,
            constants,
            &sources,
            fallback_period,
            &mut intervals,
        );
    }

    intervals
}

fn unique_static_clock_period(clock_schedules: &[dae::ClockSchedule]) -> Option<f64> {
    let [schedule] = clock_schedules else {
        return None;
    };
    if schedule.period_seconds.is_finite() && schedule.period_seconds > 0.0 {
        Some(schedule.period_seconds)
    } else {
        None
    }
}

fn add_implicit_sample_fallback_intervals(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    fallback_period: f64,
    intervals: &mut IndexMap<String, f64>,
) {
    // MLS §16 (synchronous language elements): sample(u) may use an implicit
    // clock. If a model has one unique static periodic schedule, apply that
    // period only for unresolved variables whose defining expression contains
    // an implicit one-argument sample(..) form.
    for name in dae_model
        .discrete_reals
        .keys()
        .chain(dae_model.discrete_valued.keys())
    {
        if intervals.contains_key(name.as_str()) {
            continue;
        }
        if !variable_has_implicit_clock_source(name, constants, sources) {
            continue;
        }
        intervals.insert(name.as_str().to_string(), fallback_period);
    }
}

fn variable_has_implicit_clock_source(
    name: &dae::VarName,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    let by_key = canonical_var_ref_key(name, &[], constants)
        .and_then(|key| sources.get(&key))
        .is_some_and(|exprs| {
            exprs
                .iter()
                .any(|expr| expression_uses_implicit_clock_sample(expr))
        });
    if by_key {
        return true;
    }
    sources.get(name.as_str()).is_some_and(|exprs| {
        exprs
            .iter()
            .any(|expr| expression_uses_implicit_clock_sample(expr))
    })
}

fn expression_uses_implicit_clock_sample(expr: &dae::Expression) -> bool {
    ImplicitSampleChecker::check(expr)
}

fn infer_clock_timing_next(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    if remaining_depth == 0 {
        return None;
    }
    infer_clock_timing_from_expr(
        expr,
        constants,
        sources,
        remaining_depth.saturating_sub(1),
        visiting,
    )
}

fn infer_clock_counter_form(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    if remaining_depth == 0 {
        return None;
    }
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            if short != "Clock" || args.len() != 1 {
                return None;
            }
            let raw = eval_clock_scalar_with_sources(
                args.first()?,
                constants,
                sources,
                remaining_depth.saturating_sub(1),
                visiting,
            )?;
            let rounded = raw.round();
            let tol = 1.0e-9 * rounded.abs().max(1.0);
            if !rounded.is_finite() || rounded <= 0.0 || (raw - rounded).abs() > tol {
                return None;
            }
            Some(rounded)
        }
        dae::Expression::VarRef { name, subscripts } => {
            let key = canonical_var_ref_key(name, subscripts, constants)?;
            if !visiting.insert(key.clone()) {
                return None;
            }
            let inferred = sources.get(&key).and_then(|source_exprs| {
                source_exprs.iter().find_map(|source| {
                    infer_clock_counter_form(
                        source,
                        constants,
                        sources,
                        remaining_depth.saturating_sub(1),
                        visiting,
                    )
                })
            });
            visiting.remove(&key);
            inferred
        }
        _ => None,
    }
}

fn infer_clock_constructor_timing(
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let first = args.first()?;
    if let Some(base) =
        infer_clock_timing_next(first, constants, sources, remaining_depth, visiting)
    {
        return Some(base);
    }
    if args.len() >= 2 {
        let count =
            eval_clock_scalar_with_sources(first, constants, sources, remaining_depth, visiting)?;
        let resolution = eval_clock_scalar_with_sources(
            &args[1],
            constants,
            sources,
            remaining_depth,
            visiting,
        )?;
        if resolution.is_finite() && resolution > 0.0 {
            return valid_positive_period(count / resolution).map(|period| (period, 0.0));
        }
    }
    let period =
        eval_clock_scalar_with_sources(first, constants, sources, remaining_depth, visiting)?;
    valid_positive_period(period).map(|period| (period, 0.0))
}

fn infer_subsample_timing(
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    if let Some(counter) =
        infer_clock_counter_form(args.first()?, constants, sources, remaining_depth, visiting)
    {
        let resolution =
            eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)
                .unwrap_or(1.0);
        return valid_positive_period(counter / resolution).map(|period| (period, 0.0));
    }
    let base =
        infer_clock_timing_next(args.first()?, constants, sources, remaining_depth, visiting)?;
    let factor = eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)
        .unwrap_or(1.0);
    valid_positive_period(base.0 * factor).map(|period| (period, base.1))
}

fn infer_supersample_timing(
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let base =
        infer_clock_timing_next(args.first()?, constants, sources, remaining_depth, visiting)?;
    let factor = eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)
        .unwrap_or(1.0);
    valid_positive_period(base.0 / factor).map(|period| (period, base.1))
}

fn infer_shift_like_timing(
    short: &str,
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let base =
        infer_clock_timing_next(args.first()?, constants, sources, remaining_depth, visiting)?;
    let shift = eval_clock_scalar_with_sources(
        args.get(1).unwrap_or(args.first()?),
        constants,
        sources,
        remaining_depth,
        visiting,
    )?;
    let offset = if args.len() >= 3 {
        let resolution = eval_clock_scalar_with_sources(
            &args[2],
            constants,
            sources,
            remaining_depth,
            visiting,
        )?;
        if resolution.is_finite() && resolution != 0.0 {
            shift / resolution
        } else {
            shift * base.0
        }
    } else {
        shift * base.0
    };
    let phase = if short == "shiftSample" {
        base.1 + offset
    } else {
        base.1 - offset
    };
    valid_positive_period(base.0).map(|period| (period, phase))
}

fn infer_clock_timing_from_clock_function(
    short: &str,
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    match short {
        "Clock" => {
            infer_clock_constructor_timing(args, constants, sources, remaining_depth, visiting)
        }
        "subSample" => infer_subsample_timing(args, constants, sources, remaining_depth, visiting),
        "superSample" => {
            infer_supersample_timing(args, constants, sources, remaining_depth, visiting)
        }
        "shiftSample" | "backSample" => {
            infer_shift_like_timing(short, args, constants, sources, remaining_depth, visiting)
        }
        _ => None,
    }
}

fn infer_clock_timing_from_expr_list(
    exprs: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    exprs.iter().find_map(|expr| {
        infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)
    })
}

fn infer_clock_timing_from_builtin_call(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    match function {
        dae::BuiltinFunction::Sample if args.len() >= 2 => {
            infer_clock_timing_next(&args[1], constants, sources, remaining_depth, visiting)
        }
        dae::BuiltinFunction::Pre if !args.is_empty() => {
            infer_clock_timing_next(&args[0], constants, sources, remaining_depth, visiting)
        }
        _ => infer_clock_timing_from_expr_list(args, constants, sources, remaining_depth, visiting),
    }
}

fn infer_clock_timing_from_var_ref(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let key = canonical_var_ref_key(name, subscripts, constants)?;
    if !visiting.insert(key.clone()) {
        return None;
    }
    let inferred = infer_clock_timing_from_source_entries(
        sources.get(&key),
        constants,
        sources,
        remaining_depth,
        visiting,
    )
    .or_else(|| {
        (!subscripts.is_empty()).then(|| {
            infer_clock_timing_from_source_entries(
                sources.get(name.as_str()),
                constants,
                sources,
                remaining_depth,
                visiting,
            )
        })?
    })
    .or_else(|| {
        infer_clock_timing_from_reverse_alias_sources(
            &key,
            constants,
            sources,
            remaining_depth,
            visiting,
        )
    });
    visiting.remove(&key);
    inferred
}

fn infer_clock_timing_from_source_entries(
    source_exprs: Option<&Vec<&dae::Expression>>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    source_exprs.and_then(|exprs| {
        exprs.iter().find_map(|expr| {
            infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)
        })
    })
}

fn infer_clock_timing_from_reverse_alias_sources(
    key: &str,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    sources.iter().find_map(|(target, source_exprs)| {
        if target.contains('[') {
            return None;
        }
        let alias_hit = source_exprs.iter().any(|expr| {
            if let dae::Expression::VarRef { name, subscripts } = expr {
                canonical_var_ref_key(name, subscripts, constants).as_deref() == Some(key)
            } else {
                false
            }
        });
        if !alias_hit {
            return None;
        }
        infer_clock_timing_next(
            &dae::Expression::VarRef {
                name: dae::VarName::new(target.as_str()),
                subscripts: vec![],
            },
            constants,
            sources,
            remaining_depth,
            visiting,
        )
    })
}

fn infer_clock_timing_from_if_expr(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    for (condition, value) in branches {
        if eval_scalar_const_expr(condition, constants).is_some_and(|flag| flag != 0.0) {
            return infer_clock_timing_next(value, constants, sources, remaining_depth, visiting);
        }
    }
    branches
        .iter()
        .find_map(|(_, value)| {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        })
        .or_else(|| {
            infer_clock_timing_next(else_branch, constants, sources, remaining_depth, visiting)
        })
}

fn infer_clock_timing_from_subscripts(
    subscripts: &[dae::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    subscripts.iter().find_map(|sub| {
        if let dae::Subscript::Expr(value) = sub {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        } else {
            None
        }
    })
}

fn infer_clock_timing_from_range(
    start: &dae::Expression,
    step: Option<&dae::Expression>,
    end: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    infer_clock_timing_next(start, constants, sources, remaining_depth, visiting)
        .or_else(|| {
            step.and_then(|value| {
                infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
            })
        })
        .or_else(|| infer_clock_timing_next(end, constants, sources, remaining_depth, visiting))
}

fn infer_clock_timing_from_comprehension(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)
        .or_else(|| {
            indices.iter().find_map(|idx| {
                infer_clock_timing_next(&idx.range, constants, sources, remaining_depth, visiting)
            })
        })
        .or_else(|| {
            filter.and_then(|value| {
                infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
            })
        })
}

fn infer_clock_timing_from_expr(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    if remaining_depth == 0 {
        return None;
    }
    infer_clock_timing_from_expr_inner(expr, constants, sources, remaining_depth, visiting)
}

fn infer_clock_timing_from_function_call_expr(
    name: &dae::VarName,
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    infer_clock_timing_from_clock_function(
        short,
        args,
        constants,
        sources,
        remaining_depth,
        visiting,
    )
    .or_else(|| {
        infer_clock_timing_from_expr_list(args, constants, sources, remaining_depth, visiting)
    })
}

fn infer_clock_timing_from_index_expr(
    base: &dae::Expression,
    subscripts: &[dae::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    infer_clock_timing_next(base, constants, sources, remaining_depth, visiting).or_else(|| {
        infer_clock_timing_from_subscripts(
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        )
    })
}

fn infer_clock_timing_from_expr_inner(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            infer_clock_timing_from_function_call_expr(
                name,
                args,
                constants,
                sources,
                remaining_depth,
                visiting,
            )
        }
        dae::Expression::BuiltinCall { function, args } => infer_clock_timing_from_builtin_call(
            *function,
            args,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::VarRef { name, subscripts } => infer_clock_timing_from_var_ref(
            name,
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::If {
            branches,
            else_branch,
        } => infer_clock_timing_from_if_expr(
            branches,
            else_branch,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Binary { lhs, rhs, .. } => {
            infer_clock_timing_next(lhs, constants, sources, remaining_depth, visiting).or_else(
                || infer_clock_timing_next(rhs, constants, sources, remaining_depth, visiting),
            )
        }
        dae::Expression::Unary { rhs, .. } | dae::Expression::FieldAccess { base: rhs, .. } => {
            infer_clock_timing_next(rhs, constants, sources, remaining_depth, visiting)
        }
        dae::Expression::Index { base, subscripts } => infer_clock_timing_from_index_expr(
            base,
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            infer_clock_timing_from_expr_list(
                elements,
                constants,
                sources,
                remaining_depth,
                visiting,
            )
        }
        dae::Expression::Range { start, step, end } => infer_clock_timing_from_range(
            start,
            step.as_deref(),
            end,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => infer_clock_timing_from_comprehension(
            expr,
            indices,
            filter.as_deref(),
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Literal(_) | dae::Expression::Empty => None,
    }
}

struct ClockConstructorExprCollector<'a> {
    constants: &'a HashMap<String, f64>,
    out: &'a mut Vec<dae::Expression>,
}

impl ExpressionVisitor for ClockConstructorExprCollector<'_> {
    fn visit_function_call(
        &mut self,
        name: &dae::VarName,
        args: &[dae::Expression],
        is_constructor: bool,
    ) {
        let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
        if is_clock_constructor_function_name(short) {
            self.out.push(dae::Expression::FunctionCall {
                name: name.clone(),
                args: args.to_vec(),
                is_constructor,
            });
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_if(
        &mut self,
        branches: &[(dae::Expression, dae::Expression)],
        else_branch: &dae::Expression,
    ) {
        for (cond, value) in branches {
            let cond_value = eval_scalar_const_expr(cond, self.constants);
            if cond_value == Some(0.0) {
                continue;
            }
            if cond_value.is_some() {
                self.visit_expression(value);
                return;
            }
            self.visit_expression(cond);
            self.visit_expression(value);
        }
        self.visit_expression(else_branch);
    }
}

fn collect_clock_constructor_exprs(
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
    out: &mut Vec<dae::Expression>,
) {
    let mut collector = ClockConstructorExprCollector { constants, out };
    collector.visit_expression(expr);
}
