use super::ToDaeError;
use super::expression_identity::UniqueExpressions;
use super::{eval_scalar_const_expr, extract_time_event_instant};
use indexmap::{IndexMap, IndexSet};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;
use rumoca_ir_dae::{ImplicitSampleChecker, VarRefWithSubscriptsCollector};

mod event_clock;
mod timing_inference;

use timing_inference::*;

struct SourceMap<'a> {
    forward: HashMap<String, Vec<&'a rumoca_core::Expression>>,
    reverse_alias: HashMap<String, Vec<String>>,
    timing_cache: RefCell<HashMap<String, Option<(f64, f64)>>>,
    scalar_cache: RefCell<HashMap<String, Option<f64>>>,
}

impl<'a> SourceMap<'a> {
    fn new(forward: HashMap<String, Vec<&'a rumoca_core::Expression>>) -> Self {
        Self {
            forward,
            reverse_alias: HashMap::new(),
            timing_cache: RefCell::new(HashMap::new()),
            scalar_cache: RefCell::new(HashMap::new()),
        }
    }

    fn get(&self, key: &str) -> Option<&Vec<&'a rumoca_core::Expression>> {
        self.forward.get(key)
    }

    fn reverse_targets_for(&self, key: &str) -> Option<&Vec<String>> {
        self.reverse_alias.get(key)
    }
}
type ClockRuntimeMetadata = (
    Vec<rumoca_core::Expression>,
    Vec<dae::ClockSchedule>,
    IndexMap<String, f64>,
    IndexMap<String, dae::ClockSchedule>,
    Vec<rumoca_core::Expression>,
);

/// Extract the condition expression from `Clock(condition)` event-clock constructors.
fn extract_event_clock_condition(
    expr: &rumoca_core::Expression,
) -> Option<rumoca_core::Expression> {
    if let rumoca_core::Expression::FunctionCall { args, .. } = expr {
        args.first().cloned()
    } else {
        None
    }
}

pub(super) fn compute_clock_runtime_metadata(
    dae_model: &dae::Dae,
    compile_time_scalars: &HashMap<String, f64>,
) -> Result<ClockRuntimeMetadata, ToDaeError> {
    if !contains_clock_runtime_constructs(dae_model, compile_time_scalars) {
        return Ok(empty_clock_runtime_metadata());
    }

    let mut clock_constructor_exprs = Vec::new();
    for eq in clock_runtime_equations(dae_model) {
        let mut equation_constructors = Vec::new();
        collect_clock_constructor_exprs(&eq.rhs, compile_time_scalars, &mut equation_constructors);
        extend_unique_expressions(&mut clock_constructor_exprs, equation_constructors);
    }
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
            if event_clock::is_non_static_event_clock_constructor(
                expr,
                dae_model,
                compile_time_scalars,
                &clock_sources,
            ) {
                // Extract the condition expression from Clock(condition)
                triggered_clock_conditions.extend(extract_event_clock_condition(expr));
                continue;
            }
            if is_non_static_inferred_clock_composition(expr, compile_time_scalars, &clock_sources)
            {
                continue;
            }
            unresolved_clock_exprs.push(format_unresolved_clock_expr(
                expr,
                dae_model,
                compile_time_scalars,
                &clock_sources,
            )?);
            continue;
        };
        clock_schedules.push(dae::ClockSchedule {
            period_seconds: period,
            phase_seconds: phase,
            source_span: expr.span().unwrap_or(rumoca_core::Span::DUMMY),
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
    let event_clock_variables =
        event_clock::event_clock_variable_names(dae_model, compile_time_scalars, &clock_sources);
    let mut clock_timings =
        infer_clock_timings_by_variable(dae_model, compile_time_scalars, &clock_schedules);
    clock_timings.retain(|name, _| !event_clock_variables.contains(name));
    let clock_intervals = clock_timings
        .iter()
        .map(|(name, timing)| (name.clone(), timing.period_seconds))
        .collect();

    Ok((
        clock_constructor_exprs,
        clock_schedules,
        clock_intervals,
        clock_timings,
        triggered_clock_conditions,
    ))
}

fn empty_clock_runtime_metadata() -> ClockRuntimeMetadata {
    (
        Vec::new(),
        Vec::new(),
        IndexMap::new(),
        IndexMap::new(),
        Vec::new(),
    )
}

fn clock_runtime_equations(dae_model: &dae::Dae) -> impl Iterator<Item = &dae::Equation> {
    dae_model
        .continuous
        .equations
        .iter()
        .chain(dae_model.initialization.equations.iter())
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.conditions.equations.iter())
}

fn contains_clock_runtime_constructs(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
) -> bool {
    clock_runtime_equations(dae_model)
        .any(|eq| expression_contains_clock_runtime_construct(&eq.rhs, constants))
}

fn expression_contains_clock_runtime_construct(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> bool {
    let mut constructors = Vec::new();
    collect_clock_constructor_exprs(expr, constants, &mut constructors);
    !constructors.is_empty()
}

fn unresolved_clock_debug_enabled() -> bool {
    #[cfg(feature = "tracing")]
    {
        tracing::enabled!(target: "rumoca_phase_dae::clock", tracing::Level::DEBUG)
    }
    #[cfg(not(feature = "tracing"))]
    {
        false
    }
}

fn format_unresolved_clock_expr(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> Result<String, ToDaeError> {
    if !unresolved_clock_debug_enabled() {
        return Ok(format!("{expr:?}"));
    }
    let context = summarize_unresolved_clock_context(expr, dae_model, constants, sources)?;
    Ok(format!("{expr:?} [{context}]"))
}

fn summarize_unresolved_clock_context(
    expr: &rumoca_core::Expression,
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> Result<String, ToDaeError> {
    let refs = collect_unique_clock_var_refs(expr, constants);
    if refs.is_empty() {
        return Ok("no_var_refs".to_string());
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
                &rumoca_core::Expression::VarRef {
                    name: name.clone().into(),
                    subscripts: subscripts.clone(),
                    span: rumoca_core::Span::DUMMY,
                },
                constants,
                sources,
                24,
                &mut HashSet::new(),
            );
            let (kind, start) =
                dae_var_kind_and_start(dae_model, name.as_str()).ok_or_else(|| {
                    ToDaeError::runtime_contract_violation_at(
                        format!(
                            "clock expression references missing DAE variable `{}`",
                            name.as_str()
                        ),
                        expr.span().unwrap_or(rumoca_core::Span::DUMMY),
                    )
                })?;
            Ok(format!(
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
            ))
        })
        .collect::<Result<Vec<_>, ToDaeError>>()
        .map(|parts| parts.join("; "))
}

fn short_expr(expr: &rumoca_core::Expression, max_len: usize) -> String {
    let rendered = format!("{expr:?}");
    if rendered.len() <= max_len {
        return rendered;
    }
    format!("{}...", &rendered[..max_len])
}

fn dae_var_kind_and_start(
    dae_model: &dae::Dae,
    name: &str,
) -> Option<(&'static str, Option<String>)> {
    let lookup = |kind: &'static str,
                  vars: &indexmap::IndexMap<rumoca_core::VarName, dae::Variable>|
     -> Option<(&'static str, Option<String>)> {
        vars.get(&rumoca_core::VarName::new(name))
            .map(|var| (kind, var.start.as_ref().map(|expr| short_expr(expr, 320))))
    };

    lookup("parameter", &dae_model.variables.parameters)
        .or_else(|| lookup("constant", &dae_model.variables.constants))
        .or_else(|| lookup("input", &dae_model.variables.inputs))
        .or_else(|| lookup("discrete_real", &dae_model.variables.discrete_reals))
        .or_else(|| lookup("discrete_valued", &dae_model.variables.discrete_valued))
        .or_else(|| lookup("state", &dae_model.variables.states))
        .or_else(|| lookup("algebraic", &dae_model.variables.algebraics))
        .or_else(|| lookup("output", &dae_model.variables.outputs))
}

fn collect_unique_clock_var_refs(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
) -> Vec<(rumoca_core::VarName, Vec<rumoca_core::Subscript>, String)> {
    let mut collector = VarRefWithSubscriptsCollector::new();
    collector.visit_expression(expr);
    let refs = collector.into_refs();
    let mut seen = HashSet::new();
    refs.into_iter()
        .filter_map(|(name, subscripts)| {
            let key = canonical_var_name_key(&name, &subscripts, constants)?;
            if seen.insert(key.clone()) {
                Some((name, subscripts, key))
            } else {
                None
            }
        })
        .collect()
}

pub(super) fn extend_unique_expressions(
    expressions: &mut Vec<rumoca_core::Expression>,
    candidates: impl IntoIterator<Item = rumoca_core::Expression>,
) {
    let existing = std::mem::take(expressions);
    let mut unique = UniqueExpressions::default();
    for expr in existing {
        unique.push(expr);
    }
    for candidate in candidates {
        unique.push(candidate);
    }
    *expressions = unique.into_vec();
}

pub(super) fn collect_synthetic_root_conditions_expr(
    expr: &rumoca_core::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    out: &mut UniqueExpressions,
) {
    let mut collector = SyntheticRootConditionCollector {
        suppress_events,
        constants,
        out,
    };
    collector.visit_expression(expr);
}

struct SyntheticRootConditionCollector<'a> {
    suppress_events: bool,
    constants: &'a HashMap<String, f64>,
    out: &'a mut UniqueExpressions,
}

impl SyntheticRootConditionCollector<'_> {
    fn visit_with_event_suppression(&mut self, args: &[rumoca_core::Expression]) {
        let previous = self.suppress_events;
        self.suppress_events = true;
        for arg in args {
            self.visit_expression(arg);
        }
        self.suppress_events = previous;
    }
}

impl ExpressionVisitor for SyntheticRootConditionCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        match expr {
            rumoca_core::Expression::If { branches, .. } => {
                for (cond, _) in branches {
                    push_relation_root_if_event_condition(
                        cond,
                        self.suppress_events,
                        self.constants,
                        self.out,
                    );
                }
                self.walk_expression(expr);
            }
            rumoca_core::Expression::Binary { .. } => {
                push_relation_root_if_event_condition(
                    expr,
                    self.suppress_events,
                    self.constants,
                    self.out,
                );
                self.walk_expression(expr);
            }
            rumoca_core::Expression::BuiltinCall {
                function: rumoca_core::BuiltinFunction::NoEvent,
                args,
                ..
            } => {
                self.visit_with_event_suppression(args);
            }
            rumoca_core::Expression::BuiltinCall { function, args, .. }
                if !self.suppress_events
                    && matches!(
                        function,
                        rumoca_core::BuiltinFunction::Abs | rumoca_core::BuiltinFunction::Sign
                    ) =>
            {
                if let Some(arg) = args.first() {
                    self.out.push(arg.clone());
                }
                self.walk_expression(expr);
            }
            _ => self.walk_expression(expr),
        }
    }
}

fn push_relation_root_if_event_condition(
    expr: &rumoca_core::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    out: &mut UniqueExpressions,
) {
    if suppress_events || extract_time_event_instant(expr, constants).is_some() {
        return;
    }
    if let Some(root) = relation_root_expression(expr) {
        out.push(root);
    }
}

pub(super) fn relation_root_expression(
    expr: &rumoca_core::Expression,
) -> Option<rumoca_core::Expression> {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = expr else {
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
    Some(rumoca_core::Expression::Binary {
        op: rumoca_core::OpBinary::Sub,
        lhs: lhs.clone(),
        rhs: rhs.clone(),
        span: expr.span().unwrap_or(rumoca_core::Span::DUMMY),
    })
}

fn is_clock_constructor_function_name(short: &str) -> bool {
    matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample"
    )
}

fn requires_static_clock_schedule(expr: &rumoca_core::Expression) -> bool {
    if let rumoca_core::Expression::BuiltinCall { function, args, .. } = expr {
        return matches!(function, rumoca_core::BuiltinFunction::Sample) && args.len() >= 2;
    }
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.last_segment();
    if short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
        return args.len() >= 2;
    }
    match short {
        "Clock" => !args.is_empty(),
        "subSample" | "superSample" | "shiftSample" | "backSample" => true,
        _ => true,
    }
}

fn is_non_static_inferred_clock_composition(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    let short = name.last_segment();
    if !matches!(
        short,
        "subSample" | "superSample" | "shiftSample" | "backSample"
    ) {
        return false;
    }
    if matches!(short, "subSample" | "superSample") && args.len() == 1 {
        return true;
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
    expr: &rumoca_core::Expression,
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
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    let key = canonical_var_ref_key(name, subscripts, constants)?;
    if let Some(cached) = sources.scalar_cache.borrow().get(&key).cloned() {
        return cached;
    }
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
    sources.scalar_cache.borrow_mut().insert(key, inferred);
    inferred
}

fn eval_clock_scalar_with_sources(
    expr: &rumoca_core::Expression,
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
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => eval_clock_scalar_from_var_ref(
            name,
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::Unary {
            op: rumoca_core::OpUnary::Minus,
            rhs,
            ..
        } => eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)
            .map(|value| -value),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Add,
            lhs,
            rhs,
            ..
        } => Some(
            eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?
                + eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?,
        ),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Sub,
            lhs,
            rhs,
            ..
        } => Some(
            eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?
                - eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?,
        ),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => Some(
            eval_clock_scalar_child(lhs, constants, sources, remaining_depth, visiting)?
                * eval_clock_scalar_child(rhs, constants, sources, remaining_depth, visiting)?,
        ),
        rumoca_core::Expression::Binary {
            op: rumoca_core::OpBinary::Div,
            lhs,
            rhs,
            ..
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
    expr: Option<&rumoca_core::Expression>,
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
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
) -> Option<String> {
    canonical_var_name_key(name.var_name(), subscripts, constants)
}

pub(super) fn canonical_var_name_key(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
) -> Option<String> {
    if subscripts.is_empty() {
        return Some(name.as_str().to_string());
    }

    let mut key = name.as_str().to_string();
    for subscript in subscripts {
        match subscript {
            rumoca_core::Subscript::Index { value: index, .. } => {
                key.push('[');
                let _ = write!(&mut key, "{index}");
                key.push(']');
            }
            rumoca_core::Subscript::Expr { expr, .. } => {
                let raw = eval_scalar_const_expr(expr, constants)?;
                let rounded = raw.round();
                if !rounded.is_finite() {
                    return None;
                }
                key.push('[');
                let _ = write!(&mut key, "{}", rounded as i64);
                key.push(']');
            }
            rumoca_core::Subscript::Colon { .. } => return None,
        }
    }
    Some(key)
}

fn collect_assignments_from_residual<'a>(
    expr: &'a rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    out: &mut Vec<(String, &'a rumoca_core::Expression)>,
) {
    match expr {
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (_, value) in branches {
                collect_assignments_from_residual(value, constants, out);
            }
            collect_assignments_from_residual(else_branch, constants, out);
        }
        _ => {
            let rumoca_core::Expression::Binary {
                op: rumoca_core::OpBinary::Sub,
                lhs,
                rhs,
                span: _,
            } = expr
            else {
                return;
            };

            let lhs_key = if let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = lhs.as_ref()
            {
                canonical_var_ref_key(name, subscripts, constants)
            } else {
                None
            };
            let rhs_key = if let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = rhs.as_ref()
            {
                canonical_var_ref_key(name, subscripts, constants)
            } else {
                None
            };

            match (lhs_key, rhs_key) {
                (Some(lhs_key), Some(rhs_key)) => {
                    out.push((lhs_key, rhs.as_ref()));
                    out.push((rhs_key, lhs.as_ref()));
                }
                (Some(target), None) => out.push((target, rhs.as_ref())),
                (None, Some(target)) => out.push((target, lhs.as_ref())),
                (None, None) => {}
            }
        }
    }
}

fn collect_assignment_sources<'a>(
    eq: &'a dae::Equation,
    constants: &HashMap<String, f64>,
    out: &mut Vec<(String, &'a rumoca_core::Expression)>,
) {
    if let Some(lhs) = eq.lhs.as_ref()
        && let Some(key) = canonical_var_name_key(lhs, &[], constants)
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
    let mut forward = HashMap::new();
    let mut assignment_sources = Vec::new();
    for eq in dae_model
        .discrete
        .real_updates
        .iter()
        .chain(dae_model.discrete.valued_updates.iter())
        .chain(dae_model.continuous.equations.iter())
    {
        assignment_sources.clear();
        collect_assignment_sources(eq, constants, &mut assignment_sources);
        for (target, source) in assignment_sources.iter() {
            forward
                .entry(target.clone())
                .or_insert_with(Vec::new)
                .push(*source);
        }
    }
    let mut sources = SourceMap::new(forward);
    sources.reverse_alias = build_reverse_alias_index(&sources.forward, constants);
    sources
}

fn build_reverse_alias_index(
    forward: &HashMap<String, Vec<&rumoca_core::Expression>>,
    constants: &HashMap<String, f64>,
) -> HashMap<String, Vec<String>> {
    let mut reverse = HashMap::new();
    for (target, source_exprs) in forward {
        if rumoca_core::has_top_level_subscript(target) {
            continue;
        }
        for expr in source_exprs {
            let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = expr
            else {
                continue;
            };
            let Some(source_key) = canonical_var_ref_key(name, subscripts, constants) else {
                continue;
            };
            let targets = reverse.entry(source_key).or_insert_with(Vec::new);
            if !targets.iter().any(|existing| existing == target) {
                targets.push(target.clone());
            }
        }
    }
    reverse
}

fn propagate_clock_timings_across_equations(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    timings: &mut IndexMap<String, dae::ClockSchedule>,
) {
    let candidates = clock_interval_candidate_names(dae_model)
        .into_iter()
        .filter_map(|name| canonical_var_name_key(name, &[], constants))
        .collect::<HashSet<_>>();
    let graph = build_clock_equivalence_graph(dae_model, constants, &candidates);
    let mut seen = HashSet::new();
    for key in candidates {
        if seen.contains(&key) {
            continue;
        }
        let component = collect_clock_equivalence_component(&key, &graph, &mut seen);
        let Some(timing) = unique_component_clock_timing(&component, timings) else {
            continue;
        };
        for item in component {
            if let Some(existing) = timings.get(&item) {
                assert_compatible_clock_schedule(&item, existing, &timing);
            } else {
                timings.insert(item, timing.clone());
            }
        }
    }
}

fn assert_compatible_clock_schedule(
    item: &str,
    existing: &dae::ClockSchedule,
    timing: &dae::ClockSchedule,
) {
    assert!(
        existing.period_seconds == timing.period_seconds
            && existing.phase_seconds == timing.phase_seconds,
        "conflicting clock schedules for `{item}`: \
         existing period={}/phase={} vs new period={}/phase={} — \
         check that all clock sources are consistent",
        existing.period_seconds,
        existing.phase_seconds,
        timing.period_seconds,
        timing.phase_seconds,
    );
}

fn build_clock_equivalence_graph(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
) -> HashMap<String, Vec<String>> {
    let mut graph = HashMap::new();
    let mut assignment_sources = Vec::new();
    for eq in dae_model
        .continuous
        .equations
        .iter()
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
    {
        assignment_sources.clear();
        collect_assignment_sources(eq, constants, &mut assignment_sources);
        for (target, source) in assignment_sources.iter() {
            add_clock_equivalence_edges(target, source, constants, candidates, &mut graph);
        }
    }
    add_shared_no_argument_clock_guard_edges(dae_model, constants, candidates, &mut graph);
    graph
}

fn add_shared_no_argument_clock_guard_edges(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
    graph: &mut HashMap<String, Vec<String>>,
) {
    let mut guarded_targets = IndexMap::<rumoca_core::Span, IndexSet<String>>::new();
    for eq in dae_model
        .continuous
        .equations
        .iter()
        .chain(dae_model.discrete.real_updates.iter())
        .chain(dae_model.discrete.valued_updates.iter())
    {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        let Some(target) = canonical_var_name_key(lhs, &[], constants) else {
            continue;
        };
        if !candidates.contains(&target) {
            continue;
        }
        let clock_spans = no_argument_clock_guard_spans(&eq.rhs);
        for span in clock_spans {
            guarded_targets
                .entry(span)
                .or_default()
                .insert(target.clone());
        }
    }
    for targets in guarded_targets.values() {
        let mut targets = targets.iter();
        let Some(first) = targets.next() else {
            continue;
        };
        for target in targets {
            insert_clock_equivalence_edge(graph, first, target);
            insert_clock_equivalence_edge(graph, target, first);
        }
    }
}

fn no_argument_clock_guard_spans(expr: &rumoca_core::Expression) -> Vec<rumoca_core::Span> {
    let mut collector = NoArgumentClockGuardCollector {
        in_if_condition: false,
        spans: Vec::new(),
    };
    collector.visit_expression(expr);
    collector.spans
}

struct NoArgumentClockGuardCollector {
    in_if_condition: bool,
    spans: Vec<rumoca_core::Span>,
}

impl ExpressionVisitor for NoArgumentClockGuardCollector {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.in_if_condition
            && let rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } = expr
            && name.last_segment() == "Clock"
            && args.is_empty()
        {
            let clock_span = if *span != rumoca_core::Span::DUMMY {
                Some(*span)
            } else {
                name.component_ref()
                    .map(|component_ref| component_ref.span)
                    .filter(|span| *span != rumoca_core::Span::DUMMY)
            };
            if let Some(span) = clock_span
                && !self.spans.contains(&span)
            {
                self.spans.push(span);
            }
            return;
        }
        self.walk_expression(expr);
    }

    fn visit_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
    ) {
        for (condition, value) in branches {
            let old = self.in_if_condition;
            self.in_if_condition = true;
            self.visit_expression(condition);
            self.in_if_condition = old;
            self.visit_expression(value);
        }
        self.visit_expression(else_branch);
    }
}

fn add_clock_equivalence_edges(
    target: &str,
    source: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
    graph: &mut HashMap<String, Vec<String>>,
) {
    if !candidates.contains(target) {
        return;
    }
    let mut refs = Vec::new();
    collect_same_clock_var_refs(source, constants, candidates, &mut refs);
    for rhs in refs {
        insert_clock_equivalence_edge(graph, target, rhs.as_str());
        insert_clock_equivalence_edge(graph, rhs.as_str(), target);
    }
}

fn insert_clock_equivalence_edge(graph: &mut HashMap<String, Vec<String>>, lhs: &str, rhs: &str) {
    let edges = graph.entry(lhs.to_string()).or_default();
    if !edges.iter().any(|existing| existing == rhs) {
        edges.push(rhs.to_string());
    }
}

fn collect_same_clock_var_refs(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
    out: &mut Vec<String>,
) {
    let mut collector = SameClockVarRefCollector {
        constants,
        candidates,
        out,
    };
    collector.visit_expression(expr);
}

struct SameClockVarRefCollector<'a> {
    constants: &'a HashMap<String, f64>,
    candidates: &'a HashSet<String>,
    out: &'a mut Vec<String>,
}

impl ExpressionVisitor for SameClockVarRefCollector<'_> {
    fn visit_var_ref(
        &mut self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
    ) {
        collect_same_clock_var_ref(
            name.var_name(),
            subscripts,
            self.constants,
            self.candidates,
            self.out,
        );
        for subscript in subscripts {
            self.visit_subscript(subscript);
        }
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        _is_constructor: bool,
    ) {
        collect_same_clock_function_refs(
            name.var_name(),
            args,
            self.constants,
            self.candidates,
            self.out,
        );
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        collect_same_clock_builtin_refs(*function, args, self.constants, self.candidates, self.out);
    }
}

fn collect_same_clock_var_ref(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
    out: &mut Vec<String>,
) {
    let Some(key) = canonical_var_name_key(name, subscripts, constants) else {
        return;
    };
    if candidates.contains(&key) && !out.iter().any(|existing| existing == &key) {
        out.push(key);
    }
}

fn collect_same_clock_function_refs(
    name: &rumoca_core::VarName,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
    out: &mut Vec<String>,
) {
    let short = name.last_segment();
    let first_arg_is_different_clock = matches!(
        short,
        "superSample" | "subSample" | "shiftSample" | "backSample"
    );
    let skip_value_arg =
        first_arg_is_different_clock || short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME;
    for (index, arg) in args.iter().enumerate() {
        if skip_value_arg && index == 0 {
            continue;
        }
        collect_same_clock_var_refs(arg, constants, candidates, out);
    }
}

fn collect_same_clock_builtin_refs(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    candidates: &HashSet<String>,
    out: &mut Vec<String>,
) {
    let skip_value_arg = matches!(function, rumoca_core::BuiltinFunction::Sample);
    for (index, arg) in args.iter().enumerate() {
        if skip_value_arg && index == 0 {
            continue;
        }
        collect_same_clock_var_refs(arg, constants, candidates, out);
    }
}

fn collect_clock_equivalence_component(
    key: &str,
    graph: &HashMap<String, Vec<String>>,
    seen: &mut HashSet<String>,
) -> Vec<String> {
    let mut component = Vec::new();
    let mut stack = vec![key.to_string()];
    while let Some(item) = stack.pop() {
        if !seen.insert(item.clone()) {
            continue;
        }
        if let Some(edges) = graph.get(&item) {
            stack.extend(edges.iter().cloned());
        }
        component.push(item);
    }
    component
}

fn unique_component_clock_timing(
    component: &[String],
    timings: &IndexMap<String, dae::ClockSchedule>,
) -> Option<dae::ClockSchedule> {
    let mut unique = None;
    for key in component {
        let Some(timing) = timings.get(key) else {
            continue;
        };
        if let Some(existing) = unique.as_ref()
            && !clock_schedules_equivalent(existing, timing)
        {
            return None;
        }
        unique = Some(timing.clone());
    }
    unique
}

fn clock_schedules_equivalent(lhs: &dae::ClockSchedule, rhs: &dae::ClockSchedule) -> bool {
    (lhs.period_seconds - rhs.period_seconds).abs() <= 1.0e-12
        && (lhs.phase_seconds - rhs.phase_seconds).abs() <= 1.0e-12
}

fn infer_clock_timings_by_variable(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    clock_schedules: &[dae::ClockSchedule],
) -> IndexMap<String, dae::ClockSchedule> {
    let sources = build_clock_source_map(dae_model, constants);
    let mut timings = IndexMap::new();
    let mut visiting = HashSet::new();

    for name in clock_interval_candidate_names(dae_model) {
        visiting.clear();
        if let Some((period, phase)) =
            infer_clock_timing_from_var_name(name, &[], constants, &sources, 24, &mut visiting)
            && period.is_finite()
            && period > 0.0
        {
            timings.insert(
                name.as_str().to_string(),
                dae::ClockSchedule {
                    period_seconds: period,
                    phase_seconds: phase,
                    source_span: sources
                        .get(name.as_str())
                        .and_then(|exprs| exprs.first().and_then(|expr| expr.span()))
                        .unwrap_or(rumoca_core::Span::DUMMY),
                },
            );
        }
    }

    propagate_clock_timings_across_equations(dae_model, constants, &mut timings);

    if let Some(fallback_period) = unique_static_clock_period(clock_schedules) {
        add_implicit_sample_fallback_timings(
            dae_model,
            constants,
            &sources,
            fallback_period,
            &mut timings,
        );
        propagate_clock_timings_across_equations(dae_model, constants, &mut timings);
    }

    timings
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

fn add_implicit_sample_fallback_timings(
    dae_model: &dae::Dae,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    fallback_period: f64,
    timings: &mut IndexMap<String, dae::ClockSchedule>,
) {
    // MLS §16 (synchronous language elements): sample(u) may use an implicit
    // clock. If a model has one unique static periodic schedule, apply that
    // period only for unresolved variables whose defining expression contains
    // an implicit one-argument sample(..) form.
    for name in clock_interval_candidate_names(dae_model) {
        if timings.contains_key(name.as_str()) {
            continue;
        }
        if !variable_has_unique_static_clock_fallback_source(name, constants, sources) {
            continue;
        }
        timings.insert(
            name.as_str().to_string(),
            dae::ClockSchedule {
                period_seconds: fallback_period,
                phase_seconds: 0.0,
                source_span: sources
                    .get(name.as_str())
                    .and_then(|exprs| exprs.first().and_then(|expr| expr.span()))
                    .unwrap_or(rumoca_core::Span::DUMMY),
            },
        );
    }
}

fn variable_has_unique_static_clock_fallback_source(
    name: &rumoca_core::VarName,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
) -> bool {
    variable_source_matches(
        name,
        constants,
        sources,
        expression_uses_unique_static_clock_fallback,
    )
}

fn clock_interval_candidate_names(dae_model: &dae::Dae) -> Vec<&rumoca_core::VarName> {
    dae_model
        .variables
        .states
        .keys()
        .chain(dae_model.variables.algebraics.keys())
        .chain(dae_model.variables.outputs.keys())
        .chain(dae_model.variables.inputs.keys())
        .chain(dae_model.variables.discrete_reals.keys())
        .chain(dae_model.variables.discrete_valued.keys())
        .collect()
}

fn variable_source_matches(
    name: &rumoca_core::VarName,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    predicate: fn(&rumoca_core::Expression) -> bool,
) -> bool {
    let by_key = canonical_var_name_key(name, &[], constants)
        .and_then(|key| sources.get(&key))
        .is_some_and(|exprs| exprs.iter().any(|expr| predicate(expr)));
    if by_key {
        return true;
    }
    sources
        .get(name.as_str())
        .is_some_and(|exprs| exprs.iter().any(|expr| predicate(expr)))
}

fn expression_uses_unique_static_clock_fallback(expr: &rumoca_core::Expression) -> bool {
    ImplicitSampleChecker::check(expr) || expression_uses_no_argument_clock(expr)
}

fn expression_uses_no_argument_clock(expr: &rumoca_core::Expression) -> bool {
    struct Checker {
        found: bool,
    }

    impl ExpressionVisitor for Checker {
        fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
            if !self.found {
                self.walk_expression(expr);
            }
        }

        fn visit_function_call(
            &mut self,
            name: &rumoca_core::Reference,
            args: &[rumoca_core::Expression],
            _is_constructor: bool,
        ) {
            let short = name.last_segment();
            self.found = short == "Clock" && args.is_empty();
            if self.found {
                return;
            }
            for arg in args {
                self.visit_expression(arg);
            }
        }
    }

    let mut checker = Checker { found: false };
    checker.visit_expression(expr);
    checker.found
}
