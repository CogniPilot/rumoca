//! Precompute runtime metadata on DAE so solver backends can stay thin.

use super::ToDaeError;
use std::collections::{HashMap, HashSet};

use rumoca_core::timing::{maybe_elapsed_seconds, maybe_start_timer_if};
use rumoca_ir_dae as dae;

mod clock;

pub(crate) fn populate_runtime_precompute(dae_model: &mut dae::Dae) -> Result<(), ToDaeError> {
    let profile = std::env::var("RUMOCA_TODAE_PROFILE").is_ok();
    let compile_time_scalars = collect_compile_time_scalars(dae_model);

    let synthetic_root_start = maybe_start_timer_if(profile);
    let mut synthetic_roots = Vec::new();
    for eq in &dae_model.f_x {
        clock::collect_synthetic_root_conditions_expr(
            &eq.rhs,
            false,
            &compile_time_scalars,
            &mut synthetic_roots,
        );
    }
    clock::dedupe_expressions_in_place(&mut synthetic_roots);
    if synthetic_root_start.is_some() {
        eprintln!(
            "ToDae runtime_precompute synthetic_roots: {:.3}s",
            maybe_elapsed_seconds(synthetic_root_start)
        );
    }

    let time_event_start = maybe_start_timer_if(profile);
    let mut seen_time_events = HashSet::new();
    let mut scheduled_time_events = Vec::new();
    for eq in dae_model
        .f_x
        .iter()
        .chain(dae_model.f_z.iter())
        .chain(dae_model.f_m.iter())
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
    if time_event_start.is_some() {
        eprintln!(
            "ToDae runtime_precompute scheduled_time_events: {:.3}s",
            maybe_elapsed_seconds(time_event_start)
        );
    }

    let (clock_constructor_exprs, clock_schedules, clock_intervals, triggered_clock_conditions) =
    let clock_metadata_start = maybe_start_timer_if(profile);
    let (clock_constructor_exprs, clock_schedules, clock_intervals) =
        clock::compute_clock_runtime_metadata(dae_model, &compile_time_scalars)?;
    if clock_metadata_start.is_some() {
        eprintln!(
            "ToDae runtime_precompute clock_metadata: {:.3}s",
            maybe_elapsed_seconds(clock_metadata_start)
        );
    }

    let prune_start = maybe_start_timer_if(profile);
    prune_time_only_relation_roots(dae_model, &compile_time_scalars);
    if prune_start.is_some() {
        eprintln!(
            "ToDae runtime_precompute prune_time_only_relations: {:.3}s",
            maybe_elapsed_seconds(prune_start)
        );
    }
    dae_model.synthetic_root_conditions = synthetic_roots;
    dae_model.scheduled_time_events = scheduled_time_events;
    dae_model.clock_constructor_exprs = clock_constructor_exprs;
    dae_model.clock_schedules = clock_schedules;
    dae_model.clock_intervals = clock_intervals;
    dae_model.triggered_clock_conditions = triggered_clock_conditions;
    Ok(())
}

fn prune_time_only_relation_roots(dae_model: &mut dae::Dae, constants: &HashMap<String, f64>) {
    if dae_model.relation.is_empty() || dae_model.f_c.is_empty() {
        return;
    }

    let old_relations = std::mem::take(&mut dae_model.relation);
    let old_conditions = std::mem::take(&mut dae_model.f_c);
    let mut kept_relations = Vec::with_capacity(old_relations.len());
    let mut kept_conditions = Vec::with_capacity(old_conditions.len());

    for (relation, equation) in old_relations.into_iter().zip(old_conditions) {
        if extract_time_event_instant(&relation, constants).is_some() {
            continue;
        }

        let condition_index = kept_relations.len() + 1;
        kept_conditions.push(dae::Equation::explicit(
            dae::VarName::new(format!("c[{condition_index}]")),
            relation.clone(),
            equation.span,
            equation.origin,
        ));
        kept_relations.push(relation);
    }

    dae_model.relation = kept_relations;
    dae_model.f_c = kept_conditions;
}

fn insert_compile_time_scalar(values: &mut HashMap<String, f64>, name: &str, value: f64) -> bool {
    let mut changed = false;
    if values
        .get(name)
        .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
    {
        values.insert(name.to_string(), value);
        changed = true;
    }
    if let Some(base) = dae::component_base_name(name)
        && values
            .get(base.as_str())
            .is_none_or(|existing| (existing - value).abs() > 1.0e-12)
    {
        values.insert(base, value);
        changed = true;
    }
    changed
}

fn collect_array_elements_scalar_entries(
    name: &dae::VarName,
    elements: &[dae::Expression],
    values: &HashMap<String, f64>,
) -> Vec<(String, f64)> {
    elements
        .iter()
        .enumerate()
        .filter_map(|(index, element)| {
            eval_scalar_const_expr(element, values)
                .map(|value| (format!("{}[{}]", name.as_str(), index + 1), value))
        })
        .collect()
}

fn collect_range_scalar_entries(
    name: &dae::VarName,
    start: &dae::Expression,
    step: Option<&dae::Expression>,
    end: &dae::Expression,
    values: &HashMap<String, f64>,
) -> Vec<(String, f64)> {
    let Some(mut current) = eval_scalar_const_expr(start, values) else {
        return Vec::new();
    };
    let Some(end_value) = eval_scalar_const_expr(end, values) else {
        return Vec::new();
    };
    let step_value = step
        .and_then(|expr| eval_scalar_const_expr(expr, values))
        .unwrap_or(1.0);
    if step_value.abs() <= f64::EPSILON {
        return Vec::new();
    }

    let mut entries = Vec::new();
    let mut index = 1usize;
    if step_value > 0.0 {
        while current <= end_value + 1.0e-12 && index <= 10_000 {
            entries.push((format!("{}[{}]", name.as_str(), index), current));
            current += step_value;
            index += 1;
        }
    } else {
        while current >= end_value - 1.0e-12 && index <= 10_000 {
            entries.push((format!("{}[{}]", name.as_str(), index), current));
            current += step_value;
            index += 1;
        }
    }
    entries
}

fn collect_array_scalar_entries(
    name: &dae::VarName,
    start: &dae::Expression,
    values: &HashMap<String, f64>,
) -> Vec<(String, f64)> {
    match start {
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            collect_array_elements_scalar_entries(name, elements, values)
        }
        dae::Expression::Range { start, step, end } => {
            collect_range_scalar_entries(name, start, step.as_deref(), end, values)
        }
        _ => Vec::new(),
    }
}

fn collect_compile_time_scalars(dae_model: &dae::Dae) -> HashMap<String, f64> {
    let mut values = HashMap::new();
    for (literal, ordinal) in &dae_model.enum_literal_ordinals {
        values.insert(literal.clone(), *ordinal as f64);
    }
    let bindings: Vec<(&dae::VarName, &dae::Expression)> = dae_model
        .parameters
        .iter()
        .chain(dae_model.constants.iter())
        .chain(dae_model.inputs.iter())
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

fn bool_to_scalar(flag: bool) -> f64 {
    if flag { 1.0 } else { 0.0 }
}

fn scalar_to_bool(value: f64) -> Option<bool> {
    value.is_finite().then_some(value.abs() > 1.0e-12)
}

fn scalar_almost_eq(lhs: f64, rhs: f64) -> bool {
    (lhs - rhs).abs() <= 1.0e-12 * (1.0 + lhs.abs().max(rhs.abs()))
}

fn lookup_scalar_const_var_ref(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    if subscripts.is_empty() {
        return constants.get(name.as_str()).copied().or_else(|| {
            dae::component_base_name(name.as_str()).and_then(|base| constants.get(&base).copied())
        });
    }
    let key = clock::canonical_var_ref_key(name, subscripts, constants)?;
    constants
        .get(&key)
        .copied()
        .or_else(|| dae::component_base_name(&key).and_then(|base| constants.get(&base).copied()))
}

fn eval_scalar_named_function(
    short_name: &str,
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    let arg = |i: usize| -> Option<f64> {
        args.get(i)
            .and_then(|value| eval_scalar_const_expr(value, constants))
    };
    match short_name {
        "Integer" | "integer" | "floor" => arg(0).map(f64::floor),
        "ceil" => arg(0).map(f64::ceil),
        "abs" => arg(0).map(f64::abs),
        "sign" => arg(0).map(f64::signum),
        "min" => match (arg(0), arg(1)) {
            (Some(lhs), Some(rhs)) => Some(lhs.min(rhs)),
            _ => None,
        },
        "max" => match (arg(0), arg(1)) {
            (Some(lhs), Some(rhs)) => Some(lhs.max(rhs)),
            _ => None,
        },
        "div" => {
            let lhs = arg(0)?;
            let rhs = arg(1)?;
            (rhs.abs() > f64::EPSILON).then_some((lhs / rhs).trunc())
        }
        "mod" | "rem" => {
            let lhs = arg(0)?;
            let rhs = arg(1)?;
            (rhs.abs() > f64::EPSILON).then_some(lhs % rhs)
        }
        _ => None,
    }
}

fn eval_scalar_unary(
    op: &rumoca_ir_core::OpUnary,
    rhs: &dae::Expression,
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    let value = eval_scalar_const_expr(rhs, constants)?;
    match op {
        rumoca_ir_core::OpUnary::Plus(_) | rumoca_ir_core::OpUnary::DotPlus(_) => Some(value),
        rumoca_ir_core::OpUnary::Minus(_) | rumoca_ir_core::OpUnary::DotMinus(_) => Some(-value),
        rumoca_ir_core::OpUnary::Not(_) => scalar_to_bool(value).map(|flag| bool_to_scalar(!flag)),
        _ => None,
    }
}

fn eval_scalar_relation(op: &rumoca_ir_core::OpBinary, lhs: f64, rhs: f64) -> Option<f64> {
    let flag = match op {
        rumoca_ir_core::OpBinary::Lt(_) => lhs < rhs,
        rumoca_ir_core::OpBinary::Le(_) => lhs <= rhs,
        rumoca_ir_core::OpBinary::Gt(_) => lhs > rhs,
        rumoca_ir_core::OpBinary::Ge(_) => lhs >= rhs,
        rumoca_ir_core::OpBinary::Eq(_) => scalar_almost_eq(lhs, rhs),
        rumoca_ir_core::OpBinary::Neq(_) => !scalar_almost_eq(lhs, rhs),
        _ => return None,
    };
    Some(bool_to_scalar(flag))
}

fn eval_scalar_logical(op: &rumoca_ir_core::OpBinary, lhs: f64, rhs: f64) -> Option<f64> {
    let lhs_flag = scalar_to_bool(lhs)?;
    let rhs_flag = scalar_to_bool(rhs)?;
    let flag = match op {
        rumoca_ir_core::OpBinary::And(_) => lhs_flag && rhs_flag,
        rumoca_ir_core::OpBinary::Or(_) => lhs_flag || rhs_flag,
        _ => return None,
    };
    Some(bool_to_scalar(flag))
}

fn eval_scalar_division(lhs: f64, rhs: f64) -> Option<f64> {
    (rhs.abs() > f64::EPSILON).then_some(lhs / rhs)
}

fn eval_scalar_binary(
    op: &rumoca_ir_core::OpBinary,
    lhs: &dae::Expression,
    rhs: &dae::Expression,
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    let lhs_value = eval_scalar_const_expr(lhs, constants)?;
    let rhs_value = eval_scalar_const_expr(rhs, constants)?;
    match op {
        rumoca_ir_core::OpBinary::Add(_) | rumoca_ir_core::OpBinary::AddElem(_) => {
            Some(lhs_value + rhs_value)
        }
        rumoca_ir_core::OpBinary::Sub(_) | rumoca_ir_core::OpBinary::SubElem(_) => {
            Some(lhs_value - rhs_value)
        }
        rumoca_ir_core::OpBinary::Mul(_) | rumoca_ir_core::OpBinary::MulElem(_) => {
            Some(lhs_value * rhs_value)
        }
        rumoca_ir_core::OpBinary::Div(_) | rumoca_ir_core::OpBinary::DivElem(_) => {
            eval_scalar_division(lhs_value, rhs_value)
        }
        rumoca_ir_core::OpBinary::Exp(_) | rumoca_ir_core::OpBinary::ExpElem(_) => {
            Some(lhs_value.powf(rhs_value))
        }
        rumoca_ir_core::OpBinary::And(_) | rumoca_ir_core::OpBinary::Or(_) => {
            eval_scalar_logical(op, lhs_value, rhs_value)
        }
        rumoca_ir_core::OpBinary::Lt(_)
        | rumoca_ir_core::OpBinary::Le(_)
        | rumoca_ir_core::OpBinary::Gt(_)
        | rumoca_ir_core::OpBinary::Ge(_)
        | rumoca_ir_core::OpBinary::Eq(_)
        | rumoca_ir_core::OpBinary::Neq(_) => eval_scalar_relation(op, lhs_value, rhs_value),
        _ => None,
    }
}

fn eval_scalar_builtin(
    function: dae::BuiltinFunction,
    args: &[dae::Expression],
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    let arg = |i: usize| -> Option<f64> {
        args.get(i)
            .and_then(|value| eval_scalar_const_expr(value, constants))
    };
    match function {
        dae::BuiltinFunction::NoEvent => arg(0),
        dae::BuiltinFunction::Smooth => arg(1).or_else(|| arg(0)),
        dae::BuiltinFunction::Abs => arg(0).map(f64::abs),
        dae::BuiltinFunction::Sign => arg(0).map(f64::signum),
        dae::BuiltinFunction::Floor | dae::BuiltinFunction::Integer => arg(0).map(f64::floor),
        dae::BuiltinFunction::Ceil => arg(0).map(f64::ceil),
        dae::BuiltinFunction::Min => match (arg(0), arg(1)) {
            (Some(lhs), Some(rhs)) => Some(lhs.min(rhs)),
            _ => None,
        },
        dae::BuiltinFunction::Max => match (arg(0), arg(1)) {
            (Some(lhs), Some(rhs)) => Some(lhs.max(rhs)),
            _ => None,
        },
        dae::BuiltinFunction::Div => {
            let lhs = arg(0)?;
            let rhs = arg(1)?;
            (rhs.abs() > f64::EPSILON).then_some((lhs / rhs).trunc())
        }
        dae::BuiltinFunction::Mod | dae::BuiltinFunction::Rem => {
            let lhs = arg(0)?;
            let rhs = arg(1)?;
            (rhs.abs() > f64::EPSILON).then_some(lhs % rhs)
        }
        _ => None,
    }
}

fn eval_scalar_const_expr(expr: &dae::Expression, constants: &HashMap<String, f64>) -> Option<f64> {
    match expr {
        dae::Expression::Literal(dae::Literal::Integer(value)) => Some(*value as f64),
        dae::Expression::Literal(dae::Literal::Real(value)) => Some(*value),
        dae::Expression::Literal(dae::Literal::Boolean(value)) => Some(bool_to_scalar(*value)),
        dae::Expression::VarRef { name, subscripts } => {
            lookup_scalar_const_var_ref(name, subscripts, constants)
        }
        dae::Expression::Index { base, subscripts } => {
            if let dae::Expression::VarRef {
                name,
                subscripts: base_subscripts,
            } = base.as_ref()
            {
                let mut merged = base_subscripts.clone();
                merged.extend(subscripts.iter().cloned());
                lookup_scalar_const_var_ref(name, &merged, constants)
            } else {
                None
            }
        }
        dae::Expression::Unary { op, rhs } => eval_scalar_unary(op, rhs, constants),
        dae::Expression::Binary { op, lhs, rhs } => eval_scalar_binary(op, lhs, rhs, constants),
        dae::Expression::BuiltinCall { function, args } => {
            eval_scalar_builtin(*function, args, constants)
        }
        dae::Expression::FunctionCall { name, args, .. } => {
            let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
            eval_scalar_named_function(short, args, constants)
        }
        _ => None,
    }
}

fn extract_time_event_instant(
    cond: &dae::Expression,
    constants: &HashMap<String, f64>,
) -> Option<f64> {
    let dae::Expression::Binary { op, lhs, rhs } = cond else {
        return None;
    };
    if !matches!(
        op,
        rumoca_ir_core::OpBinary::Lt(_)
            | rumoca_ir_core::OpBinary::Le(_)
            | rumoca_ir_core::OpBinary::Gt(_)
            | rumoca_ir_core::OpBinary::Ge(_)
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
    expr: &dae::Expression,
    constants: &HashMap<String, f64>,
) -> Option<(f64, f64)> {
    match expr {
        dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
            if name.as_str() == "time" {
                return Some((1.0, 0.0));
            }
            eval_scalar_const_expr(expr, constants).map(|value| (0.0, value))
        }
        dae::Expression::Literal(dae::Literal::Integer(value)) => Some((0.0, *value as f64)),
        dae::Expression::Literal(dae::Literal::Real(value)) => Some((0.0, *value)),
        dae::Expression::Unary {
            op: rumoca_ir_core::OpUnary::Minus(_),
            rhs,
        } => affine_time_coefficients(rhs, constants).map(|(a, b)| (-a, -b)),
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Add(_),
            lhs,
            rhs,
        } => {
            let (a_lhs, b_lhs) = affine_time_coefficients(lhs, constants)?;
            let (a_rhs, b_rhs) = affine_time_coefficients(rhs, constants)?;
            Some((a_lhs + a_rhs, b_lhs + b_rhs))
        }
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            let (a_lhs, b_lhs) = affine_time_coefficients(lhs, constants)?;
            let (a_rhs, b_rhs) = affine_time_coefficients(rhs, constants)?;
            Some((a_lhs - a_rhs, b_lhs - b_rhs))
        }
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Mul(_),
            lhs,
            rhs,
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
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Div(_),
            lhs,
            rhs,
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
    expr: &dae::Expression,
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

fn collect_time_discontinuity_events_slice<'a, I>(
    exprs: I,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) where
    I: IntoIterator<Item = &'a dae::Expression>,
{
    for expr in exprs {
        collect_time_discontinuity_events_expr(expr, suppress_events, constants, seen, out);
    }
}

fn collect_time_discontinuity_events_if(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    for (cond, then_expr) in branches {
        maybe_push_time_event_condition(cond, suppress_events, constants, seen, out);
        collect_time_discontinuity_events_expr(cond, suppress_events, constants, seen, out);
        collect_time_discontinuity_events_expr(then_expr, suppress_events, constants, seen, out);
    }
    collect_time_discontinuity_events_expr(else_branch, suppress_events, constants, seen, out);
}

fn collect_time_discontinuity_events_range(
    start: &dae::Expression,
    step: &Option<Box<dae::Expression>>,
    end: &dae::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    collect_time_discontinuity_events_expr(start, suppress_events, constants, seen, out);
    if let Some(step_expr) = step {
        collect_time_discontinuity_events_expr(step_expr, suppress_events, constants, seen, out);
    }
    collect_time_discontinuity_events_expr(end, suppress_events, constants, seen, out);
}

fn collect_time_discontinuity_events_array_comprehension(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: &Option<Box<dae::Expression>>,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    collect_time_discontinuity_events_expr(expr, suppress_events, constants, seen, out);
    for idx in indices {
        collect_time_discontinuity_events_expr(&idx.range, suppress_events, constants, seen, out);
    }
    if let Some(filter_expr) = filter {
        collect_time_discontinuity_events_expr(filter_expr, suppress_events, constants, seen, out);
    }
}

fn collect_time_discontinuity_events_subscripts(
    subscripts: &[dae::Subscript],
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    for subscript in subscripts {
        if let dae::Subscript::Expr(expr) = subscript {
            collect_time_discontinuity_events_expr(expr, suppress_events, constants, seen, out);
        }
    }
}

fn collect_time_discontinuity_events_expr(
    expr: &dae::Expression,
    suppress_events: bool,
    constants: &HashMap<String, f64>,
    seen: &mut HashSet<String>,
    out: &mut Vec<f64>,
) {
    match expr {
        dae::Expression::If {
            branches,
            else_branch,
        } => collect_time_discontinuity_events_if(
            branches,
            else_branch,
            suppress_events,
            constants,
            seen,
            out,
        ),
        dae::Expression::Binary { lhs, rhs, .. } => {
            maybe_push_time_event_condition(expr, suppress_events, constants, seen, out);
            collect_time_discontinuity_events_expr(lhs, suppress_events, constants, seen, out);
            collect_time_discontinuity_events_expr(rhs, suppress_events, constants, seen, out);
        }
        dae::Expression::Unary { rhs, .. } => {
            collect_time_discontinuity_events_expr(rhs, suppress_events, constants, seen, out);
        }
        dae::Expression::BuiltinCall { function, args } => {
            let suppressed = suppress_events
                || matches!(
                    function,
                    dae::BuiltinFunction::NoEvent | dae::BuiltinFunction::Smooth
                );
            collect_time_discontinuity_events_slice(args, suppressed, constants, seen, out);
        }
        dae::Expression::FunctionCall { args, .. } => {
            collect_time_discontinuity_events_slice(args, suppress_events, constants, seen, out);
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            collect_time_discontinuity_events_slice(
                elements,
                suppress_events,
                constants,
                seen,
                out,
            );
        }
        dae::Expression::Range { start, step, end } => collect_time_discontinuity_events_range(
            start,
            step,
            end,
            suppress_events,
            constants,
            seen,
            out,
        ),
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => collect_time_discontinuity_events_array_comprehension(
            expr,
            indices,
            filter,
            suppress_events,
            constants,
            seen,
            out,
        ),
        dae::Expression::Index { base, subscripts } => {
            collect_time_discontinuity_events_expr(base, suppress_events, constants, seen, out);
            collect_time_discontinuity_events_subscripts(
                subscripts,
                suppress_events,
                constants,
                seen,
                out,
            );
        }
        dae::Expression::FieldAccess { base, .. } => {
            collect_time_discontinuity_events_expr(base, suppress_events, constants, seen, out);
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {}
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::Span;

    use super::*;

    fn time_gt(value: f64) -> dae::Expression {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Gt(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("time"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(value))),
        }
    }

    fn lit(value: f64) -> dae::Expression {
        dae::Expression::Literal(dae::Literal::Real(value))
    }

    fn var(name: &str) -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new(name),
            subscripts: vec![],
        }
    }

    fn dae_with_if_condition(cond: dae::Expression) -> dae::Dae {
        let mut dae_model = dae::Dae::default();
        dae_model.states.insert(
            dae::VarName::new("x"),
            dae::Variable::new(dae::VarName::new("x")),
        );
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(cond, lit(1.0))],
                else_branch: Box::new(lit(0.0)),
            },
            Span::DUMMY,
            "test_if_condition",
        ));
        dae_model
    }

    #[test]
    fn test_runtime_precompute_collects_event_without_synthetic_root_for_time_if_condition() {
        let mut dae_model = dae::Dae::default();
        let cond = time_gt(5.0);
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(
                    cond.clone(),
                    dae::Expression::Literal(dae::Literal::Real(1.0)),
                )],
                else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
            },
            Span::DUMMY,
            "test_if_condition",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

        assert!(
            dae_model
                .synthetic_root_conditions
                .iter()
                .all(|expr| format!("{expr:?}") != format!("{cond:?}")),
            "time-only branch conditions should be scheduled events, not synthetic roots"
        );
        assert!(
            dae_model
                .scheduled_time_events
                .iter()
                .any(|time| (*time - 5.0).abs() <= 1.0e-12),
            "expected precompute to capture scheduled event at t=5"
        );
    }

    #[test]
    fn test_runtime_precompute_dedupes_and_orders_root_and_time_event_metadata() {
        let root_cond = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Gt(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(0.0)),
        };
        let mut dae_model = dae_with_if_condition(root_cond.clone());

        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                        lhs: Box::new(var("x")),
                        rhs: Box::new(lit(0.0)),
                    },
                    lit(2.0),
                )],
                else_branch: Box::new(lit(-1.0)),
            },
            Span::DUMMY,
            "duplicate_root_condition",
        ));

        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(time_gt(1.5), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
            },
            Span::DUMMY,
            "time_event_late",
        ));
        dae_model.f_m.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(time_gt(0.5), lit(1.0))],
                else_branch: Box::new(lit(0.0)),
            },
            Span::DUMMY,
            "time_event_early",
        ));
        dae_model.f_m.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(time_gt(1.5), lit(2.0))],
                else_branch: Box::new(lit(0.0)),
            },
            Span::DUMMY,
            "time_event_late_duplicate",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

        assert_eq!(
            dae_model.synthetic_root_conditions.len(),
            1,
            "synthetic roots should be deduplicated"
        );
        assert_eq!(
            format!("{:?}", dae_model.synthetic_root_conditions[0]),
            format!("{root_cond:?}"),
            "synthetic root order should remain stable by first occurrence"
        );
        assert_eq!(
            dae_model.scheduled_time_events,
            vec![0.5, 1.5],
            "scheduled time events should be deduplicated and sorted"
        );
    }

    #[test]
    fn test_runtime_precompute_skips_noevent_wrapped_conditions_for_events() {
        let mut dae_model = dae::Dae::default();
        let cond = time_gt(2.0);
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::NoEvent,
                args: vec![dae::Expression::If {
                    branches: vec![(cond, dae::Expression::Literal(dae::Literal::Real(1.0)))],
                    else_branch: Box::new(dae::Expression::Literal(dae::Literal::Real(0.0))),
                }],
            },
            Span::DUMMY,
            "test_noevent",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert!(
            dae_model.scheduled_time_events.is_empty(),
            "noEvent-wrapped conditions should not produce scheduled event times"
        );
    }

    #[test]
    fn test_runtime_precompute_skips_time_vs_parameter_synthetic_roots() {
        let cond = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(Default::default()),
            lhs: Box::new(var("time")),
            rhs: Box::new(var("switch_time")),
        };
        let mut dae_model = dae_with_if_condition(cond.clone());
        dae_model.relation = vec![cond.clone()];
        dae_model.f_c = vec![dae::Equation::explicit(
            dae::VarName::new("c[1]"),
            cond.clone(),
            Span::DUMMY,
            "condition equation from test".to_string(),
        )];
        let mut switch_time = dae::Variable::new(dae::VarName::new("switch_time"));
        switch_time.start = Some(lit(2.5));
        dae_model
            .parameters
            .insert(dae::VarName::new("switch_time"), switch_time);

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

        assert!(
            dae_model
                .synthetic_root_conditions
                .iter()
                .all(|expr| format!("{expr:?}") != format!("{cond:?}")),
            "time-vs-parameter branch conditions should be scheduled time events, not synthetic roots"
        );
        assert_eq!(dae_model.scheduled_time_events, vec![2.5]);
        assert!(
            dae_model.relation.is_empty(),
            "time-vs-parameter conditions should be represented as scheduled events, not solver roots"
        );
        assert!(
            dae_model.f_c.is_empty(),
            "condition equations must stay aligned with pruned time-only relation roots"
        );
    }

    #[test]
    fn test_runtime_precompute_renumbers_condition_partition_after_prune() {
        let time_only = time_gt(2.5);
        let root_cond = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Gt(Default::default()),
            lhs: Box::new(var("x")),
            rhs: Box::new(lit(0.0)),
        };
        let mut dae_model = dae_with_if_condition(root_cond.clone());
        dae_model.relation = vec![time_only.clone(), root_cond.clone()];
        dae_model.f_c = vec![
            dae::Equation::explicit(
                dae::VarName::new("c[1]"),
                time_only,
                Span::DUMMY,
                "condition equation from test",
            ),
            dae::Equation::explicit(
                dae::VarName::new("c[2]"),
                root_cond.clone(),
                Span::DUMMY,
                "condition equation from test",
            ),
        ];

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

        assert_eq!(dae_model.relation, vec![root_cond.clone()]);
        assert_eq!(dae_model.f_c.len(), 1);
        assert_eq!(
            dae_model.f_c[0].lhs.as_ref(),
            Some(&dae::VarName::new("c[1]")),
            "surviving condition equations must be renumbered after pruning"
        );
        assert_eq!(dae_model.f_c[0].rhs, root_cond);
    }

    #[test]
    fn test_runtime_precompute_keeps_time_vs_state_synthetic_roots() {
        let cond = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(Default::default()),
            lhs: Box::new(var("time")),
            rhs: Box::new(var("x")),
        };
        let mut dae_model = dae_with_if_condition(cond.clone());
        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

        assert!(
            dae_model
                .synthetic_root_conditions
                .iter()
                .any(|expr| format!("{expr:?}") == format!("{cond:?}")),
            "time-vs-state branch conditions should remain synthetic roots"
        );
        assert!(
            dae_model.scheduled_time_events.is_empty(),
            "time-vs-state branch conditions should not be scheduled as static events"
        );
    }

    #[test]
    fn test_runtime_precompute_extracts_affine_time_event() {
        let cond = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Le(Default::default()),
            lhs: Box::new(dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Add(Default::default()),
                lhs: Box::new(var("time")),
                rhs: Box::new(var("delay")),
            }),
            rhs: Box::new(var("switch_at")),
        };
        let mut dae_model = dae_with_if_condition(cond);
        let mut delay = dae::Variable::new(dae::VarName::new("delay"));
        delay.start = Some(lit(0.25));
        dae_model
            .parameters
            .insert(dae::VarName::new("delay"), delay);
        let mut switch_at = dae::Variable::new(dae::VarName::new("switch_at"));
        switch_at.start = Some(lit(1.5));
        dae_model
            .parameters
            .insert(dae::VarName::new("switch_at"), switch_at);

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert_eq!(dae_model.scheduled_time_events.len(), 1);
        assert!((dae_model.scheduled_time_events[0] - 1.25).abs() <= 1e-12);
    }

    #[test]
    fn test_runtime_precompute_extracts_discrete_partition_events() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("c"),
            dae::Variable::new(dae::VarName::new("c")),
        );
        dae_model.f_m.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("c")),
                rhs: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Gt(Default::default()),
                    lhs: Box::new(var("time")),
                    rhs: Box::new(lit(0.5)),
                }),
            },
            Span::DUMMY,
            "test_discrete_partition",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert_eq!(dae_model.scheduled_time_events, vec![0.5]);
    }

    #[test]
    fn test_runtime_precompute_collects_clock_constructor_exprs() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_reals.insert(
            dae::VarName::new("s"),
            dae::Variable::new(dae::VarName::new("s")),
        );
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("s"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sample,
                    args: vec![
                        dae::Expression::VarRef {
                            name: dae::VarName::new("u"),
                            subscripts: vec![],
                        },
                        dae::Expression::FunctionCall {
                            name: dae::VarName::new("Clock"),
                            args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
                            is_constructor: false,
                        },
                    ],
                }),
            },
            Span::DUMMY,
            "test_clock_constructor",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");

        assert_eq!(dae_model.clock_constructor_exprs.len(), 1);
        assert_eq!(dae_model.clock_schedules.len(), 1);
        assert!((dae_model.clock_schedules[0].period_seconds - 0.1).abs() <= 1e-12);
        assert!(dae_model.clock_schedules[0].phase_seconds.abs() <= 1e-12);
        assert!((dae_model.clock_intervals["s"] - 0.1).abs() <= 1e-12);
    }

    #[test]
    fn test_runtime_precompute_assigns_implicit_sample_interval_from_unique_schedule() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_reals.insert(
            dae::VarName::new("simTime"),
            dae::Variable::new(dae::VarName::new("simTime")),
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("clockY"),
            dae::Variable::new(dae::VarName::new("clockY")),
        );

        // simTime = sample(time) (implicit clock sample form)
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("simTime")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sample,
                    args: vec![var("time")],
                }),
            },
            Span::DUMMY,
            "implicit_clocked_sample",
        ));

        // clockY = Clock(0.1) gives the unique static schedule in this model.
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("clockY")),
                rhs: Box::new(dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
                    is_constructor: false,
                }),
            },
            Span::DUMMY,
            "periodic_clock_constructor",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert_eq!(dae_model.clock_schedules.len(), 1);
        assert!((dae_model.clock_intervals["simTime"] - 0.1).abs() <= 1e-12);
    }

    #[test]
    fn test_runtime_precompute_does_not_assign_fallback_interval_for_non_sample_clock_ops() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("b"),
            dae::Variable::new(dae::VarName::new("b")),
        );
        dae_model.discrete_reals.insert(
            dae::VarName::new("clockY"),
            dae::Variable::new(dae::VarName::new("clockY")),
        );

        // b = pre(b) is discrete/event logic, not an implicit sample(..) form.
        dae_model.f_m.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("b")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![var("b")],
                }),
            },
            Span::DUMMY,
            "pre_based_discrete_update",
        ));

        // Add one static periodic schedule in the model.
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("clockY")),
                rhs: Box::new(dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
                    is_constructor: false,
                }),
            },
            Span::DUMMY,
            "periodic_clock_constructor",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert_eq!(dae_model.clock_schedules.len(), 1);
        assert!(
            !dae_model.clock_intervals.contains_key("b"),
            "fallback interval must only apply to implicit sample(..) sources",
        );
    }

    #[test]
    fn test_runtime_precompute_extracts_shifted_clock_schedule() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_m.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("b"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::FunctionCall {
                    name: dae::VarName::new("shiftSample"),
                    args: vec![
                        dae::Expression::FunctionCall {
                            name: dae::VarName::new("Clock"),
                            args: vec![dae::Expression::Literal(dae::Literal::Real(0.2))],
                            is_constructor: false,
                        },
                        dae::Expression::Literal(dae::Literal::Real(1.0)),
                    ],
                    is_constructor: false,
                }),
            },
            Span::DUMMY,
            "test_shifted_clock_constructor",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert_eq!(dae_model.clock_constructor_exprs.len(), 2);
        assert_eq!(dae_model.clock_schedules.len(), 2);

        let has_base = dae_model.clock_schedules.iter().any(|sched| {
            (sched.period_seconds - 0.2).abs() <= 1e-12 && sched.phase_seconds.abs() <= 1e-12
        });
        let has_shifted = dae_model.clock_schedules.iter().any(|sched| {
            (sched.period_seconds - 0.2).abs() <= 1e-12
                && (sched.phase_seconds - 0.2).abs() <= 1e-12
        });
        assert!(has_base);
        assert!(has_shifted);
    }

    #[test]
    fn test_runtime_precompute_resolves_shift_sample_via_sample_clock_alias_chain() {
        let mut dae_model = dae::Dae::default();

        for name in ["factor", "resolutionFactor"] {
            let mut p = dae::Variable::new(dae::VarName::new(name));
            p.start = Some(if name == "factor" {
                lit(20.0)
            } else {
                lit(1000.0)
            });
            dae_model.parameters.insert(dae::VarName::new(name), p);
        }

        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("periodicClock.y")),
                rhs: Box::new(var("periodicClock.c")),
            },
            Span::DUMMY,
            "periodicClock_alias",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("sample1.clock")),
                rhs: Box::new(var("periodicClock.y")),
            },
            Span::DUMMY,
            "clock_alias",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("sample1.y")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sample,
                    args: vec![var("sample1.u"), var("sample1.clock")],
                }),
            },
            Span::DUMMY,
            "sample_rhs",
        ));
        dae_model.f_x.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("shiftSample1.u")),
                rhs: Box::new(var("sample1.y")),
            },
            Span::DUMMY,
            "shift_source_alias",
        ));

        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.c"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("subSample"),
                args: vec![
                    dae::Expression::FunctionCall {
                        name: dae::VarName::new("Clock"),
                        args: vec![var("factor")],
                        is_constructor: false,
                    },
                    var("resolutionFactor"),
                ],
                is_constructor: false,
            },
            Span::DUMMY,
            "periodic_clock",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("shifted"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("shiftSample"),
                args: vec![var("shiftSample1.u"), lit(1.0), lit(1.0)],
                is_constructor: false,
            },
            Span::DUMMY,
            "shifted_clock",
        ));

        populate_runtime_precompute(&mut dae_model).expect("runtime precompute should succeed");
        assert!(
            dae_model.clock_schedules.iter().any(|sched| {
                (sched.period_seconds - 0.02).abs() <= 1e-12
                    && (sched.phase_seconds - 1.0).abs() <= 1e-12
            }),
            "expected shifted periodic schedule resolved through sample clock aliases"
        );
    }

    #[test]
    fn test_runtime_precompute_resolves_shift_sample_with_reversed_clock_alias_equation() {
        let mut dae_model = dae::Dae::default();

        for (name, value) in [("factor", 20.0), ("resolutionFactor", 1000.0)] {
            let mut p = dae::Variable::new(dae::VarName::new(name));
            p.start = Some(lit(value));
            dae_model.parameters.insert(dae::VarName::new(name), p);
        }

        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.c"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("subSample"),
                args: vec![
                    dae::Expression::FunctionCall {
                        name: dae::VarName::new("Clock"),
                        args: vec![var("factor")],
                        is_constructor: false,
                    },
                    var("resolutionFactor"),
                ],
                is_constructor: false,
            },
            Span::DUMMY,
            "periodic_clock",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.y"),
            var("periodicClock.c"),
            Span::DUMMY,
            "periodic_clock_output",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.y"),
            var("sample1.clock"),
            Span::DUMMY,
            "reversed_connection_alias",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("sample1.y"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![var("sample1.u"), var("sample1.clock")],
            },
            Span::DUMMY,
            "sample_rhs",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("shiftSample1.u"),
            var("sample1.y"),
            Span::DUMMY,
            "shift_source_alias",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("shifted"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("shiftSample"),
                args: vec![var("shiftSample1.u"), lit(1.0), lit(1.0)],
                is_constructor: false,
            },
            Span::DUMMY,
            "shifted_clock",
        ));

        populate_runtime_precompute(&mut dae_model).expect(
            "clock schedule should resolve even when connection alias equation orientation is reversed",
        );
        assert!(
            dae_model.clock_schedules.iter().any(|sched| {
                (sched.period_seconds - 0.02).abs() <= 1e-12
                    && (sched.phase_seconds - 1.0).abs() <= 1e-12
            }),
            "expected shifted periodic schedule resolved through reversed connection alias equation"
        );
    }

    #[test]
    fn test_runtime_precompute_prunes_dead_clock_constructor_branch_from_const_relation() {
        let mut dae_model = dae::Dae::default();

        for (name, value) in [
            ("resolution", 1.0),
            ("threshold", 2.0),
            ("factor", 20.0),
            ("resolutionFactor", 1000.0),
        ] {
            let mut p = dae::Variable::new(dae::VarName::new(name));
            p.start = Some(lit(value));
            dae_model.parameters.insert(dae::VarName::new(name), p);
        }

        let cond = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(Default::default()),
            lhs: Box::new(var("resolution")),
            rhs: Box::new(var("threshold")),
        };
        let live_branch = dae::Expression::FunctionCall {
            name: dae::VarName::new("subSample"),
            args: vec![
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![var("factor")],
                    is_constructor: false,
                },
                var("resolutionFactor"),
            ],
            is_constructor: false,
        };
        let dead_branch = dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![
                var("periodicClock.factor"),
                var("periodicClock.resolutionFactor"),
            ],
            is_constructor: false,
        };

        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.c"),
            dae::Expression::If {
                branches: vec![(cond, live_branch)],
                else_branch: Box::new(dead_branch),
            },
            Span::DUMMY,
            "periodic_clock_conditional_constructor",
        ));

        populate_runtime_precompute(&mut dae_model)
            .expect("const-true branch should prune dead unresolved clock constructor");
        assert!(
            dae_model
                .clock_schedules
                .iter()
                .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
            "expected resolved subSample schedule from live branch only"
        );
    }

    fn seed_conditional_residual_alias_chain_params(dae_model: &mut dae::Dae) {
        for (name, value) in [
            ("periodicClock.factor", 20.0),
            ("periodicClock.resolution", 2.0),
            ("periodicClock.resolutionFactor", 1000.0),
            ("shiftSample1.shiftCounter", 4.0),
            ("shiftSample1.resolution", 2.0),
            ("threshold", 3.0),
        ] {
            let mut p = dae::Variable::new(dae::VarName::new(name));
            p.start = Some(lit(value));
            dae_model.parameters.insert(dae::VarName::new(name), p);
        }
    }

    fn add_conditional_residual_clock_equation(dae_model: &mut dae::Dae) {
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Lt(Default::default()),
                        lhs: Box::new(var("periodicClock.resolution")),
                        rhs: Box::new(var("threshold")),
                    },
                    dae::Expression::Binary {
                        op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                        lhs: Box::new(var("periodicClock.c")),
                        rhs: Box::new(dae::Expression::FunctionCall {
                            name: dae::VarName::new("subSample"),
                            args: vec![
                                dae::Expression::FunctionCall {
                                    name: dae::VarName::new("Clock"),
                                    args: vec![var("periodicClock.factor")],
                                    is_constructor: false,
                                },
                                var("periodicClock.resolutionFactor"),
                            ],
                            is_constructor: false,
                        }),
                    },
                )],
                else_branch: Box::new(dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(var("periodicClock.c")),
                    rhs: Box::new(dae::Expression::FunctionCall {
                        name: dae::VarName::new("Clock"),
                        args: vec![
                            var("periodicClock.factor"),
                            var("periodicClock.resolutionFactor"),
                        ],
                        is_constructor: false,
                    }),
                }),
            },
            Span::DUMMY,
            "periodic_clock_conditional_residual",
        ));
    }

    fn add_shift_alias_chain_equations(dae_model: &mut dae::Dae) {
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.y"),
            var("periodicClock.c"),
            Span::DUMMY,
            "periodic_clock_output_alias",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("sample1.clock"),
            var("periodicClock.y"),
            Span::DUMMY,
            "sample_clock_alias",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("sample1.y"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![var("u"), var("sample1.clock")],
            },
            Span::DUMMY,
            "sample_output",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("shiftSample1.u"),
            var("sample1.y"),
            Span::DUMMY,
            "shift_input_alias",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("shiftSample1.y"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("shiftSample"),
                args: vec![
                    var("shiftSample1.u"),
                    var("shiftSample1.shiftCounter"),
                    var("shiftSample1.resolution"),
                ],
                is_constructor: false,
            },
            Span::DUMMY,
            "shift_output",
        ));
    }

    #[test]
    fn test_runtime_precompute_resolves_clock_alias_chain_from_conditional_residual_branch() {
        let mut dae_model = dae::Dae::default();
        seed_conditional_residual_alias_chain_params(&mut dae_model);
        add_conditional_residual_clock_equation(&mut dae_model);
        add_shift_alias_chain_equations(&mut dae_model);
        populate_runtime_precompute(&mut dae_model)
            .expect("conditional residual clock aliases should resolve to a periodic schedule");
        assert!(
            dae_model
                .clock_schedules
                .iter()
                .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
            "expected 20/1000 second periodic schedule through conditional-residual alias chain"
        );
    }

    #[test]
    fn test_runtime_precompute_resolves_clock_resolution_factor_from_table_lookup() {
        let mut dae_model = dae::Dae::default();

        let mut factor = dae::Variable::new(dae::VarName::new("periodicClock.factor"));
        factor.start = Some(lit(20.0));
        dae_model
            .parameters
            .insert(dae::VarName::new("periodicClock.factor"), factor);

        let mut resolution = dae::Variable::new(dae::VarName::new("periodicClock.resolution"));
        resolution.start = Some(lit(2.0));
        dae_model
            .parameters
            .insert(dae::VarName::new("periodicClock.resolution"), resolution);

        let mut conversion_table =
            dae::Variable::new(dae::VarName::new("periodicClock.conversionTable"));
        conversion_table.start = Some(dae::Expression::Array {
            elements: vec![lit(1.0), lit(1000.0)],
            is_matrix: false,
        });
        dae_model.parameters.insert(
            dae::VarName::new("periodicClock.conversionTable"),
            conversion_table,
        );

        let mut resolution_factor =
            dae::Variable::new(dae::VarName::new("periodicClock.resolutionFactor"));
        resolution_factor.start = Some(dae::Expression::VarRef {
            name: dae::VarName::new("periodicClock.conversionTable"),
            subscripts: vec![dae::Subscript::Expr(Box::new(
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Integer"),
                    args: vec![var("periodicClock.resolution")],
                    is_constructor: false,
                },
            ))],
        });
        dae_model.parameters.insert(
            dae::VarName::new("periodicClock.resolutionFactor"),
            resolution_factor,
        );

        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.c"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![
                    var("periodicClock.factor"),
                    var("periodicClock.resolutionFactor"),
                ],
                is_constructor: false,
            },
            Span::DUMMY,
            "periodic_clock_from_conversion_table",
        ));

        populate_runtime_precompute(&mut dae_model)
            .expect("clock schedule should resolve through indexed conversion table lookup");
        assert!(
            dae_model
                .clock_schedules
                .iter()
                .any(|sched| (sched.period_seconds - 0.02).abs() <= 1e-12),
            "expected period 20/1000 = 0.02"
        );
    }

    #[test]
    fn test_runtime_precompute_skips_inferred_clock_forms_without_static_schedule() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("b"),
            dae::Expression::If {
                branches: vec![(
                    dae::Expression::FunctionCall {
                        name: dae::VarName::new("Clock"),
                        args: vec![],
                        is_constructor: false,
                    },
                    dae::Expression::Unary {
                        op: rumoca_ir_core::OpUnary::Not(Default::default()),
                        rhs: Box::new(dae::Expression::BuiltinCall {
                            function: dae::BuiltinFunction::Pre,
                            args: vec![var("b")],
                        }),
                    },
                )],
                else_branch: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Pre,
                    args: vec![var("b")],
                }),
            },
            Span::DUMMY,
            "inferred_clock_toggle",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("y"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("superSample"),
                args: vec![var("b")],
                is_constructor: false,
            },
            Span::DUMMY,
            "inferred_super_sample",
        ));

        populate_runtime_precompute(&mut dae_model)
            .expect("inferred clock forms should remain as dynamic runtime clock expressions");
        assert!(
            dae_model.clock_schedules.is_empty(),
            "inferred clock forms should not synthesize static periodic schedules"
        );
        assert_eq!(
            dae_model.clock_constructor_exprs.len(),
            2,
            "all clock constructor expressions should remain visible for diagnostics"
        );
    }

    #[test]
    fn test_runtime_precompute_skips_event_clock_constructor_without_static_schedule() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("tick"),
            dae::Variable::new(dae::VarName::new("tick")),
        );
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("eventClock"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![var("tick")],
                is_constructor: false,
            },
            Span::DUMMY,
            "event_clock_constructor",
        ));

        populate_runtime_precompute(&mut dae_model)
            .expect("Clock(condition) should be treated as non-static event clock");
        assert!(
            dae_model.clock_schedules.is_empty(),
            "Clock(condition) should not synthesize static periodic schedules"
        );
        assert_eq!(
            dae_model.clock_constructor_exprs.len(),
            1,
            "event clock constructor remains visible in diagnostics metadata"
        );
    }

    #[test]
    fn test_runtime_precompute_skips_shift_sample_with_non_static_source_clock() {
        let mut dae_model = dae::Dae::default();
        let mut shift_counter = dae::Variable::new(dae::VarName::new("shiftCounter"));
        shift_counter.start = Some(lit(2.0));
        dae_model
            .parameters
            .insert(dae::VarName::new("shiftCounter"), shift_counter);
        let mut resolution = dae::Variable::new(dae::VarName::new("resolution"));
        resolution.start = Some(lit(10.0));
        dae_model
            .parameters
            .insert(dae::VarName::new("resolution"), resolution);
        dae_model.algebraics.insert(
            dae::VarName::new("u"),
            dae::Variable::new(dae::VarName::new("u")),
        );

        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("y"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("shiftSample"),
                args: vec![var("u"), var("shiftCounter"), var("resolution")],
                is_constructor: false,
            },
            Span::DUMMY,
            "shift_sample_inferred_source_clock",
        ));

        populate_runtime_precompute(&mut dae_model)
            .expect("shiftSample with non-static source clock should remain dynamic");
        assert!(
            dae_model.clock_schedules.is_empty(),
            "dynamic shiftSample form should not synthesize a static schedule"
        );
        assert_eq!(
            dae_model.clock_constructor_exprs.len(),
            1,
            "dynamic shiftSample should remain visible in constructor metadata"
        );
    }

    #[test]
    fn test_runtime_precompute_rejects_dynamic_clock_constructor_without_schedule() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_z.push(dae::Equation::residual(
            dae::Expression::Binary {
                op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                lhs: Box::new(var("s")),
                rhs: Box::new(dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::Sample,
                    args: vec![
                        var("u"),
                        dae::Expression::FunctionCall {
                            name: dae::VarName::new("Clock"),
                            args: vec![var("u")],
                            is_constructor: false,
                        },
                    ],
                }),
            },
            Span::DUMMY,
            "test_dynamic_clock_constructor",
        ));

        let err = populate_runtime_precompute(&mut dae_model)
            .expect_err("dynamic clock constructors must fail during ToDae runtime precompute");
        assert!(
            matches!(err, ToDaeError::UnresolvedClockSchedule { .. }),
            "expected unresolved clock schedule error, got {err:?}"
        );
    }
}
