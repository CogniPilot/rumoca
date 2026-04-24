use crate::runtime::assignment::canonical_var_ref_key;
use crate::runtime::hotpath_stats;
use crate::runtime::scalar_eval::{eval_scalar_bool_expr_fast, eval_scalar_expr_fast};
use rumoca_eval_dae::runtime::VarEnv;
use rumoca_ir_dae as dae;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::thread_local;

const MAX_CLOCK_INFERENCE_DEPTH: usize = 16;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct ClockExprCacheKey {
    dae_ptr: usize,
    expr_ptr: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct ClockVarRefCacheKey {
    dae_ptr: usize,
    canonical: String,
}

thread_local! {
    static EXPLICIT_CLOCK_CACHE: RefCell<HashMap<ClockExprCacheKey, bool>> =
        RefCell::new(HashMap::new());
    static EXPLICIT_CLOCK_VARREF_CACHE: RefCell<HashMap<ClockVarRefCacheKey, bool>> =
        RefCell::new(HashMap::new());
    static CLOCK_ALIAS_SOURCE_CACHE: RefCell<HashMap<ClockVarRefCacheKey, Option<dae::Expression>>> =
        RefCell::new(HashMap::new());
    static DISCRETE_CLOCK_ACTIVITY_CACHE: RefCell<HashMap<usize, bool>> =
        RefCell::new(HashMap::new());
}

fn clock_expr_cache_key(dae: &dae::Dae, expr: &dae::Expression) -> ClockExprCacheKey {
    ClockExprCacheKey {
        dae_ptr: dae as *const dae::Dae as usize,
        expr_ptr: expr as *const dae::Expression as usize,
    }
}

fn clock_varref_cache_key(
    dae: &dae::Dae,
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
) -> Option<ClockVarRefCacheKey> {
    let canonical = canonical_var_ref_key(name, subscripts)?;
    Some(ClockVarRefCacheKey {
        dae_ptr: dae as *const dae::Dae as usize,
        canonical,
    })
}

fn clock_varref_cache_key_from_expr(
    dae: &dae::Dae,
    expr: &dae::Expression,
) -> Option<ClockVarRefCacheKey> {
    let dae::Expression::VarRef { name, subscripts } = expr else {
        return None;
    };
    clock_varref_cache_key(dae, name, subscripts)
}

fn cached_explicit_clock_value(dae: &dae::Dae, expr: &dae::Expression) -> Option<bool> {
    EXPLICIT_CLOCK_CACHE.with(|cache| {
        cache
            .borrow()
            .get(&clock_expr_cache_key(dae, expr))
            .copied()
    })
}

fn store_explicit_clock_value(dae: &dae::Dae, expr: &dae::Expression, value: bool) {
    EXPLICIT_CLOCK_CACHE.with(|cache| {
        cache
            .borrow_mut()
            .insert(clock_expr_cache_key(dae, expr), value);
    });
}

fn cached_varref_explicit_clock_value(key: &ClockVarRefCacheKey) -> Option<bool> {
    EXPLICIT_CLOCK_VARREF_CACHE.with(|cache| cache.borrow().get(key).copied())
}

fn store_varref_explicit_clock_value(key: &ClockVarRefCacheKey, value: bool) {
    EXPLICIT_CLOCK_VARREF_CACHE.with(|cache| {
        cache.borrow_mut().insert(key.clone(), value);
    });
}

pub fn reset_runtime_clock_caches() {
    EXPLICIT_CLOCK_CACHE.with(|cache| cache.borrow_mut().clear());
    EXPLICIT_CLOCK_VARREF_CACHE.with(|cache| cache.borrow_mut().clear());
    CLOCK_ALIAS_SOURCE_CACHE.with(|cache| cache.borrow_mut().clear());
    DISCRETE_CLOCK_ACTIVITY_CACHE.with(|cache| cache.borrow_mut().clear());
}

fn clock_bool(value: f64) -> bool {
    value.is_finite() && value > 0.5
}

fn periodic_clock_schedule_matches_time(schedule: &dae::ClockSchedule, t: f64) -> bool {
    let period = schedule.period_seconds;
    let phase = schedule.phase_seconds;
    if !(period.is_finite() && phase.is_finite() && t.is_finite()) || period <= 0.0 {
        return false;
    }
    let k = ((t - phase) / period).round();
    if !k.is_finite() || k < 0.0 {
        return false;
    }
    crate::timeline::sample_time_match_with_tol(phase + k * period, t)
}

pub(crate) fn static_periodic_clock_event_active(
    dae: &dae::Dae,
    env: &VarEnv<f64>,
) -> Option<bool> {
    if dae.clock_schedules.is_empty() || !dae.triggered_clock_conditions.is_empty() {
        return None;
    }
    let t = env.vars.get("time").copied()?;
    Some(
        dae.clock_schedules
            .iter()
            .any(|schedule| periodic_clock_schedule_matches_time(schedule, t)),
    )
}

fn fast_clock_bool(expr: &dae::Expression, env: &VarEnv<f64>) -> Option<bool> {
    eval_scalar_expr_fast(expr, env).map(clock_bool)
}

fn sample_clock_alias_source_expr_uncached(
    dae: &dae::Dae,
    target: &str,
) -> Option<dae::Expression> {
    hotpath_stats::inc_clock_alias_source_scan();
    let mut direct_varref_source: Option<dae::Expression> = None;
    let mut reverse_varref_source: Option<dae::Expression> = None;
    for eq in dae.f_z.iter().chain(dae.f_m.iter()).chain(dae.f_x.iter()) {
        if let Some(lhs) = eq.lhs.as_ref()
            && lhs.as_str() == target
        {
            if matches!(eq.rhs, dae::Expression::VarRef { .. }) {
                direct_varref_source = Some(eq.rhs.clone());
                continue;
            }
            return Some(eq.rhs.clone());
        }
        if let Some(source) = extract_residual_alias_source_expr(target, &eq.rhs) {
            return Some(source);
        }
        if let Some(lhs) = eq.lhs.as_ref()
            && let dae::Expression::VarRef { name, subscripts } = &eq.rhs
            && subscripts.is_empty()
            && canonical_var_ref_key(name, subscripts).as_deref() == Some(target)
        {
            reverse_varref_source = Some(dae::Expression::VarRef {
                name: lhs.clone(),
                subscripts: vec![],
            });
            continue;
        }
        if let dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } = &eq.rhs
        {
            let lhs_key = match lhs.as_ref() {
                dae::Expression::VarRef { name, subscripts } => {
                    canonical_var_ref_key(name, subscripts)
                }
                _ => None,
            };
            let rhs_key = match rhs.as_ref() {
                dae::Expression::VarRef { name, subscripts } => {
                    canonical_var_ref_key(name, subscripts)
                }
                _ => None,
            };
            if lhs_key.as_deref() == Some(target) {
                return Some(rhs.as_ref().clone());
            }
            if rhs_key.as_deref() == Some(target) {
                return Some(lhs.as_ref().clone());
            }
        }
    }
    direct_varref_source.or(reverse_varref_source)
}

fn extract_residual_alias_source_expr(
    target: &str,
    expr: &dae::Expression,
) -> Option<dae::Expression> {
    match expr {
        dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Sub(_),
            lhs,
            rhs,
        } => {
            let lhs_key = match lhs.as_ref() {
                dae::Expression::VarRef { name, subscripts } => {
                    canonical_var_ref_key(name, subscripts)
                }
                _ => None,
            };
            let rhs_key = match rhs.as_ref() {
                dae::Expression::VarRef { name, subscripts } => {
                    canonical_var_ref_key(name, subscripts)
                }
                _ => None,
            };
            if lhs_key.as_deref() == Some(target) {
                return Some(rhs.as_ref().clone());
            }
            if rhs_key.as_deref() == Some(target) {
                return Some(lhs.as_ref().clone());
            }
            None
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            let mut extracted_branches = Vec::with_capacity(branches.len());
            for (condition, value) in branches {
                let extracted = extract_residual_alias_source_expr(target, value)?;
                extracted_branches.push((condition.clone(), extracted));
            }
            let extracted_else = extract_residual_alias_source_expr(target, else_branch)?;
            Some(dae::Expression::If {
                branches: extracted_branches,
                else_branch: Box::new(extracted_else),
            })
        }
        _ => None,
    }
}

pub(crate) fn sample_clock_alias_source_expr(
    dae: &dae::Dae,
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
) -> Option<dae::Expression> {
    let key = clock_varref_cache_key(dae, name, subscripts)?;
    if let Some(cached) = CLOCK_ALIAS_SOURCE_CACHE.with(|cache| cache.borrow().get(&key).cloned()) {
        return cached;
    }
    let resolved = sample_clock_alias_source_expr_uncached(dae, key.canonical.as_str());
    CLOCK_ALIAS_SOURCE_CACHE.with(|cache| {
        cache.borrow_mut().insert(key, resolved.clone());
    });
    resolved
}

fn sample_clock_arg_is_explicit_clock_inner(
    dae: &dae::Dae,
    clock_arg: &dae::Expression,
    env: &VarEnv<f64>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    if let dae::Expression::If {
        branches,
        else_branch,
    } = clock_arg
    {
        for (condition, value) in branches {
            let Some(active) = eval_scalar_bool_expr_fast(condition, env).or_else(|| {
                Some(clock_bool(rumoca_eval_dae::runtime::eval_expr::<f64>(
                    condition, env,
                )))
            }) else {
                continue;
            };
            if active {
                return sample_clock_arg_is_explicit_clock_inner(
                    dae,
                    value,
                    env,
                    remaining_depth,
                    visiting,
                );
            }
        }
        return sample_clock_arg_is_explicit_clock_inner(
            dae,
            else_branch,
            env,
            remaining_depth,
            visiting,
        );
    }
    if let dae::Expression::FunctionCall { name, .. } = clock_arg {
        let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
        if matches!(
            short,
            "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample" | "firstTick"
        ) {
            return true;
        }
    }
    if rumoca_eval_dae::runtime::infer_clock_timing_seconds(clock_arg, env).is_some() {
        return true;
    }
    if let dae::Expression::VarRef { name, subscripts } = clock_arg
        && subscripts.is_empty()
    {
        if remaining_depth == 0 {
            return false;
        }
        let Some(canonical) = canonical_var_ref_key(name, subscripts) else {
            return false;
        };
        // MLS §16.5.1: the explicit clock argument of `sample(u, c)` must be
        // a clock expression (or an alias of one), not an arbitrary discrete
        // Boolean/Integer signal that merely changes at events.
        if !visiting.insert(canonical.clone()) {
            return false;
        }
        let source = sample_clock_alias_source_expr(dae, name, subscripts);
        let inferred = source.is_some_and(|source| {
            sample_clock_arg_is_explicit_clock_inner(
                dae,
                &source,
                env,
                remaining_depth.saturating_sub(1),
                visiting,
            )
        });
        visiting.remove(&canonical);
        return inferred;
    }
    false
}

pub fn sample_clock_arg_is_explicit_clock(
    dae: &dae::Dae,
    clock_arg: &dae::Expression,
    env: &VarEnv<f64>,
) -> bool {
    hotpath_stats::inc_explicit_clock_inference();
    if let Some(key) = clock_varref_cache_key_from_expr(dae, clock_arg)
        && let Some(cached) = cached_varref_explicit_clock_value(&key)
    {
        return cached;
    }
    if let Some(cached) = cached_explicit_clock_value(dae, clock_arg) {
        return cached;
    }
    let inferred = sample_clock_arg_is_explicit_clock_inner(
        dae,
        clock_arg,
        env,
        MAX_CLOCK_INFERENCE_DEPTH,
        &mut HashSet::new(),
    );
    store_explicit_clock_value(dae, clock_arg, inferred);
    if let Some(key) = clock_varref_cache_key_from_expr(dae, clock_arg) {
        store_varref_explicit_clock_value(&key, inferred);
    }
    inferred
}

struct ClockInferenceContext<'a, FClockName, FClockEdge, FSampleActive>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    sources: &'a HashMap<String, &'a dae::Expression>,
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
}

fn infer_clock_active_next<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    infer_clock_active_from_expression(ctx, expr, remaining_depth.saturating_sub(1), visiting)
}

pub(crate) fn resolve_derived_clock_source_expr(
    dae: &dae::Dae,
    expr: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<dae::Expression> {
    let dae::Expression::VarRef { name, subscripts } = expr else {
        return Some(expr.clone());
    };
    if !subscripts.is_empty() || remaining_depth == 0 {
        return Some(expr.clone());
    }
    let Some(canonical) = canonical_var_ref_key(name, subscripts) else {
        return Some(expr.clone());
    };
    if !visiting.insert(canonical.clone()) {
        return Some(expr.clone());
    }
    let resolved = sample_clock_alias_source_expr(dae, name, subscripts)
        .and_then(|source| {
            resolve_derived_clock_source_expr(dae, &source, remaining_depth - 1, visiting)
        })
        .or_else(|| Some(expr.clone()));
    visiting.remove(&canonical);
    resolved
}

fn infer_clock_active_from_function_call<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    short: &str,
    args: &[dae::Expression],
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if (ctx.is_clock_function_name)(short) {
        if let Some(value) = (ctx.eval_clock_edge_assignment)(ctx.dae, expr, ctx.env) {
            return Some(clock_bool(value));
        }
        if matches!(
            short,
            "subSample" | "superSample" | "shiftSample" | "backSample"
        ) && let Some(source_expr) = args.first()
            && let Some(resolved_source) = resolve_derived_clock_source_expr(
                ctx.dae,
                source_expr,
                remaining_depth,
                &mut HashSet::new(),
            )
        {
            let mut resolved_args = Vec::with_capacity(args.len());
            resolved_args.push(resolved_source);
            resolved_args.extend(args.iter().skip(1).cloned());
            let resolved_expr = dae::Expression::FunctionCall {
                name: dae::VarName::new(short),
                args: resolved_args,
                is_constructor: false,
            };
            if rumoca_eval_dae::runtime::infer_clock_timing_seconds(&resolved_expr, ctx.env)
                .is_some()
            {
                return fast_clock_bool(&resolved_expr, ctx.env);
            }
        }
        if rumoca_eval_dae::runtime::infer_clock_timing_seconds(expr, ctx.env).is_some() {
            return fast_clock_bool(expr, ctx.env);
        }
        if matches!(short, "shiftSample" | "backSample")
            && let Some(source_expr) = args.first()
        {
            return infer_clock_active_next(ctx, source_expr, remaining_depth, visiting);
        }
    }

    if matches!(short, "previous" | "hold")
        && let Some(source_expr) = args.first()
    {
        return infer_clock_active_next(ctx, source_expr, remaining_depth, visiting);
    }
    None
}

fn infer_clock_active_from_builtin_call<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    match function {
        dae::BuiltinFunction::Sample if args.len() >= 2 => {
            let clock_arg = &args[1];
            if sample_clock_arg_is_explicit_clock(ctx.dae, clock_arg, ctx.env) {
                return Some((ctx.eval_sample_clock_active)(ctx.dae, clock_arg, ctx.env));
            }
            // sample(start, interval) and lowered internal
            // sample(id, start, interval) forms are both event indicators.
            let sample_args = if args.len() >= 3 {
                vec![args[1].clone(), args[2].clone()]
            } else {
                args.to_vec()
            };
            let sample_expr = dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: sample_args,
            };
            fast_clock_bool(&sample_expr, ctx.env)
        }
        dae::BuiltinFunction::Pre if !args.is_empty() => {
            infer_clock_active_next(ctx, &args[0], remaining_depth, visiting)
        }
        _ => None,
    }
}

fn infer_clock_active_from_var_ref<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if !subscripts.is_empty() {
        return None;
    }
    let key = canonical_var_ref_key(name, subscripts)?;
    if !visiting.insert(key.clone()) {
        return None;
    }
    let inferred = ctx
        .sources
        .get(key.as_str())
        .copied()
        .and_then(|rhs| infer_clock_active_next(ctx, rhs, remaining_depth, visiting));
    visiting.remove(&key);
    inferred
}

fn infer_clock_active_from_if_expression<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    for (condition, value) in branches {
        match eval_scalar_bool_expr_fast(condition, ctx.env) {
            Some(true) => return infer_clock_active_next(ctx, value, remaining_depth, visiting),
            Some(false) => continue,
            None => return None,
        }
    }
    infer_clock_active_next(ctx, else_branch, remaining_depth, visiting)
}

fn infer_clock_active_from_expression<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockInferenceContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<bool>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if remaining_depth == 0 {
        return None;
    }

    match expr {
        dae::Expression::FunctionCall { name, args, .. } => infer_clock_active_from_function_call(
            ctx,
            expr,
            name.as_str().rsplit('.').next().unwrap_or(name.as_str()),
            args,
            remaining_depth,
            visiting,
        ),
        dae::Expression::BuiltinCall { function, args } => {
            infer_clock_active_from_builtin_call(ctx, function, args, remaining_depth, visiting)
        }
        dae::Expression::VarRef { name, subscripts } => {
            infer_clock_active_from_var_ref(ctx, name, subscripts, remaining_depth, visiting)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => infer_clock_active_from_if_expression(
            ctx,
            branches,
            else_branch,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Binary { lhs, rhs, .. } => {
            infer_clock_active_next(ctx, lhs, remaining_depth, visiting)
                .or_else(|| infer_clock_active_next(ctx, rhs, remaining_depth, visiting))
        }
        dae::Expression::Unary { rhs, .. }
        | dae::Expression::FieldAccess { base: rhs, .. }
        | dae::Expression::Index { base: rhs, .. } => {
            infer_clock_active_next(ctx, rhs, remaining_depth, visiting)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .find_map(|element| infer_clock_active_next(ctx, element, remaining_depth, visiting)),
        dae::Expression::Range { start, step, end } => {
            infer_clock_active_next(ctx, start, remaining_depth, visiting)
                .or_else(|| {
                    step.as_deref().and_then(|value| {
                        infer_clock_active_next(ctx, value, remaining_depth, visiting)
                    })
                })
                .or_else(|| infer_clock_active_next(ctx, end, remaining_depth, visiting))
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => infer_clock_active_next(ctx, expr, remaining_depth, visiting)
            .or_else(|| {
                indices.iter().find_map(|index| {
                    infer_clock_active_next(ctx, &index.range, remaining_depth, visiting)
                })
            })
            .or_else(|| {
                filter.as_deref().and_then(|value| {
                    infer_clock_active_next(ctx, value, remaining_depth, visiting)
                })
            }),
        dae::Expression::Literal(_) | dae::Expression::Empty => None,
    }
}

fn function_name_has_clock_activity(name: &dae::VarName) -> bool {
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    matches!(
        short,
        "Clock"
            | "firstTick"
            | "hold"
            | "noClock"
            | "previous"
            | "subSample"
            | "superSample"
            | "shiftSample"
            | "backSample"
    )
}

fn clock_inference_requires_next(
    expr: &dae::Expression,
    sources: &HashMap<String, &dae::Expression>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    expression_requires_clock_inference_from_sources(
        expr,
        sources,
        remaining_depth.saturating_sub(1),
        visiting,
    )
}

fn source_varref_requires_clock_inference(
    name: &dae::VarName,
    subscripts: &[dae::Subscript],
    sources: &HashMap<String, &dae::Expression>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    let Some(key) = canonical_var_ref_key(name, subscripts) else {
        return false;
    };
    if !visiting.insert(key.clone()) {
        return false;
    }
    let requires = sources.get(key.as_str()).is_some_and(|source| {
        clock_inference_requires_next(source, sources, remaining_depth, visiting)
    });
    visiting.remove(&key);
    requires
}

fn source_if_requires_clock_inference(
    branches: &[(dae::Expression, dae::Expression)],
    else_branch: &dae::Expression,
    sources: &HashMap<String, &dae::Expression>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    branches.iter().any(|(condition, value)| {
        clock_inference_requires_next(condition, sources, remaining_depth, visiting)
            || clock_inference_requires_next(value, sources, remaining_depth, visiting)
    }) || clock_inference_requires_next(else_branch, sources, remaining_depth, visiting)
}

fn source_array_comprehension_requires_clock_inference(
    expr: &dae::Expression,
    indices: &[dae::ComprehensionIndex],
    filter: Option<&dae::Expression>,
    sources: &HashMap<String, &dae::Expression>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    clock_inference_requires_next(expr, sources, remaining_depth, visiting)
        || indices.iter().any(|index| {
            clock_inference_requires_next(&index.range, sources, remaining_depth, visiting)
        })
        || filter.is_some_and(|value| {
            clock_inference_requires_next(value, sources, remaining_depth, visiting)
        })
}

fn expression_requires_clock_inference_from_sources(
    expr: &dae::Expression,
    sources: &HashMap<String, &dae::Expression>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> bool {
    if remaining_depth == 0 {
        return false;
    }

    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            function_name_has_clock_activity(name)
                || args.iter().any(|arg| {
                    clock_inference_requires_next(arg, sources, remaining_depth, visiting)
                })
        }
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre | dae::BuiltinFunction::Sample
            ) || args
                .iter()
                .any(|arg| clock_inference_requires_next(arg, sources, remaining_depth, visiting))
        }
        dae::Expression::VarRef { name, subscripts } => source_varref_requires_clock_inference(
            name,
            subscripts,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::If {
            branches,
            else_branch,
        } => source_if_requires_clock_inference(
            branches,
            else_branch,
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Binary { lhs, rhs, .. } => {
            clock_inference_requires_next(lhs, sources, remaining_depth, visiting)
                || clock_inference_requires_next(rhs, sources, remaining_depth, visiting)
        }
        dae::Expression::Unary { rhs, .. }
        | dae::Expression::FieldAccess { base: rhs, .. }
        | dae::Expression::Index { base: rhs, .. } => {
            clock_inference_requires_next(rhs, sources, remaining_depth, visiting)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(|element| {
                clock_inference_requires_next(element, sources, remaining_depth, visiting)
            })
        }
        dae::Expression::Range { start, step, end } => {
            clock_inference_requires_next(start, sources, remaining_depth, visiting)
                || step.as_ref().is_some_and(|value| {
                    clock_inference_requires_next(
                        value.as_ref(),
                        sources,
                        remaining_depth,
                        visiting,
                    )
                })
                || clock_inference_requires_next(end, sources, remaining_depth, visiting)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => source_array_comprehension_requires_clock_inference(
            expr,
            indices,
            filter.as_deref(),
            sources,
            remaining_depth,
            visiting,
        ),
        dae::Expression::Literal(_) | dae::Expression::Empty => false,
    }
}

pub(crate) fn expression_may_trigger_clock_event_from_sources(
    expr: &dae::Expression,
    sources: &HashMap<String, &dae::Expression>,
) -> bool {
    expression_may_have_clock_activity(expr)
        || expression_requires_clock_inference_from_sources(
            expr,
            sources,
            MAX_CLOCK_INFERENCE_DEPTH,
            &mut HashSet::new(),
        )
}

fn expression_may_have_clock_activity(expr: &dae::Expression) -> bool {
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            function_name_has_clock_activity(name)
                || args.iter().any(expression_may_have_clock_activity)
        }
        dae::Expression::BuiltinCall { function, args } => {
            matches!(
                function,
                dae::BuiltinFunction::Pre | dae::BuiltinFunction::Sample
            ) || args.iter().any(expression_may_have_clock_activity)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(condition, value)| {
                expression_may_have_clock_activity(condition)
                    || expression_may_have_clock_activity(value)
            }) || expression_may_have_clock_activity(else_branch)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expression_may_have_clock_activity(lhs) || expression_may_have_clock_activity(rhs)
        }
        dae::Expression::Unary { rhs, .. }
        | dae::Expression::FieldAccess { base: rhs, .. }
        | dae::Expression::Index { base: rhs, .. } => expression_may_have_clock_activity(rhs),
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
            elements.iter().any(expression_may_have_clock_activity)
        }
        dae::Expression::Range { start, step, end } => {
            expression_may_have_clock_activity(start)
                || step
                    .as_ref()
                    .is_some_and(|value| expression_may_have_clock_activity(value.as_ref()))
                || expression_may_have_clock_activity(end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_may_have_clock_activity(expr)
                || indices
                    .iter()
                    .any(|index| expression_may_have_clock_activity(&index.range))
                || filter
                    .as_ref()
                    .is_some_and(|value| expression_may_have_clock_activity(value.as_ref()))
        }
        dae::Expression::Literal(_) | dae::Expression::VarRef { .. } | dae::Expression::Empty => {
            false
        }
    }
}

fn equation_may_have_clock_activity(eq: &dae::Equation) -> bool {
    expression_may_have_clock_activity(&eq.rhs)
}

pub fn dae_may_have_discrete_clock_activity(dae: &dae::Dae) -> bool {
    let dae_ptr = dae as *const dae::Dae as usize;
    if let Some(cached) =
        DISCRETE_CLOCK_ACTIVITY_CACHE.with(|cache| cache.borrow().get(&dae_ptr).copied())
    {
        return cached;
    }
    let has_clock_activity = dae
        .f_z
        .iter()
        .chain(dae.f_m.iter())
        .chain(dae.f_x.iter())
        .any(equation_may_have_clock_activity);
    DISCRETE_CLOCK_ACTIVITY_CACHE.with(|cache| {
        cache.borrow_mut().insert(dae_ptr, has_clock_activity);
    });
    has_clock_activity
}

struct ClockEventContext<'a, FClockName, FClockEdge, FSampleActive>
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
}

fn expression_has_active_clock_event<FClockName, FClockEdge, FSampleActive>(
    dae: &dae::Dae,
    expr: &dae::Expression,
    env: &VarEnv<f64>,
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    let ctx = ClockEventContext {
        dae,
        env,
        is_clock_function_name,
        eval_clock_edge_assignment,
        eval_sample_clock_active,
    };
    expression_has_active_clock_event_in_context(&ctx, expr)
}

fn function_call_has_active_clock_event<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockEventContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    name: &dae::VarName,
    args: &[dae::Expression],
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    let short = name.as_str().rsplit('.').next().unwrap_or(name.as_str());
    if short == "Clock" && args.is_empty() {
        return false;
    }
    if (ctx.is_clock_function_name)(short) {
        if (ctx.eval_clock_edge_assignment)(ctx.dae, expr, ctx.env).is_some_and(clock_bool) {
            return true;
        }
        if rumoca_eval_dae::runtime::infer_clock_timing_seconds(expr, ctx.env).is_some()
            && fast_clock_bool(expr, ctx.env).unwrap_or(false)
        {
            return true;
        }
    }
    args.iter()
        .any(|arg| expression_has_active_clock_event_in_context(ctx, arg))
}

fn builtin_call_has_active_clock_event<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockEventContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
    function: &dae::BuiltinFunction,
    args: &[dae::Expression],
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    if *function == dae::BuiltinFunction::Sample && args.len() >= 2 {
        let clock_arg = &args[1];
        let has_clock_expr = sample_clock_arg_is_explicit_clock(ctx.dae, clock_arg, ctx.env);
        if has_clock_expr {
            return (ctx.eval_sample_clock_active)(ctx.dae, clock_arg, ctx.env);
        }
        if args.len() >= 3 {
            let sample_expr = dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![args[1].clone(), args[2].clone()],
            };
            return fast_clock_bool(&sample_expr, ctx.env).unwrap_or(false);
        }
        return fast_clock_bool(expr, ctx.env).unwrap_or(false);
    }
    args.iter()
        .any(|arg| expression_has_active_clock_event_in_context(ctx, arg))
}

fn expression_has_active_clock_event_in_context<FClockName, FClockEdge, FSampleActive>(
    ctx: &ClockEventContext<'_, FClockName, FClockEdge, FSampleActive>,
    expr: &dae::Expression,
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    match expr {
        dae::Expression::FunctionCall { name, args, .. } => {
            function_call_has_active_clock_event(ctx, expr, name, args)
        }
        dae::Expression::BuiltinCall { function, args } => {
            builtin_call_has_active_clock_event(ctx, expr, function, args)
        }
        dae::Expression::Binary { lhs, rhs, .. } => {
            expression_has_active_clock_event_in_context(ctx, lhs)
                || expression_has_active_clock_event_in_context(ctx, rhs)
        }
        dae::Expression::Unary { rhs, .. } | dae::Expression::FieldAccess { base: rhs, .. } => {
            expression_has_active_clock_event_in_context(ctx, rhs)
        }
        dae::Expression::If {
            branches,
            else_branch,
        } => {
            branches.iter().any(|(cond, value)| {
                expression_has_active_clock_event_in_context(ctx, cond)
                    || expression_has_active_clock_event_in_context(ctx, value)
            }) || expression_has_active_clock_event_in_context(ctx, else_branch)
        }
        dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => elements
            .iter()
            .any(|element| expression_has_active_clock_event_in_context(ctx, element)),
        dae::Expression::Range { start, step, end } => {
            expression_has_active_clock_event_in_context(ctx, start)
                || step
                    .as_deref()
                    .is_some_and(|value| expression_has_active_clock_event_in_context(ctx, value))
                || expression_has_active_clock_event_in_context(ctx, end)
        }
        dae::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
        } => {
            expression_has_active_clock_event_in_context(ctx, expr)
                || indices
                    .iter()
                    .any(|index| expression_has_active_clock_event_in_context(ctx, &index.range))
                || filter
                    .as_deref()
                    .is_some_and(|value| expression_has_active_clock_event_in_context(ctx, value))
        }
        dae::Expression::Index { base, subscripts } => {
            expression_has_active_clock_event_in_context(ctx, base)
                || subscripts.iter().any(|sub| match sub {
                    dae::Subscript::Expr(value) => {
                        expression_has_active_clock_event_in_context(ctx, value)
                    }
                    _ => false,
                })
        }
        dae::Expression::VarRef { .. } | dae::Expression::Literal(_) | dae::Expression::Empty => {
            false
        }
    }
}

pub fn discrete_clock_event_active_from_sources<'a, FClockName, FClockEdge, FSampleActive>(
    dae: &'a dae::Dae,
    env: &'a VarEnv<f64>,
    sources: &HashMap<String, &'a dae::Expression>,
    active_solutions: &[&'a dae::Expression],
    is_clock_function_name: FClockName,
    eval_clock_edge_assignment: FClockEdge,
    eval_sample_clock_active: FSampleActive,
) -> bool
where
    FClockName: Copy + Fn(&str) -> bool,
    FClockEdge: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> Option<f64>,
    FSampleActive: Copy + Fn(&dae::Dae, &dae::Expression, &VarEnv<f64>) -> bool,
{
    let ctx = ClockInferenceContext {
        dae,
        env,
        sources,
        is_clock_function_name,
        eval_clock_edge_assignment,
        eval_sample_clock_active,
    };
    active_solutions.iter().any(|solution| {
        if expression_has_active_clock_event(
            dae,
            solution,
            env,
            is_clock_function_name,
            eval_clock_edge_assignment,
            eval_sample_clock_active,
        ) {
            return true;
        }
        if !expression_requires_clock_inference_from_sources(
            solution,
            sources,
            MAX_CLOCK_INFERENCE_DEPTH,
            &mut HashSet::new(),
        ) {
            return false;
        }
        infer_clock_active_from_expression(
            &ctx,
            solution,
            MAX_CLOCK_INFERENCE_DEPTH,
            &mut HashSet::new(),
        )
        .is_some_and(|active| active)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_core::Span;
    use rumoca_ir_core::OpBinary;
    use rumoca_ir_dae as dae;

    fn sample_internal_id_start_period(start: f64, period: f64) -> dae::Expression {
        dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Integer(1)),
                dae::Expression::Literal(dae::Literal::Real(start)),
                dae::Expression::Literal(dae::Literal::Real(period)),
            ],
        }
    }

    fn assign_clock_alias_expr() -> dae::Expression {
        dae::Expression::VarRef {
            name: dae::VarName::new("assignClock1.clock"),
            subscripts: vec![],
        }
    }

    fn periodic_clock_if_expr(use_residual: bool) -> dae::Expression {
        let resolution_lt_s = dae::Expression::Binary {
            op: rumoca_ir_core::OpBinary::Lt(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("periodicClock.resolution"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("Modelica.Clocked.Types.Resolution.s"),
                subscripts: vec![],
            }),
        };
        let subsample_clock = dae::Expression::FunctionCall {
            name: dae::VarName::new("subSample"),
            args: vec![
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("Clock"),
                    args: vec![dae::Expression::VarRef {
                        name: dae::VarName::new("periodicClock.factor"),
                        subscripts: vec![],
                    }],
                    is_constructor: false,
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("periodicClock.resolutionFactor"),
                    subscripts: vec![],
                },
            ],
            is_constructor: false,
        };
        let full_clock = dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![
                dae::Expression::VarRef {
                    name: dae::VarName::new("periodicClock.factor"),
                    subscripts: vec![],
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("periodicClock.resolutionFactor"),
                    subscripts: vec![],
                },
            ],
            is_constructor: false,
        };
        let branch = |clock_expr| {
            if use_residual {
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("periodicClock.c"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(clock_expr),
                }
            } else {
                clock_expr
            }
        };
        dae::Expression::If {
            branches: vec![(resolution_lt_s, branch(subsample_clock))],
            else_branch: Box::new(branch(full_clock)),
        }
    }

    fn build_conditional_clock_alias_chain_dae(use_residual_conditional_clock: bool) -> dae::Dae {
        let mut dae_model = dae::Dae::default();
        dae_model
            .enum_literal_ordinals
            .insert("Modelica.Clocked.Types.Resolution.s".to_string(), 5);
        for name in [
            "periodicClock.resolution",
            "periodicClock.factor",
            "periodicClock.resolutionFactor",
        ] {
            dae_model.parameters.insert(
                dae::VarName::new(name),
                dae::Variable::new(dae::VarName::new(name)),
            );
        }
        for name in ["assignClock1.clock", "periodicClock.y"] {
            dae_model.algebraics.insert(
                dae::VarName::new(name),
                dae::Variable::new(dae::VarName::new(name)),
            );
        }
        dae_model.discrete_valued.insert(
            dae::VarName::new("periodicClock.c"),
            dae::Variable::new(dae::VarName::new("periodicClock.c")),
        );
        let conditional_clock = periodic_clock_if_expr(use_residual_conditional_clock);
        if use_residual_conditional_clock {
            dae_model.f_z.push(dae::Equation::residual(
                conditional_clock,
                Span::DUMMY,
                "periodicClock residual conditional clock",
            ));
        } else {
            dae_model.f_z.push(dae::Equation::explicit(
                dae::VarName::new("periodicClock.c"),
                conditional_clock,
                Span::DUMMY,
                "periodicClock.c = if resolution < s then subSample(Clock(factor), resolutionFactor) else Clock(factor, resolutionFactor)",
            ));
        }
        for origin in [
            "periodicClock.y = periodicClock.c",
            "periodicClock.y = assignClock1.clock",
        ] {
            let rhs_name = if origin.ends_with("periodicClock.c") {
                "periodicClock.c"
            } else {
                "assignClock1.clock"
            };
            dae_model.f_x.push(dae::Equation::residual(
                dae::Expression::Binary {
                    op: rumoca_ir_core::OpBinary::Sub(Default::default()),
                    lhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new("periodicClock.y"),
                        subscripts: vec![],
                    }),
                    rhs: Box::new(dae::Expression::VarRef {
                        name: dae::VarName::new(rhs_name),
                        subscripts: vec![],
                    }),
                },
                Span::DUMMY,
                origin,
            ));
        }
        dae_model
    }

    fn conditional_clock_alias_env() -> VarEnv<f64> {
        let mut env = VarEnv::<f64>::new();
        env.set("periodicClock.resolution", 6.0);
        env.set("periodicClock.factor", 20.0);
        env.set("periodicClock.resolutionFactor", 1000.0);
        env
    }

    #[test]
    fn discrete_clock_event_active_from_sources_handles_two_arg_timing_clock() {
        let clock_expr = dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![
                dae::Expression::VarRef {
                    name: dae::VarName::new("count"),
                    subscripts: vec![],
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("resolution"),
                    subscripts: vec![],
                },
            ],
            is_constructor: false,
        };
        let dae_model = dae::Dae::default();
        let active = vec![&clock_expr];
        let sources: HashMap<String, &dae::Expression> = HashMap::new();

        let mut env = VarEnv::<f64>::new();
        env.set("count", 3.0);
        env.set("resolution", 10.0);
        env.set("time", 0.45);
        let off_tick = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |short| matches!(short, "Clock"),
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(
            !off_tick,
            "two-arg Clock(count, resolution) must be false between ticks"
        );

        env.set("time", 0.6);
        let on_tick = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |short| matches!(short, "Clock"),
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(
            on_tick,
            "two-arg Clock(count, resolution) must tick at exact periods"
        );
    }

    #[test]
    fn lowered_internal_sample_three_arg_is_treated_as_periodic_event_indicator() {
        let dae_model = dae::Dae::default();
        let sample_expr = sample_internal_id_start_period(0.3, 0.3);
        let active = vec![&sample_expr];
        let sources: HashMap<String, &dae::Expression> = HashMap::new();

        let mut env = VarEnv::<f64>::new();
        env.set("time", 0.45);
        let off_tick = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |_| false,
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(
            !off_tick,
            "3-arg lowered sample must be false between ticks"
        );

        env.set("time", 0.6);
        let on_tick = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |_| false,
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(on_tick, "3-arg lowered sample must tick at start+n*period");
    }

    #[test]
    fn discrete_clock_event_active_from_sources_infers_periodic_sample_alias() {
        let sample_expr = dae::Expression::BuiltinCall {
            function: dae::BuiltinFunction::Sample,
            args: vec![
                dae::Expression::Literal(dae::Literal::Real(0.3)),
                dae::Expression::Literal(dae::Literal::Real(0.3)),
            ],
        };
        let active_expr = dae::Expression::VarRef {
            name: dae::VarName::new("tick"),
            subscripts: vec![],
        };
        let active = vec![&active_expr];
        let dae_model = dae::Dae::default();
        let mut sources: HashMap<String, &dae::Expression> = HashMap::new();
        sources.insert("tick".to_string(), &sample_expr);

        let mut env = VarEnv::<f64>::new();
        env.set("time", 0.45);
        let off_tick = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |_| false,
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(
            !off_tick,
            "aliased periodic sample must be false between ticks"
        );

        env.set("time", 0.6);
        let on_tick = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |_| false,
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(
            on_tick,
            "aliased periodic sample must tick at start+n*period"
        );
    }

    #[test]
    fn discrete_clock_event_active_from_sources_ignores_plain_varref_alias_chain() {
        let source_expr = dae::Expression::Binary {
            op: OpBinary::Add(Default::default()),
            lhs: Box::new(dae::Expression::VarRef {
                name: dae::VarName::new("u"),
                subscripts: vec![],
            }),
            rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
        };
        let active_expr = dae::Expression::VarRef {
            name: dae::VarName::new("tick"),
            subscripts: vec![],
        };
        let active = vec![&active_expr];
        let dae_model = dae::Dae::default();
        let mut sources: HashMap<String, &dae::Expression> = HashMap::new();
        sources.insert("tick".to_string(), &source_expr);

        let mut env = VarEnv::<f64>::new();
        env.set("u", 2.0);
        env.set("time", 0.6);
        let active_now = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |_| false,
            |_dae, _expr, _env| None,
            |_dae, _expr, _env| false,
        );
        assert!(
            !active_now,
            "plain value aliases must not trigger clock inference"
        );
    }

    #[test]
    fn sample_clock_alias_source_expr_resolves_reverse_plain_equality_alias() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_m.push(dae::Equation::explicit(
            dae::VarName::new("periodicClock.y"),
            dae::Expression::VarRef {
                name: dae::VarName::new("sample1.clock"),
                subscripts: vec![],
            },
            rumoca_core::Span::DUMMY,
            "periodicClock.y = sample1.clock",
        ));

        let resolved =
            sample_clock_alias_source_expr(&dae_model, &dae::VarName::new("sample1.clock"), &[])
                .expect("reverse alias equality should resolve");

        assert_eq!(
            resolved,
            dae::Expression::VarRef {
                name: dae::VarName::new("periodicClock.y"),
                subscripts: vec![],
            }
        );
    }

    #[test]
    fn discrete_clock_event_active_from_sources_handles_noevent_wrapped_if_condition() {
        let clock_expr = dae::Expression::FunctionCall {
            name: dae::VarName::new("Clock"),
            args: vec![dae::Expression::Literal(dae::Literal::Real(1.0))],
            is_constructor: false,
        };
        let conditional = dae::Expression::If {
            branches: vec![(
                dae::Expression::BuiltinCall {
                    function: dae::BuiltinFunction::NoEvent,
                    args: vec![dae::Expression::Literal(dae::Literal::Boolean(true))],
                },
                dae::Expression::VarRef {
                    name: dae::VarName::new("clk"),
                    subscripts: vec![],
                },
            )],
            else_branch: Box::new(dae::Expression::Literal(dae::Literal::Boolean(false))),
        };
        let dae_model = dae::Dae::default();
        let active = vec![&conditional];
        let mut sources: HashMap<String, &dae::Expression> = HashMap::new();
        sources.insert("clk".to_string(), &clock_expr);

        let mut env = VarEnv::<f64>::new();
        env.set("time", 1.0);
        let active_now = discrete_clock_event_active_from_sources(
            &dae_model,
            &env,
            &sources,
            &active,
            |short| matches!(short, "Clock"),
            |_dae, expr, env| match expr {
                dae::Expression::FunctionCall { .. } => eval_scalar_expr_fast(expr, env),
                _ => None,
            },
            |_dae, _expr, _env| false,
        );
        assert!(
            active_now,
            "noEvent-wrapped true branch must still expose the clock event"
        );
    }

    #[test]
    fn dae_may_have_discrete_clock_activity_ignores_plain_discrete_varrefs() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("z"),
            dae::Expression::Binary {
                op: OpBinary::Add(Default::default()),
                lhs: Box::new(dae::Expression::VarRef {
                    name: dae::VarName::new("u"),
                    subscripts: vec![],
                }),
                rhs: Box::new(dae::Expression::Literal(dae::Literal::Real(1.0))),
            },
            Span::DUMMY,
            "test",
        ));
        assert!(!dae_may_have_discrete_clock_activity(&dae_model));
    }

    #[test]
    fn dae_may_have_discrete_clock_activity_detects_sample_and_clock_aliases() {
        let mut dae_model = dae::Dae::default();
        dae_model.f_x.push(dae::Equation::explicit(
            dae::VarName::new("clk"),
            dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![dae::Expression::Literal(dae::Literal::Real(0.1))],
                is_constructor: false,
            },
            Span::DUMMY,
            "test",
        ));
        dae_model.f_z.push(dae::Equation::explicit(
            dae::VarName::new("z"),
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Sample,
                args: vec![
                    dae::Expression::VarRef {
                        name: dae::VarName::new("u"),
                        subscripts: vec![],
                    },
                    dae::Expression::VarRef {
                        name: dae::VarName::new("clk"),
                        subscripts: vec![],
                    },
                ],
            },
            Span::DUMMY,
            "test",
        ));
        assert!(dae_may_have_discrete_clock_activity(&dae_model));
    }

    #[test]
    fn static_periodic_clock_event_active_matches_tick_instants() {
        let dae_model = dae::Dae {
            clock_schedules: vec![dae::ClockSchedule {
                period_seconds: 0.5,
                phase_seconds: 0.25,
            }],
            ..Default::default()
        };
        let mut env = VarEnv::<f64>::new();
        env.set("time", 0.75);
        assert_eq!(
            static_periodic_clock_event_active(&dae_model, &env),
            Some(true)
        );
        env.set("time", 0.6);
        assert_eq!(
            static_periodic_clock_event_active(&dae_model, &env),
            Some(false)
        );
    }

    #[test]
    fn sample_clock_arg_is_explicit_clock_rejects_plain_discrete_signal_varref() {
        let mut dae_model = dae::Dae::default();
        dae_model.discrete_valued.insert(
            dae::VarName::new("trig"),
            dae::Variable::new(dae::VarName::new("trig")),
        );
        let mut env = VarEnv::<f64>::new();
        env.set("trig", 1.0);

        assert!(
            !sample_clock_arg_is_explicit_clock(
                &dae_model,
                &dae::Expression::VarRef {
                    name: dae::VarName::new("trig"),
                    subscripts: vec![],
                },
                &env,
            ),
            "ordinary discrete signals are not explicit clock expressions"
        );
    }

    #[test]
    fn sample_clock_arg_is_explicit_clock_accepts_active_conditional_clock_branch() {
        let mut dae_model = dae::Dae::default();
        dae_model.parameters.insert(
            dae::VarName::new("use_fast"),
            dae::Variable::new(dae::VarName::new("use_fast")),
        );
        let mut env = VarEnv::<f64>::new();
        env.set("use_fast", 0.0);

        let conditional_clock = dae::Expression::If {
            branches: vec![(
                dae::Expression::VarRef {
                    name: dae::VarName::new("use_fast"),
                    subscripts: vec![],
                },
                dae::Expression::FunctionCall {
                    name: dae::VarName::new("subSample"),
                    args: vec![
                        dae::Expression::FunctionCall {
                            name: dae::VarName::new("Clock"),
                            args: vec![dae::Expression::Literal(dae::Literal::Real(20.0))],
                            is_constructor: false,
                        },
                        dae::Expression::Literal(dae::Literal::Real(1000.0)),
                    ],
                    is_constructor: false,
                },
            )],
            else_branch: Box::new(dae::Expression::FunctionCall {
                name: dae::VarName::new("Clock"),
                args: vec![
                    dae::Expression::Literal(dae::Literal::Real(20.0)),
                    dae::Expression::Literal(dae::Literal::Real(1000.0)),
                ],
                is_constructor: false,
            }),
        };

        assert!(sample_clock_arg_is_explicit_clock(
            &dae_model,
            &conditional_clock,
            &env,
        ));
    }

    #[test]
    fn sample_clock_arg_is_explicit_clock_accepts_solver_alias_chain_to_conditional_clock() {
        let dae_model = build_conditional_clock_alias_chain_dae(false);
        let env = conditional_clock_alias_env();
        assert!(sample_clock_arg_is_explicit_clock(
            &dae_model,
            &assign_clock_alias_expr(),
            &env
        ));
    }

    #[test]
    fn sample_clock_arg_is_explicit_clock_accepts_residual_if_clock_alias_chain() {
        let dae_model = build_conditional_clock_alias_chain_dae(true);
        let env = conditional_clock_alias_env();
        assert!(sample_clock_arg_is_explicit_clock(
            &dae_model,
            &assign_clock_alias_expr(),
            &env
        ));
    }
}
