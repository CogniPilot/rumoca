use super::*;

type ClockTimingResult = Result<Option<ClockLattice>, rumoca_core::ClockLatticeErrorKind>;

fn first_clock_timing(
    candidates: impl IntoIterator<Item = ClockTimingResult>,
) -> ClockTimingResult {
    for candidate in candidates {
        if let Some(lattice) = candidate? {
            return Ok(Some(lattice));
        }
    }
    Ok(None)
}

pub(super) fn infer_clock_timing_next(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    if remaining_depth == 0 {
        return Ok(None);
    }
    infer_clock_timing_from_expr(
        expr,
        constants,
        sources,
        remaining_depth.saturating_sub(1),
        visiting,
    )
}

pub(super) fn infer_clock_constructor_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    if args.len() == 1
        && event_clock::expression_resolves_to_boolean_condition(first, constants, sources)
    {
        return Ok(None);
    }
    if let Some(base) =
        infer_clock_timing_next(first, constants, sources, remaining_depth, visiting)?
    {
        return Ok(Some(base));
    }
    if args.len() >= 2
        && let Some(lattice) =
            infer_interval_counter_clock(args, constants, sources, remaining_depth, visiting)?
    {
        // MLS §16.3 `Clock(intervalCounter, resolution)` is an exact rational
        // period; it never goes through seconds.
        return Ok(Some(lattice));
    }
    let Some(period) =
        eval_clock_scalar_with_sources(first, constants, sources, remaining_depth, visiting)
    else {
        return Ok(None);
    };
    periodic_lattice_from_seconds(period, 0.0)
}

/// MLS §16.3: `Clock(intervalCounter, resolution)`.
///
/// With the Integer arguments the spec mandates, the period is the exact
/// rational `intervalCounter / resolution`. Non-integral arguments are not
/// legal Modelica; they keep the seconds quotient so no previously accepted
/// model starts failing here.
fn infer_interval_counter_clock(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    let Some(count) =
        eval_clock_scalar_with_sources(first, constants, sources, remaining_depth, visiting)
    else {
        return Ok(None);
    };
    let Some(resolution) =
        eval_clock_scalar_with_sources(&args[1], constants, sources, remaining_depth, visiting)
    else {
        return Ok(None);
    };
    if !resolution.is_finite() || resolution <= 0.0 {
        return Ok(None);
    }
    if let (Some(counter), Some(resolution)) = (
        exact_positive_integer(count)?,
        exact_positive_integer(resolution)?,
    ) {
        return ClockLattice::from_interval_counter(counter, resolution).map(Some);
    }
    periodic_lattice_from_seconds(count / resolution, 0.0)
}

pub(super) fn infer_subsample_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    // MLS §16.5.2 Operator 16.9: the clock of `subSample(u, factor)` is
    // `factor` times slower than the clock of `u`, so the period is multiplied.
    // This is also the shape MSL 4.1.0 `PeriodicExactClock` emits —
    // `subSample(Clock(factor), resolutionFactor)` — which its own source
    // documents as equivalent to `Clock(factor*resolutionFactor, 1)`.
    let Some(first) = args.first() else {
        return Ok(None);
    };
    let Some(base) = infer_clock_timing_next(first, constants, sources, remaining_depth, visiting)?
    else {
        return Ok(None);
    };
    let Some(factor) =
        eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)?
    else {
        return Ok(None);
    };
    base.sub_sample(factor).map(Some)
}

pub(super) fn infer_supersample_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    let Some(base) = infer_clock_timing_next(first, constants, sources, remaining_depth, visiting)?
    else {
        return Ok(None);
    };
    let Some(factor) =
        eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)?
    else {
        return Ok(None);
    };
    base.super_sample(factor).map(Some)
}

pub(super) fn infer_shift_like_timing(
    short: &str,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(first) = args.first() else {
        return Ok(None);
    };
    let Some(base) = infer_clock_timing_next(first, constants, sources, remaining_depth, visiting)?
    else {
        return Ok(None);
    };
    let Some(shift) = eval_clock_scalar_with_sources(
        args.get(1).unwrap_or(first),
        constants,
        sources,
        remaining_depth,
        visiting,
    ) else {
        return Ok(None);
    };
    let Some(counter) = non_negative_integer(shift)? else {
        return Ok(None);
    };
    // MLS §16.5.2: shiftSample/backSample shift by `counter/resolution` of
    // interval(u), an exact rational fraction of the source clock interval,
    // not by an absolute number of seconds. `resolution` defaults to 1.
    let resolution = match args.get(2) {
        Some(expr) => {
            let Some(raw) =
                eval_clock_scalar_with_sources(expr, constants, sources, remaining_depth, visiting)
            else {
                return Ok(None);
            };
            let Some(resolution) = positive_integer(raw)? else {
                return Ok(None);
            };
            resolution
        }
        None => 1,
    };
    if short == "shiftSample" {
        base.shift_sample(counter, resolution).map(Some)
    } else {
        base.back_sample(counter, resolution).map(Some)
    }
}

pub(super) fn infer_clock_timing_from_clock_function(
    short: &str,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
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
        _ => Ok(None),
    }
}

pub(super) fn infer_clock_timing_from_expr_list(
    exprs: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    first_clock_timing(
        exprs.iter().map(|expr| {
            infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)
        }),
    )
}

pub(super) fn infer_clock_timing_from_builtin_call(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    match function {
        rumoca_core::BuiltinFunction::Sample if args.len() >= 2 => {
            let inferred =
                infer_clock_timing_next(&args[1], constants, sources, remaining_depth, visiting)?;
            if inferred.is_some() {
                return Ok(inferred);
            }
            infer_sample_start_interval_timing(args, constants, sources, remaining_depth, visiting)
        }
        rumoca_core::BuiltinFunction::Pre if !args.is_empty() => {
            infer_clock_timing_next(&args[0], constants, sources, remaining_depth, visiting)
        }
        _ => infer_clock_timing_from_expr_list(args, constants, sources, remaining_depth, visiting),
    }
}

pub(super) fn infer_sample_start_interval_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let [start_expr, interval_expr, ..] = args else {
        return Ok(None);
    };
    let mut start_visiting = visiting.clone();
    let Some(phase) = eval_clock_scalar_with_sources(
        start_expr,
        constants,
        sources,
        remaining_depth,
        &mut start_visiting,
    ) else {
        return Ok(None);
    };
    let mut interval_visiting = visiting.clone();
    let Some(period) = eval_clock_scalar_with_sources(
        interval_expr,
        constants,
        sources,
        remaining_depth,
        &mut interval_visiting,
    ) else {
        return Ok(None);
    };
    periodic_lattice_from_seconds(period, phase)
}

pub(super) fn infer_clock_timing_from_var_ref(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(key) = canonical_var_ref_key(name, subscripts, constants) else {
        return Ok(None);
    };
    let base_key = (!subscripts.is_empty()).then(|| name.as_str().to_string());
    infer_clock_timing_from_key(
        key,
        base_key.as_deref(),
        constants,
        sources,
        remaining_depth,
        visiting,
    )
}

pub(super) fn infer_clock_timing_from_var_name(
    name: &rumoca_core::VarName,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(key) = canonical_var_name_key(name, subscripts, constants) else {
        return Ok(None);
    };
    let base_key = (!subscripts.is_empty()).then(|| name.as_str().to_string());
    infer_clock_timing_from_key(
        key,
        base_key.as_deref(),
        constants,
        sources,
        remaining_depth,
        visiting,
    )
}

pub(super) fn infer_clock_timing_from_key(
    key: String,
    base_key: Option<&str>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    if let Some(cached) = sources.timing_cache.borrow().get(&key).cloned() {
        return cached;
    }
    if !visiting.insert(key.clone()) {
        return Ok(None);
    }
    let mut inferred = infer_clock_timing_from_source_entries(
        sources.get(&key),
        constants,
        sources,
        remaining_depth,
        visiting,
    )?;
    if inferred.is_none()
        && let Some(base_key) = base_key
    {
        inferred = infer_clock_timing_from_source_entries(
            sources.get(base_key),
            constants,
            sources,
            remaining_depth,
            visiting,
        )?;
    }
    if inferred.is_none() {
        inferred = infer_clock_timing_from_reverse_alias_sources(
            &key,
            constants,
            sources,
            remaining_depth,
            visiting,
        )?;
    }
    visiting.remove(&key);
    sources.timing_cache.borrow_mut().insert(key, Ok(inferred));
    Ok(inferred)
}

pub(super) fn infer_clock_timing_from_source_entries(
    source_exprs: Option<&Vec<&rumoca_core::Expression>>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(exprs) = source_exprs else {
        return Ok(None);
    };
    first_clock_timing(
        exprs.iter().map(|expr| {
            infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)
        }),
    )
}

pub(super) fn infer_clock_timing_from_reverse_alias_sources(
    key: &str,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let Some(targets) = sources.reverse_targets_for(key) else {
        return Ok(None);
    };
    first_clock_timing(targets.iter().map(|target| {
        if !source_target_is_exact_component(&target.name) {
            return Ok(None);
        }
        infer_clock_timing_next(
            &rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(target.name.as_str()),
                subscripts: vec![],
                span: target.span,
            },
            constants,
            sources,
            remaining_depth,
            visiting,
        )
    }))
}

pub(super) fn source_target_is_exact_component(target: &str) -> bool {
    dae::component_base_name(target).is_some_and(|base| base == target)
}

pub(super) fn infer_clock_timing_from_if_expr(
    branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
    else_branch: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let mut dynamic_branch_values = Vec::new();
    for (condition, value) in branches {
        match eval_scalar_const_expr(condition, constants) {
            Some(flag) if flag != 0.0 => {
                return infer_clock_timing_next(
                    value,
                    constants,
                    sources,
                    remaining_depth,
                    visiting,
                );
            }
            Some(_) => {}
            None => dynamic_branch_values.push(value),
        }
    }

    let inferred = first_clock_timing(dynamic_branch_values.iter().map(|value| {
        infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
    }))?;
    if inferred.is_some() {
        return Ok(inferred);
    }
    infer_clock_timing_next(else_branch, constants, sources, remaining_depth, visiting)
}

pub(super) fn infer_clock_timing_from_subscripts(
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    first_clock_timing(subscripts.iter().map(|sub| {
        if let rumoca_core::Subscript::Expr { expr: value, .. } = sub {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        } else {
            Ok(None)
        }
    }))
}

pub(super) fn infer_clock_timing_from_range(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let mut candidates = vec![start, end];
    if let Some(step) = step {
        candidates.insert(1, step);
    }
    first_clock_timing(
        candidates.into_iter().map(|value| {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        }),
    )
}

pub(super) fn infer_clock_timing_from_comprehension(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let inferred = infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)?;
    if inferred.is_some() {
        return Ok(inferred);
    }
    let inferred = first_clock_timing(indices.iter().map(|idx| {
        infer_clock_timing_next(&idx.range, constants, sources, remaining_depth, visiting)
    }))?;
    if inferred.is_some() {
        return Ok(inferred);
    }
    match filter {
        Some(value) => {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        }
        None => Ok(None),
    }
}

pub(super) fn infer_clock_timing_from_expr(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    if remaining_depth == 0 {
        return Ok(None);
    }
    infer_clock_timing_from_expr_inner(expr, constants, sources, remaining_depth, visiting)
}

pub(super) fn infer_clock_timing_from_function_call_expr(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let short = name.last_segment();
    if is_sample_timing_function_name(short) && args.len() >= 2 {
        if let Some(sample_args) = internal_sample_start_interval_args(short, args) {
            return infer_sample_start_interval_timing(
                sample_args,
                constants,
                sources,
                remaining_depth,
                visiting,
            );
        }
        let inferred =
            infer_clock_timing_next(&args[1], constants, sources, remaining_depth, visiting)?;
        if inferred.is_some() {
            return Ok(inferred);
        }
        return infer_sample_start_interval_timing(
            args,
            constants,
            sources,
            remaining_depth,
            visiting,
        );
    }
    if matches!(
        short,
        "Clock" | "subSample" | "superSample" | "shiftSample" | "backSample"
    ) {
        return infer_clock_timing_from_clock_function(
            short,
            args,
            constants,
            sources,
            remaining_depth,
            visiting,
        );
    }
    infer_clock_timing_from_expr_list(args, constants, sources, remaining_depth, visiting)
}

pub(super) fn infer_clock_timing_from_index_expr(
    base: &rumoca_core::Expression,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let inferred = infer_clock_timing_next(base, constants, sources, remaining_depth, visiting)?;
    if inferred.is_some() {
        return Ok(inferred);
    }
    infer_clock_timing_from_subscripts(subscripts, constants, sources, remaining_depth, visiting)
}

fn infer_clock_timing_from_binary(
    lhs: &rumoca_core::Expression,
    rhs: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    let inferred = infer_clock_timing_next(lhs, constants, sources, remaining_depth, visiting)?;
    if inferred.is_some() {
        return Ok(inferred);
    }
    infer_clock_timing_next(rhs, constants, sources, remaining_depth, visiting)
}

pub(super) fn infer_clock_timing_from_expr_inner(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> ClockTimingResult {
    match expr {
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            infer_clock_timing_from_function_call_expr(
                name,
                args,
                constants,
                sources,
                remaining_depth,
                visiting,
            )
        }
        rumoca_core::Expression::BuiltinCall { function, args, .. } => {
            infer_clock_timing_from_builtin_call(
                *function,
                args,
                constants,
                sources,
                remaining_depth,
                visiting,
            )
        }
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => infer_clock_timing_from_var_ref(
            name,
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => infer_clock_timing_from_if_expr(
            branches,
            else_branch,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            infer_clock_timing_from_binary(lhs, rhs, constants, sources, remaining_depth, visiting)
        }
        rumoca_core::Expression::Unary { rhs, .. }
        | rumoca_core::Expression::FieldAccess { base: rhs, .. } => {
            infer_clock_timing_next(rhs, constants, sources, remaining_depth, visiting)
        }
        rumoca_core::Expression::Index {
            base, subscripts, ..
        } => infer_clock_timing_from_index_expr(
            base,
            subscripts,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::Array { elements, .. }
        | rumoca_core::Expression::Tuple { elements, .. } => infer_clock_timing_from_expr_list(
            elements,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => infer_clock_timing_from_range(
            start,
            step.as_deref(),
            end,
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => infer_clock_timing_from_comprehension(
            expr,
            indices,
            filter.as_deref(),
            constants,
            sources,
            remaining_depth,
            visiting,
        ),
        rumoca_core::Expression::Literal { value: _, .. }
        | rumoca_core::Expression::Empty { .. } => Ok(None),
    }
}

pub(super) struct ClockConstructorExprCollector<'a> {
    constants: &'a HashMap<String, f64>,
    out: &'a mut Vec<rumoca_core::Expression>,
}

impl ExpressionVisitor for ClockConstructorExprCollector<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        match expr {
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.visit_builtin_call_with_span(*function, args, *span),
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor,
                span,
            } => self.visit_function_call_with_span(name, args, *is_constructor, *span),
            _ => self.walk_expression(expr),
        }
    }

    fn visit_if(
        &mut self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
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

impl ClockConstructorExprCollector<'_> {
    fn visit_builtin_call_with_span(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) {
        if !matches!(
            static_sample_start_interval_timing(&function, args, self.constants),
            Ok(None)
        ) {
            // MLS §16.5.1: sample(start, interval) defines a periodic time
            // event and must participate in the runtime event schedule.
            self.out.push(rumoca_core::Expression::BuiltinCall {
                function,
                args: args.to_vec(),
                span,
            });
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call_with_span(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        is_constructor: bool,
        span: rumoca_core::Span,
    ) {
        let short = name.last_segment();
        if is_sample_timing_function_name(short) {
            if !matches!(
                function_sample_start_interval_timing(short, args, self.constants),
                Ok(None)
            ) {
                self.out.push(rumoca_core::Expression::FunctionCall {
                    name: name.clone(),
                    args: args.to_vec(),
                    is_constructor,
                    span,
                });
            }
            for arg in args {
                self.visit_expression(arg);
            }
            return;
        }
        if is_clock_constructor_function_name(short) {
            self.out.push(rumoca_core::Expression::FunctionCall {
                name: name.clone(),
                args: args.to_vec(),
                is_constructor,
                span,
            });
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

pub(super) fn static_sample_start_interval_timing(
    function: &rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
) -> ClockTimingResult {
    if *function != rumoca_core::BuiltinFunction::Sample {
        return Ok(None);
    }
    sample_start_interval_timing(args, constants)
}

pub(super) fn function_sample_start_interval_timing(
    short: &str,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
) -> ClockTimingResult {
    if short != rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
        && rumoca_core::source_temporal_function_short_name(short)
            .is_none_or(|name| name != "sample")
    {
        return Ok(None);
    }
    let args = internal_sample_start_interval_args(short, args).unwrap_or(args);
    sample_start_interval_timing(args, constants)
}

fn internal_sample_start_interval_args<'a>(
    short: &str,
    args: &'a [rumoca_core::Expression],
) -> Option<&'a [rumoca_core::Expression]> {
    (short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME && args.len() >= 3).then_some(&args[1..])
}

fn is_sample_timing_function_name(short: &str) -> bool {
    short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
        || rumoca_core::source_temporal_function_short_name(short)
            .is_some_and(|name| name == "sample")
}

pub(super) fn sample_start_interval_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
) -> ClockTimingResult {
    let [start_expr, interval_expr, ..] = args else {
        return Ok(None);
    };
    let Some(phase) = eval_scalar_const_expr(start_expr, constants) else {
        return Ok(None);
    };
    let Some(period) = eval_scalar_const_expr(interval_expr, constants) else {
        return Ok(None);
    };
    periodic_lattice_from_seconds(period, phase)
}

pub(super) fn collect_clock_constructor_exprs(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    out: &mut Vec<rumoca_core::Expression>,
) {
    let mut collector = ClockConstructorExprCollector { constants, out };
    collector.visit_expression(expr);
}
