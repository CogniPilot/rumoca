use super::*;

pub(super) fn infer_clock_timing_next(
    expr: &rumoca_core::Expression,
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

pub(super) fn infer_clock_counter_form(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<f64> {
    if remaining_depth == 0 {
        return None;
    }
    match expr {
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            let short = name.last_segment();
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
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } => {
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

pub(super) fn infer_clock_constructor_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let first = args.first()?;
    if args.len() == 1
        && event_clock::expression_resolves_to_boolean_condition(first, constants, sources)
    {
        return None;
    }
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

pub(super) fn infer_subsample_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    if let Some(counter) =
        infer_clock_counter_form(args.first()?, constants, sources, remaining_depth, visiting)
    {
        let resolution =
            eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)?;
        return valid_positive_period(counter / resolution).map(|period| (period, 0.0));
    }
    let base =
        infer_clock_timing_next(args.first()?, constants, sources, remaining_depth, visiting)?;
    let factor = eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)?;
    valid_positive_period(base.0 * factor).map(|period| (period, base.1))
}

pub(super) fn infer_supersample_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let base =
        infer_clock_timing_next(args.first()?, constants, sources, remaining_depth, visiting)?;
    let factor = eval_positive_factor(args.get(1), constants, sources, remaining_depth, visiting)?;
    valid_positive_period(base.0 / factor).map(|period| (period, base.1))
}

pub(super) fn infer_shift_like_timing(
    short: &str,
    args: &[rumoca_core::Expression],
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
            // MLS §16.5.2: shiftSample/backSample shift by a fraction of the
            // source clock interval, not by an absolute number of seconds.
            (shift / resolution) * base.0
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

pub(super) fn infer_clock_timing_from_clock_function(
    short: &str,
    args: &[rumoca_core::Expression],
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

pub(super) fn infer_clock_timing_from_expr_list(
    exprs: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    exprs.iter().find_map(|expr| {
        infer_clock_timing_next(expr, constants, sources, remaining_depth, visiting)
    })
}

pub(super) fn infer_clock_timing_from_builtin_call(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    match function {
        rumoca_core::BuiltinFunction::Sample if args.len() >= 2 => {
            infer_clock_timing_next(&args[1], constants, sources, remaining_depth, visiting)
                .or_else(|| {
                    infer_sample_start_interval_timing(
                        args,
                        constants,
                        sources,
                        remaining_depth,
                        visiting,
                    )
                })
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
) -> Option<(f64, f64)> {
    let [start_expr, interval_expr, ..] = args else {
        return None;
    };
    let mut start_visiting = visiting.clone();
    let phase = eval_clock_scalar_with_sources(
        start_expr,
        constants,
        sources,
        remaining_depth,
        &mut start_visiting,
    )?;
    let mut interval_visiting = visiting.clone();
    let period = eval_clock_scalar_with_sources(
        interval_expr,
        constants,
        sources,
        remaining_depth,
        &mut interval_visiting,
    )?;
    valid_positive_period(period).map(|period| (period, phase))
}

pub(super) fn infer_clock_timing_from_var_ref(
    name: &rumoca_core::Reference,
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let key = canonical_var_ref_key(name, subscripts, constants)?;
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
) -> Option<(f64, f64)> {
    let key = canonical_var_name_key(name, subscripts, constants)?;
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
) -> Option<(f64, f64)> {
    if let Some(cached) = sources.timing_cache.borrow().get(&key).cloned() {
        return cached;
    }
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
        let base_key = base_key?;
        infer_clock_timing_from_source_entries(
            sources.get(base_key),
            constants,
            sources,
            remaining_depth,
            visiting,
        )
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
    sources.timing_cache.borrow_mut().insert(key, inferred);
    inferred
}

pub(super) fn infer_clock_timing_from_source_entries(
    source_exprs: Option<&Vec<&rumoca_core::Expression>>,
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

pub(super) fn infer_clock_timing_from_reverse_alias_sources(
    key: &str,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    sources.reverse_targets_for(key)?.iter().find_map(|target| {
        if !source_target_is_exact_component(target) {
            return None;
        }
        infer_clock_timing_next(
            &rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(target.as_str()),
                subscripts: vec![],
                span: rumoca_core::Span::DUMMY,
            },
            constants,
            sources,
            remaining_depth,
            visiting,
        )
    })
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
) -> Option<(f64, f64)> {
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

    dynamic_branch_values
        .iter()
        .find_map(|value| {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        })
        .or_else(|| {
            infer_clock_timing_next(else_branch, constants, sources, remaining_depth, visiting)
        })
}

pub(super) fn infer_clock_timing_from_subscripts(
    subscripts: &[rumoca_core::Subscript],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    subscripts.iter().find_map(|sub| {
        if let rumoca_core::Subscript::Expr { expr: value, .. } = sub {
            infer_clock_timing_next(value, constants, sources, remaining_depth, visiting)
        } else {
            None
        }
    })
}

pub(super) fn infer_clock_timing_from_range(
    start: &rumoca_core::Expression,
    step: Option<&rumoca_core::Expression>,
    end: &rumoca_core::Expression,
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

pub(super) fn infer_clock_timing_from_comprehension(
    expr: &rumoca_core::Expression,
    indices: &[rumoca_core::ComprehensionIndex],
    filter: Option<&rumoca_core::Expression>,
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

pub(super) fn infer_clock_timing_from_expr(
    expr: &rumoca_core::Expression,
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

pub(super) fn infer_clock_timing_from_function_call_expr(
    name: &rumoca_core::Reference,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
    let short = name.last_segment();
    if short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME && args.len() >= 2 {
        return infer_clock_timing_next(&args[1], constants, sources, remaining_depth, visiting)
            .or_else(|| {
                infer_sample_start_interval_timing(
                    args,
                    constants,
                    sources,
                    remaining_depth,
                    visiting,
                )
            });
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

pub(super) fn infer_clock_timing_from_expr_inner(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    sources: &SourceMap<'_>,
    remaining_depth: usize,
    visiting: &mut HashSet<String>,
) -> Option<(f64, f64)> {
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
            infer_clock_timing_next(lhs, constants, sources, remaining_depth, visiting).or_else(
                || infer_clock_timing_next(rhs, constants, sources, remaining_depth, visiting),
            )
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
        | rumoca_core::Expression::Empty { .. } => None,
    }
}

pub(super) struct ClockConstructorExprCollector<'a> {
    constants: &'a HashMap<String, f64>,
    out: &'a mut Vec<rumoca_core::Expression>,
}

impl ExpressionVisitor for ClockConstructorExprCollector<'_> {
    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if static_sample_start_interval_timing(function, args, self.constants).is_some() {
            // MLS §16.5.1: sample(start, interval) defines a periodic time
            // event and must participate in the runtime event schedule.
            self.out.push(rumoca_core::Expression::BuiltinCall {
                function: *function,
                args: args.to_vec(),
                span: rumoca_core::Span::DUMMY,
            });
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
        let short = name.last_segment();
        if short == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
            if sample_start_interval_timing(args, self.constants).is_some() {
                self.out.push(rumoca_core::Expression::FunctionCall {
                    name: name.clone(),
                    args: args.to_vec(),
                    is_constructor,
                    span: rumoca_core::Span::DUMMY,
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
                span: rumoca_core::Span::DUMMY,
            });
        }
        for arg in args {
            self.visit_expression(arg);
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

pub(super) fn static_sample_start_interval_timing(
    function: &rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
) -> Option<(f64, f64)> {
    if *function != rumoca_core::BuiltinFunction::Sample {
        return None;
    }
    sample_start_interval_timing(args, constants)
}

pub(super) fn sample_start_interval_timing(
    args: &[rumoca_core::Expression],
    constants: &HashMap<String, f64>,
) -> Option<(f64, f64)> {
    let [start_expr, interval_expr, ..] = args else {
        return None;
    };
    let phase = eval_scalar_const_expr(start_expr, constants)?;
    let period = eval_scalar_const_expr(interval_expr, constants)?;
    valid_positive_period(period).map(|period| (period, phase))
}

pub(super) fn collect_clock_constructor_exprs(
    expr: &rumoca_core::Expression,
    constants: &HashMap<String, f64>,
    out: &mut Vec<rumoca_core::Expression>,
) {
    let mut collector = ClockConstructorExprCollector { constants, out };
    collector.visit_expression(expr);
}
