use super::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(in crate::construction) struct ClockPlan {
    pub(in crate::construction) lattice: ClockLattice,
    pub(in crate::construction) constructor_span: Span,
}

#[derive(Clone, Copy)]
pub(in crate::construction) struct SampledValuePlan {
    pub(in crate::construction) clock: ClockPlan,
    pub(in crate::construction) sample_span: Span,
}

pub(super) struct ClockAnalysis {
    pub(super) plans: HashMap<VarName, ClockPlan>,
    pub(super) equation_rows: HashSet<usize>,
    pub(super) sampled_values: HashMap<VarName, SampledValuePlan>,
}

pub(super) fn analyze_clocks(
    flat: &flat::Model,
    constants: &EvalContext,
) -> Result<ClockAnalysis, ToDaeError> {
    let clock_names = flat
        .variable_type_names
        .iter()
        .filter_map(|(name, type_name)| (type_name == "Clock").then_some(name.clone()))
        .collect::<HashSet<_>>();
    let mut plans = HashMap::new();
    let mut aliases = Vec::new();
    let mut equation_rows = HashSet::new();
    derive_bound_clock_plans(flat, constants, &clock_names, &mut plans)?;
    for (row, equation) in flat.equations.iter().enumerate() {
        let Some((lhs, rhs)) = subtraction_operands(&equation.residual) else {
            if expression_mentions_clock(&equation.residual, &clock_names) {
                return Err(unsupported_clock_equation(equation));
            }
            continue;
        };
        let lhs_clock = whole_clock_reference(lhs, &clock_names);
        let rhs_clock = whole_clock_reference(rhs, &clock_names);
        match (lhs_clock, rhs_clock, periodic_constructor(rhs, constants)?) {
            (Some(target), None, Some(plan)) => {
                insert_plan(&mut plans, target, plan, equation.span)?;
                equation_rows.insert(row);
            }
            (Some(lhs), Some(rhs), None) => {
                aliases.push((lhs.clone(), rhs.clone(), equation.span));
                equation_rows.insert(row);
            }
            _ if expression_mentions_clock(&equation.residual, &clock_names) => {
                return Err(unsupported_clock_equation(equation));
            }
            _ => {}
        }
    }

    propagate_aliases(&mut plans, &aliases)?;
    for name in &clock_names {
        if !plans.contains_key(name) {
            return Err(ToDaeError::unsupported_flat(
                "clock ownership proof",
                format!("clock coordinate `{name}` has no unique constructor through its aliases"),
                flat.variables[name].source_span,
            ));
        }
    }
    let sampled_values = analyze_sampled_values(flat, &plans, &equation_rows)?;
    Ok(ClockAnalysis {
        plans,
        equation_rows,
        sampled_values,
    })
}

fn derive_bound_clock_plans(
    flat: &flat::Model,
    constants: &EvalContext,
    clock_names: &HashSet<VarName>,
    plans: &mut HashMap<VarName, ClockPlan>,
) -> Result<(), ToDaeError> {
    for _ in 0..clock_names.len() {
        let mut progress = false;
        for name in clock_names {
            if plans.contains_key(name) {
                continue;
            }
            let Some(binding) = flat.variables[name].binding.as_ref() else {
                continue;
            };
            let Some(plan) = bound_clock_plan(binding, constants, clock_names, plans)? else {
                continue;
            };
            insert_plan(plans, name, plan, expression_span(binding)?)?;
            progress = true;
        }
        if !progress {
            break;
        }
    }
    Ok(())
}

fn bound_clock_plan(
    expression: &Expression,
    constants: &EvalContext,
    clock_names: &HashSet<VarName>,
    plans: &HashMap<VarName, ClockPlan>,
) -> Result<Option<ClockPlan>, ToDaeError> {
    if let Some(plan) = periodic_constructor(expression, constants)? {
        return Ok(Some(plan));
    }
    if let Some(name) = whole_clock_reference(expression, clock_names) {
        return Ok(plans.get(name).copied());
    }
    let Expression::FunctionCall {
        name, args, span, ..
    } = expression
    else {
        return Err(ToDaeError::unsupported_flat(
            "clock ownership proof",
            "a Clock binding must be a constructor, alias, or exact derived clock",
            expression_span(expression)?,
        ));
    };
    let operator = name.as_str();
    let Some(source) = args.first() else {
        return Err(invalid_clock_operator(
            operator,
            "requires a source clock",
            *span,
        ));
    };
    let Some(source_name) = whole_clock_reference(source, clock_names) else {
        return Err(invalid_clock_operator(
            operator,
            "requires a whole Clock coordinate as its first argument",
            *span,
        ));
    };
    let Some(source_plan) = plans.get(source_name).copied() else {
        return Ok(None);
    };
    let lattice = match (operator, args.as_slice()) {
        ("subSample", [_, factor]) => source_plan
            .lattice
            .sub_sample(clock_integer(factor, constants, operator, *span)?),
        ("superSample", [_, factor]) => source_plan
            .lattice
            .super_sample(clock_integer(factor, constants, operator, *span)?),
        ("shiftSample", [_, counter]) => source_plan
            .lattice
            .shift_sample(clock_integer(counter, constants, operator, *span)?, 1),
        ("shiftSample", [_, counter, resolution]) => source_plan.lattice.shift_sample(
            clock_integer(counter, constants, operator, *span)?,
            clock_integer(resolution, constants, operator, *span)?,
        ),
        ("backSample", [_, counter]) => source_plan
            .lattice
            .back_sample(clock_integer(counter, constants, operator, *span)?, 1),
        ("backSample", [_, counter, resolution]) => source_plan.lattice.back_sample(
            clock_integer(counter, constants, operator, *span)?,
            clock_integer(resolution, constants, operator, *span)?,
        ),
        ("noClock", [_]) => {
            return Err(invalid_clock_operator(
                operator,
                "has no exact periodic lattice for checked clock ownership",
                *span,
            ));
        }
        ("subSample" | "superSample" | "shiftSample" | "backSample" | "noClock", _) => {
            return Err(invalid_clock_operator(
                operator,
                "has invalid clock conversion arity",
                *span,
            ));
        }
        _ => {
            return Err(ToDaeError::unresolved_reference(operator, *span));
        }
    }
    .map_err(|error| {
        ToDaeError::unsupported_runtime_operator(operator, error.to_string(), *span)
    })?;
    Ok(Some(ClockPlan {
        lattice,
        constructor_span: *span,
    }))
}

fn clock_integer(
    expression: &Expression,
    constants: &EvalContext,
    operator: &str,
    span: Span,
) -> Result<i64, ToDaeError> {
    eval_expr(expression, constants)
        .ok()
        .and_then(|value| value.as_integer())
        .ok_or_else(|| {
            invalid_clock_operator(
                operator,
                "requires parameter-evaluable Integer conversion arguments",
                span,
            )
        })
}

fn invalid_clock_operator(operator: &str, detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_runtime_operator(operator, detail, span)
}

fn analyze_sampled_values(
    flat: &flat::Model,
    plans: &HashMap<VarName, ClockPlan>,
    clock_equation_rows: &HashSet<usize>,
) -> Result<HashMap<VarName, SampledValuePlan>, ToDaeError> {
    let inferred = flat
        .variables
        .keys()
        .filter_map(|name| plans.get(name).copied())
        .next();
    let Some(inferred) = inferred else {
        if let Some(equation) = flat
            .equations
            .iter()
            .find(|equation| expression_mentions_value_sample(&equation.residual))
        {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value) has no inferred clock constructor",
                equation.span,
            ));
        }
        return Ok(HashMap::new());
    };
    if plans.values().any(|plan| plan != &inferred) {
        for (row, equation) in flat.equations.iter().enumerate() {
            if !clock_equation_rows.contains(&row)
                && expression_mentions_value_sample(&equation.residual)
            {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    "sample(value) has more than one possible inferred clock",
                    equation.span,
                ));
            }
        }
    }
    let mut sampled = HashMap::new();
    for (row, equation) in flat.equations.iter().enumerate() {
        if clock_equation_rows.contains(&row) {
            continue;
        }
        if let Some((target, sample_span)) = sampled_value_target(&equation.residual) {
            if plans.values().any(|plan| plan != &inferred) {
                continue;
            }
            let plan = SampledValuePlan {
                clock: inferred,
                sample_span,
            };
            if sampled.insert(target.clone(), plan).is_some() {
                return Err(ToDaeError::unsupported_flat(
                    "clocked sample ownership proof",
                    format!("sampled coordinate `{target}` has more than one definition"),
                    equation.span,
                ));
            }
        } else if expression_mentions_value_sample(&equation.residual) {
            return Err(ToDaeError::unsupported_flat(
                "clocked sample ownership proof",
                "sample(value) must be the complete right-hand side of one coordinate definition",
                equation.span,
            ));
        }
    }
    Ok(sampled)
}

fn sampled_value_target(expression: &Expression) -> Option<(&VarName, Span)> {
    let (lhs, rhs) = subtraction_operands(expression)?;
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs
    else {
        return None;
    };
    let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        span,
        ..
    } = rhs
    else {
        return None;
    };
    (subscripts.is_empty() && args.len() == 1).then_some((name.var_name(), *span))
}

fn expression_mentions_value_sample(expression: &Expression) -> bool {
    matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            ..
        } if args.len() == 1
    ) || expression_children(expression)
        .into_iter()
        .any(expression_mentions_value_sample)
}

fn subtraction_operands(expression: &Expression) -> Option<(&Expression, &Expression)> {
    let Expression::Binary {
        op: OpBinary::Sub,
        lhs,
        rhs,
        ..
    } = expression
    else {
        return None;
    };
    Some((lhs, rhs))
}

fn whole_clock_reference<'expression>(
    expression: &'expression Expression,
    clock_names: &HashSet<VarName>,
) -> Option<&'expression VarName> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    (subscripts.is_empty() && clock_names.contains(name.var_name())).then_some(name.var_name())
}

fn periodic_constructor(
    expression: &Expression,
    constants: &EvalContext,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let Expression::FunctionCall {
        name, args, span, ..
    } = expression
    else {
        return Ok(None);
    };
    if name.as_str() != "Clock" {
        return Ok(None);
    }
    let [interval] = args.as_slice() else {
        return Err(ToDaeError::unsupported_runtime_operator(
            "Clock",
            "the canonical clock proof currently requires Clock(interval)",
            *span,
        ));
    };
    let seconds = evaluate_clock_seconds(interval, constants, "Clock interval", *span)?;
    let period = ClockRational::from_seconds(seconds).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("Clock", error.to_string(), *span)
    })?;
    let lattice = ClockLattice::new(period, ClockRational::ZERO).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("Clock", error.to_string(), *span)
    })?;
    Ok(Some(ClockPlan {
        lattice,
        constructor_span: *span,
    }))
}

fn insert_plan(
    plans: &mut HashMap<VarName, ClockPlan>,
    target: &VarName,
    plan: ClockPlan,
    span: Span,
) -> Result<(), ToDaeError> {
    if let Some(existing) = plans.get(target)
        && existing.lattice != plan.lattice
    {
        return Err(ToDaeError::unsupported_flat(
            "clock ownership proof",
            format!("clock coordinate `{target}` has conflicting constructors"),
            span,
        ));
    }
    plans.insert(target.clone(), plan);
    Ok(())
}

fn propagate_aliases(
    plans: &mut HashMap<VarName, ClockPlan>,
    aliases: &[(VarName, VarName, Span)],
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for (lhs, rhs, span) in aliases {
            match (plans.get(lhs).copied(), plans.get(rhs).copied()) {
                (Some(lhs_plan), Some(rhs_plan)) if lhs_plan.lattice != rhs_plan.lattice => {
                    return Err(ToDaeError::unsupported_flat(
                        "clock ownership proof",
                        format!("clock alias `{lhs} = {rhs}` joins conflicting constructors"),
                        *span,
                    ));
                }
                (Some(plan), None) => {
                    plans.insert(rhs.clone(), plan);
                    progress = true;
                }
                (None, Some(plan)) => {
                    plans.insert(lhs.clone(), plan);
                    progress = true;
                }
                (Some(_), Some(_)) | (None, None) => {}
            }
        }
        if !progress {
            return Ok(());
        }
    }
}

fn expression_mentions_clock(expression: &Expression, clock_names: &HashSet<VarName>) -> bool {
    matches!(
        expression,
        Expression::VarRef { name, .. } if clock_names.contains(name.var_name())
    ) || matches!(
        expression,
        Expression::FunctionCall { name, .. } if name.as_str() == "Clock"
    ) || expression_children(expression)
        .into_iter()
        .any(|child| expression_mentions_clock(child, clock_names))
}

fn unsupported_clock_equation(equation: &flat::Equation) -> ToDaeError {
    ToDaeError::unsupported_flat(
        "clock ownership proof",
        "a clock equation must be an exact whole-coordinate constructor or alias",
        equation.span,
    )
}
