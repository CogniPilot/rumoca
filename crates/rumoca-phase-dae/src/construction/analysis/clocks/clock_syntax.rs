use super::*;
use rumoca_eval_flat::constant::Value;

pub(super) fn bound_clock_plan(
    expression: &Expression,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
    coordinate: InstanceId,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let ordinals = expression_preorder_ordinals(expression)?;
    bound_clock_plan_at(&ordinals, expression, constants, clocks, plans, coordinate)
}

fn bound_clock_plan_at(
    ordinals: &HashMap<usize, u32>,
    expression: &Expression,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
    plans: &HashMap<InstanceId, ClockPlan>,
    coordinate: InstanceId,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let occurrence = ordinals
        .get(&expression_address(expression))
        .copied()
        .ok_or_else(|| {
            ToDaeError::internal("nested Clock definition has no issued expression occurrence")
        })?;
    let owner = if occurrence == 0 {
        ClockOwnerId::Coordinate(coordinate)
    } else {
        ClockOwnerId::Definition {
            coordinate,
            occurrence,
        }
    };
    if let Some(plan) = periodic_constructor(expression, constants, owner)? {
        return Ok(Some(plan));
    }
    if let Some(source) = whole_clock_reference(expression, clocks) {
        return Ok(plans.get(&source.instance_id).copied());
    }
    let Expression::BuiltinCall {
        function,
        args,
        span,
    } = expression
    else {
        return Err(ToDaeError::unresolved_clock_schedule(
            "Clock binding",
            "a Clock binding must be a constructor, alias, or exact derived clock",
            expression_span(expression)?,
        ));
    };
    let operator = function.name();
    let Some(source) = args.first() else {
        return Err(invalid_clock_operator(
            operator,
            "requires a source clock",
            *span,
        ));
    };
    let Some(source_plan) =
        bound_clock_plan_at(ordinals, source, constants, clocks, plans, coordinate)?
    else {
        return Ok(None);
    };
    let kind = clock_binding_transfer_kind(*function, args, constants, *span)?;
    let lattice = transfer_target_lattice(kind, source_plan.lattice).map_err(|source| {
        ToDaeError::from(dae::DaeConstructionError::InvalidClockLattice {
            source,
            span: *span,
        })
    })?;
    Ok(Some(ClockPlan {
        owner,
        lattice,
        constructor_span: *span,
        lineage: ClockLineage::Conversion {
            source: source_plan.owner,
            kind,
        },
    }))
}

fn clock_binding_transfer_kind(
    function: BuiltinFunction,
    args: &[Expression],
    constants: &EvalContext,
    span: Span,
) -> Result<dae::ClockTransferKind, ToDaeError> {
    let operator = function.name();
    let kind = match (&function, args) {
        (BuiltinFunction::SubSample, [_, factor]) => dae::ClockTransferKind::SubSample {
            factor: clock_positive(factor, constants, operator)?,
        },
        (BuiltinFunction::SuperSample, [_, factor]) => dae::ClockTransferKind::SuperSample {
            factor: clock_positive(factor, constants, operator)?,
        },
        (BuiltinFunction::ShiftSample, [_, counter]) => dae::ClockTransferKind::ShiftSample {
            counter: clock_nonnegative(counter, constants, operator)?,
            resolution: 1,
        },
        (BuiltinFunction::ShiftSample, [_, counter, resolution]) => {
            dae::ClockTransferKind::ShiftSample {
                counter: clock_nonnegative(counter, constants, operator)?,
                resolution: clock_positive(resolution, constants, operator)?,
            }
        }
        (BuiltinFunction::BackSample, [_, counter]) => dae::ClockTransferKind::BackSample {
            counter: clock_nonnegative(counter, constants, operator)?,
            resolution: 1,
        },
        (BuiltinFunction::BackSample, [_, counter, resolution]) => {
            dae::ClockTransferKind::BackSample {
                counter: clock_nonnegative(counter, constants, operator)?,
                resolution: clock_positive(resolution, constants, operator)?,
            }
        }
        (BuiltinFunction::NoClock, [_]) => {
            return Err(invalid_clock_operator(
                operator,
                "has no exact periodic lattice for checked clock ownership",
                span,
            ));
        }
        (
            BuiltinFunction::SubSample
            | BuiltinFunction::SuperSample
            | BuiltinFunction::ShiftSample
            | BuiltinFunction::BackSample
            | BuiltinFunction::NoClock,
            _,
        ) => {
            return Err(invalid_clock_operator(
                operator,
                "has invalid clock conversion arity",
                span,
            ));
        }
        _ => {
            return Err(ToDaeError::unresolved_clock_schedule(
                "Clock binding",
                "a Clock binding must use an exact predefined clock conversion",
                span,
            ));
        }
    };
    Ok(kind)
}

#[cfg(test)]
pub(in crate::construction) fn expression_preorder_ordinal(
    root: &Expression,
    target: &Expression,
) -> Option<u32> {
    expression_preorder_ordinals(root)
        .ok()?
        .get(&expression_address(target))
        .copied()
}

fn expression_preorder_ordinals(root: &Expression) -> Result<HashMap<usize, u32>, ToDaeError> {
    let mut ordinals = HashMap::new();
    let mut pending = vec![root];
    while let Some(expression) = pending.pop() {
        let ordinal = u32::try_from(ordinals.len())
            .map_err(|_| ToDaeError::internal("expression occurrence identity exceeds u32"))?;
        if ordinals
            .insert(expression_address(expression), ordinal)
            .is_some()
        {
            return Err(ToDaeError::internal(
                "one expression address appears twice in a Flat expression tree",
            ));
        }
        let children = expression_children(expression);
        pending.extend(children.into_iter().rev());
    }
    Ok(ordinals)
}

fn clock_integer(
    expression: &Expression,
    constants: &EvalContext,
    operator: &'static str,
) -> Result<i64, ToDaeError> {
    let span = expression_span(expression)?;
    let value = eval_expr(expression, constants).map_err(|error| {
        ToDaeError::from(dae::DaeConstructionError::InvalidClockParameter {
            operator,
            detail: format!("argument is not parameter-evaluable: {error}"),
            span: error.span().unwrap_or(span),
        })
    })?;
    match value {
        Value::Integer(value) => Ok(value),
        _ => Err(ToDaeError::from(
            dae::DaeConstructionError::InvalidClockParameter {
                operator,
                detail: "argument must evaluate to an exact scalar Integer".to_string(),
                span,
            },
        )),
    }
}

pub(super) fn clock_positive(
    expression: &Expression,
    constants: &EvalContext,
    operator: &'static str,
) -> Result<i64, ToDaeError> {
    let value = clock_integer(expression, constants, operator)?;
    if value <= 0 {
        return Err(ToDaeError::from(
            dae::DaeConstructionError::InvalidClockLattice {
                source: rumoca_core::ClockLatticeErrorKind::NonPositiveFactor,
                span: expression_span(expression)?,
            },
        ));
    }
    Ok(value)
}

pub(super) fn clock_nonnegative(
    expression: &Expression,
    constants: &EvalContext,
    operator: &'static str,
) -> Result<i64, ToDaeError> {
    let value = clock_integer(expression, constants, operator)?;
    if value < 0 {
        return Err(ToDaeError::from(
            dae::DaeConstructionError::InvalidClockParameter {
                operator,
                detail: "counter must be nonnegative".to_string(),
                span: expression_span(expression)?,
            },
        ));
    }
    Ok(value)
}

fn clock_strictly_positive_integer(
    expression: &Expression,
    constants: &EvalContext,
    role: &'static str,
) -> Result<i64, ToDaeError> {
    let value = clock_integer(expression, constants, "Clock")?;
    if value <= 0 {
        return Err(ToDaeError::from(
            dae::DaeConstructionError::InvalidClockParameter {
                operator: "Clock",
                detail: format!("{role} must be strictly positive"),
                span: expression_span(expression)?,
            },
        ));
    }
    Ok(value)
}

fn clock_interval_seconds(
    expression: &Expression,
    constants: &EvalContext,
) -> Result<f64, ToDaeError> {
    let span = expression_span(expression)?;
    let value = eval_expr(expression, constants).map_err(|error| {
        ToDaeError::from(dae::DaeConstructionError::InvalidClockParameter {
            operator: "Clock",
            detail: format!("interval is not parameter-evaluable: {error}"),
            span: error.span().unwrap_or(span),
        })
    })?;
    let seconds = match value {
        Value::Real(value) => value,
        Value::Integer(value) => value as f64,
        _ => {
            return Err(ToDaeError::from(
                dae::DaeConstructionError::InvalidClockParameter {
                    operator: "Clock",
                    detail: "interval must evaluate to a scalar Real".to_string(),
                    span,
                },
            ));
        }
    };
    if !seconds.is_finite() {
        return Err(ToDaeError::from(
            dae::DaeConstructionError::InvalidClockParameter {
                operator: "Clock",
                detail: "interval must evaluate to a finite scalar Real".to_string(),
                span,
            },
        ));
    }
    Ok(seconds)
}

pub(super) fn invalid_clock_operator(operator: &str, detail: &str, span: Span) -> ToDaeError {
    ToDaeError::unsupported_runtime_operator(operator, detail, span)
}

pub(super) fn named_sample_clock_plan(
    clock: &flat::Variable,
    plans: &HashMap<InstanceId, ClockPlan>,
    span: Span,
) -> Result<ClockPlan, ToDaeError> {
    plans.get(&clock.instance_id).copied().ok_or_else(|| {
        ToDaeError::unresolved_clock_schedule(
            clock.name.as_str(),
            "the clock operand of a value sample must resolve to a static schedule",
            span,
        )
    })
}

type SampledValueTarget<'flat> = (&'flat flat::Variable, Span, Option<&'flat flat::Variable>);

pub(super) fn sampled_value_target<'flat>(
    expression: &Expression,
    flat: &'flat flat::Model,
    clocks: &ClockCoordinates<'flat>,
) -> Option<SampledValueTarget<'flat>> {
    let (lhs, rhs) = subtraction_operands(expression)?;
    let Expression::VarRef {
        name, subscripts, ..
    } = lhs
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    let (_, clock) = value_sample_operands(rhs, clocks)?;
    let span = rhs.span()?;
    flat.variables
        .get(name.var_name())
        .map(|variable| (variable, span, clock))
}

pub(super) fn expression_mentions_value_sample(
    expression: &Expression,
    clocks: &ClockCoordinates<'_>,
) -> bool {
    value_sample_operands(expression, clocks).is_some()
        || expression_children(expression)
            .into_iter()
            .any(|child| expression_mentions_value_sample(child, clocks))
}

pub(super) fn subtraction_operands(expression: &Expression) -> Option<(&Expression, &Expression)> {
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

pub(super) fn whole_clock_reference<'flat>(
    expression: &Expression,
    clocks: &ClockCoordinates<'flat>,
) -> Option<&'flat flat::Variable> {
    let Expression::VarRef {
        name, subscripts, ..
    } = expression
    else {
        return None;
    };
    subscripts
        .is_empty()
        .then(|| clocks.resolve(name.var_name()))
        .flatten()
}

pub(super) fn periodic_constructor(
    expression: &Expression,
    constants: &EvalContext,
    owner: ClockOwnerId,
) -> Result<Option<ClockPlan>, ToDaeError> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Clock,
        args,
        span,
    } = expression
    else {
        return Ok(None);
    };
    let lattice = match args.as_slice() {
        // MLS §16.3 `Clock(interval)`: a period given in seconds.
        [interval] => {
            let interval_span = expression_span(interval)?;
            let seconds = clock_interval_seconds(interval, constants)?;
            ClockRational::from_seconds(seconds)
                .and_then(|period| ClockLattice::new(period, ClockRational::ZERO))
                .map_err(|source| (source, interval_span))
        }
        // MLS §16.3 `Clock(intervalCounter, resolution)`: the exact rational
        // period `intervalCounter / resolution` seconds, which is the only form
        // that keeps sub-millisecond periods free of binary rounding.
        [interval_counter, resolution] => {
            let resolution_span = expression_span(resolution)?;
            let interval_counter = clock_strictly_positive_integer(
                interval_counter,
                constants,
                "interval counter",
            )?;
            let resolution =
                clock_strictly_positive_integer(resolution, constants, "resolution")?;
            ClockLattice::from_interval_counter(interval_counter, resolution)
                .map_err(|source| (source, resolution_span))
        }
        _ => {
            return Err(ToDaeError::unsupported_runtime_operator(
                "Clock",
                "the canonical clock proof requires `Clock(interval)` or `Clock(intervalCounter, resolution)`",
                *span,
            ));
        }
    }
    .map_err(|(source, span)| {
        ToDaeError::from(dae::DaeConstructionError::InvalidClockLattice {
            source,
            span,
        })
    })?;
    Ok(Some(ClockPlan::periodic(owner, lattice, *span)))
}

/// MLS §16.7: clock partitioning is a static property of the model, so an
/// `if`-equation that defines a `Clock` coordinate must be decided by the
/// model's parameter values. Fold such an equation to the branch those values
/// select; a condition that is not parameter-evaluable has no static schedule.
pub(super) fn static_clock_branch<'expression>(
    residual: &'expression Expression,
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
) -> Result<&'expression Expression, ToDaeError> {
    let mut current = residual;
    while let Expression::If {
        branches,
        else_branch,
        span,
    } = current
    {
        if !expression_mentions_clock(current, clocks) {
            return Ok(current);
        }
        current = statically_selected_branch(branches, else_branch, constants).ok_or_else(|| {
            ToDaeError::unresolved_clock_schedule(
                "clock equation",
                "an `if` equation that defines a Clock coordinate needs parameter-evaluable Boolean conditions",
                *span,
            )
        })?;
    }
    Ok(current)
}

pub(super) fn insert_plan(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    target: &flat::Variable,
    plan: ClockPlan,
    span: Span,
) -> Result<(), ToDaeError> {
    if plans.contains_key(&target.instance_id) {
        return Err(ToDaeError::unresolved_clock_schedule(
            target.name.as_str(),
            "more than one semantic clock owner binds this coordinate",
            span,
        ));
    }
    plans.insert(target.instance_id, plan);
    Ok(())
}

/// Close the clock-coordinate plans over the model's own definitions.
///
/// Equation order carries no meaning, so a derived clock (`c = subSample(base,
/// 2)`) and an alias (`c = y`) both have to wait for their source to acquire a
/// plan. Both are replayed until nothing new resolves; a coordinate that never
/// acquires a plan is reported by the caller against its own declaration.
pub(super) fn resolve_clock_definitions(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    derived: &[(&flat::Variable, &Expression, Span)],
    aliases: &[(&flat::Variable, &flat::Variable, Span)],
    constants: &EvalContext,
    clocks: &ClockCoordinates<'_>,
) -> Result<(), ToDaeError> {
    loop {
        let mut progress = false;
        for (target, expression, span) in derived {
            if plans.contains_key(&target.instance_id) {
                continue;
            }
            let Some(plan) =
                bound_clock_plan(expression, constants, clocks, plans, target.instance_id)?
            else {
                continue;
            };
            insert_plan(plans, target, plan, *span)?;
            progress = true;
        }
        progress |= propagate_aliases(plans, aliases)?;
        if !progress {
            return Ok(());
        }
    }
}

fn propagate_aliases(
    plans: &mut HashMap<InstanceId, ClockPlan>,
    aliases: &[(&flat::Variable, &flat::Variable, Span)],
) -> Result<bool, ToDaeError> {
    let mut resolved = false;
    loop {
        let mut progress = false;
        for (lhs, rhs, span) in aliases {
            match (
                plans.get(&lhs.instance_id).copied(),
                plans.get(&rhs.instance_id).copied(),
            ) {
                (Some(lhs_plan), Some(rhs_plan)) if !lhs_plan.matches_exactly(rhs_plan) => {
                    return Err(ToDaeError::unresolved_clock_schedule(
                        format!("{} = {}", lhs.name, rhs.name),
                        "this clock alias joins distinct semantic clock owners",
                        *span,
                    ));
                }
                (Some(plan), None) => {
                    plans.insert(rhs.instance_id, plan);
                    progress = true;
                }
                (None, Some(plan)) => {
                    plans.insert(lhs.instance_id, plan);
                    progress = true;
                }
                (Some(_), Some(_)) | (None, None) => {}
            }
        }
        if !progress {
            return Ok(resolved);
        }
        resolved = true;
    }
}

pub(super) fn expression_mentions_clock(
    expression: &Expression,
    clocks: &ClockCoordinates<'_>,
) -> bool {
    // MLS §16.3 `sample(u, c)` names its clock as an operand of a *value*
    // sample, not as a clock definition. That occurrence is proven separately by
    // `validate_clocked_value_samples`, so it must not make the surrounding
    // equation a clock equation.
    if let Some((value, _)) = value_sample_operands(expression, clocks) {
        return expression_mentions_clock(value, clocks);
    }
    matches!(
        expression,
        Expression::VarRef { name, .. } if clocks.contains(name.var_name())
    ) || matches!(
        expression,
        Expression::BuiltinCall {
            function: BuiltinFunction::Clock,
            ..
        }
    ) || expression_children(expression)
        .into_iter()
        .any(|child| expression_mentions_clock(child, clocks))
}

/// Split an MLS §16.3 value sample into `(sampled value, named clock)`.
///
/// The one-operand form leaves the clock to §16.5.1 inference; the two-operand
/// form names it, and only a whole `Clock` coordinate can be that operand —
/// `sample(start, interval)` (MLS §3.7.5) keeps its Real second operand and is
/// deliberately not matched here.
fn value_sample_operands<'expression, 'flat>(
    expression: &'expression Expression,
    clocks: &ClockCoordinates<'flat>,
) -> Option<(&'expression Expression, Option<&'flat flat::Variable>)> {
    let Expression::BuiltinCall {
        function: BuiltinFunction::Sample,
        args,
        ..
    } = expression
    else {
        return None;
    };
    match args.as_slice() {
        [value] => Some((value, None)),
        [value, clock] => whole_clock_reference(clock, clocks).map(|clock| (value, Some(clock))),
        _ => None,
    }
}

pub(super) fn unsupported_clock_equation(equation: &flat::Equation) -> ToDaeError {
    ToDaeError::unresolved_clock_schedule(
        "clock equation",
        "a clock equation must be an exact whole-coordinate constructor or alias",
        equation.span,
    )
}
