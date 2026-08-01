use super::*;

pub(super) fn validate_condition_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<(), ToDaeError> {
    validate_condition_expression_in_context(
        expression,
        roles,
        states,
        constants,
        sample_lattices,
        PreContext::Continuous,
    )
}

/// Validate a condition a when-clause *body* evaluates.
///
/// These are the guards of if-equations and assertions written inside the body,
/// which the event instant reaches only after the clause has already activated.
/// MLS §3.7.5 therefore admits `pre()` of a continuous coordinate in them for
/// the same reason it admits one in the body's definitions. The clause's own
/// activation condition is a different context and uses
/// [`validate_condition_expression`].
pub(super) fn validate_when_condition_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    validate_condition_expression_in_context(
        expression,
        roles,
        states,
        constants,
        sample_lattices,
        when_body_context(clocked),
    )
}

fn validate_condition_expression_in_context(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
    when_clause: PreContext,
) -> Result<(), ToDaeError> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Initial,
            args,
            span,
        } => {
            if args.is_empty() {
                Ok(())
            } else {
                Err(ToDaeError::unsupported_runtime_operator(
                    "initial",
                    "initial() takes no arguments",
                    *span,
                ))
            }
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } => {
            let lattice = evaluate_sample_lattice(args, constants, *span)?;
            if !sample_lattices
                .iter()
                .any(|(existing, _)| *existing == *span)
            {
                sample_lattices.push((*span, lattice));
            }
            Ok(())
        }
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => validate_condition_expression_in_context(
            rhs,
            roles,
            states,
            constants,
            sample_lattices,
            when_clause,
        ),
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            validate_condition_expression_in_context(
                lhs,
                roles,
                states,
                constants,
                sample_lattices,
                when_clause,
            )?;
            validate_condition_expression_in_context(
                rhs,
                roles,
                states,
                constants,
                sample_lattices,
                when_clause,
            )
        }
        // MLS §8.5 states the vector form as one of the two ways to enable a
        // `when` during initialization — "`when initial() then` or
        // `when {…, initial(), …} then`" — and `lower_vector_condition` lowers
        // each element through the same condition tree as a scalar activation.
        // Validating the elements as plain expressions instead would reject
        // that spelling of `initial()`, and would let a `sample(...)` element
        // reach lowering with no collected clock lattice.
        Expression::Array { elements, .. } => {
            reject_rescheduling_initial_activation(elements, constants)?;
            for element in elements {
                validate_condition_expression_in_context(
                    element,
                    roles,
                    states,
                    constants,
                    sample_lattices,
                    when_clause,
                )?;
            }
            Ok(())
        }
        _ => validate_expression_in_context(expression, roles, states, when_clause),
    }
}

pub(super) fn validate_algorithm_condition(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
) -> Result<(), ToDaeError> {
    match expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Change,
            args,
            span,
        } => {
            let [argument] = args.as_slice() else {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "change(...) requires exactly one discrete coordinate",
                    *span,
                ));
            };
            let Some((name, _)) = derivative_reference(argument) else {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "change(...) requires a discrete coordinate reference",
                    *span,
                ));
            };
            if !matches!(
                roles.get(name.var_name()),
                Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
            ) {
                return Err(ToDaeError::unsupported_algorithm(
                    "model",
                    "change(...) requires a discrete coordinate",
                    *span,
                ));
            }
            Ok(())
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } => {
            let lattice = evaluate_sample_lattice(args, constants, *span)?;
            if !sample_lattices
                .iter()
                .any(|(existing, _)| *existing == *span)
            {
                sample_lattices.push((*span, lattice));
            }
            Ok(())
        }
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => validate_algorithm_condition(rhs, roles, states, constants, sample_lattices),
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            validate_algorithm_condition(lhs, roles, states, constants, sample_lattices)?;
            validate_algorithm_condition(rhs, roles, states, constants, sample_lattices)
        }
        // Same MLS §8.5 vector activation, reached through `lower_algorithm_when`.
        Expression::Array { elements, .. } => {
            reject_rescheduling_initial_activation(elements, constants)?;
            for element in elements {
                validate_algorithm_condition(element, roles, states, constants, sample_lattices)?;
            }
            Ok(())
        }
        _ => validate_expression(expression, roles, states),
    }
}

/// Reject the one vector activation this module newly made reachable and cannot
/// yet simulate: `initial()` beside a `time` threshold the event itself moves.
///
/// `when {time >= pre(nextEvent), initial()} then nextEvent := …` is the shape
/// `Modelica.Blocks.Sources.TimeTable` and `CombiTimeTable` are written in. The
/// reschedule it asks for has no checked DAE owner, and what rumoca produces is
/// not a missing trace but a wrong one — the accumulating threshold lags, so the
/// `when` fires a few times at the wrong instants and stops.
///
/// The gate is `initial()`, and that is not arbitrary: it is exactly the door
/// element-wise validation opened. A vector activation of plain relations was
/// already validated and accepted before — `when {time >= (pre(count) + 1) *
/// period, false}` simulates correctly and is covered by
/// `periodic_source_counter_regression`, and the same lag is reachable through
/// the scalar `when time >= pre(next)` either way, so neither is this module's
/// to reject. What was *not* reachable was any vector carrying `initial()`,
/// because `initial()` had no owner in plain expression validation and failed
/// the model outright. Validating the elements properly is right, and buys the
/// static forms their agreement with OpenModelica; this keeps the accident it
/// removed from turning into a silently wrong trace, and nothing wider.
fn reject_rescheduling_initial_activation(
    elements: &[Expression],
    constants: &EvalContext,
) -> Result<(), ToDaeError> {
    let enables_initialization = elements.iter().any(|element| {
        matches!(
            element,
            Expression::BuiltinCall {
                function: BuiltinFunction::Initial,
                ..
            }
        )
    });
    if !enables_initialization {
        return Ok(());
    }
    for element in elements {
        let Expression::Binary { op, lhs, rhs, span } = element else {
            continue;
        };
        if is_rescheduling_time_relation(op, lhs, rhs, constants) {
            return Err(ToDaeError::unsupported_runtime_operator(
                "when",
                "an `initial()` vector activation whose `time` threshold is moved by the event \
                 itself needs a rescheduled time event, which has no checked DAE owner yet",
                *span,
            ));
        }
    }
    Ok(())
}

pub(super) fn evaluate_sample_lattice(
    arguments: &[Expression],
    constants: &EvalContext,
    span: Span,
) -> Result<ClockLattice, ToDaeError> {
    let [start, interval] = arguments else {
        return Err(ToDaeError::unsupported_runtime_operator(
            "sample",
            "sample(start, interval) requires exactly two scalar parameter arguments",
            span,
        ));
    };
    let start = evaluate_clock_seconds(start, constants, "sample start", span)?;
    let interval = evaluate_clock_seconds(interval, constants, "sample interval", span)?;
    let phase = ClockRational::from_seconds(start).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("sample", error.to_string(), span)
    })?;
    let period = ClockRational::from_seconds(interval).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("sample", error.to_string(), span)
    })?;
    ClockLattice::new(period, phase).map_err(|error| {
        ToDaeError::unsupported_runtime_operator("sample", error.to_string(), span)
    })
}

pub(super) fn evaluate_clock_seconds(
    expression: &Expression,
    constants: &EvalContext,
    owner: &'static str,
    span: Span,
) -> Result<f64, ToDaeError> {
    let value = eval_expr(expression, constants).map_err(|error| {
        ToDaeError::unsupported_runtime_operator(
            "sample",
            format!("{owner} is not parameter-evaluable: {error}"),
            span,
        )
    })?;
    value
        .to_real()
        .filter(|value| value.is_finite())
        .ok_or_else(|| {
            ToDaeError::unsupported_runtime_operator(
                "sample",
                format!("{owner} must evaluate to a finite scalar Real"),
                span,
            )
        })
}
