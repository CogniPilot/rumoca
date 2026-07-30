use super::*;

pub(super) fn validate_condition_expression(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    states: &HashSet<VarName>,
    constants: &EvalContext,
    sample_lattices: &mut Vec<(Span, ClockLattice)>,
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
        } => validate_condition_expression(rhs, roles, states, constants, sample_lattices),
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            validate_condition_expression(lhs, roles, states, constants, sample_lattices)?;
            validate_condition_expression(rhs, roles, states, constants, sample_lattices)
        }
        _ => validate_expression(expression, roles, states),
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
        _ => validate_expression(expression, roles, states),
    }
}

fn evaluate_sample_lattice(
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
