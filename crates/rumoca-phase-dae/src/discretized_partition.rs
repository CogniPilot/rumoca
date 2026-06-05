use indexmap::IndexMap;
use rumoca_core::{Expression, Literal, OpBinary, Reference, Span, VarName};
use rumoca_ir_dae as dae;

use crate::ToDaeError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ClockSolver {
    ImplicitEuler,
    Unsupported,
}

#[derive(Debug, Default)]
struct ClockSolverInfo {
    use_solver: Option<bool>,
    method: Option<ClockSolver>,
}

/// MLS §16.8.1: continuous-time equations inside a clocked discretized
/// partition are evaluated by the clock's integration method.
pub(crate) fn lower_clocked_discretized_partitions(
    dae_model: &mut dae::Dae,
) -> Result<usize, ToDaeError> {
    let mut lowered = Vec::new();
    let mut lower_flags = Vec::with_capacity(dae_model.continuous.equations.len());
    let solver = active_clock_solver(dae_model);

    for equation in &dae_model.continuous.equations {
        let Some((state, derivative_rhs)) = clocked_state_derivative(equation, dae_model) else {
            lower_flags.push(false);
            continue;
        };
        require_implicit_euler_solver(solver, equation)?;
        lower_flags.push(true);
        lowered.push((
            state.clone(),
            derivative_rhs,
            equation.span,
            equation.origin.clone(),
        ));
    }
    let lowered_count = lowered.len();

    dae_model.continuous.equations = dae_model
        .continuous
        .equations
        .drain(..)
        .zip(lower_flags)
        .filter_map(|(equation, lower)| (!lower).then_some(equation))
        .collect();
    for (state, derivative_rhs, span, origin) in lowered {
        move_state_to_discrete_real(dae_model, &state, span)?;
        dae_model
            .discrete
            .real_updates
            .push(dae::Equation::explicit(
                state.clone(),
                implicit_euler_update(&state, derivative_rhs, span),
                span,
                format!("clocked implicit-euler discretization of {origin}"),
            ));
    }

    Ok(lowered_count)
}

fn clocked_state_derivative(
    equation: &dae::Equation,
    dae_model: &dae::Dae,
) -> Option<(VarName, Expression)> {
    let Expression::Binary { op, lhs, rhs, .. } = &equation.rhs else {
        return None;
    };
    if *op != OpBinary::Sub {
        return None;
    }

    if let Some(state) = derivative_state(lhs)
        && is_clocked_state(dae_model, &state)
    {
        return Some((state, *rhs.clone()));
    }
    if let Some(state) = derivative_state(rhs)
        && is_clocked_state(dae_model, &state)
    {
        return Some((state, *lhs.clone()));
    }
    None
}

fn derivative_state(expr: &Expression) -> Option<VarName> {
    let Expression::BuiltinCall {
        function: rumoca_core::BuiltinFunction::Der,
        args,
        ..
    } = expr
    else {
        return None;
    };
    let [
        Expression::VarRef {
            name, subscripts, ..
        },
    ] = args.as_slice()
    else {
        return None;
    };
    subscripts.is_empty().then(|| name.var_name().clone())
}

fn is_clocked_state(dae_model: &dae::Dae, state: &VarName) -> bool {
    dae_model.variables.states.contains_key(state)
        && dae_model.clocks.intervals.contains_key(state.as_str())
}

fn require_implicit_euler_solver(
    solver: Option<ClockSolver>,
    equation: &dae::Equation,
) -> Result<(), ToDaeError> {
    match solver {
        Some(ClockSolver::ImplicitEuler) => Ok(()),
        Some(ClockSolver::Unsupported) => Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "clocked discretized partition `{}` uses an unsupported solver method",
                equation.origin
            ),
            equation.span,
        )),
        None => Err(ToDaeError::runtime_contract_violation_at(
            format!(
                "clocked discretized partition `{}` requires an explicit supported solver method",
                equation.origin
            ),
            equation.span,
        )),
    }
}

fn move_state_to_discrete_real(
    dae_model: &mut dae::Dae,
    state: &VarName,
    span: Span,
) -> Result<(), ToDaeError> {
    let Some(variable) = dae_model.variables.states.shift_remove(state) else {
        return Err(ToDaeError::runtime_contract_violation_at(
            format!("clocked discretized state `{state}` was not present in x"),
            span,
        ));
    };
    dae_model
        .variables
        .discrete_reals
        .insert(state.clone(), variable);
    Ok(())
}

fn implicit_euler_update(state: &VarName, derivative_rhs: Expression, span: Span) -> Expression {
    add(
        previous(var(state, span), span),
        mul(
            interval(var(state, span), span),
            Expression::If {
                branches: vec![(first_tick(span), real(0.0, span))],
                else_branch: Box::new(derivative_rhs),
                span,
            },
            span,
        ),
        span,
    )
}

fn active_clock_solver(dae_model: &dae::Dae) -> Option<ClockSolver> {
    let mut solvers: IndexMap<Vec<String>, ClockSolverInfo> = IndexMap::new();
    for variable in dae_model.variables.parameters.values() {
        let Some((clock_path, field)) = clock_parameter_path(variable) else {
            continue;
        };
        let entry = solvers.entry(clock_path).or_default();
        match field.as_str() {
            "useSolver" => entry.use_solver = literal_bool(variable.start.as_ref()),
            "solverMethod" => entry.method = literal_solver(variable.start.as_ref()),
            _ => {}
        }
    }
    solvers
        .values()
        .find(|info| info.use_solver == Some(true))
        .map(|info| match info.method {
            Some(method) => method,
            None => ClockSolver::Unsupported,
        })
}

fn clock_parameter_path(variable: &dae::Variable) -> Option<(Vec<String>, String)> {
    let component_ref = variable.component_ref.as_ref()?;
    let (field, prefix) = component_ref.parts.split_last()?;
    let prefix = prefix.iter().map(|part| part.ident.clone()).collect();
    Some((prefix, field.ident.clone()))
}

fn literal_bool(expr: Option<&Expression>) -> Option<bool> {
    match expr {
        Some(Expression::Literal {
            value: Literal::Boolean(value),
            ..
        }) => Some(*value),
        _ => None,
    }
}

fn literal_solver(expr: Option<&Expression>) -> Option<ClockSolver> {
    match expr {
        Some(Expression::Literal {
            value: Literal::String(value),
            ..
        }) if value == "ImplicitEuler" => Some(ClockSolver::ImplicitEuler),
        Some(Expression::Literal {
            value: Literal::String(_),
            ..
        }) => Some(ClockSolver::Unsupported),
        _ => None,
    }
}

fn var(name: &VarName, span: Span) -> Expression {
    Expression::VarRef {
        name: Reference::from(name.as_str()),
        subscripts: Vec::new(),
        span,
    }
}

fn real(value: f64, span: Span) -> Expression {
    Expression::Literal {
        value: Literal::Real(value),
        span,
    }
}

fn call(name: &str, args: Vec<Expression>, span: Span) -> Expression {
    Expression::FunctionCall {
        name: Reference::from(name),
        args,
        is_constructor: false,
        span,
    }
}

fn previous(arg: Expression, span: Span) -> Expression {
    call("previous", vec![arg], span)
}

fn interval(arg: Expression, span: Span) -> Expression {
    call("interval", vec![arg], span)
}

fn first_tick(span: Span) -> Expression {
    call("firstTick", Vec::new(), span)
}

fn add(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Add,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

fn mul(lhs: Expression, rhs: Expression, span: Span) -> Expression {
    Expression::Binary {
        op: OpBinary::Mul,
        lhs: Box::new(lhs),
        rhs: Box::new(rhs),
        span,
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{BuiltinFunction, ComponentRefPart, ComponentReference};

    use super::*;

    #[test]
    fn lowers_clocked_state_derivative_to_implicit_euler_update() {
        let mut dae_model = clocked_derivative_dae("ImplicitEuler");

        let lowered = lower_clocked_discretized_partitions(&mut dae_model)
            .expect("ImplicitEuler clocked derivative should lower");

        assert_eq!(lowered, 1);
        assert!(
            !dae_model
                .variables
                .states
                .contains_key(&VarName::new("PI.x"))
        );
        assert!(
            dae_model
                .variables
                .discrete_reals
                .contains_key(&VarName::new("PI.x"))
        );
        assert!(dae_model.continuous.equations.is_empty());
        assert_eq!(dae_model.discrete.real_updates.len(), 1);
        assert_eq!(
            dae_model.discrete.real_updates[0].lhs.as_ref(),
            Some(&VarName::new("PI.x"))
        );
        let update = &dae_model.discrete.real_updates[0].rhs;
        assert!(expr_has_call(update, "previous"));
        assert!(expr_has_call(update, "interval"));
        assert!(expr_has_call(update, "firstTick"));
    }

    #[test]
    fn rejects_clocked_state_derivative_without_supported_solver() {
        let mut dae_model = clocked_derivative_dae("ExplicitEuler");

        let err = lower_clocked_discretized_partitions(&mut dae_model)
            .expect_err("unsupported solver method should fail early");

        assert!(format!("{err}").contains("unsupported solver method"));
        assert!(
            dae_model
                .variables
                .states
                .contains_key(&VarName::new("PI.x"))
        );
        assert_eq!(dae_model.continuous.equations.len(), 1);
    }

    fn clocked_derivative_dae(method: &str) -> dae::Dae {
        let mut dae_model = dae::Dae::default();
        dae_model.variables.states.insert(
            VarName::new("PI.x"),
            dae::Variable::new(VarName::new("PI.x")),
        );
        dae_model.variables.discrete_reals.insert(
            VarName::new("PI.u"),
            dae::Variable::new(VarName::new("PI.u")),
        );
        dae_model.variables.parameters.insert(
            VarName::new("PI.T"),
            dae::Variable::new(VarName::new("PI.T")),
        );
        dae_model.variables.parameters.insert(
            VarName::new("periodicClock.useSolver"),
            parameter("periodicClock", "useSolver", Literal::Boolean(true)),
        );
        dae_model.variables.parameters.insert(
            VarName::new("periodicClock.solverMethod"),
            parameter(
                "periodicClock",
                "solverMethod",
                Literal::String(method.to_string()),
            ),
        );
        dae_model.clocks.intervals.insert("PI.x".to_string(), 0.1);
        dae_model.continuous.equations.push(dae::Equation::residual(
            sub(der(var_ref("PI.x")), div(var_ref("PI.u"), var_ref("PI.T"))),
            Span::DUMMY,
            "equation from PI",
        ));
        dae_model
    }

    fn parameter(prefix: &str, field: &str, value: Literal) -> dae::Variable {
        dae::Variable {
            name: VarName::new(format!("{prefix}.{field}")),
            component_ref: Some(ComponentReference {
                local: false,
                span: Span::DUMMY,
                parts: vec![
                    ComponentRefPart {
                        ident: prefix.to_string(),
                        span: Span::DUMMY,
                        subs: Vec::new(),
                    },
                    ComponentRefPart {
                        ident: field.to_string(),
                        span: Span::DUMMY,
                        subs: Vec::new(),
                    },
                ],
                def_id: None,
            }),
            start: Some(Expression::Literal {
                value,
                span: Span::DUMMY,
            }),
            source_span: Span::DUMMY,
            ..dae::Variable::default()
        }
    }

    fn var_ref(name: &str) -> Expression {
        Expression::VarRef {
            name: Reference::from(name),
            subscripts: Vec::new(),
            span: Span::DUMMY,
        }
    }

    fn der(expr: Expression) -> Expression {
        Expression::BuiltinCall {
            function: BuiltinFunction::Der,
            args: vec![expr],
            span: Span::DUMMY,
        }
    }

    fn div(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Div,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: Span::DUMMY,
        }
    }

    fn sub(lhs: Expression, rhs: Expression) -> Expression {
        Expression::Binary {
            op: OpBinary::Sub,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            span: Span::DUMMY,
        }
    }

    fn expr_has_call(expr: &Expression, call_name: &str) -> bool {
        match expr {
            Expression::FunctionCall { name, args, .. } => {
                name.as_str() == call_name || args.iter().any(|arg| expr_has_call(arg, call_name))
            }
            Expression::BuiltinCall { args, .. } => {
                args.iter().any(|arg| expr_has_call(arg, call_name))
            }
            Expression::Binary { lhs, rhs, .. } => {
                expr_has_call(lhs, call_name) || expr_has_call(rhs, call_name)
            }
            Expression::Unary { rhs, .. } => expr_has_call(rhs, call_name),
            Expression::If {
                branches,
                else_branch,
                ..
            } => {
                branches.iter().any(|(condition, value)| {
                    expr_has_call(condition, call_name) || expr_has_call(value, call_name)
                }) || expr_has_call(else_branch, call_name)
            }
            _ => false,
        }
    }
}
