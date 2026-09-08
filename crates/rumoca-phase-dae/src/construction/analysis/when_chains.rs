use super::*;

/// The evidence one `when` chain is validated against.
///
/// The two role maps are the point of this struct and they are **not**
/// interchangeable:
///
/// * `roles` is the *coordinate* plan. It answers the ownership questions —
///   may this name be assigned, is it a state, does it own a clock, is this
///   `previous(...)` operand a clocked coordinate.
/// * `expression_roles` is that plan plus the values a name may denote without
///   being a coordinate: the enumeration literals of
///   `Model::enum_literal_ordinals` (MLS §4.9.5) and the record aggregates. It
///   is the map every expression *read* inside the chain resolves against.
///
/// Reads used to resolve against the coordinate plan, which is why
/// `mode = Mode.Armed` inside a `when` was rejected as an unresolved Flat
/// reference (`ED008`) while the identical literal in a plain equation
/// resolved: `validate_model_expressions` already used the expression roles.
#[derive(Clone, Copy)]
struct WhenScope<'a> {
    roles: &'a HashMap<VarName, PlannedRole>,
    expression_roles: &'a HashMap<VarName, PlannedRole>,
    constants: &'a EvalContext,
    /// The exact literal-name and enumeration-declaration evidence, so a name
    /// that merely spells like a cataloged literal without its enumeration's
    /// declaration identity stays unresolved here too.
    model_values: &'a ShapeEnvironment,
}

pub(super) fn validate_when_chains(
    chains: &[flat::WhenChain],
    roles: &HashMap<VarName, PlannedRole>,
    expression_roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
    model_values: &ShapeEnvironment,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
) -> Result<(), ToDaeError> {
    let scope = WhenScope {
        roles,
        expression_roles,
        constants,
        model_values,
    };
    for chain in chains {
        for branch in chain.branches() {
            // A when-clause's own activation condition is evaluated to decide
            // whether the event happens, so it is not yet inside the event it
            // guards: `pre()` of a continuous coordinate has no left limit to
            // read there. OMC rejects the shape for the same reason
            // ("Argument 1 of pre must be a discrete expression").
            validate_when_activation_condition(
                &branch.condition,
                scope.expression_roles,
                scope.constants,
                sample_lattices,
                scope.model_values,
            )?;
            // MLS §16.5.1: a branch is clocked when it names a `Clock`
            // coordinate or uses the inferred-clock constructor `Clock()`; the
            // clock-domain analysis proves the inferred branch's owner.
            let clocked = matches!(
                &branch.condition,
                Expression::VarRef { name, .. }
                    if matches!(scope.roles.get(name.var_name()), Some(PlannedRole::Clock))
            ) || is_inferred_clock_condition(&branch.condition);
            validate_when_equations(&branch.equations, scope, sample_lattices, clocked)?;
        }
    }
    Ok(())
}

fn validate_when_equations(
    equations: &[flat::WhenEquation],
    scope: WhenScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign {
                target,
                value,
                span,
                ..
            } => validate_assignment(target, value, *span, scope, clocked)?,
            flat::WhenEquation::Reinit {
                state, value, span, ..
            } => validate_reinitialization(state, value, *span, scope, clocked)?,
            flat::WhenEquation::Assert {
                condition,
                message,
                level,
                ..
            } => {
                validate_branch_condition(condition, scope, sample_lattices, clocked)?;
                validate_read(message, scope, clocked)?;
                if let Some(level) = level {
                    validate_read(level, scope, clocked)?;
                }
            }
            flat::WhenEquation::Terminate { message, .. } => {
                validate_read(message, scope, clocked)?;
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                span,
                ..
            } => {
                validate_non_real_branch_targets(branches, else_branch, scope.roles, *span)?;
                for (condition, equations) in branches {
                    validate_branch_condition(condition, scope, sample_lattices, clocked)?;
                    validate_when_equations(equations, scope, sample_lattices, clocked)?;
                }
                if let Some(else_branch) = else_branch {
                    validate_when_equations(else_branch, scope, sample_lattices, clocked)?;
                }
            }
            flat::WhenEquation::FunctionCallOutputs { span, .. } => {
                return Err(ToDaeError::unsupported_flat(
                    "when function-call outputs",
                    "multi-output event calls require a checked function-action owner",
                    *span,
                ));
            }
        }
    }
    Ok(())
}

/// One expression the when body *reads*, resolved against the expression roles.
fn validate_read(
    expression: &Expression,
    scope: WhenScope<'_>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    validate_when_expression(
        expression,
        scope.expression_roles,
        clocked,
        scope.model_values,
    )
}

/// The guard of an if-equation or assertion written inside the when body. The
/// body has already activated when it is reached, so it keeps the body's
/// `pre()` rule — and, like every other read, the expression roles.
fn validate_branch_condition(
    condition: &Expression,
    scope: WhenScope<'_>,
    sample_lattices: &mut Vec<(Span, PeriodicClockSchedule)>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    validate_when_condition_expression(
        condition,
        scope.expression_roles,
        scope.constants,
        sample_lattices,
        clocked,
        scope.model_values,
    )
}

fn validate_non_real_branch_targets(
    branches: &[(Expression, Vec<flat::WhenEquation>)],
    else_branch: &Option<Vec<flat::WhenEquation>>,
    roles: &HashMap<VarName, PlannedRole>,
    span: Span,
) -> Result<(), ToDaeError> {
    let expected = branches
        .first()
        .map(|(_, equations)| non_real_targets(equations, roles))
        .unwrap_or_default();
    if branches
        .iter()
        .skip(1)
        .map(|(_, equations)| non_real_targets(equations, roles))
        .chain(
            else_branch
                .iter()
                .map(|equations| non_real_targets(equations, roles)),
        )
        .all(|targets| targets == expected)
    {
        return Ok(());
    }
    Err(ToDaeError::discrete_solved_form_violation(
        "all branches of a non-Real if-equation must assign the same resolved coordinates",
        span,
    ))
}

fn non_real_targets(
    equations: &[flat::WhenEquation],
    roles: &HashMap<VarName, PlannedRole>,
) -> HashSet<VarName> {
    let mut targets = HashSet::new();
    for equation in equations {
        match equation {
            flat::WhenEquation::Assign { target, .. }
                if matches!(roles.get(target), Some(PlannedRole::DiscreteValue)) =>
            {
                targets.insert(target.clone());
            }
            flat::WhenEquation::Conditional {
                branches,
                else_branch,
                ..
            } => {
                for (_, equations) in branches {
                    targets.extend(non_real_targets(equations, roles));
                }
                if let Some(else_branch) = else_branch {
                    targets.extend(non_real_targets(else_branch, roles));
                }
            }
            flat::WhenEquation::Assign { .. }
            | flat::WhenEquation::Reinit { .. }
            | flat::WhenEquation::Assert { .. }
            | flat::WhenEquation::Terminate { .. }
            | flat::WhenEquation::FunctionCallOutputs { .. } => {}
        }
    }
    targets
}

fn validate_assignment(
    target: &VarName,
    value: &Expression,
    span: Span,
    scope: WhenScope<'_>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    if !matches!(
        scope.roles.get(target),
        Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
    ) {
        return Err(ToDaeError::unsupported_flat(
            "when assignment",
            format!("`{target}` is not a discrete coordinate"),
            span,
        ));
    }
    // `previous(...)` names a coordinate, so its operand is proven against the
    // coordinate plan, not the expression catalog.
    validate_clocked_temporal_expressions(value, scope.roles, clocked)?;
    validate_read(value, scope, clocked)
}

fn validate_clocked_temporal_expressions(
    expression: &Expression,
    roles: &HashMap<VarName, PlannedRole>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    if let Expression::BuiltinCall {
        function: BuiltinFunction::Previous,
        args,
        span,
    } = expression
    {
        if !clocked {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires an owning clocked when-clause",
                *span,
            ));
        }
        let [argument] = args.as_slice() else {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires exactly one coordinate operand",
                *span,
            ));
        };
        let Some((coordinate, _)) = derivative_reference(argument) else {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires a direct component expression",
                *span,
            ));
        };
        if !matches!(
            roles.get(coordinate.var_name()),
            Some(PlannedRole::DiscreteReal | PlannedRole::DiscreteValue)
        ) {
            return Err(ToDaeError::unsupported_runtime_operator(
                "previous",
                "requires a clocked discrete coordinate",
                *span,
            ));
        }
        return Ok(());
    }
    for child in expression_children(expression) {
        validate_clocked_temporal_expressions(child, roles, clocked)?;
    }
    Ok(())
}

/// MLS §8.3.6 `reinit(x, expr)`.
///
/// The value expression is evaluated at the event instant, so `pre(x)` inside
/// it is the ordinary left limit `x(t^pre)` and lowers through the same
/// event-entry lane as every other continuous `pre()` read. It used to be
/// rewritten to a plain `x` here, which read the wrong value once the lane
/// existed and turned `reinit(x, pre(x) + 1)` into the unsolvable
/// `reinit(x, x + 1)`.
fn validate_reinitialization(
    state: &VarName,
    value: &Expression,
    span: Span,
    scope: WhenScope<'_>,
    clocked: bool,
) -> Result<(), ToDaeError> {
    if !matches!(scope.roles.get(state), Some(PlannedRole::State)) {
        return Err(ToDaeError::reinit_non_state(state.as_str(), span));
    }
    validate_read(value, scope, clocked)
}
