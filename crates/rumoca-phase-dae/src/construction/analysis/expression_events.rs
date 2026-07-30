//! MLS §8.5 event owners that occur inside model equation expressions.
//!
//! Appendix B keeps the event surface of `f(x)` explicit: a relation that can
//! change sign while the solver integrates owns a `relation`/root pair, a
//! relation over `time` alone owns an exactly scheduled time event, and the
//! Boolean `sample(start, interval)` operator of MLS §3.7.5 owns a periodic
//! clock. Analysis proves the owner of every such occurrence here so
//! construction only has to build the checked DAE node for it.
//!
//! Scope is the residual of each `flat::Model::equations` row. Those residuals
//! are the only model expressions lowered exactly once, so one source relation
//! always owns exactly one checked DAE relation. Conditions of `when` clauses,
//! assertions, and algorithms already own their events through their own
//! semantic owners and are deliberately not collected again here.

use super::*;

#[derive(Clone, Copy)]
pub(in crate::construction) enum ExpressionEventPlan {
    /// A relation whose truth can change during continuous integration: MLS
    /// §8.5 requires the crossing to be located, so it owns a root function.
    StateRelation,
    /// A relation over `time` and parameter-evaluable operands alone. MLS §8.5
    /// gives its instant exactly, so it is scheduled instead of searched for.
    TimeEvent(ClockRational),
    /// `sample(start, interval)`, the MLS §3.7.5 periodic Boolean operator.
    SampleClock(ClockLattice),
}

#[derive(Default)]
pub(in crate::construction) struct ExpressionEventPlans {
    ordered: Vec<(Span, ExpressionEventPlan)>,
    by_span: HashMap<Span, ExpressionEventPlan>,
}

impl ExpressionEventPlans {
    pub(in crate::construction) fn plan(&self, span: Span) -> Option<ExpressionEventPlan> {
        self.by_span.get(&span).copied()
    }

    /// Collected owners in source order, so the checked DAE arenas they build
    /// are deterministic across runs.
    pub(in crate::construction) fn ordered(
        &self,
    ) -> impl Iterator<Item = (Span, ExpressionEventPlan)> + '_ {
        self.ordered.iter().copied()
    }

    fn insert(&mut self, span: Span, plan: ExpressionEventPlan) {
        if self.by_span.insert(span, plan).is_none() {
            self.ordered.push((span, plan));
        }
    }
}

struct EventScope<'analysis> {
    flat: &'analysis flat::Model,
    roles: &'analysis HashMap<VarName, PlannedRole>,
    constants: &'analysis EvalContext,
}

pub(super) fn analyze_expression_events(
    flat: &flat::Model,
    roles: &HashMap<VarName, PlannedRole>,
    constants: &EvalContext,
) -> Result<ExpressionEventPlans, ToDaeError> {
    let scope = EventScope {
        flat,
        roles,
        constants,
    };
    let mut plans = ExpressionEventPlans::default();
    for equation in &flat.equations {
        collect_event_owners(&equation.residual, false, &scope, &mut plans)?;
    }
    Ok(plans)
}

fn collect_event_owners(
    expression: &Expression,
    suppressed: bool,
    scope: &EventScope<'_>,
    plans: &mut ExpressionEventPlans,
) -> Result<(), ToDaeError> {
    match expression {
        // MLS §3.7.5: `noEvent` and `smooth` remove event generation from the
        // relations they enclose, so no owner is collected below them.
        Expression::BuiltinCall {
            function: BuiltinFunction::NoEvent | BuiltinFunction::Smooth,
            args,
            ..
        } => {
            for argument in args {
                collect_event_owners(argument, true, scope, plans)?;
            }
            return Ok(());
        }
        // MLS §3.7.5 `sample(start, interval)` is a periodic Boolean operator
        // whose schedule is metadata, not a zero crossing, so `noEvent` cannot
        // suppress it. The clocked-value forms of MLS §16.5.1 are owned by
        // clock-domain analysis instead.
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } if !is_clocked_value_sample(scope.flat, args) => {
            let lattice = evaluate_sample_lattice(args, scope.constants, *span)?;
            plans.insert(*span, ExpressionEventPlan::SampleClock(lattice));
            return Ok(());
        }
        Expression::Binary { op, lhs, rhs, span }
            if op.is_relational() && !suppressed && relation_can_vary(expression, scope) =>
        {
            let plan = time_event_instant(op, lhs, rhs, scope.constants).map_or(
                ExpressionEventPlan::StateRelation,
                ExpressionEventPlan::TimeEvent,
            );
            plans.insert(*span, plan);
        }
        _ => {}
    }
    for child in expression_children(expression) {
        collect_event_owners(child, suppressed, scope, plans)?;
    }
    Ok(())
}

/// True for the MLS §16.5.1 clocked-value forms `sample(u)` and `sample(u, c)`.
fn is_clocked_value_sample(flat: &flat::Model, arguments: &[Expression]) -> bool {
    match arguments {
        [_] => true,
        [_, clock] => is_whole_clock_coordinate(flat, clock),
        _ => false,
    }
}

/// True when the relation's truth can still change once simulation starts.
///
/// A relation over parameters and constants alone is settled before the first
/// step, so MLS §8.5 gives it no event and the checked DAE gives it no root.
fn relation_can_vary(expression: &Expression, scope: &EventScope<'_>) -> bool {
    match expression {
        Expression::VarRef { name, .. }
            if name.as_str() == "time"
                || !matches!(
                    scope.roles.get(name.var_name()),
                    Some(
                        PlannedRole::Parameter
                            | PlannedRole::Constant
                            | PlannedRole::EnumerationLiteral
                    )
                ) =>
        {
            true
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            ..
        } if !is_clocked_value_sample(scope.flat, args) => true,
        _ => expression_children(expression)
            .into_iter()
            .any(|child| relation_can_vary(child, scope)),
    }
}

/// The exact instant of an MLS §8.5 time event, when one exists.
///
/// Both operands must be affine in `time` with parameter-evaluable
/// coefficients; the crossing is then the single root of their difference.
fn time_event_instant(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    constants: &EvalContext,
) -> Option<ClockRational> {
    if !matches!(
        op,
        OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge
    ) {
        return None;
    }
    let (lhs_slope, lhs_offset) = affine_time_coefficients(lhs, constants)?;
    let (rhs_slope, rhs_offset) = affine_time_coefficients(rhs, constants)?;
    let slope = lhs_slope - rhs_slope;
    if slope == 0.0 {
        return None;
    }
    let instant = (rhs_offset - lhs_offset) / slope;
    if !instant.is_finite() || instant < 0.0 {
        return None;
    }
    ClockRational::from_seconds(instant).ok()
}

/// `(slope, offset)` of an expression that is affine in `time`.
fn affine_time_coefficients(
    expression: &Expression,
    constants: &EvalContext,
) -> Option<(f64, f64)> {
    if let Expression::VarRef {
        name, subscripts, ..
    } = expression
        && name.as_str() == "time"
        && subscripts.is_empty()
    {
        return Some((1.0, 0.0));
    }
    if let Ok(value) = eval_expr(expression, constants)
        && let Some(value) = value.to_real()
        && value.is_finite()
    {
        return Some((0.0, value));
    }
    match expression {
        Expression::Unary {
            op: OpUnary::Minus,
            rhs,
            ..
        } => affine_time_coefficients(rhs, constants).map(|(slope, offset)| (-slope, -offset)),
        Expression::Unary {
            op: OpUnary::Plus,
            rhs,
            ..
        } => affine_time_coefficients(rhs, constants),
        Expression::Binary {
            op: op @ (OpBinary::Add | OpBinary::Sub),
            lhs,
            rhs,
            ..
        } => {
            let (lhs_slope, lhs_offset) = affine_time_coefficients(lhs, constants)?;
            let (rhs_slope, rhs_offset) = affine_time_coefficients(rhs, constants)?;
            let sign = if matches!(op, OpBinary::Add) {
                1.0
            } else {
                -1.0
            };
            Some((lhs_slope + sign * rhs_slope, lhs_offset + sign * rhs_offset))
        }
        Expression::Binary {
            op: OpBinary::Mul,
            lhs,
            rhs,
            ..
        } => {
            let (lhs_slope, lhs_offset) = affine_time_coefficients(lhs, constants)?;
            let (rhs_slope, rhs_offset) = affine_time_coefficients(rhs, constants)?;
            if lhs_slope != 0.0 && rhs_slope != 0.0 {
                return None;
            }
            Some((
                lhs_slope * rhs_offset + rhs_slope * lhs_offset,
                lhs_offset * rhs_offset,
            ))
        }
        Expression::Binary {
            op: OpBinary::Div,
            lhs,
            rhs,
            ..
        } => {
            let (lhs_slope, lhs_offset) = affine_time_coefficients(lhs, constants)?;
            let (rhs_slope, rhs_offset) = affine_time_coefficients(rhs, constants)?;
            if rhs_slope != 0.0 || rhs_offset == 0.0 {
                return None;
            }
            Some((lhs_slope / rhs_offset, lhs_offset / rhs_offset))
        }
        _ => None,
    }
}
