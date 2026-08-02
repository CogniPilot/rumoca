//! MLS §8.5 event owners of model equation expressions and `when` activations.
//!
//! Appendix B keeps the event surface of `f(x)` explicit: a relation that can
//! change sign while the solver integrates owns a `relation`/root pair, a
//! relation over `time` alone owns an exactly scheduled time event, and the
//! Boolean `sample(start, interval)` operator of MLS §3.7.5 owns a periodic
//! clock. Analysis proves the owner of every such occurrence here so
//! construction only has to build the checked DAE node for it.
//!
//! Scope is every binding equation whose expression is runtime-varying, the residual of each
//! `flat::Model::equations` row, and the activation condition of every `when`.
//! Binding expressions and residuals are the model expressions lowered exactly once, so
//! one source relation always owns exactly one checked DAE relation, and both
//! owner kinds are collected for them. Occurrence identity deduplicates a
//! binding that is also represented by an explicit residual.
//!
//! A `when` activation is different: `lower_condition_tree` already builds one
//! checked relation per relational leaf of the activation and gives each of
//! them a root, so only the *exactly scheduled* owner is collected there — see
//! [`collect_activation_time_events`]. Without that collection a `when time >
//! 0.5` would own a zero crossing instead of an instant, and §8.5 also says
//! what a relation reads between events — *"an event generating expression has
//! an internal buffer, and the value of the expression can only be changed at
//! event instants"* — so the located crossing lands exactly on `t = 0.5`, where
//! the strict relation is still false: the activation is consumed without ever
//! becoming true. Conditions of assertions and of algorithm `if` statements keep
//! owning their events through their own semantic owners and are deliberately
//! not collected here.
//!
//! One operator has no arm below on purpose: MLS §3.7.4.5 `semiLinear(x, kp, kn)`
//! returns `smooth(0, if x >= 0 then kp*x else kn*x)`, so the `x >= 0` relation
//! that DAE construction builds for it is already inside a `smooth`. §3.7.5
//! grants a freedom there rather than imposing a rule — *"a tool is free to not
//! generate events for expressions inside `smooth`. However, `smooth` does not
//! guarantee that no events will be generated"* — and rumoca elects to take it,
//! which is what the `Smooth` arm below encodes. The operator is C0-continuous,
//! so no crossing has to be located to keep the residual continuous, and OMC
//! emits no zero crossing for it either. Relations written inside its operands
//! are ordinary model relations and still reach their own arm through the child
//! recursion.

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
    SampleClock(rumoca_core::PeriodicClockSchedule),
}

/// One collected owner: the operands that name it, and the plan proved for them.
struct PlannedOccurrence {
    operands: Vec<Expression>,
    plan: ExpressionEventPlan,
}

impl PlannedOccurrence {
    fn names(&self, operands: &[&Expression]) -> bool {
        self.operands.len() == operands.len()
            && self
                .operands
                .iter()
                .zip(operands)
                .all(|(held, probe)| held == *probe)
    }
}

/// The proved owners, addressed by source span *and* by occurrence.
///
/// A span alone does not name an owner. Flattening replicates one source span
/// across every instance of its class, so `Trigger early(threshold = 0.33)` and
/// `Trigger late(threshold = 0.73)` produce two flat occurrences of the single
/// written `time > threshold`, differing only in the operands each instance
/// resolved — and therefore in the instant each owns. Keyed by span alone the
/// second occurrence is silently dropped while lowering still reads the first
/// one's plan for both, which leaves the late instance with no event owner at
/// all: no scheduled instant of its own, and no zero crossing either, because
/// the plan it wrongly read suppressed one.
///
/// The operands are the identity because they are the only thing that varies and
/// the only thing both sides hold: analysis takes them from `flat`, and lowering
/// lowers those same `flat` operands with nothing rewriting them in between.
#[derive(Default)]
pub(in crate::construction) struct ExpressionEventPlans {
    ordered: Vec<(Span, ExpressionEventPlan)>,
    by_span: HashMap<Span, Vec<PlannedOccurrence>>,
}

impl ExpressionEventPlans {
    /// The plan proved for the occurrence of `span` carrying `operands`.
    pub(in crate::construction) fn plan(
        &self,
        span: Span,
        operands: &[&Expression],
    ) -> Option<ExpressionEventPlan> {
        self.by_span
            .get(&span)?
            .iter()
            .find_map(|occurrence| occurrence.names(operands).then_some(occurrence.plan))
    }

    /// Collected owners in source order, one per occurrence, so the checked DAE
    /// arenas they build are deterministic across runs.
    pub(in crate::construction) fn ordered(
        &self,
    ) -> impl Iterator<Item = (Span, ExpressionEventPlan)> + '_ {
        self.ordered.iter().copied()
    }

    /// Record the plan proved for one occurrence.
    ///
    /// A repeat of the same occurrence is the same proof and is dropped. A
    /// repeat that disagrees is not: two owners cannot both be the owner of one
    /// occurrence, and silently keeping either is how a dropped instant becomes
    /// an event that never fires. Analysis derives the plan from the operands
    /// alone, so a disagreement here is a broken proof, not a model error.
    fn insert(
        &mut self,
        span: Span,
        operands: &[&Expression],
        plan: ExpressionEventPlan,
    ) -> Result<(), ToDaeError> {
        let occurrences = self.by_span.entry(span).or_default();
        if let Some(existing) = occurrences
            .iter()
            .find(|occurrence| occurrence.names(operands))
        {
            if existing.plan.same_owner(plan) {
                return Ok(());
            }
            return Err(ToDaeError::unsupported_flat(
                "event owner",
                "one occurrence of this expression was proved to own two different events",
                span,
            ));
        }
        occurrences.push(PlannedOccurrence {
            operands: operands.iter().map(|operand| (*operand).clone()).collect(),
            plan,
        });
        self.ordered.push((span, plan));
        Ok(())
    }
}

impl ExpressionEventPlan {
    /// Whether two plans name the same event owner.
    fn same_owner(self, other: Self) -> bool {
        match (self, other) {
            (Self::StateRelation, Self::StateRelation) => true,
            (Self::TimeEvent(left), Self::TimeEvent(right)) => left == right,
            (Self::SampleClock(left), Self::SampleClock(right)) => left == right,
            _ => false,
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
    for variable in flat.variables.values() {
        if let Some(binding) = &variable.binding {
            collect_event_owners(binding, false, &scope, &mut plans)?;
        }
    }
    for equation in &flat.equations {
        collect_event_owners(&equation.residual, false, &scope, &mut plans)?;
    }
    for chain in &flat.when_chains {
        for branch in chain.branches() {
            collect_activation_time_events(&branch.condition, constants, &mut plans)?;
        }
    }
    for algorithm in &flat.algorithms {
        collect_statement_activation_time_events(&algorithm.statements, constants, &mut plans)?;
    }
    Ok(plans)
}

/// Collect the exactly scheduled MLS §8.5 owners of a `when` activation.
///
/// `lower_condition_tree` builds one checked relation per relational leaf of the
/// activation's `not`/`and`/`or`/vector tree, so those leaves — and only those —
/// are the spans an activation owner can claim. This walk mirrors that tree
/// exactly, which is why it does not descend into operator arguments: a relation
/// under `noEvent`/`smooth` is lowered as one opaque discrete condition, never as
/// an activation relation, so claiming it here would schedule an instant nothing
/// reads.
///
/// Exact time-event and periodic-sample owners are recorded. A relational leaf
/// that can change sign during integration already owns its zero crossing
/// through the activation's own root, and recording a second owner for it would
/// make `lower_expression_event` build that root a second time.
fn collect_activation_time_events(
    expression: &Expression,
    constants: &EvalContext,
    plans: &mut ExpressionEventPlans,
) -> Result<(), ToDaeError> {
    match expression {
        Expression::Unary {
            op: OpUnary::Not,
            rhs,
            ..
        } => collect_activation_time_events(rhs, constants, plans)?,
        Expression::Binary {
            op: OpBinary::And | OpBinary::Or,
            lhs,
            rhs,
            ..
        } => {
            collect_activation_time_events(lhs, constants, plans)?;
            collect_activation_time_events(rhs, constants, plans)?;
        }
        Expression::Array { elements, .. } => {
            for element in elements {
                collect_activation_time_events(element, constants, plans)?;
            }
        }
        Expression::BuiltinCall {
            function: BuiltinFunction::Sample,
            args,
            span,
        } => {
            let schedule = evaluate_sample_schedule(args, constants, *span)?;
            let operands: Vec<&Expression> = args.iter().collect();
            plans.insert(*span, &operands, ExpressionEventPlan::SampleClock(schedule))?;
        }
        Expression::Binary { op, lhs, rhs, span } if op.is_relational() => {
            if let Some(instant) = time_event_instant(op, lhs, rhs, constants) {
                plans.insert(*span, &[lhs, rhs], ExpressionEventPlan::TimeEvent(instant))?;
            }
        }
        _ => {}
    }
    Ok(())
}

/// The same collection over the `when` statements of a model algorithm section.
///
/// `lower_algorithm_when` lowers each guarded block's condition through the very
/// same `lower_condition_tree`, so an algorithm `when time > 0.5` owns its
/// instant exactly like the equation form. `if` statements are deliberately
/// skipped: their conditions are ordinary continuous guards, not event
/// activations.
fn collect_statement_activation_time_events(
    statements: &[rumoca_core::Statement],
    constants: &EvalContext,
    plans: &mut ExpressionEventPlans,
) -> Result<(), ToDaeError> {
    for statement in statements {
        match statement {
            rumoca_core::Statement::When { blocks, .. } => {
                for block in blocks {
                    collect_activation_time_events(&block.cond, constants, plans)?;
                    collect_statement_activation_time_events(&block.stmts, constants, plans)?;
                }
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => {
                for block in cond_blocks {
                    collect_statement_activation_time_events(&block.stmts, constants, plans)?;
                }
                if let Some(else_block) = else_block {
                    collect_statement_activation_time_events(else_block, constants, plans)?;
                }
            }
            rumoca_core::Statement::For { equations, .. } => {
                collect_statement_activation_time_events(equations, constants, plans)?;
            }
            rumoca_core::Statement::While { block, .. } => {
                collect_statement_activation_time_events(&block.stmts, constants, plans)?;
            }
            _ => {}
        }
    }
    Ok(())
}

fn collect_event_owners(
    expression: &Expression,
    suppressed: bool,
    scope: &EventScope<'_>,
    plans: &mut ExpressionEventPlans,
) -> Result<(), ToDaeError> {
    match expression {
        // MLS §3.7.5: `noEvent` mandates it — "no zero crossing functions shall
        // be used to monitor any of the normally event-generating
        // subexpressions inside expr" — while `smooth` only permits it: "a tool
        // is free to not generate events for expressions inside smooth". rumoca
        // takes that freedom uniformly, so no owner is collected below either.
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
            let schedule = evaluate_sample_schedule(args, scope.constants, *span)?;
            let operands: Vec<&Expression> = args.iter().collect();
            plans.insert(*span, &operands, ExpressionEventPlan::SampleClock(schedule))?;
            return Ok(());
        }
        Expression::Binary { op, lhs, rhs, span }
            if op.is_relational() && !suppressed && relation_can_vary(expression, scope) =>
        {
            let plan = time_event_instant(op, lhs, rhs, scope.constants).map_or(
                ExpressionEventPlan::StateRelation,
                ExpressionEventPlan::TimeEvent,
            );
            plans.insert(*span, &[lhs, rhs], plan)?;
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

/// True when a relation names `time` against a threshold the model moves.
///
/// This is the self-rescheduling shape — `time >= pre(nextEvent)` — whose next
/// instant is not knowable in advance because the event it schedules is what
/// sets it. It is the complement of [`time_event_instant`] on the axis that
/// matters: `time > 0` and `time > 2 * p` also yield no instant, but for
/// reasons that leave the relation an ordinary crossing, whereas this one needs
/// a reschedule the checked DAE does not yet own.
///
/// Exactly one operand must be affine in `time`; if neither is, the relation is
/// over model quantities and reschedules nothing, and if both are, its threshold
/// is parameter-evaluable and settled before the first step.
pub(super) fn is_rescheduling_time_relation(
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
    constants: &EvalContext,
) -> bool {
    if !matches!(
        op,
        OpBinary::Lt | OpBinary::Le | OpBinary::Gt | OpBinary::Ge
    ) {
        return false;
    }
    let affine = (
        affine_time_coefficients(lhs, constants),
        affine_time_coefficients(rhs, constants),
    );
    match affine {
        (Some((slope, _)), None) | (None, Some((slope, _))) => slope != 0.0,
        _ => false,
    }
}

/// The exact instant of an MLS §8.5 time event, when one exists.
///
/// Both operands must be affine in `time` with parameter-evaluable
/// coefficients; the crossing is then the single root of their difference.
///
/// The instant must lie strictly after the start of the simulated interval, and
/// that bound is a semantic one rather than a convenience. §8.5 defines an event
/// as the instant at which an event generating expression *changes* its value,
/// and nothing changes at the start: initialization has already fixed every
/// relation's buffered value there, so `time >= 0` is simply true from the
/// outset with no edge to activate a `when`, and `time > 0` becomes true only
/// after the start, at the first event instant beyond it. Scheduling a stop at
/// the start instant serves neither: it is a stop at which `time > 0` still
/// reads false, and then no later event exists to retry it. Both spellings are
/// left to the zero crossing, which locates them at the start and applies them
/// at its right limit — which is where OpenModelica applies them too, stamping
/// its own right-limit row (`t = 1e-10` for `when time > 0`).
///
/// Leaving them to the crossing is what this owner decides, and it is all it
/// decides. It buys parity for `when time > 0`, which then fires once on the
/// rk-like session, exactly as OpenModelica does.
///
/// `when time >= 0` used to diverge here — rumoca fired it at `t = 0` where
/// OpenModelica leaves it unfired, because the relation is already true at the
/// start and a `when` runs on a rising edge. That was the initial-activation
/// defect (#90), and it is **fixed**: seeding the activation buffer from the
/// settled initialization values (`when` condition memory, commit 4032af2a)
/// removed the manufactured edge, so both sessions now leave `when time >= 0`
/// unfired as OpenModelica does. `crates/rumoca/tests/time_event_when_activation.rs`
/// pins the pair.
///
/// What this bound still does not reach is the `Bdf` session, where
/// `when time > 0` does not fire at all: the located crossing at the start
/// instant is never applied there. That is a diffsol event-boundary defect
/// rather than an owner decision, and no test pins it yet.
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
    if !instant.is_finite() || instant <= 0.0 {
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
