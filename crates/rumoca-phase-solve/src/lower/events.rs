use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::clocks::LoweredClocks;
use super::{
    LoweredLayout, ScalarCompiler, ScalarRows, delay_value_scalar_slot, variable_scalar_slot,
};
use crate::LowerError;

pub(super) fn lower_discrete_and_events<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(solve::DiscreteSolveSystem, solve::SolveEventPartition), LowerError> {
    let mut discrete = DiscreteRows::default();
    lower_discrete_real_equations(view, layout, clocks, &mut discrete)?;
    lower_discrete_value_owners(view, layout, clocks, &mut discrete)?;
    let mut event_actions = Vec::new();
    let mut action_conditions = ScalarRows::default();
    lower_event_actions(
        view,
        layout,
        clocks,
        &mut discrete,
        &mut event_actions,
        &mut action_conditions,
    )?;
    lower_condition_memory(view, layout, &mut discrete)?;
    let roots = lower_roots(view, layout)?;
    let scheduled_time_events = lower_time_events(view);
    let delays = lower_delays(view, layout)?;
    let discrete = discrete.finish()?;
    let events = solve::SolveEventPartition {
        root_conditions: roots.programs,
        root_relation_memory_targets: vec![None; roots.count],
        root_zero_domains: roots.zero_domains,
        condition_memory_parameter_indices: layout.condition_memory.clone(),
        scheduled_time_events,
        action_conditions: action_conditions.into_scalar_block()?,
        actions: event_actions,
        has_terminal_event: false,
        delays,
        ..solve::SolveEventPartition::default()
    };
    Ok((discrete, events))
}

fn lower_delays<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
) -> Result<solve::SolveDelayPartition, LowerError> {
    let mut source_rhs = ScalarRows::default();
    let mut delay_time_rhs = ScalarRows::default();
    let mut delay_max_rhs = ScalarRows::default();
    let mut value_parameter_indices = Vec::new();
    let mut source_is_discrete = Vec::new();
    for index in 0..view.delay_count() {
        let id = view.delay_id(index).expect("dense delay identity resolves");
        let delay = view.delay(id).expect("checked delay identity resolves");
        let span = delay.provenance().span();
        let (delay_time, delay_max) = match delay.operation() {
            dae::DelayOperation::ParameterDelay { delay_time } => {
                (delay_time.expression(), delay_time.expression())
            }
            dae::DelayOperation::BoundedDelay {
                delay_time,
                delay_max,
            } => (delay_time, delay_max.expression()),
        };
        let scalar_count = delay
            .value_type()
            .scalar_count()
            .expect("checked delay value type has scalar capacity");
        for scalar in 0..scalar_count {
            let channel = value_parameter_indices.len();
            source_rhs.push(
                ScalarCompiler::new(view, layout, None).program(delay.source(), scalar)?,
                span,
                channel,
            );
            delay_time_rhs.push(
                ScalarCompiler::new(view, layout, None).program(delay_time, 0)?,
                span,
                channel,
            );
            delay_max_rhs.push(
                ScalarCompiler::new(view, layout, None).program(delay_max, 0)?,
                span,
                channel,
            );
            let slot = delay_value_scalar_slot(layout, id.index(), scalar, span)?;
            let solve::ScalarSlot::P { index, .. } = slot else {
                unreachable!("delay values always occupy runtime-managed P slots")
            };
            value_parameter_indices.push(index);
            source_is_discrete.push(delay.variability() != dae::ExpressionVariability::Continuous);
        }
    }
    Ok(solve::SolveDelayPartition {
        source_rhs: source_rhs.into_scalar_block()?,
        delay_time_rhs: delay_time_rhs.into_scalar_block()?,
        delay_max_rhs: delay_max_rhs.into_scalar_block()?,
        value_parameter_indices,
        source_is_discrete,
    })
}

#[derive(Default)]
struct DiscreteRows {
    rows: ScalarRows,
    targets: Vec<solve::ScalarSlot>,
    roles: Vec<solve::DiscreteRowRole>,
    pre_modes: Vec<solve::DiscreteEventPreMode>,
    clock_owners: Vec<Option<solve::PeriodicClockId>>,
}

impl DiscreteRows {
    fn push(
        &mut self,
        program: Vec<solve::LinearOp>,
        span: Span,
        target: solve::ScalarSlot,
        role: solve::DiscreteRowRole,
        pre_mode: solve::DiscreteEventPreMode,
        clock_owner: Option<solve::PeriodicClockId>,
    ) {
        let output = self.targets.len();
        self.rows.push(program, span, output);
        self.targets.push(target);
        self.roles.push(role);
        self.pre_modes.push(pre_mode);
        self.clock_owners.push(clock_owner);
    }

    fn finish(self) -> Result<solve::DiscreteSolveSystem, LowerError> {
        let rhs = self.rows.into_scalar_block()?;
        Ok(solve::DiscreteSolveSystem {
            update_targets: self.targets,
            row_roles: self.roles,
            pre_modes: self.pre_modes,
            observation_refresh: vec![false; rhs.programs().len()],
            clock_owners: self.clock_owners,
            rhs,
            ..solve::DiscreteSolveSystem::default()
        })
    }
}

fn lower_discrete_real_equations<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows,
) -> Result<(), LowerError> {
    let mut conditional = Vec::new();
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let span = equation.provenance().span();
        let (target, value) = direct_discrete_real_definition(view, equation.residual())
            .ok_or_else(|| {
                LowerError::non_computable(
                    "coupled discrete Real residual is not an explicit computable definition",
                    span,
                )
            })?;
        let variable = dae::VariableId::from(target);
        match equation.activation() {
            dae::DiscreteRealActivation::Always => {
                lower_unconditional_discrete_real(
                    view, layout, clocks, rows, variable, value, span,
                )?;
            }
            dae::DiscreteRealActivation::When { trigger, guard } => {
                conditional.push(EventUpdate {
                    trigger,
                    guard,
                    variable: variable.index(),
                    value,
                    span,
                    clock: checked_discrete_real_activation_clock(
                        view, clocks, variable, guard, span,
                    )?,
                });
            }
        }
    }
    lower_guarded_updates(
        view,
        layout,
        clocks,
        rows,
        &conditional,
        solve::DiscreteRowRole::Equation,
    )
}

fn checked_discrete_real_activation_clock<'dae>(
    view: dae::DaeView<'dae>,
    clocks: &LoweredClocks<'dae>,
    variable: dae::VariableId<'dae>,
    guard: dae::ConditionId<'dae>,
    span: Span,
) -> Result<Option<dae::ClockId<'dae>>, LowerError> {
    let activation = condition_clock_owner(view, guard);
    let owner = clocks.variable_owner(variable).map(|(clock, _)| clock);
    match (activation, owner) {
        (None, None) => Ok(None),
        (Some(activation), Some(owner)) if activation == owner => Ok(Some(owner)),
        (Some(_), None) => Err(LowerError::non_computable(
            "clock-activated discrete Real definition has no matching target clock owner",
            span,
        )),
        (None, Some(_)) => Err(LowerError::non_computable(
            "clock-owned discrete Real target has a non-clock event activation",
            span,
        )),
        (Some(_), Some(_)) => Err(LowerError::non_computable(
            "discrete Real definition activation does not match its target clock owner",
            span,
        )),
    }
}

fn lower_unconditional_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows,
    variable: dae::VariableId<'dae>,
    value: dae::ExprId<'dae>,
    span: Span,
) -> Result<(), LowerError> {
    let value_type = view
        .expression(value)
        .expect("checked discrete definition value resolves")
        .value_type();
    let clock = clocks.variable_owner(variable);
    for scalar in 0..value_type
        .scalar_count()
        .expect("checked expression scalar capacity")
    {
        let program = match clock {
            Some((clock, _)) => {
                ScalarCompiler::new(view, layout, None).clocked_program(clock, value, scalar)?
            }
            None => ScalarCompiler::new(view, layout, None).program(value, scalar)?,
        };
        let target = variable_scalar_slot(layout, variable.index(), scalar, span)?;
        rows.push(
            program,
            span,
            target,
            solve::DiscreteRowRole::Equation,
            expression_pre_mode(view, value),
            clock.map(|(_, solve)| solve),
        );
    }
    Ok(())
}

fn direct_discrete_real_definition<'dae>(
    view: dae::DaeView<'dae>,
    residual: dae::ExprId<'dae>,
) -> Option<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)> {
    let residual = view.expression(residual)?;
    let dae::ExpressionOperation::Binary {
        operator: dae::BinaryOperator::Subtract,
        lhs,
        rhs,
    } = residual.operation()
    else {
        return None;
    };
    match (
        whole_discrete_real(view, lhs),
        whole_discrete_real(view, rhs),
    ) {
        (Some(target), None) => compatible_discrete_definition(view, target, rhs),
        (None, Some(target)) => compatible_discrete_definition(view, target, lhs),
        (None, None) | (Some(_), Some(_)) => None,
    }
}

fn compatible_discrete_definition<'dae>(
    view: dae::DaeView<'dae>,
    target: dae::DiscreteRealId<'dae>,
    value: dae::ExprId<'dae>,
) -> Option<(dae::DiscreteRealId<'dae>, dae::ExprId<'dae>)> {
    let variable = view.variable(dae::VariableId::from(target))?;
    let expression = view.expression(value)?;
    (variable.value_type() == expression.value_type()
        && !dae::expr_contains_var(view, value, dae::VariableId::from(target)))
    .then_some((target, value))
}

fn whole_discrete_real<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> Option<dae::DiscreteRealId<'dae>> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::DiscreteReal(variable)) => {
            Some(variable)
        }
        _ => None,
    }
}

fn lower_discrete_value_owners<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows,
) -> Result<(), LowerError> {
    for index in 0..view.discrete_value_owner_count() {
        let id = view
            .discrete_value_owner_id(index)
            .expect("dense B.1c owner identity resolves");
        let owner = view
            .discrete_value_owner(id)
            .expect("checked B.1c owner resolves");
        let first = owner
            .branches()
            .get(0)
            .expect("checked B.1c owner has a nonempty branch set");
        match first.activation() {
            dae::DiscreteBranchActivation::Always => {
                lower_unconditional_discrete_value_owner(view, layout, rows, owner)?;
            }
            dae::DiscreteBranchActivation::When { .. } => {
                lower_conditional_discrete_value_owner(view, layout, clocks, rows, owner)?;
            }
        }
    }
    Ok(())
}

fn lower_unconditional_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    rows: &mut DiscreteRows,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    let branch = owner
        .branches()
        .get(0)
        .expect("checked unconditional B.1c owner has one branch");
    debug_assert_eq!(owner.branches().len(), 1);
    for (target, (value, provenance)) in owner.targets().iter().zip(branch.values().iter()) {
        let expression = view
            .expression(value)
            .expect("checked B.1c value expression resolves");
        let span = provenance.span();
        for scalar in 0..expression
            .value_type()
            .scalar_count()
            .expect("checked B.1c value scalar capacity")
        {
            let program = ScalarCompiler::new(view, layout, None).program(value, scalar)?;
            let target = variable_scalar_slot(layout, target.index(), scalar, span)?;
            rows.push(
                program,
                span,
                target,
                solve::DiscreteRowRole::Equation,
                expression_pre_mode(view, value),
                None,
            );
        }
    }
    Ok(())
}

fn lower_conditional_discrete_value_owner<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows,
    owner: dae::DiscreteValueOwnerView<'dae>,
) -> Result<(), LowerError> {
    for (target_ordinal, target) in owner.targets().iter().enumerate() {
        let variable = view
            .variable(dae::VariableId::from(target))
            .expect("checked B.1c target resolves");
        for scalar in 0..variable.scalar_count() {
            let mut lowered = Vec::new();
            for branch in owner.branches().iter() {
                let branch = lower_checked_discrete_value_branch(
                    view,
                    layout,
                    clocks,
                    target,
                    target_ordinal,
                    scalar,
                    branch,
                )?;
                record_guarded_target(
                    &mut lowered,
                    branch.target,
                    branch.assignment,
                    branch.clock,
                    branch.has_pre,
                    branch.span,
                )?;
            }
            let [target] = lowered.as_slice() else {
                unreachable!("one B.1c target and scalar creates one guarded target")
            };
            let program = match target.clock {
                Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                    .clocked_guarded_assignments_program(
                        clock,
                        &target.branches,
                        target.target,
                        target.span,
                    )?,
                None => ScalarCompiler::new(view, layout, None).guarded_assignments_program(
                    &target.branches,
                    target.target,
                    target.span,
                )?,
            };
            rows.push(
                program,
                target.span,
                target.target,
                solve::DiscreteRowRole::EventAction,
                target.pre_mode,
                target.clock.map(|(_, clock)| clock),
            );
        }
    }
    Ok(())
}

struct LoweredDiscreteValueBranch<'dae> {
    target: solve::ScalarSlot,
    assignment: GuardedAssignment<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    has_pre: bool,
    span: Span,
}

fn lower_checked_discrete_value_branch<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    target: dae::DiscreteValueId<'dae>,
    target_ordinal: usize,
    scalar: usize,
    branch: dae::DiscreteValueBranchView<'dae>,
) -> Result<LoweredDiscreteValueBranch<'dae>, LowerError> {
    let dae::DiscreteBranchActivation::When { trigger, guard } = branch.activation() else {
        unreachable!("checked B.1c owner cannot mix always and when branches")
    };
    let (value, provenance) = branch
        .values()
        .get(target_ordinal)
        .expect("checked B.1c branch arity matches its target set");
    let span = provenance.span();
    let target = variable_scalar_slot(layout, target.index(), scalar, span)?;
    let clock = condition_clock_owner(view, guard)
        .map(|clock| clocks.clock(clock).map(|solve| (clock, solve)))
        .transpose()?;
    let trigger_memory = condition_memory(layout, trigger, span)?;
    let has_pre = expression_contains_pre(view, value)
        || condition_contains_pre(view, trigger)
        || condition_contains_pre(view, guard);
    Ok(LoweredDiscreteValueBranch {
        target,
        assignment: (trigger, guard, value, scalar, trigger_memory),
        clock,
        has_pre,
        span,
    })
}

fn lower_event_actions<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    discrete: &mut DiscreteRows,
    actions: &mut Vec<solve::SolveEventAction>,
    action_conditions: &mut ScalarRows,
) -> Result<(), LowerError> {
    let mut updates = Vec::new();
    for index in 0..view.event_action_count() {
        let id = view
            .event_action_id(index)
            .expect("dense event action identity resolves");
        let action = view
            .event_action(id)
            .expect("checked event action identity resolves");
        match action.operation() {
            dae::EventActionOperation::Assert { message, level } => {
                if level.is_some() {
                    return Err(LowerError::unsupported(
                        "assertion levels do not yet have checked Solve lowering",
                        action.provenance().span(),
                    ));
                }
                if condition_clock_owner(view, action.guard()).is_some() {
                    return Err(LowerError::unsupported(
                        "clocked assertions do not yet have checked Solve action scheduling",
                        action.provenance().span(),
                    ));
                }
                push_message_action(
                    view,
                    layout,
                    action,
                    message,
                    solve::SolveEventActionKind::Assert,
                    actions,
                    action_conditions,
                )?;
            }
            dae::EventActionOperation::Terminate { message } => {
                if condition_clock_owner(view, action.guard()).is_some() {
                    return Err(LowerError::unsupported(
                        "clocked termination does not yet have checked Solve action scheduling",
                        action.provenance().span(),
                    ));
                }
                push_message_action(
                    view,
                    layout,
                    action,
                    message,
                    solve::SolveEventActionKind::Terminate,
                    actions,
                    action_conditions,
                )?;
            }
            dae::EventActionOperation::Reinitialize { state, value } => {
                updates.push(EventUpdate {
                    trigger: action.trigger(),
                    guard: action.guard(),
                    variable: state.index(),
                    value,
                    span: action.provenance().span(),
                    clock: condition_clock_owner(view, action.guard()),
                });
            }
        }
    }
    lower_guarded_updates(
        view,
        layout,
        clocks,
        discrete,
        &updates,
        solve::DiscreteRowRole::EventAction,
    )
}

fn push_message_action<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    action: dae::EventActionView<'dae>,
    message: dae::ExprId<'dae>,
    kind: solve::SolveEventActionKind,
    actions: &mut Vec<solve::SolveEventAction>,
    conditions: &mut ScalarRows,
) -> Result<(), LowerError> {
    let span = action.provenance().span();
    let message = lower_message(view, layout, message)?;
    let trigger_memory = condition_memory(layout, action.trigger(), span)?;
    let program = ScalarCompiler::new(view, layout, None).edge_condition_program(
        action.trigger(),
        action.guard(),
        trigger_memory,
        span,
    )?;
    conditions.push(program, span, actions.len());
    actions.push(solve::SolveEventAction {
        kind,
        message,
        span,
        origin: action.provenance().origin().to_string(),
    });
    Ok(())
}

fn lower_message<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    message: dae::ExprId<'dae>,
) -> Result<solve::SolveEventMessage, LowerError> {
    let mut parts = Vec::new();
    lower_message_parts(view, layout, message, &mut parts)?;
    Ok(solve::SolveEventMessage { parts })
}

fn lower_message_parts<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    message: dae::ExprId<'dae>,
    parts: &mut Vec<solve::SolveEventMessagePart>,
) -> Result<(), LowerError> {
    let expression = view
        .expression(message)
        .expect("checked event message expression resolves");
    match expression.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => {
            parts.push(solve::SolveEventMessagePart::Text(message.clone()));
            Ok(())
        }
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } if expression.value_type().scalar_type() == dae::ScalarType::String => {
            lower_message_parts(view, layout, lhs, parts)?;
            lower_message_parts(view, layout, rhs, parts)
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            let source = match view
                .expression(value)
                .expect("checked String conversion value resolves")
                .value_type()
                .scalar_type()
            {
                dae::ScalarType::Real => solve::SolveStringConversionSource::Real,
                dae::ScalarType::Integer => solve::SolveStringConversionSource::Integer,
                dae::ScalarType::Boolean => solve::SolveStringConversionSource::Boolean,
                dae::ScalarType::Enumeration
                | dae::ScalarType::String
                | dae::ScalarType::Record => {
                    unreachable!("checked String conversion has a supported scalar source")
                }
            };
            let value = ScalarCompiler::new(view, layout, None).program(value, 0)?;
            let format = lower_message_format(view, layout, format)?;
            parts.push(solve::SolveEventMessagePart::Conversion {
                value,
                source,
                format,
            });
            Ok(())
        }
        _ => Err(LowerError::unsupported(
            "Solve event messages require String literals, concatenation, or checked String conversions",
            expression.provenance().span(),
        )),
    }
}

fn lower_message_format<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    format: dae::StringConversionFormatView<'dae>,
) -> Result<solve::SolveStringConversionFormat, LowerError> {
    Ok(match format {
        dae::StringConversionFormatView::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } => solve::SolveStringConversionFormat::Options {
            minimum_length: lower_message_option(view, layout, minimum_length)?,
            left_justified: lower_message_option(view, layout, left_justified)?,
            significant_digits: lower_message_option(view, layout, significant_digits)?,
        },
        dae::StringConversionFormatView::Format { value } => {
            let expression = view
                .expression(value)
                .expect("checked String format expression resolves");
            return Err(LowerError::unsupported(
                "explicit String format is not representable in checked Solve event messages",
                expression.provenance().span(),
            ));
        }
    })
}

fn lower_message_option<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    value: Option<dae::ExprId<'dae>>,
) -> Result<Option<Vec<solve::LinearOp>>, LowerError> {
    value
        .map(|value| ScalarCompiler::new(view, layout, None).program(value, 0))
        .transpose()
}

#[derive(Clone, Copy)]
struct EventUpdate<'dae> {
    trigger: dae::ConditionId<'dae>,
    guard: dae::ConditionId<'dae>,
    variable: u32,
    value: dae::ExprId<'dae>,
    span: Span,
    clock: Option<dae::ClockId<'dae>>,
}

type GuardedAssignment<'dae> = (
    dae::ConditionId<'dae>,
    dae::ConditionId<'dae>,
    dae::ExprId<'dae>,
    usize,
    usize,
);

struct GuardedTarget<'dae> {
    target: solve::ScalarSlot,
    span: Span,
    branches: Vec<GuardedAssignment<'dae>>,
    pre_mode: solve::DiscreteEventPreMode,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
}

fn lower_guarded_updates<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows,
    updates: &[EventUpdate<'dae>],
    role: solve::DiscreteRowRole,
) -> Result<(), LowerError> {
    let mut targets = Vec::<GuardedTarget<'dae>>::new();
    for update in updates {
        let expression = view
            .expression(update.value)
            .expect("checked event update expression resolves");
        let has_pre = expression_contains_pre(view, update.value)
            || condition_contains_pre(view, update.trigger)
            || condition_contains_pre(view, update.guard);
        for scalar in 0..expression
            .value_type()
            .scalar_count()
            .expect("checked event update scalar capacity")
        {
            let target = variable_scalar_slot(layout, update.variable, scalar, update.span)?;
            let clock = update
                .clock
                .map(|clock| clocks.clock(clock).map(|solve| (clock, solve)))
                .transpose()?;
            let trigger_memory = condition_memory(layout, update.trigger, update.span)?;
            let branch = (
                update.trigger,
                update.guard,
                update.value,
                scalar,
                trigger_memory,
            );
            record_guarded_target(&mut targets, target, branch, clock, has_pre, update.span)?;
        }
    }
    for target in targets {
        let program = match target.clock {
            Some((clock, _)) => ScalarCompiler::new(view, layout, None)
                .clocked_guarded_assignments_program(
                    clock,
                    &target.branches,
                    target.target,
                    target.span,
                )?,
            None => ScalarCompiler::new(view, layout, None).guarded_assignments_program(
                &target.branches,
                target.target,
                target.span,
            )?,
        };
        rows.push(
            program,
            target.span,
            target.target,
            role,
            target.pre_mode,
            target.clock.map(|(_, clock)| clock),
        );
    }
    Ok(())
}

fn record_guarded_target<'dae>(
    targets: &mut Vec<GuardedTarget<'dae>>,
    target: solve::ScalarSlot,
    branch: GuardedAssignment<'dae>,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
    has_pre: bool,
    span: Span,
) -> Result<(), LowerError> {
    let Some(group) = targets
        .iter_mut()
        .find(|group| same_target(group.target, target))
    else {
        targets.push(GuardedTarget {
            target,
            span,
            branches: vec![branch],
            pre_mode: event_update_pre_mode(has_pre),
            clock,
        });
        return Ok(());
    };
    if group.clock != clock {
        return Err(LowerError::non_computable(
            "one event target has incompatible clock activation owners",
            span,
        ));
    }
    group.branches.push(branch);
    group.pre_mode =
        event_update_pre_mode(has_pre || group.pre_mode == solve::DiscreteEventPreMode::EventEntry);
    Ok(())
}

fn event_update_pre_mode(has_pre: bool) -> solve::DiscreteEventPreMode {
    if has_pre {
        solve::DiscreteEventPreMode::EventEntry
    } else {
        solve::DiscreteEventPreMode::FollowCurrent
    }
}

fn lower_condition_memory<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    rows: &mut DiscreteRows,
) -> Result<(), LowerError> {
    for index in 0..view.condition_count() {
        let condition = view
            .condition_id(index)
            .expect("dense condition identity resolves");
        let condition_view = view
            .condition(condition)
            .expect("checked condition identity resolves");
        if condition_clock_owner(view, condition).is_some() {
            continue;
        }
        let span = condition_view.provenance().span();
        let memory = condition_memory(layout, condition, span)?;
        let target = solve::scalar_slot_p(memory);
        let program = ScalarCompiler::new(view, layout, None).condition_program(condition)?;
        rows.push(
            program,
            span,
            target,
            solve::DiscreteRowRole::ConditionMemory,
            solve::DiscreteEventPreMode::FollowCurrent,
            None,
        );
    }
    Ok(())
}

fn condition_clock_owner<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ConditionId<'dae>,
) -> Option<dae::ClockId<'dae>> {
    let condition = view
        .condition(condition)
        .expect("checked condition identity resolves");
    match condition.operation() {
        dae::ConditionOperation::Initial => None,
        dae::ConditionOperation::Clock(clock) => Some(clock),
        dae::ConditionOperation::And(lhs, rhs) => merge_condition_clocks(
            condition_clock_owner(view, lhs),
            condition_clock_owner(view, rhs),
            false,
        ),
        dae::ConditionOperation::Or(lhs, rhs) => merge_condition_clocks(
            condition_clock_owner(view, lhs),
            condition_clock_owner(view, rhs),
            true,
        ),
        dae::ConditionOperation::Relation(_)
        | dae::ConditionOperation::Discrete(_)
        | dae::ConditionOperation::Not(_) => None,
    }
}

fn merge_condition_clocks<'dae>(
    lhs: Option<dae::ClockId<'dae>>,
    rhs: Option<dae::ClockId<'dae>>,
    disjunction: bool,
) -> Option<dae::ClockId<'dae>> {
    match (lhs, rhs) {
        (Some(lhs), Some(rhs)) if lhs == rhs => Some(lhs),
        (Some(clock), None) | (None, Some(clock)) if !disjunction => Some(clock),
        _ => None,
    }
}

fn condition_memory(
    layout: &LoweredLayout<'_>,
    condition: dae::ConditionId<'_>,
    span: Span,
) -> Result<usize, LowerError> {
    layout
        .condition_memory
        .get(condition.index() as usize)
        .copied()
        .ok_or_else(|| LowerError::contract("condition has no Solve memory slot", span))
}

fn same_target(lhs: solve::ScalarSlot, rhs: solve::ScalarSlot) -> bool {
    matches!(
        (lhs, rhs),
        (
            solve::ScalarSlot::Y {
                index: lhs_index,
                ..
            },
            solve::ScalarSlot::Y {
                index: rhs_index,
                ..
            }
        ) | (
            solve::ScalarSlot::P {
                index: lhs_index,
                ..
            },
            solve::ScalarSlot::P {
                index: rhs_index,
                ..
            }
        ) if lhs_index == rhs_index
    )
}

struct LoweredRoots {
    programs: solve::ScalarProgramBlock,
    zero_domains: Vec<solve::RootZeroDomain>,
    count: usize,
}

fn lower_roots<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
) -> Result<LoweredRoots, LowerError> {
    let mut rows = ScalarRows::default();
    let mut zero_domains = Vec::with_capacity(view.root_count());
    for index in 0..view.root_count() {
        let id = view.root_id(index).expect("dense root identity resolves");
        let root = view.root(id).expect("checked root identity resolves");
        let relation = view
            .relation(root.relation())
            .expect("checked root relation resolves");
        rows.push(
            ScalarCompiler::new(view, layout, None).root_program(root.relation())?,
            root.provenance().span(),
            index,
        );
        zero_domains.push(root_zero_domain(view, relation.expression()));
    }
    Ok(LoweredRoots {
        programs: rows.into_scalar_block()?,
        zero_domains,
        count: view.root_count(),
    })
}

fn root_zero_domain<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> solve::RootZeroDomain {
    let operation = view
        .expression(expression)
        .expect("checked relation expression resolves")
        .operation();
    match operation {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::LessEqual | dae::BinaryOperator::GreaterEqual,
            ..
        } => solve::RootZeroDomain::NonPositive,
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Less | dae::BinaryOperator::Greater,
            ..
        } => solve::RootZeroDomain::Positive,
        _ => solve::RootZeroDomain::Previous,
    }
}

fn lower_time_events(view: dae::DaeView<'_>) -> Vec<f64> {
    (0..view.time_event_count())
        .map(|index| {
            let id = view
                .time_event_id(index)
                .expect("dense time event identity resolves");
            view.time_event(id)
                .expect("checked time event identity resolves")
                .instant()
                .to_f64()
        })
        .collect()
}

fn expression_pre_mode<'dae>(
    view: dae::DaeView<'dae>,
    expression: dae::ExprId<'dae>,
) -> solve::DiscreteEventPreMode {
    if expression_contains_pre(view, expression) {
        solve::DiscreteEventPreMode::EventEntry
    } else {
        solve::DiscreteEventPreMode::FollowCurrent
    }
}

fn condition_contains_pre<'dae>(view: dae::DaeView<'dae>, root: dae::ConditionId<'dae>) -> bool {
    let mut pending = vec![root];
    let mut visited = vec![false; view.condition_count()];
    while let Some(condition) = pending.pop() {
        let index = condition.index() as usize;
        if visited[index] {
            continue;
        }
        visited[index] = true;
        let condition = view
            .condition(condition)
            .expect("checked condition identity resolves");
        match condition.operation() {
            dae::ConditionOperation::Initial => {}
            dae::ConditionOperation::Relation(relation) => {
                let expression = view
                    .relation(relation)
                    .expect("checked relation identity resolves")
                    .expression();
                if expression_contains_pre(view, expression) {
                    return true;
                }
            }
            dae::ConditionOperation::Discrete(expression) => {
                if expression_contains_pre(view, expression) {
                    return true;
                }
            }
            dae::ConditionOperation::Clock(_) => {}
            dae::ConditionOperation::Not(operand) => pending.push(operand),
            dae::ConditionOperation::And(lhs, rhs) | dae::ConditionOperation::Or(lhs, rhs) => {
                pending.push(rhs);
                pending.push(lhs);
            }
        }
    }
    false
}

fn expression_contains_pre<'dae>(view: dae::DaeView<'dae>, root: dae::ExprId<'dae>) -> bool {
    let mut found = false;
    dae::for_each_expression(view, root, |_, expression| {
        found |= matches!(
            expression.operation(),
            dae::ExpressionOperation::Coordinate(
                dae::CoordinateView::PreDiscreteReal(_)
                    | dae::CoordinateView::PreDiscreteValue(_)
                    | dae::CoordinateView::Previous(_)
            )
        );
    });
    found
}
