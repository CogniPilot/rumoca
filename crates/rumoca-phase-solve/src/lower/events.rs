use rumoca_core::Span;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::clocks::LoweredClocks;
use super::{LoweredLayout, ScalarCompiler, ScalarRows, variable_scalar_slot};
use crate::LowerError;

pub(super) fn lower_discrete_and_events<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
) -> Result<(solve::DiscreteSolveSystem, solve::SolveEventPartition), LowerError> {
    let mut discrete = DiscreteRows::default();
    lower_discrete_real_equations(view, layout, clocks, &mut discrete)?;
    lower_unconditional_assignments(view, layout, &mut discrete)?;
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
        ..solve::SolveEventPartition::default()
    };
    Ok((discrete, events))
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
            observation_refresh: vec![false; rhs.programs.len()],
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
    for index in 0..view.discrete_real_equation_count() {
        let equation = view
            .discrete_real_equation(index)
            .expect("dense checked discrete Real equation resolves");
        let span = equation.provenance().span();
        let (target, value) = direct_discrete_real_definition(view, equation.residual())
            .ok_or_else(|| {
                LowerError::unsupported(
                    "coupled discrete Real residual is not an explicit computable definition",
                    span,
                )
            })?;
        let variable = dae::VariableId::from(target);
        let clock_owner = clocks.variable_owner(variable).ok_or_else(|| {
            LowerError::unsupported(
                "discrete Real definition has no checked clock activation owner",
                span,
            )
        })?;
        let value_type = view
            .expression(value)
            .expect("checked discrete definition value resolves")
            .value_type();
        for scalar in 0..value_type
            .scalar_count()
            .expect("checked expression scalar capacity")
        {
            let program = ScalarCompiler::new(view, layout, None).program(value, scalar)?;
            let target = variable_scalar_slot(layout, variable.index(), scalar, span)?;
            rows.push(
                program,
                span,
                target,
                solve::DiscreteRowRole::Equation,
                expression_pre_mode(view, value),
                Some(clock_owner),
            );
        }
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

fn lower_unconditional_assignments<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    rows: &mut DiscreteRows,
) -> Result<(), LowerError> {
    for index in 0..view.discrete_assignment_count() {
        let id = view
            .discrete_assignment_id(index)
            .expect("dense discrete assignment identity resolves");
        let assignment = view
            .discrete_assignment(id)
            .expect("checked discrete assignment resolves");
        let value = view
            .expression(assignment.value())
            .expect("checked assignment expression resolves");
        let pre_mode = expression_pre_mode(view, assignment.value());
        for scalar in 0..value
            .value_type()
            .scalar_count()
            .expect("checked assignment scalar capacity")
        {
            let span = assignment.provenance().span();
            let program =
                ScalarCompiler::new(view, layout, None).program(assignment.value(), scalar)?;
            let target = variable_scalar_slot(layout, assignment.target().index(), scalar, span)?;
            rows.push(
                program,
                span,
                target,
                solve::DiscreteRowRole::Equation,
                pre_mode,
                None,
            );
        }
    }
    Ok(())
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
            dae::EventActionOperation::AssignDiscreteReal { target, value } => {
                updates.push(EventUpdate {
                    trigger: action.trigger(),
                    guard: action.guard(),
                    variable: target.index(),
                    value,
                    span: action.provenance().span(),
                    clock: condition_clock_owner(view, action.guard()),
                });
            }
            dae::EventActionOperation::AssignDiscreteValue { target, value } => {
                updates.push(EventUpdate {
                    trigger: action.trigger(),
                    guard: action.guard(),
                    variable: target.index(),
                    value,
                    span: action.provenance().span(),
                    clock: condition_clock_owner(view, action.guard()),
                });
            }
        }
    }
    lower_guarded_updates(view, layout, clocks, discrete, &updates)
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
    let message = literal_message(view, message, span)?;
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
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Text(message)],
        },
        span,
        origin: action.provenance().origin().to_string(),
    });
    Ok(())
}

fn literal_message<'dae>(
    view: dae::DaeView<'dae>,
    message: dae::ExprId<'dae>,
    span: Span,
) -> Result<String, LowerError> {
    let expression = view
        .expression(message)
        .expect("checked event message expression resolves");
    match expression.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::String(message)) => Ok(message.clone()),
        _ => Err(LowerError::unsupported(
            "non-literal event messages do not yet have checked Solve lowering",
            span,
        )),
    }
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

struct GuardedTarget<'dae> {
    target: solve::ScalarSlot,
    span: Span,
    branches: Vec<(
        dae::ConditionId<'dae>,
        dae::ConditionId<'dae>,
        dae::ExprId<'dae>,
        usize,
        usize,
    )>,
    pre_mode: solve::DiscreteEventPreMode,
    clock: Option<(dae::ClockId<'dae>, solve::PeriodicClockId)>,
}

fn lower_guarded_updates<'dae>(
    view: dae::DaeView<'dae>,
    layout: &LoweredLayout<'dae>,
    clocks: &LoweredClocks<'dae>,
    rows: &mut DiscreteRows,
    updates: &[EventUpdate<'dae>],
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
            solve::DiscreteRowRole::EventAction,
            target.pre_mode,
            target.clock.map(|(_, clock)| clock),
        );
    }
    Ok(())
}

fn record_guarded_target<'dae>(
    targets: &mut Vec<GuardedTarget<'dae>>,
    target: solve::ScalarSlot,
    branch: (
        dae::ConditionId<'dae>,
        dae::ConditionId<'dae>,
        dae::ExprId<'dae>,
        usize,
        usize,
    ),
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
                dae::CoordinateView::PreDiscreteReal(_) | dae::CoordinateView::PreDiscreteValue(_)
            )
        );
    });
    found
}
