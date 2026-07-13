use rumoca_core::ExpressionVisitor;
use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use crate::{
    LowerError, collect_expression_read_slots,
    discrete_pre_modes::{expression_contains_lowered_pre_ref, target_has_clock_metadata},
    discrete_update_scalar_name, normalized_discrete_update_equations,
};

pub(super) fn lower_discrete_observation_refresh(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
    runtime_assignment_targets: &[solve::ScalarSlot],
) -> Result<Vec<bool>, LowerError> {
    let rows = discrete_observation_refresh_rows(dae_model, layout)?;
    let mut refresh = rows
        .iter()
        .map(|row| {
            row.safe
                && (row.seed
                    || row_reads_runtime_assignment_target(row, runtime_assignment_targets))
        })
        .collect::<Vec<_>>();
    loop {
        let before = refresh.clone();
        let active_targets = active_refresh_targets(&rows, &refresh);
        for (idx, row) in rows.iter().enumerate() {
            if !refresh[idx]
                && row.safe
                && (row_reads_active_target(row, &active_targets)
                    || active_row_reads_target(&rows, &refresh, row.target))
            {
                refresh[idx] = true;
            }
        }
        if refresh == before {
            return Ok(refresh);
        }
    }
}

fn row_reads_runtime_assignment_target(
    row: &DiscreteObservationRefreshRow,
    runtime_assignment_targets: &[solve::ScalarSlot],
) -> bool {
    row.reads.iter().any(|slot| {
        runtime_assignment_targets
            .iter()
            .any(|target| target == slot)
    })
}

fn row_reads_active_target(
    row: &DiscreteObservationRefreshRow,
    active_targets: &[solve::ScalarSlot],
) -> bool {
    row.reads
        .iter()
        .any(|slot| active_targets.iter().any(|target| target == slot))
}

fn active_row_reads_target(
    rows: &[DiscreteObservationRefreshRow],
    refresh: &[bool],
    target: solve::ScalarSlot,
) -> bool {
    rows.iter()
        .zip(refresh.iter().copied())
        .any(|(row, active)| active && row.reads.iter().any(|slot| slot == &target))
}

#[derive(Debug)]
struct DiscreteObservationRefreshRow {
    target: solve::ScalarSlot,
    reads: Vec<solve::ScalarSlot>,
    safe: bool,
    seed: bool,
}

fn discrete_observation_refresh_rows(
    dae_model: &dae::Dae,
    layout: &solve::VarLayout,
) -> Result<Vec<DiscreteObservationRefreshRow>, LowerError> {
    let mut rows = Vec::new();
    for eq in normalized_discrete_update_equations(dae_model)? {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        let scalar_count = eq.scalar_count.max(1);
        let safe = expression_safe_for_observation_refresh(dae_model, lhs.var_name(), &eq.rhs);
        let contains_pre = expression_contains_pre_operator(&eq.rhs);
        let seed = !contains_pre
            && (expression_has_observation_pulse(dae_model, &eq.rhs)
                || expression_has_observation_event_relation(&eq.rhs));
        let mut reads = Vec::new();
        collect_expression_read_slots(dae_model, layout, &eq.rhs, &mut reads)?;
        for flat_index in 0..scalar_count {
            let name = discrete_update_scalar_name(
                dae_model,
                lhs.var_name(),
                flat_index,
                scalar_count,
                eq.span,
            )?;
            let Some(target) = layout.binding(name.as_str()) else {
                return Err(LowerError::MissingBinding { name });
            };
            rows.push(DiscreteObservationRefreshRow {
                target,
                reads: reads.clone(),
                safe,
                seed,
            });
        }
    }
    Ok(rows)
}

fn active_refresh_targets(
    rows: &[DiscreteObservationRefreshRow],
    refresh: &[bool],
) -> Vec<solve::ScalarSlot> {
    rows.iter()
        .zip(refresh.iter().copied())
        .filter_map(|(row, active)| active.then_some(row.target))
        .collect()
}

fn expression_safe_for_observation_refresh(
    dae_model: &dae::Dae,
    lhs: &rumoca_core::VarName,
    expr: &rumoca_core::Expression,
) -> bool {
    !(expression_contains_clocked_value_operator(dae_model, expr)
        || target_has_clock_metadata(dae_model, lhs.as_str())
            && expression_contains_lowered_pre_ref(expr)
            && !expression_is_direct_lowered_change_relation(expr))
}

fn expression_is_direct_lowered_change_relation(expr: &rumoca_core::Expression) -> bool {
    let rumoca_core::Expression::Binary { op, lhs, rhs, .. } = expr else {
        return false;
    };
    if !matches!(op, rumoca_core::OpBinary::Eq | rumoca_core::OpBinary::Neq) {
        return false;
    }
    current_and_lowered_pre_ref_match(lhs, rhs) || current_and_lowered_pre_ref_match(rhs, lhs)
}

fn current_and_lowered_pre_ref_match(
    current: &rumoca_core::Expression,
    previous: &rumoca_core::Expression,
) -> bool {
    let rumoca_core::Expression::VarRef {
        name: current_name,
        subscripts: current_subscripts,
        ..
    } = current
    else {
        return false;
    };
    let rumoca_core::Expression::VarRef {
        name: previous_name,
        subscripts: previous_subscripts,
        ..
    } = previous
    else {
        return false;
    };
    current_subscripts == previous_subscripts
        && rumoca_core::pre_slot_base(previous_name.as_str())
            .is_some_and(|base| base == current_name.as_str())
}

fn expression_has_observation_pulse(dae_model: &dae::Dae, expr: &rumoca_core::Expression) -> bool {
    let mut checker = ObservationPulseChecker {
        dae_model,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

fn expression_has_observation_event_relation(expr: &rumoca_core::Expression) -> bool {
    let mut checker = EventRelationChecker {
        found: false,
        no_event_depth: 0,
    };
    checker.visit_expression(expr);
    checker.found
}

fn expression_contains_pre_operator(expr: &rumoca_core::Expression) -> bool {
    let mut checker = PreOperatorChecker { found: false };
    checker.visit_expression(expr);
    checker.found
}

struct PreOperatorChecker {
    found: bool,
}

impl ExpressionVisitor for PreOperatorChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Pre {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

struct EventRelationChecker {
    found: bool,
    no_event_depth: usize,
}

impl ExpressionVisitor for EventRelationChecker {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if !self.found {
            self.walk_expression(expr);
        }
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::NoEvent {
            self.no_event_depth += 1;
            for arg in args {
                self.visit_expression(arg);
            }
            self.no_event_depth -= 1;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_binary(
        &mut self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
    ) {
        if self.no_event_depth == 0
            && matches!(
                op,
                rumoca_core::OpBinary::Lt
                    | rumoca_core::OpBinary::Le
                    | rumoca_core::OpBinary::Gt
                    | rumoca_core::OpBinary::Ge
                    | rumoca_core::OpBinary::Eq
                    | rumoca_core::OpBinary::Neq
            )
        {
            self.found = true;
            return;
        }
        self.visit_expression(lhs);
        self.visit_expression(rhs);
    }
}

fn expression_contains_clocked_value_operator(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    let mut checker = ClockedValueOperatorChecker {
        dae_model,
        found: false,
    };
    checker.visit_expression(expr);
    checker.found
}

struct ObservationPulseChecker<'a> {
    dae_model: &'a dae::Dae,
    found: bool,
}

impl ExpressionVisitor for ObservationPulseChecker<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found {
            return;
        }

        if expression_is_clock_expr(self.dae_model, expr)
            && !expression_is_triggered_clock_expr(self.dae_model, expr)
        {
            self.found = true;
            return;
        }

        self.walk_expression(expr);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Sample
            && sample_args_are_event_indicator(self.dae_model, args)
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        _is_constructor: bool,
    ) {
        if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
            && sample_args_are_event_indicator(self.dae_model, args)
        {
            self.found = true;
            return;
        }
        if function_short_name(name.as_str()) == "firstTick" {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

struct ClockedValueOperatorChecker<'a> {
    dae_model: &'a dae::Dae,
    found: bool,
}

impl ExpressionVisitor for ClockedValueOperatorChecker<'_> {
    fn visit_expression(&mut self, expr: &rumoca_core::Expression) {
        if self.found || expression_is_clock_expr(self.dae_model, expr) {
            return;
        }
        self.walk_expression(expr);
    }

    fn visit_builtin_call(
        &mut self,
        function: &rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
    ) {
        if *function == rumoca_core::BuiltinFunction::Sample
            && !sample_args_are_event_indicator(self.dae_model, args)
            && !sample_args_are_time_value(args)
        {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }

    fn visit_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        _is_constructor: bool,
    ) {
        if name.as_str() == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME
            && !sample_args_are_event_indicator(self.dae_model, args)
        {
            self.found = true;
            return;
        }
        let short = function_short_name(name.as_str());
        if matches!(
            short,
            "previous" | "subSample" | "superSample" | "shiftSample" | "backSample"
        ) {
            self.found = true;
            return;
        }
        for arg in args {
            self.visit_expression(arg);
        }
    }
}

fn sample_args_are_event_indicator(dae_model: &dae::Dae, args: &[rumoca_core::Expression]) -> bool {
    match args {
        [_, _, ..] if args.len() >= 3 => true,
        [start, clock_or_interval] => {
            expression_is_parameter_like_event_arg(dae_model, start)
                && expression_is_parameter_like_event_arg(dae_model, clock_or_interval)
                && !expression_is_clock_expr(dae_model, clock_or_interval)
        }
        _ => false,
    }
}

fn sample_args_are_time_value(args: &[rumoca_core::Expression]) -> bool {
    matches!(
        args,
        [rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        }] if subscripts.is_empty() && name.as_str() == "time"
    )
}

fn expression_is_parameter_like_event_arg(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    match expr {
        rumoca_core::Expression::Literal { .. } => true,
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            dae_model.variables.parameters.contains_key(name.var_name())
                || dae_model.variables.constants.contains_key(name.var_name())
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            expression_is_parameter_like_event_arg(dae_model, rhs)
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            expression_is_parameter_like_event_arg(dae_model, lhs)
                && expression_is_parameter_like_event_arg(dae_model, rhs)
        }
        _ => false,
    }
}

fn expression_is_clock_expr(dae_model: &dae::Dae, expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => dae_model.clocks.intervals.contains_key(name.as_str()),
        rumoca_core::Expression::FunctionCall { name, args, .. } => {
            function_call_is_clock_expr(dae_model, name.as_str(), args)
        }
        _ => false,
    }
}

fn expression_is_triggered_clock_expr(
    dae_model: &dae::Dae,
    expr: &rumoca_core::Expression,
) -> bool {
    let rumoca_core::Expression::FunctionCall { name, args, .. } = expr else {
        return false;
    };
    function_short_name(name.as_str()) == "Clock"
        && args.len() == 1
        && dae_model
            .clocks
            .triggered_conditions
            .iter()
            .any(|condition| condition == &args[0])
}

fn function_call_is_clock_expr(
    dae_model: &dae::Dae,
    name: &str,
    args: &[rumoca_core::Expression],
) -> bool {
    match function_short_name(name) {
        "Clock" => true,
        // MLS §16.5.2: clock conversion operators have Clock-valued input
        // forms. Value-rate conversion forms are not clock expressions and
        // must keep their held/sample semantics.
        "subSample" | "superSample" | "shiftSample" | "backSample" => args
            .first()
            .is_some_and(|arg| expression_is_clock_expr(dae_model, arg)),
        _ => false,
    }
}

fn function_short_name(name: &str) -> &str {
    crate::path_utils::leaf_segment(name)
}
