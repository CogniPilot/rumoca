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
                && row
                    .reads
                    .iter()
                    .any(|slot| active_targets.iter().any(|target| target == slot))
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
    for eq in normalized_discrete_update_equations(dae_model) {
        let Some(lhs) = eq.lhs.as_ref() else {
            continue;
        };
        let scalar_count = eq.scalar_count.max(1);
        let safe = expression_safe_for_observation_refresh(dae_model, lhs, &eq.rhs);
        let seed = expression_has_observation_pulse(dae_model, &eq.rhs)
            && !expression_contains_pre_operator(&eq.rhs);
        let mut reads = Vec::new();
        collect_expression_read_slots(dae_model, layout, &eq.rhs, &mut reads);
        for flat_index in 0..scalar_count {
            let name = discrete_update_scalar_name(dae_model, lhs, flat_index, scalar_count);
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
            && expression_contains_lowered_pre_ref(expr))
}

fn expression_has_observation_pulse(dae_model: &dae::Dae, expr: &rumoca_core::Expression) -> bool {
    let mut checker = ObservationPulseChecker {
        dae_model,
        found: false,
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
    rumoca_core::top_level_last_segment(name)
}
