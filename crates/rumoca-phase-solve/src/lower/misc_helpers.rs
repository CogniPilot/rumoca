use super::*;

pub(super) fn static_singleton_subscript_index(
    expr: &rumoca_core::Expression,
) -> Result<Option<usize>, LowerError> {
    lower_static_index_expr(expr)
}

pub(super) fn direct_assignment_component(
    values: &[Reg],
    flat_index: usize,
    repeat_period: Option<usize>,
) -> Option<Reg> {
    values.get(flat_index).copied().or_else(|| {
        let period = repeat_period?;
        (period > 0 && values.len() == period).then(|| values[flat_index % period])
    })
}

pub(super) fn complex_operator_call_op(name: &str) -> Option<BinaryOp> {
    match name {
        "Complex.'+'" => Some(BinaryOp::Add),
        "Complex.'-'" => Some(BinaryOp::Sub),
        "Complex.'*'" => Some(BinaryOp::Mul),
        "Complex.'/'" => Some(BinaryOp::Div),
        _ => None,
    }
}

pub(super) fn is_time_var_ref(expr: &rumoca_core::Expression) -> bool {
    matches!(
        expr,
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            ..
        } if name.as_str() == "time" && subscripts.is_empty()
    )
}

pub(super) fn size_binding_key(name: &str, dim: usize) -> String {
    format!("{SIZE_BINDING_PREFIX}{name}.{dim}")
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_current_update_target_start_before_first_clock_tick(
        &mut self,
        value: Reg,
        expression: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(target_name) = self.current_update_target_start_guard_name() else {
            return Ok(value);
        };
        let Some(phase_seconds) = self.start_guard_phase_seconds(target_name.as_str(), expression)
        else {
            return Ok(value);
        };
        let Some(start_expr) = self
            .variable_starts
            .and_then(|starts| starts.get(target_name.as_str()))
            .cloned()
        else {
            return Ok(value);
        };

        let time = self.emit_load_time();
        let phase = self.emit_const(phase_seconds);
        let tol = self.emit_const(1.0e-9);
        let first_tick_boundary = self.emit_binary(BinaryOp::Sub, phase, tol);
        let before_first_tick = self.emit_compare(CompareOp::Lt, time, first_tick_boundary);
        let start_value = self.lower_expr(&start_expr, scope, call_depth)?;
        Ok(self.emit_select(before_first_tick, start_value, value))
    }

    fn current_update_target_start_guard_name(&self) -> Option<String> {
        let target = self.current_update_target?;
        let starts = self.variable_starts?;
        self.layout.bindings().iter().find_map(|(name, slot)| {
            (*slot == target && starts.contains_key(name.as_str())).then(|| name.clone())
        })
    }

    fn start_guard_phase_seconds(
        &self,
        target_name: &str,
        expression: &rumoca_core::Expression,
    ) -> Option<f64> {
        let timings = self.clock_timings?;
        if let Some(timing) = timings.get(target_name) {
            return Some(timing.phase_seconds);
        }
        let source_name = binding_base_key(expression).ok()?;
        timings
            .get(source_name.as_str())
            .map(|timing| timing.phase_seconds)
    }

    pub(in crate::lower) fn lower_size_from_dims(
        &mut self,
        dims: &[usize],
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let dim_reg = if args.len() > 1 {
            let raw = self.lower_expr(&args[1], scope, call_depth)?;
            self.emit_round(raw)
        } else {
            self.emit_const(1.0)
        };

        let mut value = self.emit_const(1.0);
        for (idx, dim) in dims.iter().enumerate().rev() {
            let dim_idx = self.emit_const((idx + 1) as f64);
            let cond = self.emit_compare(CompareOp::Eq, dim_reg, dim_idx);
            let dim_val = self.emit_const(*dim as f64);
            value = self.emit_select(cond, dim_val, value);
        }
        Ok(value)
    }
}
