use super::*;
use crate::layout::INITIAL_EVENT_PARAMETER_NAME;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_sample_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match args {
            [] => Ok(self.emit_const(0.0)),
            [value] => self.lower_clocked_sample_value(value, scope, call_depth),
            [_internal_id, start, interval, ..] => {
                if self.value_mode == ValueMode::Pre {
                    return Ok(self.emit_const(0.0));
                }
                let phase = self.lower_expr(start, scope, call_depth)?;
                let period = self.lower_expr(interval, scope, call_depth)?;
                Ok(self.emit_periodic_tick(phase, period))
            }
            [value, clock_or_interval] => {
                if let Some(tick) =
                    self.lower_clock_tick_expr(clock_or_interval, scope, call_depth)?
                {
                    return self.lower_clocked_sample_with_tick(value, tick, scope, call_depth);
                }
                if self.current_update_target.is_some()
                    && self.is_dynamic_clock_var_ref(clock_or_interval)
                {
                    let tick = self.lower_expr(clock_or_interval, scope, call_depth)?;
                    return self.lower_clocked_sample_with_tick(value, tick, scope, call_depth);
                }
                if self.value_mode == ValueMode::Pre {
                    return Ok(self.emit_const(0.0));
                }
                let phase = self.lower_expr(value, scope, call_depth)?;
                let period = self.lower_expr(clock_or_interval, scope, call_depth)?;
                Ok(self.emit_periodic_tick(phase, period))
            }
        }
    }

    pub(super) fn is_dynamic_clock_var_ref(&self, expr: &rumoca_core::Expression) -> bool {
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expr
        else {
            return false;
        };

        subscripts.is_empty()
            && self
                .discrete_valued_names
                .is_some_and(|names| names.contains_key(name.var_name()))
    }

    pub(super) fn lower_clocked_sample_with_tick(
        &mut self,
        value: &rumoca_core::Expression,
        tick: Reg,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let sampled = self.lower_clocked_sample_value(value, scope, call_depth)?;
        let Some(target) = self.current_update_target else {
            return Ok(sampled);
        };
        let held = self.emit_slot_load(target)?;
        Ok(self.emit_select(tick, sampled, held))
    }

    pub(super) fn lower_clocked_sample_value(
        &mut self,
        value: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if is_time_var_ref(value) {
            self.lower_expr(value, scope, call_depth)
        } else {
            let sampled_pre = self.lower_expr_in_mode(value, scope, call_depth, ValueMode::Pre)?;
            let sampled_initial = self.lower_expr(value, scope, call_depth)?;
            let initial = self.lower_initial_builtin()?;
            Ok(self.emit_select(initial, sampled_initial, sampled_pre))
        }
    }

    pub(super) fn lower_initial_builtin(&mut self) -> Result<Reg, LowerError> {
        if self.value_mode == ValueMode::Pre {
            return Ok(self.emit_const(0.0));
        }
        if self.is_initial_mode {
            return Ok(self.emit_const(1.0));
        }
        let slot = self
            .layout
            .binding(INITIAL_EVENT_PARAMETER_NAME)
            .ok_or_else(|| LowerError::MissingBinding {
                name: INITIAL_EVENT_PARAMETER_NAME.to_string(),
            })?;
        self.emit_slot_load(slot)
    }

    pub(super) fn lower_edge_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(expr) = args.first() else {
            return Ok(self.emit_const(0.0));
        };
        let current = self.lower_expr(expr, scope, call_depth)?;
        let previous = self.lower_expr_in_mode(expr, scope, call_depth, ValueMode::Pre)?;
        let not_previous = self.emit_unary(UnaryOp::Not, previous);
        Ok(self.emit_binary(BinaryOp::And, current, not_previous))
    }

    pub(super) fn lower_change_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(expr) = args.first() else {
            return Ok(self.emit_const(0.0));
        };
        let current = self.lower_expr(expr, scope, call_depth)?;
        let previous = self.lower_expr_in_mode(expr, scope, call_depth, ValueMode::Pre)?;
        Ok(self.emit_compare(CompareOp::Ne, current, previous))
    }

    pub(super) fn lower_sum_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.is_empty() {
            return Ok(self.emit_const(0.0));
        }
        if args.len() == 1 {
            if let Some(reg) = self.lower_sum_range(&args[0], scope, call_depth)? {
                return Ok(reg);
            }
            let values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            if values.is_empty() {
                return Ok(self.emit_const(0.0));
            }
            let mut acc = self.emit_const(0.0);
            for value in values {
                acc = self.emit_binary(BinaryOp::Add, acc, value);
            }
            return Ok(acc);
        }

        let mut acc = self.emit_const(0.0);
        for expr in args {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary(BinaryOp::Add, acc, value);
        }
        Ok(acc)
    }

    pub(super) fn lower_product_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if args.is_empty() {
            return Ok(self.emit_const(1.0));
        }
        if args.len() == 1 {
            let values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            if values.is_empty() {
                return Ok(self.emit_const(1.0));
            }
            let mut acc = self.emit_const(1.0);
            for value in values {
                acc = self.emit_binary(BinaryOp::Mul, acc, value);
            }
            return Ok(acc);
        }

        let mut acc = self.emit_const(1.0);
        for expr in args {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary(BinaryOp::Mul, acc, value);
        }
        Ok(acc)
    }

    pub(super) fn lower_size_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(base_expr) = args.first() else {
            return Ok(self.emit_const(1.0));
        };
        let inferred_dims = self.infer_expr_dims(base_expr, scope)?;
        if !inferred_dims.is_empty() && inferred_dims.iter().all(|dim| *dim > 0) {
            return self.lower_size_from_dims(&inferred_dims, args, scope, call_depth);
        }

        let base_key = dynamic_binding_base_key(base_expr).map_err(|err| {
            err.with_fallback_span(base_expr.span().unwrap_or(rumoca_core::Span::DUMMY))
        })?;

        let source_key = component_reference_key_for_expr(base_expr)?;
        let generated_key = ComponentReferenceKey::generated(&base_key);
        let dims = infer_indexed_dims(
            source_key
                .as_ref()
                .and_then(|key| self.indexed_bindings.get(key))
                .or_else(|| self.indexed_bindings.get(&generated_key))
                .map(Vec::as_slice)
                .unwrap_or(&[]),
        );
        if dims.is_empty() {
            return Ok(self.emit_const(1.0));
        }

        self.lower_size_from_dims(&dims, args, scope, call_depth)
    }
}
