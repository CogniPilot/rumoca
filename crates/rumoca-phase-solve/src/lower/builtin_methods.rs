use super::*;
use crate::layout::INITIAL_EVENT_PARAMETER_NAME;

fn builtin_arg_span(
    args: &[rumoca_core::Expression],
    call_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    args.first()
        .and_then(rumoca_core::Expression::span)
        .or_else(|| (!call_span.is_dummy()).then_some(call_span))
        .ok_or_else(|| missing_builtin_source_span(context))
}

fn missing_builtin_argument(
    function: rumoca_core::BuiltinFunction,
    span: rumoca_core::Span,
) -> LowerError {
    LowerError::contract_violation(format!("{}() requires argument 1", function.name()), span)
}

fn expression_or_call_span(
    expr: &rumoca_core::Expression,
    call_span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    expr.span()
        .or_else(|| (!call_span.is_dummy()).then_some(call_span))
        .ok_or_else(|| missing_builtin_source_span(context))
}

fn required_builtin_call_span(
    span: rumoca_core::Span,
    context: &'static str,
) -> Result<rumoca_core::Span, LowerError> {
    (!span.is_dummy())
        .then_some(span)
        .ok_or_else(|| missing_builtin_source_span(context))
}

fn missing_builtin_source_span(context: &'static str) -> LowerError {
    LowerError::UnspannedContractViolation {
        reason: format!("{context} requires source span"),
    }
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_sample_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match args {
            [] => Err(missing_builtin_argument(
                rumoca_core::BuiltinFunction::Sample,
                call_span,
            )),
            [value] => self.lower_clocked_sample_value(value, call_span, scope, call_depth),
            [_internal_id, start, interval, ..] => {
                if self.value_mode == ValueMode::Pre {
                    return self.emit_const_at(
                        0.0,
                        expression_or_call_span(start, call_span, "sample() start argument")?,
                    );
                }
                let phase = self.lower_expr(start, scope, call_depth)?;
                let period = self.lower_expr(interval, scope, call_depth)?;
                let span = start
                    .span()
                    .or_else(|| interval.span())
                    .or_else(|| (!call_span.is_dummy()).then_some(call_span))
                    .ok_or_else(|| {
                        missing_builtin_source_span("sample() start/interval arguments")
                    })?;
                self.emit_periodic_tick(phase, period, span)
            }
            [value, clock_or_interval] => {
                if let Some(tick) =
                    self.lower_clock_tick_expr(clock_or_interval, scope, call_depth)?
                {
                    return self
                        .lower_clocked_sample_with_tick(value, tick, call_span, scope, call_depth);
                }
                if self.current_update_target.is_some()
                    && self.is_dynamic_clock_var_ref(clock_or_interval)
                {
                    let tick = self.lower_expr(clock_or_interval, scope, call_depth)?;
                    return self
                        .lower_clocked_sample_with_tick(value, tick, call_span, scope, call_depth);
                }
                if self.value_mode == ValueMode::Pre {
                    return self.emit_const_at(
                        0.0,
                        expression_or_call_span(value, call_span, "sample() value argument")?,
                    );
                }
                let phase = self.lower_expr(value, scope, call_depth)?;
                let period = self.lower_expr(clock_or_interval, scope, call_depth)?;
                let span = value
                    .span()
                    .or_else(|| clock_or_interval.span())
                    .or_else(|| (!call_span.is_dummy()).then_some(call_span))
                    .ok_or_else(|| {
                        missing_builtin_source_span("sample() value/interval arguments")
                    })?;
                self.emit_periodic_tick(phase, period, span)
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
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let sampled = self.lower_clocked_sample_value(value, call_span, scope, call_depth)?;
        let Some(target) = self.current_update_target else {
            return Ok(sampled);
        };
        let span = expression_or_call_span(value, call_span, "sample() value argument")?;
        let held = self.emit_slot_load(target, span)?;
        self.emit_select_at(tick, sampled, held, span)
    }

    pub(super) fn lower_clocked_sample_value(
        &mut self,
        value: &rumoca_core::Expression,
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if is_time_var_ref(value) {
            self.lower_expr(value, scope, call_depth)
        } else {
            let sampled_pre = self.lower_expr_in_mode(value, scope, call_depth, ValueMode::Pre)?;
            let sampled_initial = self.lower_expr(value, scope, call_depth)?;
            let initial = self.lower_initial_builtin(call_span)?;
            let span = expression_or_call_span(value, call_span, "sample() value argument")?;
            self.emit_select_at(initial, sampled_initial, sampled_pre, span)
        }
    }

    pub(super) fn lower_initial_builtin(
        &mut self,
        call_span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let span = required_builtin_call_span(call_span, "initial() call")?;
        if self.value_mode == ValueMode::Pre {
            return self.emit_const_at(0.0, span);
        }
        if self.is_initial_mode {
            return self.emit_const_at(1.0, span);
        }
        let slot = self
            .layout
            .binding(INITIAL_EVENT_PARAMETER_NAME)
            .ok_or_else(|| LowerError::MissingBinding {
                name: INITIAL_EVENT_PARAMETER_NAME.to_string(),
            })?;
        self.emit_slot_load(slot, span)
    }

    pub(super) fn lower_edge_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(expr) = args.first() else {
            return Err(missing_builtin_argument(
                rumoca_core::BuiltinFunction::Edge,
                call_span,
            ));
        };
        let span = expression_or_call_span(expr, call_span, "edge() argument")?;
        let current = self.lower_expr(expr, scope, call_depth)?;
        let previous = self.lower_expr_in_mode(expr, scope, call_depth, ValueMode::Pre)?;
        let not_previous = self.emit_unary_at(UnaryOp::Not, previous, span)?;
        self.emit_binary_at(BinaryOp::And, current, not_previous, span)
    }

    pub(super) fn lower_change_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(expr) = args.first() else {
            return Err(missing_builtin_argument(
                rumoca_core::BuiltinFunction::Change,
                call_span,
            ));
        };
        let span = expression_or_call_span(expr, call_span, "change() argument")?;
        let current = self.lower_expr(expr, scope, call_depth)?;
        let previous = self.lower_expr_in_mode(expr, scope, call_depth, ValueMode::Pre)?;
        self.emit_compare_at(CompareOp::Ne, current, previous, span)
    }

    pub(super) fn lower_sum_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let span = builtin_arg_span(args, call_span, "sum() call")?;
        if args.is_empty() {
            return self.emit_const_at(0.0, span);
        }
        if args.len() == 1 {
            if let Some(reg) = self.lower_sum_range(&args[0], scope, call_depth)? {
                return Ok(reg);
            }
            let values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            if values.is_empty() {
                return self.emit_const_at(0.0, span);
            }
            let mut acc = self.emit_const_at(0.0, span)?;
            for value in values {
                acc = self.emit_binary_at(BinaryOp::Add, acc, value, span)?;
            }
            return Ok(acc);
        }

        let mut acc = self.emit_const_at(0.0, span)?;
        for expr in args {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary_at(BinaryOp::Add, acc, value, expr.span().unwrap_or(span))?;
        }
        Ok(acc)
    }

    pub(super) fn lower_product_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let span = builtin_arg_span(args, call_span, "product() call")?;
        if args.is_empty() {
            return self.emit_const_at(1.0, span);
        }
        if args.len() == 1 {
            let values = self.lower_array_like_values(&args[0], scope, call_depth)?;
            if values.is_empty() {
                return self.emit_const_at(1.0, span);
            }
            let mut acc = self.emit_const_at(1.0, span)?;
            for value in values {
                acc = self.emit_binary_at(BinaryOp::Mul, acc, value, span)?;
            }
            return Ok(acc);
        }

        let mut acc = self.emit_const_at(1.0, span)?;
        for expr in args {
            let value = self.lower_expr(expr, scope, call_depth)?;
            acc = self.emit_binary_at(BinaryOp::Mul, acc, value, expr.span().unwrap_or(span))?;
        }
        Ok(acc)
    }

    pub(super) fn lower_size_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(base_expr) = args.first() else {
            return Err(LowerError::contract_violation(
                "size() requires at least 1 argument",
                span,
            ));
        };
        if let rumoca_core::Expression::FunctionCall {
            name,
            args: passthrough_args,
            is_constructor: false,
            ..
        } = base_expr
            && is_stream_passthrough_intrinsic(name.as_str())
            && let Some(arg) = passthrough_args.first()
        {
            let mut forwarded_args = Vec::with_capacity(args.len());
            forwarded_args.push(arg.clone());
            forwarded_args.extend(args.iter().skip(1).cloned());
            return self.lower_size_builtin(&forwarded_args, span, scope, call_depth);
        }
        let base_span = expression_or_call_span(base_expr, span, "size() base argument")?;
        if let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = base_expr
            && subscripts.is_empty()
            && let Some(dims) = self.local_binding_dims.get(name.as_str())
            && !dims.is_empty()
            && dims.iter().all(|dim| *dim > 0)
        {
            let dims = dims
                .iter()
                .copied()
                .map(usize::try_from)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|_| {
                    LowerError::contract_violation(
                        "local size() dimension is outside host range",
                        base_span,
                    )
                })?;
            return self.lower_size_from_dims(&dims, args, base_span, scope, call_depth);
        }
        let inferred_dims = if expr_component_reference_missing_def_id(base_expr) {
            Vec::new()
        } else {
            self.infer_expr_dims(base_expr, scope)?
        };
        if !inferred_dims.is_empty() && inferred_dims.iter().all(|dim| *dim > 0) {
            return self.lower_size_from_dims(&inferred_dims, args, base_span, scope, call_depth);
        }
        if let Ok(value) = self.eval_compile_time_size(args, span, &self.local_const_bindings) {
            return self.emit_const_at(value, base_span);
        }

        let base_key = match dynamic_binding_base_key(base_expr) {
            Ok(base_key) => base_key,
            Err(LowerError::DynamicBindingBase { .. }) => {
                return self.emit_const_at(1.0, base_span);
            }
            Err(err) => return Err(err.with_fallback_span(base_span)),
        };

        let generated_key = ComponentReferenceKey::generated(&base_key);
        let generated_entries = self.indexed_bindings.get(&generated_key);
        let source_key =
            if generated_entries.is_some() || expr_component_reference_missing_def_id(base_expr) {
                None
            } else {
                component_reference_key_for_expr(base_expr)?
            };
        let dims = infer_indexed_dims(
            generated_entries
                .or_else(|| {
                    source_key
                        .as_ref()
                        .and_then(|key| self.indexed_bindings.get(key))
                })
                .map(Vec::as_slice)
                .unwrap_or(&[]),
        );
        if dims.is_empty() {
            return self.emit_const_at(1.0, base_span);
        }

        self.lower_size_from_dims(&dims, args, base_span, scope, call_depth)
    }
}

fn expr_component_reference_missing_def_id(expr: &rumoca_core::Expression) -> bool {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => name
            .component_ref()
            .is_some_and(|component_ref| component_ref.def_id.is_none()),
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => {
            expr_component_reference_missing_def_id(base)
        }
        _ => false,
    }
}
