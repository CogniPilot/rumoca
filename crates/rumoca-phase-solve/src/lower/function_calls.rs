use super::function_projection::FunctionOutputProjection;
use super::*;
use rumoca_ir_solve::RandomGenerator;
mod random;
mod runtime_intrinsics;
pub(super) use runtime_intrinsics::*;

struct ComplexProjectionComprehensionCtx<'a> {
    indices: &'a [rumoca_core::ComprehensionIndex],
    filter: Option<&'a rumoca_core::Expression>,
    field: &'a str,
    scope: &'a mut Scope,
    const_scope: &'a mut IndexMap<String, f64>,
    call_depth: usize,
}

struct FlattenedRecordInputRequest<'a, 'b> {
    input: &'a rumoca_core::FunctionParam,
    fields: &'a [rumoca_core::FunctionParam],
    positional_args: &'a [&'a rumoca_core::Expression],
    positional_idx: &'b mut usize,
    caller_scope: &'a Scope,
    call_depth: usize,
}

struct FlattenedRecordPositionalInputRequest<'a, 'b> {
    function_name: &'a str,
    input: &'a rumoca_core::FunctionParam,
    inputs: &'a [rumoca_core::FunctionParam],
    input_idx: usize,
    positional_args: &'a [&'a rumoca_core::Expression],
    positional_idx: &'b mut usize,
    caller_scope: &'a Scope,
    call_depth: usize,
}

struct NamedOrPositionalArg<'a> {
    name: &'a str,
    idx: usize,
    default: f64,
}

fn split_flattened_record_input_name(name: &str) -> Option<(&str, &str)> {
    let (prefix, field) = name.split_once('_')?;
    (!prefix.is_empty() && !field.is_empty()).then_some((prefix, field))
}

fn flattened_input_has_prefix(name: &str, prefix: &str) -> bool {
    split_flattened_record_input_name(name).is_some_and(|(candidate, _)| candidate == prefix)
}

fn has_flattened_record_input_group(
    inputs: &[rumoca_core::FunctionParam],
    input_idx: usize,
    prefix: &str,
) -> bool {
    inputs.iter().enumerate().any(|(idx, candidate)| {
        idx != input_idx && flattened_input_has_prefix(&candidate.name, prefix)
    })
}

fn missing_required_function_input<T>(
    function_name: &str,
    input_name: &str,
) -> Result<T, LowerError> {
    Err(LowerError::InvalidFunction {
        name: function_name.to_string(),
        reason: format!("required input `{input_name}` has no actual argument or default binding"),
    })
}

fn synthesize_missing_flattened_record_field_arg(
    input: &rumoca_core::FunctionParam,
    inputs: &[rumoca_core::FunctionParam],
    input_idx: usize,
    positional_args: &[&rumoca_core::Expression],
    positional_idx: usize,
) -> Option<rumoca_core::Expression> {
    let (prefix, field) = split_flattened_record_input_name(&input.name)?;
    if !has_flattened_record_input_group(inputs, input_idx, prefix) {
        return None;
    }
    let search_len = positional_idx.min(positional_args.len()).min(input_idx);
    for previous_idx in (0..search_len).rev() {
        let (previous_prefix, previous_field) =
            split_flattened_record_input_name(&inputs.get(previous_idx)?.name)?;
        if previous_prefix != prefix {
            continue;
        }
        let base =
            flattened_record_field_actual_base(positional_args[previous_idx], previous_field)?;
        return Some(record_field_access_expr(base, field));
    }
    None
}

fn flattened_record_field_actual_base(
    expr: &rumoca_core::Expression,
    field: &str,
) -> Option<rumoca_core::Expression> {
    match expr {
        rumoca_core::Expression::VarRef {
            name,
            subscripts,
            span,
        } if subscripts.is_empty() => {
            let base = name.as_str().strip_suffix(&format!(".{field}"))?;
            Some(rumoca_core::Expression::VarRef {
                name: rumoca_core::Reference::new(base),
                subscripts: vec![],
                span: *span,
            })
        }
        rumoca_core::Expression::FieldAccess {
            base,
            field: actual_field,
            ..
        } if actual_field == field => Some((**base).clone()),
        _ => None,
    }
}

fn record_field_access_expr(base: rumoca_core::Expression, field: &str) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        span: base.span().unwrap_or(rumoca_core::Span::DUMMY),
        base: Box::new(base),
        field: field.to_string(),
    }
}

fn flattened_record_positional_projected_arg(
    arg_expr: &rumoca_core::Expression,
    prefix: &str,
    field: &str,
    inputs: &[rumoca_core::FunctionParam],
    input_idx: usize,
) -> rumoca_core::Expression {
    if let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = arg_expr
        && subscripts.is_empty()
        && name.as_str().ends_with(&format!(".{field}"))
    {
        return arg_expr.clone();
    }
    if let rumoca_core::Expression::FieldAccess {
        field: actual_field,
        ..
    } = arg_expr
        && actual_field == field
    {
        return arg_expr.clone();
    }
    for (candidate_idx, candidate) in inputs.iter().enumerate() {
        if candidate_idx == input_idx {
            continue;
        }
        let Some((candidate_prefix, candidate_field)) =
            split_flattened_record_input_name(&candidate.name)
        else {
            continue;
        };
        if candidate_prefix != prefix || candidate_field == field {
            continue;
        }
        if let Some(base) = flattened_record_field_actual_base(arg_expr, candidate_field) {
            return record_field_access_expr(base, field);
        }
    }
    rumoca_core::Expression::FieldAccess {
        base: Box::new(arg_expr.clone()),
        field: field.to_string(),
        span: arg_expr.span().unwrap_or(rumoca_core::Span::DUMMY),
    }
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn try_lower_qualified_standard_numeric_intrinsic(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        _span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match name.as_str() {
            // MLS §12.4: a function call is an expression. These qualified
            // Modelica Standard Library scalar math functions are rendered as
            // solve-IR expressions instead of inlining their function bodies,
            // so local helper arrays in the library function remain local and
            // cannot leak into the runtime variable layout.
            "Modelica.Math.Distributions.Uniform.quantile" => self
                .lower_uniform_quantile(args, scope, call_depth)
                .map(Some),
            "Modelica.Math.Distributions.Normal.quantile" => self
                .lower_normal_quantile(args, scope, call_depth)
                .map(Some),
            "Modelica.Math.Distributions.Weibull.quantile" => self
                .lower_weibull_quantile(args, scope, call_depth)
                .map(Some),
            "Modelica.Math.Special.erfInv" => {
                let u = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let half = self.emit_const(0.5);
                let one = self.emit_const(1.0);
                let shifted = self.emit_binary(BinaryOp::Add, u, one);
                let p = self.emit_binary(BinaryOp::Mul, half, shifted);
                let inv = self.emit_standard_normal_quantile(p);
                let sqrt_two = self.emit_const(std::f64::consts::SQRT_2);
                Ok(Some(self.emit_binary(BinaryOp::Div, inv, sqrt_two)))
            }
            "Modelica.Math.Special.erfcInv" => {
                let u = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let half = self.emit_const(0.5);
                let two = self.emit_const(2.0);
                let shifted = self.emit_binary(BinaryOp::Sub, two, u);
                let p = self.emit_binary(BinaryOp::Mul, half, shifted);
                let inv = self.emit_standard_normal_quantile(p);
                let sqrt_two = self.emit_const(std::f64::consts::SQRT_2);
                Ok(Some(self.emit_binary(BinaryOp::Div, inv, sqrt_two)))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn try_lower_intrinsic_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        self.try_lower_intrinsic_function_call_key(name.as_str(), args, span, scope, call_depth)
    }

    pub(super) fn try_lower_intrinsic_function_call_key(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if let Some(reg) =
            self.lower_complex_operator_projection(call_name, args, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_complex_math_sum_projection(call_name, args, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if intrinsic_short_name(call_name) == "interval" {
            return self
                .lower_interval_intrinsic(args, scope, call_depth)
                .map(Some);
        }
        if call_name == rumoca_core::INTERNAL_SAMPLE_FUNCTION_NAME {
            return self
                .lower_builtin(
                    rumoca_core::BuiltinFunction::Sample,
                    args,
                    span,
                    scope,
                    call_depth,
                )
                .map(Some);
        }
        if self.current_update_target.is_some()
            && matches!(
                intrinsic_short_name(call_name),
                "subSample" | "superSample" | "shiftSample" | "backSample"
            )
            && let Some(reg) =
                self.lower_synchronous_value_intrinsic(call_name, args, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) = self.lower_clock_constructor_tick(
            intrinsic_short_name(call_name),
            args,
            scope,
            call_depth,
        )? {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_synchronous_value_intrinsic(call_name, args, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if is_stream_passthrough_intrinsic(call_name) {
            return args
                .first()
                .map(|arg| self.lower_expr(arg, scope, call_depth))
                .transpose();
        }
        if let Some(reg) = self.lower_runtime_string_special_intrinsic(call_name, args)? {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_external_table_intrinsic(call_name, args, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) = self.lower_impure_random_intrinsic(call_name, args, scope, call_depth)? {
            return Ok(Some(reg));
        }
        if let Some(reg) = self.lower_random_intrinsic(call_name, args, scope, call_depth)? {
            return Ok(Some(reg));
        }
        if let Some(builtin) = resolve_intrinsic_builtin(call_name) {
            let reg = self.lower_builtin(builtin, args, span, scope, call_depth)?;
            return Ok(Some(reg));
        }
        Ok(None)
    }

    fn lower_synchronous_value_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let short = intrinsic_short_name(call_name);
        match short {
            // MLS §16.4 / §16.5.1: previous(v) reads the value from the
            // previous clock tick. DAE lowering represents that as an explicit
            // `__pre__.*` parameter slot before Solve-IR lowering.
            "previous" => args
                .first()
                .map(|arg| self.lower_expr_in_mode(arg, scope, call_depth, ValueMode::Pre))
                .transpose(),
            // MLS §16.5.1: hold(v) exposes the held value on the
            // continuous-time partition. Before the first tick of the clock
            // associated with v, the output exposes the held source start
            // value, falling back to the target start when source metadata is
            // unavailable.
            "hold" => self.lower_hold_intrinsic(args, scope, call_depth).map(Some),
            // noClock(v) removes the clock without introducing a zero-order
            // hold startup value.
            "noClock" => args
                .first()
                .map(|arg| self.lower_expr(arg, scope, call_depth))
                .transpose(),
            "firstTick" => Ok(Some(self.lower_first_tick_intrinsic())),
            // Value-form sample-rate conversion helpers update on their
            // converted target clock and hold the current output at unrelated
            // event instants.
            "subSample" | "superSample" | "shiftSample" | "backSample" => self
                .lower_clocked_value_conversion(short, args, scope, call_depth)
                .map(Some),
            _ => Ok(None),
        }
    }

    fn lower_hold_intrinsic(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(value_expr) = args.first() else {
            return Ok(self.emit_const(0.0));
        };
        let value = self.lower_expr(value_expr, scope, call_depth)?;
        let Some(start_expr) = self
            .variable_start_expr(value_expr)
            .or_else(|| self.current_update_target_start_expr())
        else {
            return Ok(value);
        };
        let source_phase = self.lower_clock_phase_expr(value_expr, scope, call_depth)?;
        let time = self.emit_load_time();
        let tol = self.emit_const(1.0e-9);
        let first_source_boundary = self.emit_binary(BinaryOp::Sub, source_phase, tol);
        let before_first_source_tick =
            self.emit_compare(CompareOp::Lt, time, first_source_boundary);
        let start_value = self.lower_expr(&start_expr, scope, call_depth)?;
        let initial = self.lower_initial_builtin()?;
        let use_start_value = self.emit_binary(BinaryOp::Or, initial, before_first_source_tick);
        Ok(self.emit_select(use_start_value, start_value, value))
    }

    fn current_update_target_start_expr(&self) -> Option<rumoca_core::Expression> {
        let target = self.current_update_target?;
        self.variable_starts?.iter().find_map(|(name, start)| {
            (self.layout.binding(name.as_str()) == Some(target)).then(|| start.clone())
        })
    }

    fn lower_clocked_value_conversion(
        &mut self,
        short: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(value_expr) = args.first() else {
            return Ok(self.emit_const(0.0));
        };
        let value = self.lower_clocked_conversion_value(short, value_expr, scope, call_depth)?;
        let Some(target) = self.current_update_target else {
            return Ok(value);
        };
        let tick = self.lower_clock_constructor_tick(short, args, scope, call_depth)?;
        let Some(tick) = tick else {
            return Ok(value);
        };
        let held = self.emit_slot_load(target)?;
        Ok(self.emit_select(tick, value, held))
    }

    fn lower_clocked_conversion_value(
        &mut self,
        short: &str,
        value_expr: &rumoca_core::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let value = self.lower_expr(value_expr, scope, call_depth)?;
        if short != "backSample" {
            return Ok(value);
        }
        let Some(start_expr) = self.variable_start_expr(value_expr) else {
            return Ok(value);
        };
        let source_phase = self.lower_clock_phase_expr(value_expr, scope, call_depth)?;
        let time = self.emit_load_time();
        let tol = self.emit_const(1.0e-9);
        let first_source_boundary = self.emit_binary(BinaryOp::Sub, source_phase, tol);
        let before_first_source_tick =
            self.emit_compare(CompareOp::Lt, time, first_source_boundary);
        let start_value = self.lower_expr(&start_expr, scope, call_depth)?;
        Ok(self.emit_select(before_first_source_tick, start_value, value))
    }

    fn variable_start_expr(
        &self,
        value_expr: &rumoca_core::Expression,
    ) -> Option<rumoca_core::Expression> {
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = value_expr
        else {
            return None;
        };
        if !subscripts.is_empty() {
            return None;
        }
        self.variable_starts
            .and_then(|starts| starts.get(name.as_str()).cloned())
    }

    fn lower_first_tick_intrinsic(&mut self) -> Reg {
        if self.value_mode == ValueMode::Pre {
            return self.emit_const(0.0);
        }
        let time = self.emit_load_time();
        let abs_time = self.emit_unary(UnaryOp::Abs, time);
        let tol = self.emit_const(1.0e-12);
        self.emit_compare(CompareOp::Le, abs_time, tol)
    }

    pub(super) fn lower_runtime_string_special_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
    ) -> Result<Option<Reg>, LowerError> {
        // The compiled evaluator is numeric-only. Keep it aligned with the
        // runtime numeric evaluator for string/file helper calls instead of
        // treating those helpers as unsupported externals on the strict path.
        Ok(lower_runtime_string_special_value(call_name, args).map(|value| self.emit_const(value)))
    }

    fn lower_external_table_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        // MLS §12.2: standard-library pure functions are ordinary function calls.
        // The table helper family is lowered to host-backed scalar ops here so
        // compiled kernels stay on the compiled path instead of falling back to
        // runtime expression evaluation.
        let short = intrinsic_short_name(call_name);
        match short {
            "getTimeTableTmin" | "getTable1DAbscissaUmin" => {
                let table_id = self.lower_optional_arg(args, 0, scope, call_depth)?;
                Ok(Some(self.emit_table_bounds(table_id, false)))
            }
            "getTimeTableTmax" | "getTable1DAbscissaUmax" => {
                let table_id = self.lower_optional_arg(args, 0, scope, call_depth)?;
                Ok(Some(self.emit_table_bounds(table_id, true)))
            }
            "getTimeTableValueNoDer"
            | "getTimeTableValueNoDer2"
            | "getTimeTableValue"
            | "getTable1DValueNoDer"
            | "getTable1DValueNoDer2"
            | "getTable1DValue" => {
                let table_id = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let column = self.lower_optional_arg(args, 1, scope, call_depth)?;
                let input = self.lower_optional_arg(args, 2, scope, call_depth)?;
                Ok(Some(self.emit_table_lookup(table_id, column, input)))
            }
            "getNextTimeEvent" => {
                let table_id = self.lower_optional_arg(args, 0, scope, call_depth)?;
                let time = self.lower_optional_arg(args, 1, scope, call_depth)?;
                Ok(Some(self.emit_table_next_event(table_id, time)))
            }
            _ => Ok(None),
        }
    }

    pub(super) fn lower_complex_math_sum_projection(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(field) = parse_complex_sum_projection_field(call_name) else {
            return Ok(None);
        };
        let Some(arg) = args.first() else {
            return Ok(Some(self.emit_const(0.0)));
        };

        let values = self.lower_complex_projection_values(arg, field, scope, call_depth)?;
        let mut acc = self.emit_const(0.0);
        for value in values {
            acc = self.emit_binary(BinaryOp::Add, acc, value);
        }
        Ok(Some(acc))
    }

    fn lower_complex_operator_projection(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some((op, field)) = parse_complex_operator_projection(call_name) else {
            return Ok(None);
        };
        let lhs = args.first().ok_or_else(|| LowerError::InvalidFunction {
            name: call_name.to_string(),
            reason: "missing lhs for complex operator projection".to_string(),
        })?;
        let rhs = args.get(1).ok_or_else(|| LowerError::InvalidFunction {
            name: call_name.to_string(),
            reason: "missing rhs for complex operator projection".to_string(),
        })?;
        let (lhs_re, lhs_im) = self.lower_complex_operand_parts(lhs, scope, call_depth)?;
        let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, scope, call_depth)?;
        let (re, im) = match op {
            BinaryOp::Add => (
                self.emit_binary(BinaryOp::Add, lhs_re, rhs_re),
                self.emit_binary(BinaryOp::Add, lhs_im, rhs_im),
            ),
            BinaryOp::Sub => (
                self.emit_binary(BinaryOp::Sub, lhs_re, rhs_re),
                self.emit_binary(BinaryOp::Sub, lhs_im, rhs_im),
            ),
            BinaryOp::Mul => {
                let ac = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                let bd = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                let ad = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                let bc = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                (
                    self.emit_binary(BinaryOp::Sub, ac, bd),
                    self.emit_binary(BinaryOp::Add, ad, bc),
                )
            }
            BinaryOp::Div => {
                let rr2 = self.emit_binary(BinaryOp::Mul, rhs_re, rhs_re);
                let ri2 = self.emit_binary(BinaryOp::Mul, rhs_im, rhs_im);
                let denom = self.emit_binary(BinaryOp::Add, rr2, ri2);
                let lhs_rr = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_re);
                let lhs_ri = self.emit_binary(BinaryOp::Mul, lhs_re, rhs_im);
                let li_rr = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_re);
                let li_ri = self.emit_binary(BinaryOp::Mul, lhs_im, rhs_im);
                let re_num = self.emit_binary(BinaryOp::Add, lhs_rr, li_ri);
                let im_num = self.emit_binary(BinaryOp::Sub, li_rr, lhs_ri);
                (
                    self.emit_binary(BinaryOp::Div, re_num, denom),
                    self.emit_binary(BinaryOp::Div, im_num, denom),
                )
            }
            _ => return Ok(None),
        };
        Ok(Some(if field == "re" { re } else { im }))
    }

    fn lower_complex_projection_values(
        &mut self,
        expr: &rumoca_core::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        match expr {
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                let mut values = Vec::new();
                for element in elements {
                    values.extend(
                        self.lower_complex_projection_values(element, field, scope, call_depth)?,
                    );
                }
                Ok(values)
            }
            rumoca_core::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
                ..
            } => {
                let mut scope = scope.clone();
                let mut const_scope = IndexMap::<String, f64>::new();
                let mut values = Vec::new();
                let mut ctx = ComplexProjectionComprehensionCtx {
                    indices,
                    filter: filter.as_deref(),
                    field,
                    scope: &mut scope,
                    const_scope: &mut const_scope,
                    call_depth,
                };
                self.lower_complex_projection_comprehension_values(expr, 0, &mut ctx, &mut values)?;
                Ok(values)
            }
            _ => {
                let projected = rumoca_core::Expression::FieldAccess {
                    base: Box::new(expr.clone()),
                    field: field.to_string(),
                    span: rumoca_core::Span::DUMMY,
                };
                Ok(vec![self.lower_expr(&projected, scope, call_depth)?])
            }
        }
    }

    fn lower_complex_projection_comprehension_values(
        &mut self,
        expr: &rumoca_core::Expression,
        depth: usize,
        ctx: &mut ComplexProjectionComprehensionCtx<'_>,
        out: &mut Vec<Reg>,
    ) -> Result<(), LowerError> {
        if depth >= ctx.indices.len() {
            if let Some(filter_expr) = ctx.filter
                && self.eval_compile_time_expr(filter_expr, ctx.const_scope)? == 0.0
            {
                return Ok(());
            }
            out.extend(self.lower_complex_projection_values(
                expr,
                ctx.field,
                ctx.scope,
                ctx.call_depth,
            )?);
            return Ok(());
        }

        let iter = &ctx.indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, ctx.const_scope)?;
        for value in iter_values {
            let iter_reg = self.emit_const(value);
            ctx.scope.push_frame();
            ctx.scope
                .insert_scoped(ComponentPath::from_flat_path(&iter.name), iter_reg);
            ctx.const_scope.insert(iter.name.clone(), value);
            let result =
                self.lower_complex_projection_comprehension_values(expr, depth + 1, ctx, out);
            ctx.const_scope.shift_remove(&iter.name);
            ctx.scope.pop_frame();
            result?;
        }
        Ok(())
    }

    pub(super) fn bind_function_inputs(
        &mut self,
        function_name: &rumoca_core::Reference,
        inputs: &[rumoca_core::FunctionParam],
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<FunctionInputBindings, LowerError> {
        self.bind_function_inputs_for_name(
            function_name.as_str(),
            inputs,
            args,
            caller_scope,
            call_depth,
        )
    }

    pub(super) fn bind_function_inputs_for_name(
        &mut self,
        function_name: &str,
        inputs: &[rumoca_core::FunctionParam],
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<FunctionInputBindings, LowerError> {
        let (named_args, positional_args) =
            split_named_and_positional_call_args(function_name, args)?;
        let mut scope = Scope::new();
        let mut const_scope = self.local_const_bindings.clone();
        let mut const_bindings = IndexMap::new();
        let mut positional_idx = 0usize;

        for (input_idx, input) in inputs.iter().enumerate() {
            if let Some(arg_expr) = named_args.get(input.name.as_str()) {
                self.bind_function_input_arg_or_closure(
                    FunctionInputBindState {
                        scope: &mut scope,
                        const_scope: &mut const_scope,
                        const_bindings: &mut const_bindings,
                    },
                    function_name,
                    input,
                    arg_expr,
                    caller_scope,
                    call_depth + 1,
                )?;
                continue;
            }
            if self.bind_flattened_record_positional_input(
                FunctionInputBindState {
                    scope: &mut scope,
                    const_scope: &mut const_scope,
                    const_bindings: &mut const_bindings,
                },
                FlattenedRecordPositionalInputRequest {
                    function_name,
                    input,
                    inputs,
                    input_idx,
                    positional_args: &positional_args,
                    positional_idx: &mut positional_idx,
                    caller_scope,
                    call_depth: call_depth + 1,
                },
            )? {
                continue;
            }
            if positional_args.len() > inputs.len()
                && let Some(fields) = self.record_constructor_fields(&input.type_name)
                && self.bind_flattened_record_function_input(
                    FunctionInputBindState {
                        scope: &mut scope,
                        const_scope: &mut const_scope,
                        const_bindings: &mut const_bindings,
                    },
                    FlattenedRecordInputRequest {
                        input,
                        fields: &fields,
                        positional_args: &positional_args,
                        positional_idx: &mut positional_idx,
                        caller_scope,
                        call_depth: call_depth + 1,
                    },
                )?
            {
                continue;
            }
            if let Some(arg_expr) =
                next_positional_function_input_arg(input, &positional_args, &mut positional_idx)
            {
                self.bind_function_input_arg_or_closure(
                    FunctionInputBindState {
                        scope: &mut scope,
                        const_scope: &mut const_scope,
                        const_bindings: &mut const_bindings,
                    },
                    function_name,
                    input,
                    arg_expr,
                    caller_scope,
                    call_depth + 1,
                )?;
                continue;
            }
            if let Some(default) = input.default.as_ref() {
                let local_scope = scope.clone();
                self.bind_function_input_arg_or_closure(
                    FunctionInputBindState {
                        scope: &mut scope,
                        const_scope: &mut const_scope,
                        const_bindings: &mut const_bindings,
                    },
                    function_name,
                    input,
                    default,
                    &local_scope,
                    call_depth + 1,
                )?;
                continue;
            }
            if let Some(synthesized) = synthesize_missing_flattened_record_field_arg(
                input,
                inputs,
                input_idx,
                &positional_args,
                positional_idx,
            ) {
                self.bind_function_input_arg_or_closure(
                    FunctionInputBindState {
                        scope: &mut scope,
                        const_scope: &mut const_scope,
                        const_bindings: &mut const_bindings,
                    },
                    function_name,
                    input,
                    &synthesized,
                    caller_scope,
                    call_depth + 1,
                )?;
                continue;
            }

            return missing_required_function_input(function_name, &input.name);
        }

        Ok(FunctionInputBindings {
            scope,
            const_bindings,
        })
    }

    fn bind_flattened_record_positional_input(
        &mut self,
        state: FunctionInputBindState<'_>,
        request: FlattenedRecordPositionalInputRequest<'_, '_>,
    ) -> Result<bool, LowerError> {
        let Some((prefix, field)) = split_flattened_record_input_name(&request.input.name) else {
            return Ok(false);
        };
        let Some(arg_expr) = request.positional_args.get(*request.positional_idx) else {
            return Ok(false);
        };
        let projected = flattened_record_positional_projected_arg(
            arg_expr,
            prefix,
            field,
            request.inputs,
            request.input_idx,
        );
        let is_record_like =
            self.is_record_like_function_actual(arg_expr, field, request.caller_scope);
        let grouped = has_flattened_record_input_group(request.inputs, request.input_idx, prefix);
        let can_project = is_record_like
            || grouped
                && self
                    .projected_flattened_record_field_available(&projected, request.caller_scope);
        if !can_project {
            return Ok(false);
        }
        self.bind_function_input_arg_or_closure(
            state,
            request.function_name,
            request.input,
            &projected,
            request.caller_scope,
            request.call_depth,
        )?;
        self.bind_flattened_record_input_alias_shape(&request.input.name, prefix, field);
        if !request
            .inputs
            .iter()
            .skip(request.input_idx + 1)
            .any(|next| flattened_input_has_prefix(&next.name, prefix))
        {
            *request.positional_idx += 1;
        }
        Ok(true)
    }

    fn projected_flattened_record_field_available(
        &self,
        projected: &rumoca_core::Expression,
        caller_scope: &Scope,
    ) -> bool {
        if !self.infer_expr_dims(projected, caller_scope).is_empty() {
            return true;
        }
        let Ok(key) = self.compile_time_binding_base_key(projected) else {
            return false;
        };
        caller_scope.contains_key(&ComponentPath::from_flat_path(&key))
            || self.layout.binding(&key).is_some()
            || !indexed_entries_for_key(&self.indexed_bindings, &key).is_empty()
            || self.layout.bindings().keys().any(|candidate| {
                parse_indexed_binding_key(candidate).is_some_and(|(base, _)| base == key)
            })
            || self.local_indexed_bindings.contains_key(&key)
            || self.local_binding_dims.contains_key(&key)
    }

    fn bind_flattened_record_input_alias_shape(
        &mut self,
        flattened_name: &str,
        prefix: &str,
        field: &str,
    ) {
        let Some(dims) = self.local_binding_dims.get(flattened_name).cloned() else {
            return;
        };
        let alias = format!("{prefix}.{field}");
        let value_count = positive_dim_product(&dims);
        self.local_binding_dims.insert(alias.clone(), dims.clone());
        self.bind_local_array_shape(&alias, &dims, value_count);
    }

    fn bind_function_input_arg_or_closure(
        &mut self,
        state: FunctionInputBindState<'_>,
        function_name: &str,
        input: &rumoca_core::FunctionParam,
        arg_expr: &rumoca_core::Expression,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if self.bind_function_input_closure(function_name, input, arg_expr, caller_scope) {
            return Ok(());
        }
        self.bind_function_input_value(state, input, arg_expr, caller_scope, call_depth)?;
        Ok(())
    }

    fn is_record_like_function_actual(
        &self,
        expr: &rumoca_core::Expression,
        field: &str,
        caller_scope: &Scope,
    ) -> bool {
        match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => {
                if !subscripts.is_empty() {
                    return false;
                }
                let field_key = format!("{}.{field}", name.as_str());
                caller_scope.contains_key(&ComponentPath::from_flat_path(&field_key))
                    || self.layout.binding(&field_key).is_some()
                    || !indexed_entries_for_key(&self.indexed_bindings, &field_key).is_empty()
                    || self.local_indexed_bindings.contains_key(&field_key)
                    || self.local_binding_dims.contains_key(&field_key)
            }
            rumoca_core::Expression::FieldAccess {
                base,
                field: actual_field,
                ..
            } => field_access_binding_key(base, actual_field)
                .ok()
                .is_some_and(|base_key| {
                    let field_key = format!("{base_key}.{field}");
                    caller_scope.contains_key(&ComponentPath::from_flat_path(&field_key))
                        || self.layout.binding(&field_key).is_some()
                        || !indexed_entries_for_key(&self.indexed_bindings, &field_key).is_empty()
                        || self.local_indexed_bindings.contains_key(&field_key)
                        || self.local_binding_dims.contains_key(&field_key)
                }),
            rumoca_core::Expression::FunctionCall {
                name,
                is_constructor,
                ..
            } => {
                self.is_record_constructor_call(name, *is_constructor)
                    || self.lookup_function(name).is_some_and(|function| {
                        matches!(
                            function.outputs.as_slice(),
                            [output]
                                if output.type_class == Some(rumoca_core::ClassType::Record)
                        )
                    })
            }
            _ => false,
        }
    }

    fn bind_function_input_closure(
        &mut self,
        current_function_name: &str,
        input: &rumoca_core::FunctionParam,
        arg_expr: &rumoca_core::Expression,
        caller_scope: &Scope,
    ) -> bool {
        if input.type_class != Some(rumoca_core::ClassType::Function)
            && !input.type_name.to_ascii_lowercase().contains("function")
        {
            return false;
        }
        let Some(closure) = self.function_closure_from_arg(arg_expr, caller_scope) else {
            return false;
        };
        self.function_closures
            .insert(ComponentPath::from_flat_path(&input.name), closure.clone());
        self.function_closures.insert(
            ComponentPath::from_flat_path(&format!("{current_function_name}.{}", input.name)),
            closure,
        );
        true
    }

    fn function_closure_from_arg(
        &self,
        arg_expr: &rumoca_core::Expression,
        caller_scope: &Scope,
    ) -> Option<FunctionClosure> {
        match arg_expr {
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                ..
            } if self.lookup_function(name).is_some() => Some(FunctionClosure {
                target_name: name.clone(),
                bound_args: args.clone(),
                captured_scope: caller_scope.clone(),
            }),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.lookup_function_closure(name).cloned(),
            _ => None,
        }
    }

    pub(super) fn bind_function_closure_inputs(
        &mut self,
        function_name: &rumoca_core::Reference,
        inputs: &[rumoca_core::FunctionParam],
        open_args: &[rumoca_core::Expression],
        open_scope: &Scope,
        closure: &FunctionClosure,
        call_depth: usize,
    ) -> Result<FunctionInputBindings, LowerError> {
        let (bound_named_args, bound_positional_args) =
            split_named_and_positional_call_args(function_name.as_str(), &closure.bound_args)?;
        let mut scope = Scope::new();
        let mut const_scope = self.local_const_bindings.clone();
        let mut const_bindings = IndexMap::new();
        let mut open_idx = 0usize;
        let mut bound_idx = 0usize;

        for input in inputs {
            let (arg_expr, arg_scope) = if let Some(open_arg) = open_args.get(open_idx) {
                open_idx += 1;
                (open_arg, open_scope)
            } else if let Some(bound_arg) = bound_named_args.get(input.name.as_str()) {
                (*bound_arg, &closure.captured_scope)
            } else if let Some(bound_arg) = bound_positional_args.get(bound_idx) {
                bound_idx += 1;
                (*bound_arg, &closure.captured_scope)
            } else if let Some(default) = input.default.as_ref() {
                let local_scope = scope.clone();
                self.bind_function_input_value(
                    FunctionInputBindState {
                        scope: &mut scope,
                        const_scope: &mut const_scope,
                        const_bindings: &mut const_bindings,
                    },
                    input,
                    default,
                    &local_scope,
                    call_depth + 1,
                )?;
                continue;
            } else {
                return Err(LowerError::InvalidFunction {
                    name: function_name.as_str().to_string(),
                    reason: format!(
                        "required input `{}` has no actual argument or default binding",
                        input.name
                    ),
                });
            };

            self.bind_function_input_value(
                FunctionInputBindState {
                    scope: &mut scope,
                    const_scope: &mut const_scope,
                    const_bindings: &mut const_bindings,
                },
                input,
                arg_expr,
                arg_scope,
                call_depth + 1,
            )?;
        }

        Ok(FunctionInputBindings {
            scope,
            const_bindings,
        })
    }

    fn record_constructor_fields(
        &self,
        type_name: &str,
    ) -> Option<Vec<rumoca_core::FunctionParam>> {
        self.functions
            .iter()
            .find(|(name, function)| {
                function.is_constructor
                    && rumoca_core::qualified_type_name_matches(name.as_str(), type_name)
            })
            .map(|(_, function)| function.inputs.clone())
            .filter(|fields| !fields.is_empty())
    }

    fn bind_flattened_record_function_input(
        &mut self,
        state: FunctionInputBindState<'_>,
        req: FlattenedRecordInputRequest<'_, '_>,
    ) -> Result<bool, LowerError> {
        if req.input.type_class != Some(rumoca_core::ClassType::Record)
            || is_complex_param(req.input)
        {
            return Ok(false);
        }
        if req
            .positional_args
            .len()
            .saturating_sub(*req.positional_idx)
            < req.fields.len()
        {
            return Ok(false);
        }

        for field in req.fields {
            let Some(arg_expr) = req.positional_args.get(*req.positional_idx).copied() else {
                return Ok(false);
            };
            *req.positional_idx += 1;
            let local_name = format!("{}.{}", req.input.name, field.name);
            self.bind_flattened_record_field_value(
                FunctionInputBindState {
                    scope: &mut *state.scope,
                    const_scope: &mut *state.const_scope,
                    const_bindings: &mut *state.const_bindings,
                },
                &local_name,
                field,
                arg_expr,
                req.caller_scope,
                req.call_depth,
            )?;
            if !field.dims.is_empty() {
                skip_array_size_actuals(
                    arg_expr,
                    field.dims.len(),
                    req.positional_args,
                    req.positional_idx,
                );
            }
        }
        Ok(true)
    }

    fn bind_flattened_record_field_value(
        &mut self,
        state: FunctionInputBindState<'_>,
        local_name: &str,
        field: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if !field.dims.is_empty() {
            let values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            self.bind_assignment_values_with_dims(state.scope, local_name, &values, &field.dims);
            self.bind_local_array_shape(local_name, &field.dims, values.len());
            return Ok(());
        }

        let inferred_dims = self.infer_expr_dims(expr, expr_scope);
        if !inferred_dims.is_empty() {
            let values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            if bind_singleton_array_actual_to_scalar_formal(
                state.scope,
                local_name,
                &inferred_dims,
                &values,
            ) {
                return Ok(());
            }
            let dims = inferred_dims
                .iter()
                .map(|dim| *dim as i64)
                .collect::<Vec<_>>();
            self.bind_assignment_values_with_dims(state.scope, local_name, &values, &dims);
            self.bind_local_array_shape(local_name, &dims, values.len());
            return Ok(());
        }

        if let Some(reg) =
            self.lower_target_projected_scalar_actual(expr, expr_scope, call_depth)?
        {
            state
                .scope
                .insert(ComponentPath::from_flat_path(local_name), reg);
            return Ok(());
        }

        let reg = self.lower_expr(expr, expr_scope, call_depth)?;
        if let Ok(value) = self.eval_compile_time_expr(expr, state.const_scope) {
            state.const_scope.insert(local_name.to_string(), value);
            state.const_bindings.insert(local_name.to_string(), value);
        }
        state
            .scope
            .insert(ComponentPath::from_flat_path(local_name), reg);
        Ok(())
    }

    fn bind_record_constructor_function_input(
        &mut self,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        state: &mut FunctionInputBindState<'_>,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if input.type_class != Some(rumoca_core::ClassType::Record) || is_complex_param(input) {
            return Ok(false);
        }
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = expr
        else {
            return Ok(false);
        };
        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(false);
        }

        let fields = self
            .record_constructor_fields(name.as_str())
            .unwrap_or_default();
        let (named_args, positional_args) =
            split_named_and_positional_call_args(name.as_str(), args)?;
        let mut bound_any = false;
        for (field_name, arg_expr) in named_args
            .into_iter()
            .filter(|(_, arg_expr)| !non_numeric_record_metadata(arg_expr))
        {
            let local_name = format!("{}.{}", input.name, field_name);
            // If the constructor is registered (fields non-empty), a missing field
            // is an ICE — earlier phases must guarantee field names match.
            // If the constructor is not registered (fields empty), fall back to
            // a scalar Real param (best-effort binding for unregistered records).
            let field = record_constructor_field_or_real(name, &fields, &field_name);
            self.bind_flattened_record_field_value(
                FunctionInputBindState {
                    scope: &mut *state.scope,
                    const_scope: &mut *state.const_scope,
                    const_bindings: &mut *state.const_bindings,
                },
                &local_name,
                &field,
                arg_expr,
                expr_scope,
                call_depth,
            )?;
            bound_any = true;
        }

        if !positional_args.is_empty() && !fields.is_empty() {
            for (field, arg_expr) in fields
                .iter()
                .zip(positional_args)
                .filter(|(_, arg_expr)| !non_numeric_record_metadata(arg_expr))
            {
                let local_name = format!("{}.{}", input.name, field.name);
                self.bind_flattened_record_field_value(
                    FunctionInputBindState {
                        scope: &mut *state.scope,
                        const_scope: &mut *state.const_scope,
                        const_bindings: &mut *state.const_bindings,
                    },
                    &local_name,
                    field,
                    arg_expr,
                    expr_scope,
                    call_depth,
                )?;
                bound_any = true;
            }
        }

        Ok(bound_any)
    }

    fn bind_function_input_value(
        &mut self,
        mut state: FunctionInputBindState<'_>,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if self.bind_record_constructor_function_input(
            input, expr, expr_scope, &mut state, call_depth,
        )? {
            return Ok(());
        }
        if self.bind_record_function_call_input(input, expr, expr_scope, &mut state, call_depth)? {
            return Ok(());
        }
        if is_complex_param(input) {
            self.bind_complex_input(state.scope, input, expr, expr_scope, call_depth)?;
            return Ok(());
        }
        if !input.dims.is_empty() {
            let mut values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            let inferred_dims = self.infer_expr_dims(expr, expr_scope);
            let binding_dims =
                if input.dims.iter().any(|dim| *dim <= 0) && !inferred_dims.is_empty() {
                    inferred_dims
                        .iter()
                        .map(|dim| *dim as i64)
                        .collect::<Vec<_>>()
                } else {
                    input.dims.clone()
                };
            let value_count = positive_dim_product(&binding_dims);
            if values.len() > value_count {
                values.truncate(value_count);
            }
            self.bind_assignment_values_with_dims(state.scope, &input.name, &values, &binding_dims);
            self.bind_local_array_shape(&input.name, &binding_dims, values.len());
            return Ok(());
        }
        if let Some(reg) =
            self.lower_target_projected_scalar_actual(expr, expr_scope, call_depth)?
        {
            self.clear_local_array_metadata(&input.name);
            state
                .scope
                .insert(ComponentPath::from_flat_path(&input.name), reg);
            return Ok(());
        }
        let inferred_dims = self.infer_expr_dims(expr, expr_scope);
        if !inferred_dims.is_empty() {
            let values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            if split_flattened_record_input_name(&input.name).is_none()
                && bind_singleton_array_actual_to_scalar_formal(
                    state.scope,
                    &input.name,
                    &inferred_dims,
                    &values,
                )
            {
                return Ok(());
            }
            let dims = inferred_dims
                .iter()
                .map(|dim| *dim as i64)
                .collect::<Vec<_>>();
            self.bind_assignment_values_with_dims(state.scope, &input.name, &values, &dims);
            self.bind_local_array_shape(&input.name, &dims, values.len());
            return Ok(());
        }
        let reg = match self.lower_expr(expr, expr_scope, call_depth) {
            Ok(reg) => reg,
            Err(err) => {
                if err.is_missing_binding()
                    && self.bind_record_input_components(
                        state.scope,
                        &input.name,
                        expr,
                        expr_scope,
                    )?
                {
                    return Ok(());
                }
                return Err(err);
            }
        };
        if let Ok(value) = self.eval_compile_time_expr(expr, state.const_scope) {
            state.const_scope.insert(input.name.clone(), value);
            state.const_bindings.insert(input.name.clone(), value);
        }
        self.clear_local_array_metadata(&input.name);
        state
            .scope
            .insert(ComponentPath::from_flat_path(&input.name), reg);
        Ok(())
    }

    fn bind_record_input_components(
        &mut self,
        scope: &mut Scope,
        input_name: &str,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
    ) -> Result<bool, LowerError> {
        let Ok(base_key) = binding_base_key(expr) else {
            return Ok(false);
        };
        let base_path = ComponentPath::from_flat_path(&base_key);
        let input_path = ComponentPath::from_flat_path(input_name);
        let mut bound_any = false;
        for (key, reg) in expr_scope.iter() {
            if let Some(suffix) = key.strip_prefix(&base_path) {
                let local_path = input_path.join(&suffix);
                let local_key = local_path.to_flat_string();
                scope.insert(local_path, reg);
                self.copy_record_input_component_dims(key.as_str(), &local_key);
                bound_any = true;
            }
        }

        let prefix = format!("{base_key}.");
        let slots = self
            .layout
            .bindings()
            .iter()
            .filter_map(|(key, slot)| {
                key.strip_prefix(prefix.as_str())
                    .map(|suffix| (key.clone(), suffix.to_string(), *slot))
            })
            .collect::<Vec<_>>();
        for (key, suffix, slot) in slots {
            let local_key = format!("{input_name}.{suffix}");
            let local_path = ComponentPath::from_flat_path(&local_key);
            if scope.contains_key(&local_path) {
                continue;
            }
            let slot = self.pre_mode_slot_for_key(&key).unwrap_or(slot);
            let reg = self.emit_slot_load(slot)?;
            scope.insert(local_path, reg);
            self.copy_record_input_component_dims(&key, &local_key);
            bound_any = true;
        }

        Ok(bound_any)
    }

    fn bind_record_function_call_input(
        &mut self,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        state: &mut FunctionInputBindState<'_>,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if input.type_class != Some(rumoca_core::ClassType::Record) || is_complex_param(input) {
            return Ok(false);
        }
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor: false,
            ..
        } = expr
        else {
            return Ok(false);
        };
        let Some(function) = self.lookup_function(name).cloned() else {
            return Ok(false);
        };
        if function.external.is_some() {
            return Ok(false);
        }
        let [output] = function.outputs.as_slice() else {
            return Ok(false);
        };
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            return Ok(false);
        }
        self.ensure_pure_inline_function(name.as_str(), &function)?;

        let Some(materialized) = self.materialize_single_record_function_call_components(
            name, args, expr_scope, call_depth,
        )?
        else {
            return Ok(false);
        };
        if materialized.components.is_empty() && materialized.indexed_components.is_empty() {
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: "record output had no assigned components".to_string(),
            });
        }

        for component in materialized.components {
            let local_key = format!("{}.{}", input.name, component.suffix);
            state
                .scope
                .insert(ComponentPath::from_flat_path(&local_key), component.reg);
            if let Some(dims) = component.dims {
                self.local_binding_dims.insert(local_key.clone(), dims);
                self.update_known_empty_local_array(&local_key, component.known_empty);
            }
        }
        for (suffix, bindings) in materialized.indexed_components {
            self.local_indexed_bindings
                .insert(format!("{}.{}", input.name, suffix), bindings);
        }
        Ok(true)
    }

    fn update_known_empty_local_array(&mut self, key: &str, known_empty: bool) {
        if known_empty {
            self.known_empty_local_arrays.insert(key.to_string());
        } else {
            self.known_empty_local_arrays.shift_remove(key);
        }
    }

    pub(super) fn materialize_single_record_function_call_components(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<MaterializedRecordComponents>, LowerError> {
        if call_depth >= MAX_FUNCTION_INLINE_DEPTH {
            return Ok(None);
        }
        let Some(function) = self.lookup_function(name).cloned() else {
            return Ok(None);
        };
        if function.external.is_some() {
            return Ok(None);
        }
        let [output] = function.outputs.as_slice() else {
            return Ok(None);
        };
        if output.type_class != Some(rumoca_core::ClassType::Record) {
            return Ok(None);
        }
        self.ensure_pure_inline_function(name.as_str(), &function)?;

        let output_name = output.name.clone();
        self.with_local_lower_frame(|this| {
            let bindings =
                this.bind_function_inputs(name, &function.inputs, args, caller_scope, call_depth)?;
            let mut function_scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut function_scope, call_depth)?;
            let _returned =
                this.lower_statements(&function.body, &mut function_scope, call_depth + 1)?;
            Ok(Some(this.materialized_record_components_from_scope(
                &output_name,
                &function_scope,
            )))
        })
    }

    fn materialized_record_components_from_scope(
        &self,
        output_name: &str,
        scope: &Scope,
    ) -> MaterializedRecordComponents {
        let source_prefix = format!("{output_name}.");
        let components = scope
            .iter()
            .into_iter()
            .filter_map(|(key, reg)| {
                let key_text = key.as_str();
                key_text.strip_prefix(source_prefix.as_str()).map(|suffix| {
                    MaterializedRecordComponent {
                        suffix: suffix.to_string(),
                        reg,
                        dims: self.local_binding_dims.get(key_text).cloned(),
                        known_empty: self.known_empty_local_arrays.contains(key_text),
                    }
                })
            })
            .collect::<Vec<_>>();
        let indexed_components = self
            .local_indexed_bindings
            .iter()
            .filter_map(|(key, bindings)| {
                key.strip_prefix(source_prefix.as_str())
                    .map(|suffix| (suffix.to_string(), bindings.clone()))
            })
            .collect::<Vec<_>>();
        MaterializedRecordComponents {
            components,
            indexed_components,
        }
    }

    fn copy_record_input_component_dims(&mut self, source_key: &str, local_key: &str) {
        if let Some(dims) = self.local_binding_dims.get(source_key).cloned() {
            self.local_binding_dims.insert(local_key.to_string(), dims);
            let value_count = positive_dim_product(
                self.local_binding_dims
                    .get(local_key)
                    .map(Vec::as_slice)
                    .unwrap_or(&[]),
            );
            if let Some(dims) = self.local_binding_dims.get(local_key).cloned() {
                self.bind_local_array_shape(local_key, &dims, value_count);
            }
            if self.known_empty_local_arrays.contains(source_key) {
                self.known_empty_local_arrays.insert(local_key.to_string());
            } else {
                self.known_empty_local_arrays.shift_remove(local_key);
            }
            return;
        }
        if let Some(shape) = self.layout.shape(source_key) {
            let dims = shape.iter().map(|dim| *dim as i64).collect::<Vec<_>>();
            let value_count = shape.iter().copied().product::<usize>().max(1);
            self.local_binding_dims
                .insert(local_key.to_string(), dims.clone());
            self.bind_local_array_shape(local_key, &dims, value_count);
            self.known_empty_local_arrays.shift_remove(local_key);
        }
        self.copy_record_indexed_component_base_dims(source_key, local_key);
    }

    fn copy_record_indexed_component_base_dims(&mut self, source_key: &str, local_key: &str) {
        let Some((source_base, _)) = parse_indexed_binding_key(source_key) else {
            return;
        };
        let Some((local_base, _)) = parse_indexed_binding_key(local_key) else {
            return;
        };
        if self.local_binding_dims.contains_key(&local_base) {
            return;
        }
        let entries = indexed_entries_for_key(&self.indexed_bindings, &source_base);
        if entries.is_empty() {
            return;
        }
        let rank = entries
            .iter()
            .map(|entry| entry.indices.len())
            .max()
            .unwrap_or(0);
        if rank == 0 {
            return;
        }
        let mut dims = vec![0_i64; rank];
        for entry in entries {
            for (idx, value) in entry.indices.iter().copied().enumerate() {
                if let Ok(value) = i64::try_from(value) {
                    dims[idx] = dims[idx].max(value);
                }
            }
        }
        let value_count = positive_dim_product(&dims);
        self.local_binding_dims
            .insert(local_base.clone(), dims.clone());
        self.bind_local_array_shape(&local_base, &dims, value_count);
    }

    pub(super) fn with_local_lower_frame<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, LowerError>,
    ) -> Result<T, LowerError> {
        let frame = LocalLowerFrame {
            structural_bindings: self.structural_bindings.clone(),
            local_indexed_bindings: self.local_indexed_bindings.clone(),
            local_binding_dims: self.local_binding_dims.clone(),
            known_empty_local_arrays: self.known_empty_local_arrays.clone(),
            local_const_bindings: self.local_const_bindings.clone(),
            function_closures: self.function_closures.clone(),
        };
        let result = f(self);
        self.structural_bindings = frame.structural_bindings;
        self.local_indexed_bindings = frame.local_indexed_bindings;
        self.local_binding_dims = frame.local_binding_dims;
        self.known_empty_local_arrays = frame.known_empty_local_arrays;
        self.local_const_bindings = frame.local_const_bindings;
        self.function_closures = frame.function_closures;
        result
    }

    pub(super) fn initialize_function_output_scope(
        &mut self,
        function: &rumoca_core::Function,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        for param in function.outputs.iter().chain(function.locals.iter()) {
            if param.default.is_some() {
                let values = self.initial_function_param_values(param, scope, call_depth)?;
                self.bind_assignment_values_with_dims(scope, &param.name, &values, &param.dims);
                self.bind_local_array_shape(&param.name, &param.dims, values.len());
                self.bind_initial_function_param_const(param);
                continue;
            }
            // A function output/local that shadows a caller variable of the
            // same name (e.g. a library `product` whose output is `X` called
            // from a model whose state is also `X`) must start fresh. Drop any
            // inherited caller bindings for this name first; otherwise stale
            // caller elements (e.g. `X[11..13]` of a larger caller array) leak
            // into reads of this function's array output.
            if !param.dims.is_empty() {
                super::function_projection::clear_indexed_scope_bindings(scope, &param.name);
                scope.shift_remove(&ComponentPath::from_flat_path(&param.name));
                self.local_indexed_bindings
                    .shift_remove(param.name.as_str());
            }
            self.initialize_declared_function_param(param);
        }
        Ok(())
    }

    fn initialize_declared_function_param(&mut self, param: &rumoca_core::FunctionParam) {
        if param.dims.is_empty() {
            self.clear_local_array_metadata(&param.name);
            return;
        }
        self.bind_declared_function_param_shape(param);
    }

    pub(super) fn ensure_pure_inline_function(
        &self,
        function_name: &str,
        function: &rumoca_core::Function,
    ) -> Result<(), LowerError> {
        if function.pure {
            return Ok(());
        }
        Err(LowerError::Unsupported {
            reason: format!(
                "impure function call `{function_name}` cannot be lowered as a pure algebraic expression"
            ),
        })
    }

    pub(super) fn bind_initial_function_param_const(&mut self, param: &rumoca_core::FunctionParam) {
        if !param.dims.is_empty() {
            return;
        }
        let Some(default) = param.default.as_ref() else {
            return;
        };
        if let Ok(value) = self.eval_compile_time_expr(default, &self.local_const_bindings) {
            self.local_const_bindings.insert(param.name.clone(), value);
        }
    }

    pub(super) fn bind_local_array_shape(&mut self, name: &str, dims: &[i64], value_count: usize) {
        let resolved_dims = resolve_array_dims_for_value_count(dims, value_count);
        for (idx, dim) in resolved_dims.iter().enumerate() {
            let dim = if *dim > 0 {
                *dim as f64
            } else if idx == 0 {
                value_count as f64
            } else {
                0.0
            };
            self.structural_bindings
                .insert(super::size_binding_key(name, idx + 1), dim);
        }
    }

    pub(super) fn clear_local_array_metadata(&mut self, name: &str) {
        self.local_binding_dims.shift_remove(name);
        self.local_indexed_bindings.shift_remove(name);
        self.known_empty_local_arrays.shift_remove(name);
        let prefix = format!("{}{}.", super::SIZE_BINDING_PREFIX, name);
        let keys = self
            .structural_bindings
            .keys()
            .filter(|key| key.starts_with(prefix.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        for key in keys {
            self.structural_bindings.shift_remove(key.as_str());
        }
    }

    pub(super) fn set_known_local_array_dims(
        &mut self,
        name: &str,
        dims: Vec<i64>,
        value_count: usize,
    ) {
        let is_empty = dims.contains(&0);
        self.local_binding_dims
            .insert(name.to_string(), dims.clone());
        self.bind_local_array_shape(name, &dims, value_count);
        if is_empty {
            self.known_empty_local_arrays.insert(name.to_string());
        } else {
            self.known_empty_local_arrays.shift_remove(name);
        }
    }

    pub(super) fn bind_declared_function_param_shape(
        &mut self,
        param: &rumoca_core::FunctionParam,
    ) {
        if param.dims.is_empty() {
            return;
        }
        if let Some(dims) = self.resolve_function_param_shape(param) {
            self.set_known_local_array_dims(
                &param.name,
                dims.clone(),
                exact_dim_value_count(&dims),
            );
            return;
        }
        if param.dims.contains(&0) && param.shape_expr.is_empty() {
            self.set_known_local_array_dims(&param.name, param.dims.clone(), 0);
            return;
        }
        if param.dims.iter().any(|dim| *dim <= 0) {
            self.known_empty_local_arrays
                .shift_remove(param.name.as_str());
            return;
        }

        let value_count = dims_scalar_count(&param.dims);
        self.set_known_local_array_dims(&param.name, param.dims.clone(), value_count);
    }

    pub(in crate::lower) fn resolve_function_param_shape(
        &self,
        param: &rumoca_core::FunctionParam,
    ) -> Option<Vec<i64>> {
        if param.shape_expr.len() != param.dims.len() {
            return None;
        }
        param
            .shape_expr
            .iter()
            .map(|subscript| match subscript {
                rumoca_core::Subscript::Index { value, .. } if *value >= 0 => Some(*value),
                rumoca_core::Subscript::Expr { expr, .. } => self
                    .eval_compile_time_int(expr, &self.local_const_bindings, "function shape")
                    .ok()
                    .filter(|dim| *dim >= 0),
                rumoca_core::Subscript::Colon { .. } | rumoca_core::Subscript::Index { .. } => None,
            })
            .collect()
    }

    fn bind_complex_input(
        &mut self,
        scope: &mut Scope,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if input.dims.is_empty() {
            let (re, im) = self.lower_complex_operand_parts(expr, expr_scope, call_depth)?;
            self.bind_complex_scalar_components(scope, &input.name, re, im);
            return Ok(());
        }

        if let rumoca_core::Expression::FieldAccess { field, .. } = expr
            && matches!(field.as_str(), "re" | "im")
        {
            let component_values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            let zeros = (0..component_values.len())
                .map(|_| self.emit_const(0.0))
                .collect::<Vec<_>>();
            if field == "re" {
                self.bind_complex_component_values(scope, &input.name, &component_values, &zeros);
            } else {
                self.bind_complex_component_values(scope, &input.name, &zeros, &component_values);
            }
            return Ok(());
        }

        let re_expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: "re".to_string(),
            span: rumoca_core::Span::DUMMY,
        };
        let im_expr = rumoca_core::Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: "im".to_string(),
            span: rumoca_core::Span::DUMMY,
        };

        let re_values = self.lower_array_like_values(&re_expr, expr_scope, call_depth)?;
        let im_values = self.lower_array_like_values(&im_expr, expr_scope, call_depth)?;
        self.bind_complex_component_values(scope, &input.name, &re_values, &im_values);
        Ok(())
    }

    fn bind_complex_scalar_components(
        &mut self,
        scope: &mut Scope,
        base_name: &str,
        re: Reg,
        im: Reg,
    ) {
        // MLS §3.7.2 and §12.6: Complex is a record with Real fields `re`
        // and `im`; scalar function inputs must bind those field components
        // whether the caller passed a record expression or an already-projected
        // component reference.
        scope.insert(ComponentPath::from_flat_path(base_name), re);
        scope.insert(
            ComponentPath::from_flat_path(&format!("{base_name}.re")),
            re,
        );
        scope.insert(
            ComponentPath::from_flat_path(&format!("{base_name}.im")),
            im,
        );
    }

    fn bind_complex_component_values(
        &mut self,
        scope: &mut Scope,
        base_name: &str,
        re_values: &[Reg],
        im_values: &[Reg],
    ) {
        let width = re_values.len().max(im_values.len());
        let zero_values = (0..width).map(|_| self.emit_const(0.0)).collect::<Vec<_>>();
        let first = re_values
            .first()
            .copied()
            .unwrap_or_else(|| self.emit_const(0.0));
        scope.insert(ComponentPath::from_flat_path(base_name), first);
        if let Some(re) = re_values.first().copied() {
            scope.insert(
                ComponentPath::from_flat_path(&format!("{base_name}.re")),
                re,
            );
        }
        if let Some(im) = im_values.first().copied() {
            scope.insert(
                ComponentPath::from_flat_path(&format!("{base_name}.im")),
                im,
            );
        }
        self.bind_assignment_values(scope, &format!("{base_name}[:].re"), re_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].im"), im_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].re.re"), re_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].re.im"), &zero_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].im.re"), &zero_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].im.im"), im_values);
        for (idx, reg) in re_values.iter().copied().enumerate() {
            scope.insert(
                ComponentPath::from_flat_path(&format!("{base_name}[{}].re", idx + 1)),
                reg,
            );
        }
        for (idx, reg) in im_values.iter().copied().enumerate() {
            scope.insert(
                ComponentPath::from_flat_path(&format!("{base_name}[{}].im", idx + 1)),
                reg,
            );
        }
    }
}

fn positive_dim_product(dims: &[i64]) -> usize {
    dims.iter()
        .filter_map(|dim| usize::try_from(*dim).ok())
        .filter(|dim| *dim > 0)
        .product::<usize>()
        .max(1)
}

fn record_constructor_field_or_real(
    _name: &rumoca_core::Reference,
    fields: &[rumoca_core::FunctionParam],
    field_name: &str,
) -> rumoca_core::FunctionParam {
    if fields.is_empty() {
        return rumoca_core::FunctionParam::new(field_name, "Real");
    }
    fields
        .iter()
        .find(|field| field.name == field_name)
        .cloned()
        .expect("registered record constructor must contain requested field")
}
