// SPEC_0021 file-size exception: function-call lowering still handles builtin,
// record, random, and projection calls together. split plan: move builtin
// families and projection-specific lowering into focused modules.

use super::function_projection::FunctionOutputProjection;
use super::*;
use rumoca_ir_solve::RandomGenerator;
mod complex_inputs;
mod helpers;
mod random;
mod runtime_intrinsics;
use helpers::{
    ComplexProjectionComprehensionCtx, FlattenedRecordInputRequest,
    FlattenedRecordPositionalInputRequest, FunctionInputRequest, NamedOrPositionalArg,
    append_complex_projection_values, checked_usize_dims_to_i64,
    complex_projection_vec_with_capacity, flattened_input_has_prefix, function_input_actual_dim,
    missing_intrinsic_argument, missing_required_function_input, record_constructor_field,
    split_flattened_record_input_name, synthesize_missing_flattened_record_field_arg,
    validate_complex_component_width,
};
pub(super) use runtime_intrinsics::*;

pub(super) fn is_flattened_record_field_actual(
    expr: &rumoca_core::Expression,
    field: &str,
) -> bool {
    match expr {
        rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } if subscripts.is_empty() => {
            name.as_str().ends_with(&format!(".{field}"))
                || name.as_str().ends_with(&format!("_{field}"))
        }
        rumoca_core::Expression::FieldAccess {
            field: actual_field,
            ..
        } => actual_field == field,
        _ => false,
    }
}

pub(super) fn referenced_function_input_names(
    function: &rumoca_core::Function,
) -> IndexSet<String> {
    let input_names: IndexSet<&str> = function
        .inputs
        .iter()
        .map(|input| input.name.as_str())
        .collect();
    let mut used = IndexSet::new();
    for statement in &function.body {
        collect_statement_var_refs(statement, &input_names, &mut used);
    }
    used
}

fn collect_statement_var_refs(
    statement: &rumoca_core::Statement,
    input_names: &IndexSet<&str>,
    used: &mut IndexSet<String>,
) {
    match statement {
        rumoca_core::Statement::Assignment { value, .. } => {
            collect_expression_var_refs(value, input_names, used);
        }
        rumoca_core::Statement::If {
            cond_blocks,
            else_block,
            ..
        } => {
            for block in cond_blocks {
                collect_expression_var_refs(&block.cond, input_names, used);
                for statement in &block.stmts {
                    collect_statement_var_refs(statement, input_names, used);
                }
            }
            if let Some(else_block) = else_block {
                for statement in else_block {
                    collect_statement_var_refs(statement, input_names, used);
                }
            }
        }
        rumoca_core::Statement::For {
            indices, equations, ..
        } => {
            for index in indices {
                collect_expression_var_refs(&index.range, input_names, used);
            }
            for statement in equations {
                collect_statement_var_refs(statement, input_names, used);
            }
        }
        rumoca_core::Statement::While { block, .. } => {
            collect_expression_var_refs(&block.cond, input_names, used);
            for statement in &block.stmts {
                collect_statement_var_refs(statement, input_names, used);
            }
        }
        rumoca_core::Statement::When { blocks, .. } => {
            for block in blocks {
                collect_expression_var_refs(&block.cond, input_names, used);
                for statement in &block.stmts {
                    collect_statement_var_refs(statement, input_names, used);
                }
            }
        }
        rumoca_core::Statement::FunctionCall { args, .. } => {
            for arg in args {
                collect_expression_var_refs(arg, input_names, used);
            }
        }
        rumoca_core::Statement::Reinit { value, .. } => {
            collect_expression_var_refs(value, input_names, used);
        }
        rumoca_core::Statement::Assert {
            condition,
            message,
            level,
            ..
        } => {
            collect_expression_var_refs(condition, input_names, used);
            collect_expression_var_refs(message, input_names, used);
            if let Some(level) = level {
                collect_expression_var_refs(level, input_names, used);
            }
        }
        rumoca_core::Statement::Empty { .. }
        | rumoca_core::Statement::Return { .. }
        | rumoca_core::Statement::Break { .. } => {}
    }
}

fn collect_expression_var_refs(
    expr: &rumoca_core::Expression,
    input_names: &IndexSet<&str>,
    used: &mut IndexSet<String>,
) {
    match expr {
        rumoca_core::Expression::VarRef { name, .. } => {
            if input_names.contains(name.as_str()) {
                used.insert(name.as_str().to_string());
            } else {
                let flattened_name = name.as_str().replace('.', "_");
                if input_names.contains(flattened_name.as_str()) {
                    used.insert(flattened_name);
                }
            }
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            collect_expression_var_refs(rhs, input_names, used);
        }
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            collect_expression_var_refs(lhs, input_names, used);
            collect_expression_var_refs(rhs, input_names, used);
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_expression_var_refs(condition, input_names, used);
                collect_expression_var_refs(value, input_names, used);
            }
            collect_expression_var_refs(else_branch, input_names, used);
        }
        rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. }
        | rumoca_core::Expression::Array { elements: args, .. } => {
            for arg in args {
                collect_expression_var_refs(arg, input_names, used);
            }
        }
        rumoca_core::Expression::FieldAccess { base, field, .. } => {
            if let rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } = base.as_ref()
                && subscripts.is_empty()
            {
                let flattened_name = format!("{}_{}", name.as_str(), field);
                if input_names.contains(flattened_name.as_str()) {
                    used.insert(flattened_name);
                }
            }
            collect_expression_var_refs(base, input_names, used);
        }
        rumoca_core::Expression::Index { base, .. } => {
            collect_expression_var_refs(base, input_names, used);
        }
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => {
            collect_expression_var_refs(start, input_names, used);
            if let Some(step) = step {
                collect_expression_var_refs(step, input_names, used);
            }
            collect_expression_var_refs(end, input_names, used);
        }
        rumoca_core::Expression::ArrayComprehension {
            expr,
            indices,
            filter,
            ..
        } => {
            collect_expression_var_refs(expr, input_names, used);
            for index in indices {
                collect_expression_var_refs(&index.range, input_names, used);
            }
            if let Some(filter) = filter {
                collect_expression_var_refs(filter, input_names, used);
            }
        }
        rumoca_core::Expression::Literal { .. } | rumoca_core::Expression::Empty { .. } => {}
    }
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_qualified_standard_numeric_intrinsic(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        match name.as_str() {
            // MLS §12.4: a function call is an expression. These qualified
            // Modelica Standard Library scalar math functions are rendered as
            // solve-IR expressions instead of inlining their function bodies,
            // so local helper arrays in the library function remain local and
            // cannot leak into the runtime variable layout.
            "Modelica.ComplexMath.real" => {
                let arg = args
                    .first()
                    .ok_or_else(|| missing_intrinsic_argument(name.as_str(), "argument 1", span))?;
                self.lower_expr(arg, scope, call_depth).map(Some)
            }
            "Modelica.ComplexMath.imag" => {
                let arg = args
                    .get(1)
                    .or_else(|| args.first())
                    .ok_or_else(|| missing_intrinsic_argument(name.as_str(), "argument 1", span))?;
                self.lower_expr(arg, scope, call_depth).map(Some)
            }
            "Modelica.Math.Distributions.Uniform.quantile" => self
                .lower_uniform_quantile(args, span, scope, call_depth)
                .map(Some),
            "Modelica.Math.Distributions.Normal.quantile" => self
                .lower_normal_quantile(args, span, scope, call_depth)
                .map(Some),
            "Modelica.Math.Distributions.Weibull.quantile" => self
                .lower_weibull_quantile(args, span, scope, call_depth)
                .map(Some),
            "Modelica.Math.Special.erfInv" => {
                let u = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
                let half = self.emit_const_at(0.5, span)?;
                let one = self.emit_const_at(1.0, span)?;
                let shifted = self.emit_binary_at(BinaryOp::Add, u, one, span)?;
                let p = self.emit_binary_at(BinaryOp::Mul, half, shifted, span)?;
                let inv = self.emit_standard_normal_quantile(p, span)?;
                let sqrt_two = self.emit_const_at(std::f64::consts::SQRT_2, span)?;
                self.emit_binary_at(BinaryOp::Div, inv, sqrt_two, span)
                    .map(Some)
            }
            "Modelica.Math.Special.erfcInv" => {
                let u = self.lower_optional_arg(args, 0, span, scope, call_depth)?;
                let half = self.emit_const_at(0.5, span)?;
                let two = self.emit_const_at(2.0, span)?;
                let shifted = self.emit_binary_at(BinaryOp::Sub, two, u, span)?;
                let p = self.emit_binary_at(BinaryOp::Mul, half, shifted, span)?;
                let inv = self.emit_standard_normal_quantile(p, span)?;
                let sqrt_two = self.emit_const_at(std::f64::consts::SQRT_2, span)?;
                self.emit_binary_at(BinaryOp::Div, inv, sqrt_two, span)
                    .map(Some)
            }
            _ => Ok(None),
        }
    }

    pub(super) fn lower_intrinsic_function_call(
        &mut self,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        self.lower_intrinsic_function_call_key(name.as_str(), args, span, scope, call_depth)
    }

    pub(super) fn lower_intrinsic_function_call_key(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        if let Some(reg) =
            self.lower_complex_operator_projection(call_name, args, span, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_complex_math_sum_projection(call_name, args, span, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if intrinsic_short_name(call_name) == "interval" {
            return self
                .lower_interval_intrinsic(args, span, scope, call_depth)
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
                self.lower_synchronous_value_intrinsic(call_name, args, span, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) = self.lower_clock_constructor_tick(
            intrinsic_short_name(call_name),
            args,
            span,
            scope,
            call_depth,
        )? {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_synchronous_value_intrinsic(call_name, args, span, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if is_stream_passthrough_intrinsic(call_name) {
            return args
                .first()
                .map(|arg| self.lower_expr(arg, scope, call_depth))
                .transpose();
        }
        if let Some(reg) = self.lower_runtime_string_special_intrinsic(call_name, args, span)? {
            return Ok(Some(reg));
        }
        if let Some(reg) = self.lower_buildings_energyplus_external_intrinsic(
            call_name, args, span, scope, call_depth,
        )? {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_external_table_intrinsic(call_name, args, span, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_impure_random_intrinsic(call_name, args, span, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(reg) = self.lower_random_intrinsic(call_name, args, span, scope, call_depth)? {
            return Ok(Some(reg));
        }
        if let Some(builtin) = resolve_intrinsic_builtin(call_name) {
            let reg = self.lower_builtin(builtin, args, span, scope, call_depth)?;
            return Ok(Some(reg));
        }
        Ok(None)
    }

    fn lower_buildings_energyplus_external_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(kind) = buildings_energyplus_external_kind(call_name) else {
            return Ok(None);
        };
        let (named_args, positional_args) = split_named_and_positional_call_args(call_name, args)?;
        let mut scalar_args = Vec::new();
        if kind == rumoca_ir_solve::ExternalFunctionKind::BuildingsEnergyPlusInitialize
            && let Some(expr) = named_args
                .get("isSynchronized")
                .copied()
                .or_else(|| positional_args.last().copied())
        {
            scalar_args.push(self.lower_expr(expr, scope, call_depth)?);
        }
        self.emit_external_call(kind, &scalar_args, 0, span)
            .map(Some)
    }

    fn lower_synchronous_value_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let short = intrinsic_short_name(call_name);
        match short {
            // MLS §16.4 / §16.5.1: previous(v) reads the value from the
            // previous clock tick. DAE lowering represents that as an explicit
            // `__pre__.*` parameter slot before Solve-IR lowering.
            "previous" => {
                let Some(arg) = args.first() else {
                    return Err(missing_intrinsic_argument("previous", "argument 1", span));
                };
                self.lower_expr_in_mode(arg, scope, call_depth, ValueMode::Pre)
                    .map(Some)
            }
            // MLS §16.5.1: hold(v) exposes the held value on the
            // continuous-time partition. Before the first tick of the clock
            // associated with v, the output exposes the held source start
            // value, falling back to the target start when source metadata is
            // unavailable.
            "hold" => self
                .lower_hold_intrinsic(args, span, scope, call_depth)
                .map(Some),
            // noClock(v) removes the clock without introducing a zero-order
            // hold startup value.
            "noClock" => {
                let Some(arg) = args.first() else {
                    return Err(missing_intrinsic_argument("noClock", "argument 1", span));
                };
                self.lower_expr(arg, scope, call_depth).map(Some)
            }
            "firstTick" => self.lower_first_tick_intrinsic(span).map(Some),
            // Value-form sample-rate conversion helpers update on their
            // converted target clock and hold the current output at unrelated
            // event instants.
            "subSample" | "superSample" | "shiftSample" | "backSample" => self
                .lower_clocked_value_conversion(short, args, span, scope, call_depth)
                .map(Some),
            _ => Ok(None),
        }
    }

    fn lower_hold_intrinsic(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(value_expr) = args.first() else {
            return Err(missing_intrinsic_argument("hold", "argument 1", call_span));
        };
        let span = value_expr.span().unwrap_or(call_span);
        let value = self.lower_expr(value_expr, scope, call_depth)?;
        let Some(start_expr) = self
            .variable_start_expr(value_expr)
            .or_else(|| self.current_update_target_start_expr())
        else {
            return Ok(value);
        };
        let source_phase = self.lower_clock_phase_expr(value_expr, scope, call_depth)?;
        let time = self.emit_load_time_at(span)?;
        let tol = self.emit_const_at(1.0e-9, span)?;
        let first_source_boundary = self.emit_binary_at(BinaryOp::Sub, source_phase, tol, span)?;
        let before_first_source_tick =
            self.emit_compare_at(CompareOp::Lt, time, first_source_boundary, span)?;
        let start_value = self.lower_expr(&start_expr, scope, call_depth)?;
        let initial = self.lower_initial_builtin(call_span)?;
        let use_start_value =
            self.emit_binary_at(BinaryOp::Or, initial, before_first_source_tick, span)?;
        self.emit_select_at(use_start_value, start_value, value, span)
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
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let Some(value_expr) = args.first() else {
            return Err(missing_intrinsic_argument(short, "argument 1", call_span));
        };
        let value =
            self.lower_clocked_conversion_value(short, value_expr, call_span, scope, call_depth)?;
        let Some(target) = self.current_update_target else {
            return Ok(value);
        };
        let tick = self.lower_clock_constructor_tick(short, args, call_span, scope, call_depth)?;
        let Some(tick) = tick else {
            return Ok(value);
        };
        let span = value_expr.span().unwrap_or(call_span);
        let held = self.emit_slot_load(target, span)?;
        self.emit_select_at(tick, value, held, span)
    }

    fn lower_clocked_conversion_value(
        &mut self,
        short: &str,
        value_expr: &rumoca_core::Expression,
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let value = self.lower_expr(value_expr, scope, call_depth)?;
        if short != "backSample" {
            return Ok(value);
        }
        let span = value_expr.span().unwrap_or(call_span);
        let Some(start_expr) = self.variable_start_expr(value_expr) else {
            return Ok(value);
        };
        let source_phase = self.lower_clock_phase_expr(value_expr, scope, call_depth)?;
        let time = self.emit_load_time_at(span)?;
        let tol = self.emit_const_at(1.0e-9, span)?;
        let first_source_boundary = self.emit_binary_at(BinaryOp::Sub, source_phase, tol, span)?;
        let before_first_source_tick =
            self.emit_compare_at(CompareOp::Lt, time, first_source_boundary, span)?;
        let start_value = self.lower_expr(&start_expr, scope, call_depth)?;
        self.emit_select_at(before_first_source_tick, start_value, value, span)
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

    fn lower_first_tick_intrinsic(&mut self, span: rumoca_core::Span) -> Result<Reg, LowerError> {
        if self.value_mode == ValueMode::Pre {
            return self.emit_const_at(0.0, span);
        }
        let time = self.emit_load_time_at(span)?;
        let abs_time = self.emit_unary_at(UnaryOp::Abs, time, span)?;
        let tol = self.emit_const_at(1.0e-12, span)?;
        self.emit_compare_at(CompareOp::Le, abs_time, tol, span)
    }

    pub(super) fn lower_runtime_string_special_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
    ) -> Result<Option<Reg>, LowerError> {
        match lower_runtime_string_special_value(call_name, args, span)? {
            Some(value) => self.emit_const_at(value, span).map(Some),
            None => Ok(None),
        }
    }

    fn lower_external_table_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        // MLS §12.2: standard-library pure functions are ordinary function calls.
        // The table helper family is lowered to host-backed scalar ops here so
        // compiled kernels stay on the compiled path instead of falling back to
        // runtime expression evaluation.
        match external_table_intrinsic_kind(call_name) {
            Some(ExternalTableIntrinsicKind::Bounds { table, upper }) => {
                let (table_id, _) =
                    self.lower_external_table_id_arg(table, args, span, scope, call_depth)?;
                self.emit_table_bounds(table_id, upper, span).map(Some)
            }
            Some(ExternalTableIntrinsicKind::Lookup { table }) => {
                let (table_id, next_arg_idx) =
                    self.lower_external_table_id_arg(table, args, span, scope, call_depth)?;
                let column =
                    self.lower_optional_arg(args, next_arg_idx, span, scope, call_depth)?;
                let input =
                    self.lower_optional_arg(args, next_arg_idx + 1, span, scope, call_depth)?;
                self.emit_table_lookup(table_id, column, input, span)
                    .map(Some)
            }
            Some(ExternalTableIntrinsicKind::NextEvent { table }) => {
                let (table_id, next_arg_idx) =
                    self.lower_external_table_id_arg(table, args, span, scope, call_depth)?;
                let time = self.lower_optional_arg(args, next_arg_idx, span, scope, call_depth)?;
                self.emit_table_next_event(table_id, time, span).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn lower_external_table_id_arg(
        &mut self,
        table: ExternalTableRecordKind,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<(Reg, usize), LowerError> {
        let Some(arg) = args.first() else {
            return self
                .lower_optional_arg(args, 0, span, scope, call_depth)
                .map(|reg| (reg, 1));
        };
        if let Some(table_id) = self.structural_table_id_for_constructor_arg(table, arg) {
            let consumed = if repeated_external_table_constructor_record_args(table, args) {
                table.flattened_field_count()
            } else {
                1
            };
            return self
                .emit_const_at(table_id, arg.span().unwrap_or(span))
                .map(|reg| (reg, consumed));
        }
        if external_table_constructor_call(arg)
            && let Some(table_id) = self.eval_external_table_constructor_arg(arg)
        {
            let consumed = if repeated_external_table_constructor_record_args(table, args) {
                table.flattened_field_count()
            } else {
                1
            };
            return self
                .emit_const_at(table_id, arg.span().unwrap_or(span))
                .map(|reg| (reg, consumed));
        }
        if let Some(table_id) = self.structural_table_id_for_flattened_args(table, args) {
            return self
                .emit_const_at(table_id, arg.span().unwrap_or(span))
                .map(|reg| (reg, table.flattened_field_count()));
        }
        if let Some(flattened_constructor) = flattened_external_table_constructor(table, args, span)
            && let Some(table_id) = self.eval_external_table_constructor_arg(&flattened_constructor)
        {
            return self
                .emit_const_at(table_id, flattened_constructor.span().unwrap_or(span))
                .map(|reg| (reg, table.flattened_field_count()));
        }
        if let Some(table_id) = self.eval_external_table_constructor_arg(arg) {
            return self
                .emit_const_at(table_id, arg.span().unwrap_or(span))
                .map(|reg| (reg, 1));
        }
        self.lower_optional_arg(args, 0, span, scope, call_depth)
            .map(|reg| (reg, 1))
    }

    fn structural_table_id_for_flattened_args(
        &self,
        table: ExternalTableRecordKind,
        args: &[rumoca_core::Expression],
    ) -> Option<f64> {
        if args.len() < table.flattened_field_count()
            || external_table_constructor_call(args.first()?)
        {
            return None;
        }
        let fields = table.flattened_field_names();
        let first_prefix = external_table_field_prefix(args.first()?, fields.first()?)?;
        for (arg, field) in args.iter().zip(fields.iter()).take(3) {
            if external_table_field_prefix(arg, field)? != first_prefix {
                return None;
            }
        }
        self.structural_bindings
            .get(format!("{first_prefix}.tableID").as_str())
            .copied()
    }

    fn structural_table_id_for_constructor_arg(
        &self,
        table: ExternalTableRecordKind,
        expr: &rumoca_core::Expression,
    ) -> Option<f64> {
        if !external_table_constructor_call(expr) {
            return None;
        }
        let rumoca_core::Expression::FunctionCall { args, .. } = expr else {
            return None;
        };
        let mut prefix = None;
        let fields = table.flattened_field_names();
        for arg in args {
            let Some(field_prefix) = external_table_field_prefix_anywhere(arg, fields) else {
                continue;
            };
            if prefix
                .as_ref()
                .is_some_and(|existing_prefix| existing_prefix != &field_prefix)
            {
                return None;
            }
            prefix = Some(field_prefix);
        }
        self.structural_bindings
            .get(format!("{}.tableID", prefix?).as_str())
            .copied()
    }

    fn eval_external_table_constructor_arg(&self, expr: &rumoca_core::Expression) -> Option<f64> {
        let env = self.external_table_eval_env();
        rumoca_eval_dae::eval_expr::<f64>(expr, &env).ok()
    }

    fn external_table_eval_env(&self) -> rumoca_eval_dae::VarEnv<f64> {
        let mut env = rumoca_eval_dae::VarEnv::new();
        env.functions = Arc::new(
            self.functions
                .iter()
                .map(|(name, func)| (name.as_str().to_string(), func.clone()))
                .collect(),
        );
        if let Some(starts) = self.variable_starts {
            env.start_exprs = Arc::new(starts.clone());
        }
        if let Some(clock_intervals) = self.clock_intervals {
            env.clock_intervals = Arc::new(clock_intervals.clone());
        }
        if let Some(dae_variables) = self.dae_variables {
            env.dims = Arc::new(dae_variable_dims(dae_variables));
        }
        for (name, value) in self.structural_bindings.iter() {
            env.set(name, *value);
        }
        for (name, value) in &self.local_const_bindings {
            env.set(name, *value);
        }
        env
    }

    pub(super) fn lower_complex_math_sum_projection(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some(field) = parse_complex_sum_projection_field(call_name) else {
            return Ok(None);
        };
        let Some(arg) = args.first() else {
            return Err(missing_intrinsic_argument(
                call_name,
                "argument 1",
                call_span,
            ));
        };
        let span = arg.span().unwrap_or(call_span);

        let values = self.lower_complex_projection_values(arg, field, span, scope, call_depth)?;
        let mut acc = self.emit_const_at(0.0, span)?;
        for value in values {
            acc = self.emit_binary_at(BinaryOp::Add, acc, value, span)?;
        }
        Ok(Some(acc))
    }

    fn lower_complex_operator_projection(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let Some((op, field)) = parse_complex_operator_projection(call_name) else {
            return Ok(None);
        };
        let lhs = args.first().ok_or_else(|| {
            missing_intrinsic_argument(call_name, "lhs for complex operator projection", call_span)
        })?;
        let rhs = args.get(1).ok_or_else(|| {
            missing_intrinsic_argument(call_name, "rhs for complex operator projection", call_span)
        })?;
        let span = lhs.span().or_else(|| rhs.span()).unwrap_or(call_span);
        let (lhs_re, lhs_im) = self.lower_complex_operand_parts(lhs, span, scope, call_depth)?;
        let (rhs_re, rhs_im) = self.lower_complex_operand_parts(rhs, span, scope, call_depth)?;
        let (re, im) = match op {
            BinaryOp::Add => (
                self.emit_binary_at(BinaryOp::Add, lhs_re, rhs_re, span)?,
                self.emit_binary_at(BinaryOp::Add, lhs_im, rhs_im, span)?,
            ),
            BinaryOp::Sub => (
                self.emit_binary_at(BinaryOp::Sub, lhs_re, rhs_re, span)?,
                self.emit_binary_at(BinaryOp::Sub, lhs_im, rhs_im, span)?,
            ),
            BinaryOp::Mul => {
                let ac = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_re, span)?;
                let bd = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_im, span)?;
                let ad = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_im, span)?;
                let bc = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_re, span)?;
                (
                    self.emit_binary_at(BinaryOp::Sub, ac, bd, span)?,
                    self.emit_binary_at(BinaryOp::Add, ad, bc, span)?,
                )
            }
            BinaryOp::Div => {
                let rr2 = self.emit_binary_at(BinaryOp::Mul, rhs_re, rhs_re, span)?;
                let ri2 = self.emit_binary_at(BinaryOp::Mul, rhs_im, rhs_im, span)?;
                let denom = self.emit_binary_at(BinaryOp::Add, rr2, ri2, span)?;
                let lhs_rr = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_re, span)?;
                let lhs_ri = self.emit_binary_at(BinaryOp::Mul, lhs_re, rhs_im, span)?;
                let li_rr = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_re, span)?;
                let li_ri = self.emit_binary_at(BinaryOp::Mul, lhs_im, rhs_im, span)?;
                let re_num = self.emit_binary_at(BinaryOp::Add, lhs_rr, li_ri, span)?;
                let im_num = self.emit_binary_at(BinaryOp::Sub, li_rr, lhs_ri, span)?;
                (
                    self.emit_binary_at(BinaryOp::Div, re_num, denom, span)?,
                    self.emit_binary_at(BinaryOp::Div, im_num, denom, span)?,
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
        fallback_span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let span = expr.span().unwrap_or(fallback_span);
        match expr {
            rumoca_core::Expression::Array { elements, .. }
            | rumoca_core::Expression::Tuple { elements, .. } => {
                let mut values = complex_projection_vec_with_capacity(
                    elements.len(),
                    "complex projection element count",
                    span,
                )?;
                for element in elements {
                    let element_values = self
                        .lower_complex_projection_values(element, field, span, scope, call_depth)?;
                    append_complex_projection_values(
                        &mut values,
                        element_values,
                        "complex projection flattened element count",
                        span,
                    )?;
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
                let mut values = complex_projection_vec_with_capacity(
                    0,
                    "complex projection comprehension value count",
                    span,
                )?;
                let mut ctx = ComplexProjectionComprehensionCtx {
                    indices,
                    filter: filter.as_deref(),
                    field,
                    scope: &mut scope,
                    const_scope: &mut const_scope,
                    call_depth,
                    fallback_span: span,
                };
                self.lower_complex_projection_comprehension_values(expr, 0, &mut ctx, &mut values)?;
                Ok(values)
            }
            _ => {
                let projected = rumoca_core::Expression::FieldAccess {
                    base: Box::new(expr.clone()),
                    field: field.to_string(),
                    span,
                };
                let mut values =
                    complex_projection_vec_with_capacity(1, "complex scalar projection", span)?;
                values.push(self.lower_expr(&projected, scope, call_depth)?);
                Ok(values)
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
            let values = self.lower_complex_projection_values(
                expr,
                ctx.field,
                expr.span()
                    .or_else(|| ctx.filter.and_then(rumoca_core::Expression::span))
                    .unwrap_or(ctx.fallback_span),
                ctx.scope,
                ctx.call_depth,
            )?;
            append_complex_projection_values(
                out,
                values,
                "complex projection comprehension output count",
                expr.span().unwrap_or(ctx.fallback_span),
            )?;
            return Ok(());
        }

        let iter = &ctx.indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, ctx.const_scope)?;
        for value in iter_values {
            let iter_reg =
                self.emit_const_at(value, iter.range.span().unwrap_or(ctx.fallback_span))?;
            ctx.scope.push_frame();
            let result = (|| {
                ctx.scope
                    .insert_scoped(generated_scope_key(&iter.name), iter_reg)?;
                ctx.const_scope.insert(iter.name.clone(), value);
                let result =
                    self.lower_complex_projection_comprehension_values(expr, depth + 1, ctx, out);
                ctx.const_scope.shift_remove(&iter.name);
                result
            })();
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

    pub(super) fn bind_used_function_inputs(
        &mut self,
        function: &rumoca_core::Function,
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<FunctionInputBindings, LowerError> {
        let used_inputs = referenced_function_input_names(function);
        self.bind_function_inputs_for_name_with_used(
            function.name.as_str(),
            &function.inputs,
            args,
            caller_scope,
            call_depth,
            Some(&used_inputs),
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
        self.bind_function_inputs_for_name_with_used(
            function_name,
            inputs,
            args,
            caller_scope,
            call_depth,
            None,
        )
    }

    fn bind_function_inputs_for_name_with_used(
        &mut self,
        function_name: &str,
        inputs: &[rumoca_core::FunctionParam],
        args: &[rumoca_core::Expression],
        caller_scope: &Scope,
        call_depth: usize,
        used_inputs: Option<&IndexSet<String>>,
    ) -> Result<FunctionInputBindings, LowerError> {
        let (named_args, positional_args) =
            split_named_and_positional_call_args(function_name, args)?;
        let mut scope = Scope::new();
        let mut const_scope = self.local_const_bindings.clone();
        let mut const_bindings = IndexMap::new();
        let mut positional_idx = 0usize;

        for (input_idx, input) in inputs.iter().enumerate() {
            if used_inputs.is_some_and(|used_inputs| !used_inputs.contains(&input.name)) {
                let Some((prefix, field)) = split_flattened_record_input_name(&input.name) else {
                    if self.try_bind_function_input(
                        FunctionInputBindState {
                            scope: &mut scope,
                            const_scope: &mut const_scope,
                            const_bindings: &mut const_bindings,
                        },
                        FunctionInputRequest {
                            function_name,
                            input,
                            inputs,
                            input_idx,
                            named_args: &named_args,
                            positional_args: &positional_args,
                            positional_idx: &mut positional_idx,
                            caller_scope,
                            call_depth: call_depth + 1,
                        },
                    )? {
                        continue;
                    }
                    return missing_required_function_input(function_name, input);
                };
                let flattened_group_has_used_sibling = inputs.iter().any(|candidate| {
                    flattened_input_has_prefix(&candidate.name, prefix)
                        && used_inputs.is_some_and(|used| used.contains(&candidate.name))
                });
                if flattened_group_has_used_sibling && !named_args.contains_key(input.name.as_str())
                {
                    let later_flattened_sibling_is_used =
                        inputs.iter().skip(input_idx + 1).any(|next| {
                            flattened_input_has_prefix(&next.name, prefix)
                                && used_inputs.is_some_and(|used| used.contains(&next.name))
                        });
                    if !later_flattened_sibling_is_used
                        || positional_args
                            .get(positional_idx)
                            .is_some_and(|arg| is_flattened_record_field_actual(arg, field))
                    {
                        positional_idx += usize::from(positional_idx < positional_args.len());
                    }
                    continue;
                }
            }
            if self.try_bind_function_input(
                FunctionInputBindState {
                    scope: &mut scope,
                    const_scope: &mut const_scope,
                    const_bindings: &mut const_bindings,
                },
                FunctionInputRequest {
                    function_name,
                    input,
                    inputs,
                    input_idx,
                    named_args: &named_args,
                    positional_args: &positional_args,
                    positional_idx: &mut positional_idx,
                    caller_scope,
                    call_depth: call_depth + 1,
                },
            )? {
                continue;
            }

            return missing_required_function_input(function_name, input);
        }

        Ok(FunctionInputBindings {
            scope,
            const_bindings,
        })
    }

    fn try_bind_function_input(
        &mut self,
        state: FunctionInputBindState<'_>,
        request: FunctionInputRequest<'_, '_>,
    ) -> Result<bool, LowerError> {
        if let Some(arg_expr) = request.named_args.get(request.input.name.as_str()) {
            return self
                .bind_function_input_arg_or_closure(
                    state,
                    request.function_name,
                    request.input,
                    arg_expr,
                    request.caller_scope,
                    request.call_depth,
                )
                .map(|()| true);
        }
        self.try_bind_non_named_function_input(state, request)
    }

    fn try_bind_non_named_function_input(
        &mut self,
        state: FunctionInputBindState<'_>,
        request: FunctionInputRequest<'_, '_>,
    ) -> Result<bool, LowerError> {
        if self.bind_flattened_record_positional_input(
            FunctionInputBindState {
                scope: state.scope,
                const_scope: state.const_scope,
                const_bindings: state.const_bindings,
            },
            FlattenedRecordPositionalInputRequest {
                function_name: request.function_name,
                input: request.input,
                inputs: request.inputs,
                input_idx: request.input_idx,
                positional_args: request.positional_args,
                positional_idx: request.positional_idx,
                caller_scope: request.caller_scope,
                call_depth: request.call_depth,
            },
        )? {
            return Ok(true);
        }
        if request.positional_args.len() > request.inputs.len()
            && let Some(fields) = self.record_constructor_fields(&request.input.type_name)
            && self.bind_flattened_record_function_input(
                FunctionInputBindState {
                    scope: state.scope,
                    const_scope: state.const_scope,
                    const_bindings: state.const_bindings,
                },
                FlattenedRecordInputRequest {
                    input: request.input,
                    fields: &fields,
                    positional_args: request.positional_args,
                    positional_idx: request.positional_idx,
                    caller_scope: request.caller_scope,
                    call_depth: request.call_depth,
                },
            )?
        {
            return Ok(true);
        }
        self.try_bind_simple_function_input(state, request)
    }

    fn try_bind_simple_function_input(
        &mut self,
        state: FunctionInputBindState<'_>,
        request: FunctionInputRequest<'_, '_>,
    ) -> Result<bool, LowerError> {
        if let Some(arg_expr) = next_positional_function_input_arg(
            request.input,
            request.positional_args,
            request.positional_idx,
        ) {
            return self
                .bind_function_input_arg_or_closure(
                    state,
                    request.function_name,
                    request.input,
                    arg_expr,
                    request.caller_scope,
                    request.call_depth,
                )
                .map(|()| true);
        }
        if let Some(default) = request.input.default.as_ref() {
            let local_scope = state.scope.clone();
            return self
                .bind_function_input_arg_or_closure(
                    state,
                    request.function_name,
                    request.input,
                    default,
                    &local_scope,
                    request.call_depth,
                )
                .map(|()| true);
        }
        if let Some(synthesized) = synthesize_missing_flattened_record_field_arg(
            request.input,
            request.inputs,
            request.input_idx,
            request.positional_args,
            *request.positional_idx,
        ) {
            return self
                .bind_function_input_arg_or_closure(
                    state,
                    request.function_name,
                    request.input,
                    &synthesized,
                    request.caller_scope,
                    request.call_depth,
                )
                .map(|()| true);
        }
        Ok(false)
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
        if !self.is_record_like_function_actual(arg_expr, field, request.caller_scope) {
            return Ok(false);
        }

        let projected = rumoca_core::Expression::FieldAccess {
            base: Box::new((*arg_expr).clone()),
            field: field.to_string(),
            span: arg_expr.span().unwrap_or(request.input.span),
        };
        self.bind_function_input_arg_or_closure(
            state,
            request.function_name,
            request.input,
            &projected,
            request.caller_scope,
            request.call_depth,
        )?;
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

    fn bind_function_input_arg_or_closure(
        &mut self,
        state: FunctionInputBindState<'_>,
        function_name: &str,
        input: &rumoca_core::FunctionParam,
        arg_expr: &rumoca_core::Expression,
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if self.bind_function_input_closure(function_name, input, arg_expr, caller_scope)? {
            return Ok(());
        }
        self.bind_function_input_value(
            state,
            function_name,
            input,
            arg_expr,
            caller_scope,
            call_depth,
        )?;
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
                caller_scope.contains_key(&generated_scope_key(&field_key))
                    || self.layout.binding(&field_key).is_some()
                    || self.local_indexed_bindings.contains_key(&field_key)
                    || self.local_binding_dims.contains_key(&field_key)
            }
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
    ) -> Result<bool, LowerError> {
        if input.type_class != Some(rumoca_core::ClassType::Function)
            && !input.type_name.to_ascii_lowercase().contains("function")
        {
            return Ok(false);
        }
        let Some(closure) = self.function_closure_from_arg(arg_expr, caller_scope)? else {
            return Ok(false);
        };
        self.function_closures
            .insert(generated_scope_key(&input.name), closure.clone());
        self.function_closures.insert(
            generated_scope_key(format!("{current_function_name}.{}", input.name)),
            closure,
        );
        Ok(true)
    }

    fn function_closure_from_arg(
        &self,
        arg_expr: &rumoca_core::Expression,
        caller_scope: &Scope,
    ) -> Result<Option<FunctionClosure>, LowerError> {
        match arg_expr {
            rumoca_core::Expression::FunctionCall {
                name,
                args,
                is_constructor: false,
                ..
            } if self.lookup_function(name).is_some() => Ok(Some(FunctionClosure {
                target_name: name.clone(),
                bound_args: args.clone(),
                captured_scope: caller_scope.clone(),
            })),
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
            } if subscripts.is_empty() => self
                .lookup_function_closure(name, *span)
                .map(|closure| closure.cloned()),
            _ => Ok(None),
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
                    function_name.as_str(),
                    input,
                    default,
                    &local_scope,
                    call_depth + 1,
                )?;
                continue;
            } else {
                return Err(LowerError::MissingActualArgument {
                    function: function_name.as_str().to_string(),
                    what: "required input",
                    input: input.name.clone(),
                    span: input.span,
                });
            };

            self.bind_function_input_value(
                FunctionInputBindState {
                    scope: &mut scope,
                    const_scope: &mut const_scope,
                    const_bindings: &mut const_bindings,
                },
                function_name.as_str(),
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
        let remaining_positional = req
            .positional_args
            .len()
            .checked_sub(*req.positional_idx)
            .ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "function input positional cursor {} exceeds argument count {}",
                        req.positional_idx,
                        req.positional_args.len()
                    ),
                    req.input.span,
                )
            })?;
        if remaining_positional < req.fields.len() {
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
            let span = expr.span().unwrap_or(field.span);
            self.bind_assignment_values_with_dims(
                state.scope,
                local_name,
                &values,
                &field.dims,
                span,
            )?;
            self.bind_local_array_shape(local_name, &field.dims, values.len(), span)?;
            return Ok(());
        }

        let inferred_dims = self.infer_expr_dims(expr, expr_scope)?;
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
            let dims = checked_usize_dims_to_i64(
                &inferred_dims,
                "flattened record field actual shape",
                expr.span().unwrap_or(field.span),
            )?;
            let span = expr.span().unwrap_or(field.span);
            self.bind_assignment_values_with_dims(state.scope, local_name, &values, &dims, span)?;
            self.bind_local_array_shape(local_name, &dims, values.len(), span)?;
            return Ok(());
        }

        let reg = self.lower_expr(expr, expr_scope, call_depth)?;
        if let Ok(value) = self.eval_compile_time_expr(expr, state.const_scope) {
            state.const_scope.insert(local_name.to_string(), value);
            state.const_bindings.insert(local_name.to_string(), value);
        }
        state.scope.insert(generated_scope_key(local_name), reg);
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
            span,
        } = expr
        else {
            return Ok(false);
        };
        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(false);
        }

        // SPEC_0008: a call recognized as a record constructor must have a
        // registered field list; fabricating scalar Real params for missing
        // registrations silently mis-binds record fields.
        let Some(fields) = self.record_constructor_fields(name.as_str()) else {
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: "record constructor has no registered field list".to_string(),
            });
        };
        let (named_args, positional_args) =
            split_named_and_positional_call_args(name.as_str(), args)?;
        let mut bound_any = false;
        for (field_name, arg_expr) in named_args
            .into_iter()
            .filter(|(_, arg_expr)| !non_numeric_record_metadata(arg_expr))
        {
            let local_name = format!("{}.{}", input.name, field_name);
            let field = record_constructor_field(name.as_str(), &fields, &field_name, *span)?;
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
        function_name: &str,
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
        if input.type_class == Some(rumoca_core::ClassType::Record)
            && !is_complex_param(input)
            && self.bind_record_input_components(
                state.scope,
                &input.name,
                expr,
                expr_scope,
                input.span,
            )?
        {
            return Ok(());
        }
        if is_complex_param(input) {
            self.bind_complex_input(
                state.scope,
                function_name,
                input,
                expr,
                expr_scope,
                call_depth,
            )?;
            return Ok(());
        }
        if self.bind_declared_array_function_input(
            &mut state,
            function_name,
            input,
            expr,
            expr_scope,
            call_depth,
        )? {
            return Ok(());
        }
        if self
            .bind_inferred_array_function_input(&mut state, input, expr, expr_scope, call_depth)?
        {
            return Ok(());
        }
        self.bind_scalar_function_input_value(state, input, expr, expr_scope, call_depth)
    }

    fn bind_declared_array_function_input(
        &mut self,
        state: &mut FunctionInputBindState<'_>,
        function_name: &str,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if input.dims.is_empty() {
            return Ok(false);
        }
        let span = expr.span().unwrap_or(input.span);
        let binding_dims = match self.resolve_function_input_binding_dims(
            function_name,
            input,
            expr,
            expr_scope,
        ) {
            Ok(dims) => dims,
            Err(err)
                if err.is_missing_binding_or_function()
                    && input.dims.iter().any(|dim| *dim == 0)
                    && input.dims.iter().all(|dim| *dim >= 0) =>
            {
                input.dims.clone()
            }
            Err(err) => return Err(err),
        };
        let expected = dims_scalar_count(
            &binding_dims,
            format!(
                "function `{function_name}` input `{}` declared shape",
                input.name
            ),
            span,
        )?;
        let values = if expected == 0 {
            Vec::new()
        } else {
            self.lower_array_like_values(expr, expr_scope, call_depth)?
        };
        if values.len() != expected {
            let shape = format_i64_dims(&binding_dims);
            return Err(LowerError::contract_violation(
                format!(
                    "function `{function_name}` input `{}` expected {expected} scalar value(s) for shape {shape}, got {}",
                    input.name,
                    values.len(),
                ),
                span,
            ));
        }
        self.bind_assignment_values_with_dims(
            &mut *state.scope,
            &input.name,
            &values,
            &binding_dims,
            span,
        )?;
        self.bind_local_array_shape(&input.name, &binding_dims, values.len(), span)?;
        Ok(true)
    }

    fn bind_inferred_array_function_input(
        &mut self,
        state: &mut FunctionInputBindState<'_>,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let inferred_dims = self.infer_expr_dims(expr, expr_scope)?;
        if inferred_dims.is_empty() {
            return Ok(false);
        }
        let values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
        if bind_singleton_array_actual_to_scalar_formal(
            &mut *state.scope,
            &input.name,
            &inferred_dims,
            &values,
        ) {
            return Ok(true);
        }
        let span = expr.span().unwrap_or(input.span);
        let dims = checked_usize_dims_to_i64(&inferred_dims, "function input actual shape", span)?;
        self.bind_assignment_values_with_dims(
            &mut *state.scope,
            &input.name,
            &values,
            &dims,
            span,
        )?;
        self.bind_local_array_shape(&input.name, &dims, values.len(), span)?;
        Ok(true)
    }

    fn bind_scalar_function_input_value(
        &mut self,
        state: FunctionInputBindState<'_>,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        let reg = match self.lower_expr(expr, expr_scope, call_depth) {
            Ok(reg) => reg,
            Err(err)
                if err.is_missing_binding()
                    && self.bind_record_input_components(
                        state.scope,
                        &input.name,
                        expr,
                        expr_scope,
                        input.span,
                    )? =>
            {
                return Ok(());
            }
            Err(err) => return Err(err),
        };
        if let Ok(value) = self.eval_compile_time_expr(expr, state.const_scope) {
            state.const_scope.insert(input.name.clone(), value);
            state.const_bindings.insert(input.name.clone(), value);
        }
        self.clear_local_array_metadata(&input.name, expr.span().unwrap_or(input.span))?;
        state.scope.insert(generated_scope_key(&input.name), reg);
        Ok(())
    }

    fn resolve_function_input_binding_dims(
        &self,
        function_name: &str,
        input: &rumoca_core::FunctionParam,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
    ) -> Result<Vec<i64>, LowerError> {
        if input.dims.iter().all(|dim| *dim > 0) {
            return Ok(input.dims.clone());
        }
        let span = expr.span().unwrap_or(input.span);
        if input.dims.iter().any(|dim| *dim < 0) {
            let shape = format_i64_dims(&input.dims);
            return Err(LowerError::contract_violation(
                format!(
                    "function `{function_name}` input `{}` has negative dimension in declared shape {shape}",
                    input.name
                ),
                span,
            ));
        }
        let actual_dims = self.infer_expr_dims(expr, expr_scope)?;
        if actual_dims.len() != input.dims.len() {
            let shape = format_i64_dims(&input.dims);
            return Err(LowerError::contract_violation(
                format!(
                    "function `{function_name}` input `{}` expected rank {} for declared shape {shape}, got rank {}",
                    input.name,
                    input.dims.len(),
                    actual_dims.len()
                ),
                span,
            ));
        }
        input
            .dims
            .iter()
            .zip(actual_dims)
            .map(|(declared, actual)| {
                function_input_actual_dim(function_name, input, *declared, actual, span)
            })
            .collect()
    }

    fn bind_record_input_components(
        &mut self,
        scope: &mut Scope,
        input_name: &str,
        expr: &rumoca_core::Expression,
        expr_scope: &Scope,
        fallback_span: rumoca_core::Span,
    ) -> Result<bool, LowerError> {
        let Ok(base_key) = binding_base_key(expr) else {
            return Ok(false);
        };
        let span = expr.span().unwrap_or(fallback_span);
        let source_prefix = format!("{base_key}.");
        let mut bound_any = false;
        let expr_scope_entries =
            expr_scope.iter_checked("record input scope component source count", span)?;
        for (key, reg) in expr_scope_entries {
            let Some(suffix) = generated_scope_key_suffix(&key, &source_prefix) else {
                continue;
            };
            self.bind_record_input_component(scope, input_name, &key, reg, suffix, span)?;
            bound_any = true;
        }

        let prefix = format!("{base_key}.");
        let mut slots = crate::lower_vec_with_capacity(
            self.layout.bindings().len(),
            "record input layout component staging count",
            span,
        )?;
        for (key, slot) in self.layout.bindings() {
            let Some(suffix) = key.strip_prefix(prefix.as_str()) else {
                continue;
            };
            slots.push((key.clone(), suffix.to_string(), *slot));
        }
        for (key, suffix, slot) in slots {
            if self
                .bind_record_input_layout_array_component(scope, input_name, &key, &suffix, span)?
            {
                bound_any = true;
                continue;
            }
            let local_key = format!("{input_name}.{suffix}");
            let local_path = generated_scope_key(&local_key);
            if scope.contains_key(&local_path) {
                continue;
            }
            let slot = self.pre_mode_slot_for_key(&key).unwrap_or(slot);
            let reg = self.emit_slot_load(slot, span)?;
            scope.insert(local_path, reg);
            self.copy_record_input_component_dims(&key, &local_key, span)?;
            bound_any = true;
        }

        Ok(bound_any)
    }

    fn bind_record_input_layout_array_component(
        &mut self,
        scope: &mut Scope,
        input_name: &str,
        source_key: &str,
        suffix: &str,
        span: rumoca_core::Span,
    ) -> Result<bool, LowerError> {
        let entries = indexed_entries_for_key(&self.indexed_bindings, source_key);
        if entries.is_empty() {
            return Ok(false);
        }
        let local_base = format!("{input_name}.{suffix}");
        let local_path = generated_scope_key(&local_base);
        for entry in sorted_flat_entries(&entries) {
            let reg = self.emit_slot_load(entry.slot, span)?;
            scope.insert_indexed(&local_path, &entry.indices, reg, span)?;
            upsert_local_indexed_binding(
                self.local_indexed_bindings
                    .entry(local_base.clone())
                    .or_default(),
                &entry.indices,
                reg,
                span,
            )?;
            let indexed_key = format_subscript_binding_key(&local_base, &entry.indices);
            scope.insert(generated_scope_key(indexed_key), reg);
        }
        if let Some(first) = self
            .local_indexed_bindings
            .get(local_base.as_str())
            .and_then(|bindings| bindings.first())
        {
            scope.insert(local_path, first.reg);
        }
        self.copy_record_input_component_dims(source_key, &local_base, span)?;
        Ok(true)
    }

    fn bind_record_input_component(
        &mut self,
        scope: &mut Scope,
        input_name: &str,
        key: &ComponentReferenceKey,
        reg: Reg,
        suffix: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let local_key = format!("{input_name}.{suffix}");
        scope.insert(generated_scope_key(&local_key), reg);
        self.bind_record_input_indexed_component(scope, &local_key, reg, span)?;
        if let Some(source_key) = generated_scope_key_name(key) {
            self.copy_record_input_component_dims(source_key, &local_key, span)?;
            self.copy_record_input_indexed_component_dims(source_key, &local_key, span)?;
        }
        Ok(())
    }

    fn bind_record_input_indexed_component(
        &mut self,
        scope: &mut Scope,
        local_key: &str,
        reg: Reg,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let Some((base, indices)) = parse_indexed_binding_key(local_key) else {
            return Ok(());
        };
        let base_path = generated_scope_key(&base);
        scope.insert_indexed(&base_path, &indices, reg, span)?;
        upsert_local_indexed_binding(
            self.local_indexed_bindings.entry(base).or_default(),
            &indices,
            reg,
            span,
        )
    }

    fn copy_record_input_indexed_component_dims(
        &mut self,
        source_key: &str,
        local_key: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let (Some((source_base, _)), Some((local_base, _))) = (
            parse_indexed_binding_key(source_key),
            parse_indexed_binding_key(local_key),
        ) else {
            return Ok(());
        };
        self.copy_record_input_component_dims(&source_base, &local_base, span)
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
        self.ensure_pure_inline_function(
            name.as_str(),
            &function,
            expr.span().unwrap_or(input.span),
        )?;

        let Some(materialized) = self.materialize_single_record_function_call_components(
            name,
            args,
            expr.span().unwrap_or(input.span),
            expr_scope,
            call_depth,
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
                .insert(generated_scope_key(&local_key), component.reg);
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
        span: rumoca_core::Span,
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
        self.ensure_pure_inline_function(name.as_str(), &function, span)?;

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
                output.span,
            )?))
        })
    }

    fn materialized_record_components_from_scope(
        &self,
        output_name: &str,
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<MaterializedRecordComponents, LowerError> {
        let source_prefix = format!("{output_name}.");
        let scope_entries = scope.iter_checked("record output component source count", span)?;
        let mut components = crate::lower_vec_with_capacity(
            scope_entries.len(),
            "record output component staging count",
            span,
        )?;
        for (key, reg) in scope_entries {
            let Some(key_text) = generated_scope_key_name(&key) else {
                continue;
            };
            let Some(suffix) = key_text.strip_prefix(source_prefix.as_str()) else {
                continue;
            };
            components.push(MaterializedRecordComponent {
                suffix: suffix.to_string(),
                reg,
                dims: self.local_binding_dims.get(key_text).cloned(),
                known_empty: self.known_empty_local_arrays.contains(key_text),
            });
        }
        let mut indexed_components = crate::lower_vec_with_capacity(
            self.local_indexed_bindings.len(),
            "record output indexed component staging count",
            span,
        )?;
        for (key, bindings) in &self.local_indexed_bindings {
            let Some(suffix) = key.strip_prefix(source_prefix.as_str()) else {
                continue;
            };
            indexed_components.push((suffix.to_string(), bindings.clone()));
        }
        Ok(MaterializedRecordComponents {
            components,
            indexed_components,
        })
    }

    fn copy_record_input_component_dims(
        &mut self,
        source_key: &str,
        local_key: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if let Some(dims) = self.local_binding_dims.get(source_key).cloned() {
            self.local_binding_dims.insert(local_key.to_string(), dims);
            if self.known_empty_local_arrays.contains(source_key) {
                self.known_empty_local_arrays.insert(local_key.to_string());
            } else {
                self.known_empty_local_arrays.shift_remove(local_key);
            }
            return Ok(());
        }
        if let Some(shape) = self.layout.shape(source_key) {
            let dims = checked_usize_dims_to_i64(shape, "record input component shape", span)?;
            self.local_binding_dims.insert(local_key.to_string(), dims);
            self.known_empty_local_arrays.shift_remove(local_key);
        }
        Ok(())
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
            guarded_uninitialized_locals: self.guarded_uninitialized_locals.clone(),
            local_const_bindings: self.local_const_bindings.clone(),
            function_closures: self.function_closures.clone(),
        };
        let result = f(self);
        self.structural_bindings = frame.structural_bindings;
        self.local_indexed_bindings = frame.local_indexed_bindings;
        self.local_binding_dims = frame.local_binding_dims;
        self.known_empty_local_arrays = frame.known_empty_local_arrays;
        self.guarded_uninitialized_locals = frame.guarded_uninitialized_locals;
        self.local_const_bindings = frame.local_const_bindings;
        self.function_closures = frame.function_closures;
        result
    }

    pub(super) fn with_local_const_bindings<T>(
        &mut self,
        const_bindings: &IndexMap<String, f64>,
        f: impl FnOnce(&mut Self) -> Result<T, LowerError>,
    ) -> Result<T, LowerError> {
        let saved = self.local_const_bindings.clone();
        self.local_const_bindings.extend(const_bindings.clone());
        let result = f(self);
        self.local_const_bindings = saved;
        result
    }

    pub(super) fn initialize_function_output_scope(
        &mut self,
        function: &rumoca_core::Function,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        for param in &function.outputs {
            self.initialize_function_scope_param(param, scope, call_depth, false)?;
        }
        for param in &function.locals {
            self.initialize_function_scope_param(param, scope, call_depth, true)?;
        }
        Ok(())
    }

    fn initialize_function_scope_param(
        &mut self,
        param: &rumoca_core::FunctionParam,
        scope: &mut Scope,
        call_depth: usize,
        guarded_local: bool,
    ) -> Result<(), LowerError> {
        self.guarded_uninitialized_locals
            .shift_remove(param.name.as_str());
        if param.default.is_some() {
            let values = self.initial_function_param_values(param, scope, call_depth)?;
            self.bind_assignment_values_with_dims(
                scope,
                &param.name,
                &values,
                &param.dims,
                param.span,
            )?;
            self.bind_local_array_shape(&param.name, &param.dims, values.len(), param.span)?;
            self.bind_initial_function_param_const(param);
            return Ok(());
        }
        if guarded_local {
            self.guarded_uninitialized_locals.insert(param.name.clone());
        }
        // A function output/local that shadows a caller variable of the
        // same name (e.g. a library `product` whose output is `X` called
        // from a model whose state is also `X`) must start fresh. Drop any
        // inherited caller bindings for this name first; otherwise stale
        // caller elements (e.g. `X[11..13]` of a larger caller array) leak
        // into reads of this function's array output.
        if !param.dims.is_empty() {
            super::function_projection::clear_indexed_scope_bindings(
                scope,
                &param.name,
                param.span,
            )?;
            scope.shift_remove(&generated_scope_key(&param.name));
            self.local_indexed_bindings
                .shift_remove(param.name.as_str());
        }
        self.initialize_declared_function_param(param)?;
        Ok(())
    }

    fn initialize_declared_function_param(
        &mut self,
        param: &rumoca_core::FunctionParam,
    ) -> Result<(), LowerError> {
        if param.dims.is_empty() {
            self.clear_local_array_metadata(&param.name, param.span)?;
            return Ok(());
        }
        self.bind_declared_function_param_shape(param)
    }

    pub(super) fn ensure_pure_inline_function(
        &self,
        function_name: &str,
        function: &rumoca_core::Function,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if function.pure {
            return Ok(());
        }
        Err(unsupported_at(
            format!(
                "impure function call `{function_name}` cannot be lowered as a pure algebraic expression"
            ),
            span,
        ))
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

    pub(super) fn bind_local_array_shape(
        &mut self,
        name: &str,
        dims: &[i64],
        value_count: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let resolved_dims = resolve_array_dims_for_value_count(
            dims,
            value_count,
            "local array shape dimension resolution",
            span,
        )?;
        for (idx, dim) in resolved_dims.iter().enumerate() {
            let dim = if *dim > 0 {
                *dim as f64
            } else if idx == 0 {
                value_count as f64
            } else {
                0.0
            };
            std::sync::Arc::make_mut(&mut self.structural_bindings)
                .insert(super::size_binding_key(name, idx + 1), dim);
        }
        Ok(())
    }

    pub(super) fn clear_local_array_metadata(
        &mut self,
        name: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        self.local_binding_dims.shift_remove(name);
        self.local_indexed_bindings.shift_remove(name);
        self.known_empty_local_arrays.shift_remove(name);
        let prefix = format!("{}{}.", super::SIZE_BINDING_PREFIX, name);
        let mut keys = crate::lower_vec_with_capacity(
            self.structural_bindings.len(),
            "local array metadata cleanup key count",
            span,
        )?;
        for key in self.structural_bindings.keys() {
            if key.starts_with(prefix.as_str()) {
                keys.push(key.clone());
            }
        }
        for key in keys {
            std::sync::Arc::make_mut(&mut self.structural_bindings).shift_remove(key.as_str());
        }
        Ok(())
    }

    pub(super) fn set_known_local_array_dims(
        &mut self,
        name: &str,
        dims: Vec<i64>,
        value_count: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let is_empty = !dims.is_empty() && dims.iter().all(|dim| *dim >= 0) && dims.contains(&0);
        self.bind_local_array_shape(name, &dims, value_count, span)?;
        self.local_binding_dims
            .insert(name.to_string(), dims.clone());
        if is_empty {
            self.known_empty_local_arrays.insert(name.to_string());
        } else {
            self.known_empty_local_arrays.shift_remove(name);
        }
        Ok(())
    }

    pub(super) fn bind_declared_function_param_shape(
        &mut self,
        param: &rumoca_core::FunctionParam,
    ) -> Result<(), LowerError> {
        if param.dims.is_empty() {
            return Ok(());
        }
        if let Some(dims) = self.resolve_function_param_shape(param) {
            self.set_known_local_array_dims(
                &param.name,
                dims.clone(),
                exact_dim_value_count(
                    &dims,
                    format!("function parameter `{}`", param.name),
                    param.span,
                )?,
                param.span,
            )?;
            return Ok(());
        }
        if param.dims.contains(&0) && param.shape_expr.is_empty() {
            self.set_known_local_array_dims(&param.name, param.dims.clone(), 0, param.span)?;
            return Ok(());
        }
        if param.dims.iter().any(|dim| *dim < 0) {
            self.known_empty_local_arrays
                .shift_remove(param.name.as_str());
            return Ok(());
        }

        let value_count = dims_scalar_count(
            &param.dims,
            format!("function parameter `{}`", param.name),
            param.span,
        )?;
        self.set_known_local_array_dims(&param.name, param.dims.clone(), value_count, param.span)?;
        Ok(())
    }

    fn resolve_function_param_shape(&self, param: &rumoca_core::FunctionParam) -> Option<Vec<i64>> {
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
}

fn external_table_constructor_call(expr: &rumoca_core::Expression) -> bool {
    let rumoca_core::Expression::FunctionCall { name, .. } = expr else {
        return false;
    };
    matches!(
        name.last_segment(),
        "ExternalCombiTimeTable" | "ExternalCombiTable1D"
    )
}

fn repeated_external_table_constructor_record_args(
    table: ExternalTableRecordKind,
    args: &[rumoca_core::Expression],
) -> bool {
    let field_count = table.flattened_field_count();
    args.len() > field_count
        && args[..field_count]
            .iter()
            .all(external_table_constructor_call)
}

fn external_table_field_prefix(expr: &rumoca_core::Expression, field: &str) -> Option<String> {
    let rumoca_core::Expression::VarRef {
        name, subscripts, ..
    } = expr
    else {
        return None;
    };
    if !subscripts.is_empty() {
        return None;
    }
    name.as_str()
        .strip_suffix(format!(".{field}").as_str())
        .map(str::to_string)
}

fn external_table_field_prefix_anywhere(
    expr: &rumoca_core::Expression,
    fields: &[&str],
) -> Option<String> {
    let direct = fields
        .iter()
        .find_map(|field| external_table_field_prefix(expr, field));
    if direct.is_some() {
        return direct;
    }
    match expr {
        rumoca_core::Expression::Binary { lhs, rhs, .. } => {
            first_matching_external_table_field_prefix([lhs.as_ref(), rhs.as_ref()], fields)
        }
        rumoca_core::Expression::Unary { rhs, .. } => {
            external_table_field_prefix_anywhere(rhs, fields)
        }
        rumoca_core::Expression::BuiltinCall { args, .. }
        | rumoca_core::Expression::FunctionCall { args, .. }
        | rumoca_core::Expression::Array { elements: args, .. }
        | rumoca_core::Expression::Tuple { elements: args, .. } => {
            first_matching_external_table_field_prefix(args.iter(), fields)
        }
        rumoca_core::Expression::If {
            branches,
            else_branch,
            ..
        } => branches
            .iter()
            .flat_map(|(condition, value)| [condition, value])
            .chain([else_branch.as_ref()])
            .find_map(|expr| external_table_field_prefix_anywhere(expr, fields)),
        rumoca_core::Expression::Range {
            start, step, end, ..
        } => [Some(start.as_ref()), step.as_deref(), Some(end.as_ref())]
            .into_iter()
            .flatten()
            .find_map(|expr| external_table_field_prefix_anywhere(expr, fields)),
        rumoca_core::Expression::ArrayComprehension { expr, filter, .. } => {
            [Some(expr.as_ref()), filter.as_deref()]
                .into_iter()
                .flatten()
                .find_map(|expr| external_table_field_prefix_anywhere(expr, fields))
        }
        rumoca_core::Expression::Index { base, .. }
        | rumoca_core::Expression::FieldAccess { base, .. } => {
            external_table_field_prefix_anywhere(base, fields)
        }
        rumoca_core::Expression::VarRef { .. }
        | rumoca_core::Expression::Literal { .. }
        | rumoca_core::Expression::Empty { .. } => None,
    }
}

fn first_matching_external_table_field_prefix<'a>(
    exprs: impl IntoIterator<Item = &'a rumoca_core::Expression>,
    fields: &[&str],
) -> Option<String> {
    exprs
        .into_iter()
        .find_map(|expr| external_table_field_prefix_anywhere(expr, fields))
}

fn flattened_external_table_constructor(
    table: ExternalTableRecordKind,
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Option<rumoca_core::Expression> {
    let field_count = table.flattened_field_count();
    if args.len() < field_count {
        return None;
    }
    if external_table_constructor_call(args.first()?) {
        return None;
    }
    Some(rumoca_core::Expression::FunctionCall {
        name: rumoca_core::Reference::from(table.constructor_name()),
        args: args[..field_count].to_vec(),
        is_constructor: true,
        span,
    })
}

fn dae_variable_dims(variables: &dae::DaeVariables) -> IndexMap<String, Vec<i64>> {
    let mut dims = IndexMap::new();
    for (name, var) in variables
        .states
        .iter()
        .chain(variables.algebraics.iter())
        .chain(variables.outputs.iter())
        .chain(variables.parameters.iter())
        .chain(variables.constants.iter())
        .chain(variables.inputs.iter())
        .chain(variables.discrete_reals.iter())
        .chain(variables.discrete_valued.iter())
    {
        dims.insert(name.as_str().to_string(), var.dims.clone());
    }
    dims
}

#[cfg(test)]
mod tests;
