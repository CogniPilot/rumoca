use super::*;

struct ComplexProjectionComprehensionCtx<'a> {
    indices: &'a [dae::ComprehensionIndex],
    filter: Option<&'a dae::Expression>,
    field: &'a str,
    scope: &'a mut Scope,
    const_scope: &'a mut IndexMap<String, f64>,
    call_depth: usize,
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn try_lower_intrinsic_function_call(
        &mut self,
        name: &dae::VarName,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let call_name = name.as_str();
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
        if let Some(reg) = self.lower_runtime_string_special_intrinsic(call_name, args)? {
            return Ok(Some(reg));
        }
        if let Some(reg) =
            self.lower_external_table_intrinsic(call_name, args, scope, call_depth)?
        {
            return Ok(Some(reg));
        }
        if let Some(builtin) = resolve_intrinsic_builtin(call_name) {
            let reg = self.lower_builtin(builtin, args, scope, call_depth)?;
            return Ok(Some(reg));
        }
        Ok(None)
    }

    pub(super) fn lower_runtime_string_special_intrinsic(
        &mut self,
        call_name: &str,
        args: &[dae::Expression],
    ) -> Result<Option<Reg>, LowerError> {
        // The compiled PR2 evaluator is numeric-only. Keep it aligned with the
        // runtime numeric evaluator for string/file helper calls instead of
        // treating those helpers as unsupported externals on the strict path.
        Ok(lower_runtime_string_special_value(call_name, args).map(|value| self.emit_const(value)))
    }

    fn lower_external_table_intrinsic(
        &mut self,
        call_name: &str,
        args: &[dae::Expression],
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

    fn lower_optional_arg(
        &mut self,
        args: &[dae::Expression],
        idx: usize,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(expr) = args.get(idx) {
            self.lower_expr(expr, scope, call_depth)
        } else {
            Ok(self.emit_const(0.0))
        }
    }

    pub(super) fn lower_complex_math_sum_projection(
        &mut self,
        call_name: &str,
        args: &[dae::Expression],
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
        args: &[dae::Expression],
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
        expr: &dae::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        match expr {
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
                let mut values = Vec::new();
                for element in elements {
                    values.extend(
                        self.lower_complex_projection_values(element, field, scope, call_depth)?,
                    );
                }
                Ok(values)
            }
            dae::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
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
                let projected = dae::Expression::FieldAccess {
                    base: Box::new(expr.clone()),
                    field: field.to_string(),
                };
                Ok(vec![self.lower_expr(&projected, scope, call_depth)?])
            }
        }
    }

    fn lower_complex_projection_comprehension_values(
        &mut self,
        expr: &dae::Expression,
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
            ctx.scope.insert(iter.name.clone(), iter_reg);
            ctx.const_scope.insert(iter.name.clone(), value);
            self.lower_complex_projection_comprehension_values(expr, depth + 1, ctx, out)?;
            ctx.const_scope.shift_remove(&iter.name);
        }
        Ok(())
    }

    pub(super) fn bind_function_inputs(
        &mut self,
        function_name: &dae::VarName,
        inputs: &[dae::FunctionParam],
        args: &[dae::Expression],
        caller_scope: &Scope,
        call_depth: usize,
    ) -> Result<Scope, LowerError> {
        let (named_args, positional_args) =
            split_named_and_positional_call_args(function_name.as_str(), args)?;
        let mut scope = Scope::new();
        let mut positional_idx = 0usize;

        for input in inputs {
            if let Some(arg_expr) = named_args.get(input.name.as_str()) {
                self.bind_function_input_value(
                    &mut scope,
                    input,
                    arg_expr,
                    caller_scope,
                    call_depth + 1,
                )?;
                continue;
            }
            if let Some(arg_expr) = positional_args.get(positional_idx) {
                positional_idx += 1;
                self.bind_function_input_value(
                    &mut scope,
                    input,
                    arg_expr,
                    caller_scope,
                    call_depth + 1,
                )?;
                continue;
            }
            if let Some(default) = input.default.as_ref() {
                let local_scope = scope.clone();
                self.bind_function_input_value(
                    &mut scope,
                    input,
                    default,
                    &local_scope,
                    call_depth + 1,
                )?;
                continue;
            }

            scope.insert(input.name.clone(), self.emit_const(0.0));
        }

        Ok(scope)
    }

    fn bind_function_input_value(
        &mut self,
        scope: &mut Scope,
        input: &dae::FunctionParam,
        expr: &dae::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if is_complex_param(input) {
            self.bind_complex_input(scope, input, expr, expr_scope, call_depth)?;
            return Ok(());
        }
        if !input.dims.is_empty() {
            let values = self.lower_array_like_values(expr, expr_scope, call_depth)?;
            self.bind_assignment_values(scope, &input.name, &values);
            return Ok(());
        }
        let reg = self.lower_expr(expr, expr_scope, call_depth)?;
        scope.insert(input.name.clone(), reg);
        Ok(())
    }

    fn bind_complex_input(
        &mut self,
        scope: &mut Scope,
        input: &dae::FunctionParam,
        expr: &dae::Expression,
        expr_scope: &Scope,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        if let dae::Expression::FieldAccess { field, .. } = expr
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

        let re_expr = dae::Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: "re".to_string(),
        };
        let im_expr = dae::Expression::FieldAccess {
            base: Box::new(expr.clone()),
            field: "im".to_string(),
        };

        if input.dims.is_empty() {
            let re = self.lower_expr(&re_expr, expr_scope, call_depth)?;
            let im = self.lower_expr(&im_expr, expr_scope, call_depth)?;
            scope.insert(input.name.clone(), re);
            scope.insert(format!("{}.re", input.name), re);
            scope.insert(format!("{}.im", input.name), im);
            return Ok(());
        }

        let re_values = self.lower_array_like_values(&re_expr, expr_scope, call_depth)?;
        let im_values = self.lower_array_like_values(&im_expr, expr_scope, call_depth)?;
        self.bind_complex_component_values(scope, &input.name, &re_values, &im_values);
        Ok(())
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
        scope.insert(base_name.to_string(), first);
        self.bind_assignment_values(scope, &format!("{base_name}[:].re"), re_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].im"), im_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].re.re"), re_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].re.im"), &zero_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].im.re"), &zero_values);
        self.bind_assignment_values(scope, &format!("{base_name}[:].im.im"), im_values);
        for (idx, reg) in re_values.iter().copied().enumerate() {
            scope.insert(format!("{base_name}[{}].re", idx + 1), reg);
        }
        for (idx, reg) in im_values.iter().copied().enumerate() {
            scope.insert(format!("{base_name}[{}].im", idx + 1), reg);
        }
    }
}

fn is_complex_param(param: &dae::FunctionParam) -> bool {
    param
        .type_name
        .rsplit('.')
        .next()
        .is_some_and(|leaf| leaf == "Complex")
}

fn parse_complex_sum_projection_field(call_name: &str) -> Option<&str> {
    let suffix = call_name.strip_prefix("Modelica.ComplexMath.sum.")?;
    match suffix {
        "re" | "im" => Some(suffix),
        "result.re" => Some("re"),
        "result.im" => Some("im"),
        _ => None,
    }
}

fn parse_complex_operator_projection(call_name: &str) -> Option<(BinaryOp, &str)> {
    let (base, field) = call_name.rsplit_once('.')?;
    let op = match base {
        "Complex.'+'" => BinaryOp::Add,
        "Complex.'-'" => BinaryOp::Sub,
        "Complex.'*'" => BinaryOp::Mul,
        "Complex.'/'" => BinaryOp::Div,
        _ => return None,
    };
    if matches!(field, "re" | "im") {
        Some((op, field))
    } else {
        None
    }
}

fn lower_runtime_string_special_value(call_name: &str, args: &[dae::Expression]) -> Option<f64> {
    match intrinsic_short_name(call_name) {
        "getInstanceName" | "fullPathName" | "loadResource" | "substring" => Some(0.0),
        "isValidTable" => Some(1.0),
        "isEmpty" => Some(
            literal_string(args.first())
                .map_or(0.0, |s| if s.trim().is_empty() { 1.0 } else { 0.0 }),
        ),
        "length" => Some(literal_string(args.first()).map_or(0.0, |s| s.chars().count() as f64)),
        "find" | "findLast" => Some(find_string_special_value(
            intrinsic_short_name(call_name),
            literal_string(args.first()),
            literal_string(args.get(1)),
        )),
        _ => None,
    }
}

fn literal_string(expr: Option<&dae::Expression>) -> Option<&str> {
    match expr {
        Some(dae::Expression::Literal(dae::Literal::String(value))) => Some(value.as_str()),
        _ => None,
    }
}

fn find_string_special_value(
    short_name: &str,
    haystack: Option<&str>,
    needle: Option<&str>,
) -> f64 {
    let (Some(haystack), Some(needle)) = (haystack, needle) else {
        return 0.0;
    };
    let idx = match short_name {
        "find" => haystack.find(needle),
        "findLast" => haystack.rfind(needle),
        _ => None,
    };
    idx.map(|i| i.saturating_add(1) as f64).unwrap_or(0.0)
}

fn decode_named_function_arg(expr: &dae::Expression) -> Option<(&str, &dae::Expression)> {
    let dae::Expression::FunctionCall {
        name,
        args,
        is_constructor: _,
    } = expr
    else {
        return None;
    };
    let named = name.as_str().strip_prefix(NAMED_FUNCTION_ARG_PREFIX)?;
    let value = args.first()?;
    Some((named, value))
}

pub(super) fn split_named_and_positional_call_args<'a>(
    function_name: &str,
    args: &'a [dae::Expression],
) -> Result<
    (
        IndexMap<String, &'a dae::Expression>,
        Vec<&'a dae::Expression>,
    ),
    LowerError,
> {
    let mut named_args = IndexMap::new();
    let mut positional_args = Vec::new();

    for arg in args {
        if let Some((name, value_expr)) = decode_named_function_arg(arg) {
            if named_args.insert(name.to_string(), value_expr).is_some() {
                return Err(LowerError::InvalidFunction {
                    name: function_name.to_string(),
                    reason: format!("named argument slot `{name}` filled more than once"),
                });
            }
        } else {
            positional_args.push(arg);
        }
    }

    Ok((named_args, positional_args))
}
