use super::*;

fn builtin_has_array_like_lowering(
    function: rumoca_core::BuiltinFunction,
    args: &[rumoca_core::Expression],
) -> bool {
    matches!(
        function,
        rumoca_core::BuiltinFunction::Cat
            | rumoca_core::BuiltinFunction::Der
            | rumoca_core::BuiltinFunction::Pre
            | rumoca_core::BuiltinFunction::Zeros
            | rumoca_core::BuiltinFunction::Ones
            | rumoca_core::BuiltinFunction::Fill
            | rumoca_core::BuiltinFunction::Identity
            | rumoca_core::BuiltinFunction::Diagonal
            | rumoca_core::BuiltinFunction::Transpose
            | rumoca_core::BuiltinFunction::Linspace
            | rumoca_core::BuiltinFunction::Scalar
            | rumoca_core::BuiltinFunction::Vector
            | rumoca_core::BuiltinFunction::Matrix
            | rumoca_core::BuiltinFunction::Cross
            | rumoca_core::BuiltinFunction::Skew
            | rumoca_core::BuiltinFunction::OuterProduct
            | rumoca_core::BuiltinFunction::Symmetric
            | rumoca_core::BuiltinFunction::NoEvent
            | rumoca_core::BuiltinFunction::Smooth
    ) || unary_array_builtin_op(&function).is_some()
        || matches!(function, rumoca_core::BuiltinFunction::Sample)
            && is_array_like_sample_value_form(args)
}

fn required_array_builtin_arg(
    args: &[rumoca_core::Expression],
    function: rumoca_core::BuiltinFunction,
    index: usize,
    call_span: rumoca_core::Span,
) -> Result<&rumoca_core::Expression, LowerError> {
    args.get(index).ok_or_else(|| {
        array_builtin_contract_error(
            format!(
                "{} array lowering requires argument {}",
                function.name(),
                index + 1
            ),
            args.first()
                .and_then(rumoca_core::Expression::span)
                .or_else(|| (!call_span.is_dummy()).then_some(call_span)),
        )
    })
}

fn diagonal_matrix_value(row: usize, col: usize, diagonal: Reg, off_diagonal: Reg) -> Reg {
    if row == col { diagonal } else { off_diagonal }
}

fn array_builtin_args_span(
    args: &[rumoca_core::Expression],
    call_span: rumoca_core::Span,
) -> rumoca_core::Span {
    args.first()
        .and_then(rumoca_core::Expression::span)
        .unwrap_or(call_span)
}

fn expression_or_call_span(
    expr: &rumoca_core::Expression,
    call_span: rumoca_core::Span,
) -> rumoca_core::Span {
    expr.span().unwrap_or(call_span)
}

fn expression_or_call_span_option(
    expr: &rumoca_core::Expression,
    call_span: rumoca_core::Span,
) -> Option<rumoca_core::Span> {
    expr.span()
        .or_else(|| (!call_span.is_dummy()).then_some(call_span))
}

fn array_builtin_contract_error(
    reason: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    let reason = reason.into();
    match span {
        Some(span) => LowerError::contract_violation(reason, span),
        None => LowerError::UnspannedContractViolation { reason },
    }
}

fn require_array_builtin_arity(
    args: &[rumoca_core::Expression],
    function: rumoca_core::BuiltinFunction,
    expected: usize,
    call_span: rumoca_core::Span,
) -> Result<(), LowerError> {
    if args.len() == expected {
        return Ok(());
    }
    Err(array_builtin_arity_error(
        args, function, expected, call_span,
    ))
}

fn array_builtin_arity_error(
    args: &[rumoca_core::Expression],
    function: rumoca_core::BuiltinFunction,
    expected: usize,
    call_span: rumoca_core::Span,
) -> LowerError {
    array_builtin_contract_error(
        format!(
            "{} requires {expected} argument(s), got {}",
            function.name(),
            args.len()
        ),
        args.first()
            .and_then(rumoca_core::Expression::span)
            .or_else(|| (!call_span.is_dummy()).then_some(call_span)),
    )
}

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn lower_builtin_first_array_like_value(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let values =
            self.lower_known_builtin_array_like_values(function, args, scope, call_depth, span)?;
        values.first().copied().ok_or_else(|| {
            array_builtin_contract_error(
                format!(
                    "{} produced no scalar values in scalar context",
                    function.name()
                ),
                Some(span),
            )
        })
    }

    pub(super) fn lower_builtin_array_like_values(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        fallback: &rumoca_core::Expression,
    ) -> Result<Vec<Reg>, LowerError> {
        if builtin_has_array_like_lowering(function, args) {
            let call_span = self.required_builtin_call_span(function, fallback)?;
            self.lower_known_builtin_array_like_values(function, args, scope, call_depth, call_span)
        } else {
            let mut values =
                reg_vec_with_capacity(1, "scalar builtin fallback value count", fallback.span())?;
            values.push(self.lower_expr(fallback, scope, call_depth)?);
            Ok(values)
        }
    }

    fn lower_known_builtin_array_like_values(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        match function {
            rumoca_core::BuiltinFunction::Cat => {
                self.lower_cat_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Der => {
                self.lower_der_array_like_values(args, scope, call_span)
            }
            rumoca_core::BuiltinFunction::Pre => self.reject_pre_array_like_values(args, call_span),
            rumoca_core::BuiltinFunction::Zeros => {
                self.lower_constant_array_builtin(args, 0.0, "zeros", call_span)
            }
            rumoca_core::BuiltinFunction::Ones => {
                self.lower_constant_array_builtin(args, 1.0, "ones", call_span)
            }
            rumoca_core::BuiltinFunction::Fill => {
                self.lower_fill_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Identity => {
                self.lower_identity_array_like_values(args, call_span)
            }
            rumoca_core::BuiltinFunction::Diagonal => {
                self.lower_diagonal_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Transpose => {
                self.lower_transpose_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Linspace => {
                self.lower_linspace_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Scalar => {
                self.lower_scalar_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Vector | rumoca_core::BuiltinFunction::Matrix => {
                let arg = required_array_builtin_arg(args, function, 0, call_span)?;
                self.lower_array_like_values(arg, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Cross => {
                self.lower_cross_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Skew => {
                self.lower_skew_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::OuterProduct => {
                self.lower_outer_product_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::Symmetric => {
                self.lower_symmetric_array_like_values(args, scope, call_depth, call_span)
            }
            rumoca_core::BuiltinFunction::NoEvent => {
                let arg = required_array_builtin_arg(args, function, 0, call_span)?;
                self.lower_array_like_values(arg, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Smooth => {
                let arg = required_array_builtin_arg(args, function, 1, call_span)?;
                self.lower_array_like_values(arg, scope, call_depth)
            }
            function if let Some(op) = unary_array_builtin_op(&function) => {
                let arg = required_array_builtin_arg(args, function, 0, call_span)?;
                let span = expression_or_call_span(arg, call_span);
                let values = self.lower_array_like_values(arg, scope, call_depth)?;
                let mut lowered = Vec::new();
                let context = format!("{} array value count", function.name());
                reserve_reg_capacity(&mut lowered, values.len(), &context, Some(span))?;
                for value in values {
                    lowered.push(self.emit_unary_at(op, value, span)?);
                }
                Ok(lowered)
            }
            rumoca_core::BuiltinFunction::Sample if is_array_like_sample_value_form(args) => {
                let arg = required_array_builtin_arg(args, function, 0, call_span)?;
                self.lower_array_like_values_in_mode(arg, scope, call_depth, ValueMode::Pre)
            }
            _ => Err(array_builtin_contract_error(
                format!(
                    "{} does not have array-like builtin lowering",
                    function.name()
                ),
                args.first()
                    .and_then(rumoca_core::Expression::span)
                    .or_else(|| (!call_span.is_dummy()).then_some(call_span)),
            )),
        }
    }

    fn lower_der_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Err(array_builtin_contract_error(
                "der() array lowering requires one argument",
                (!call_span.is_dummy()).then_some(call_span),
            ));
        };
        let span = expression_or_call_span(arg, call_span);
        let dims = self.infer_expr_dims(arg, scope)?;
        let count = checked_shape_size_or_scalar(&dims, "der() array value count", span)?;
        let mut values = reg_vec_with_capacity(count, "der() array value count", Some(span))?;
        let zero = self.emit_const_at(0.0, span)?;
        values.resize(count, zero);
        Ok(values)
    }

    fn lower_cat_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        if args.len() <= 1 {
            return Ok(Vec::new());
        }
        let dim = self.eval_compile_time_positive_index(
            &args[0],
            &self.local_const_bindings,
            "cat dimension",
        )?;
        let mut operands = Vec::new();
        reserve_reg_capacity(
            &mut operands,
            args.len() - 1,
            "cat operand count",
            Some(array_builtin_args_span(args, call_span)),
        )?;
        for arg in args.iter().skip(1) {
            operands.push(self.lower_array_operand(arg, scope, call_depth)?);
        }
        // MLS §10.4.2.1: cat(k, A, B, ...) concatenates along dimension k.
        // Do not ignore the leading dimension argument; preserve row-major
        // scalar order for the selected concatenation dimension.
        Ok(
            cat_array_operands(dim, &operands, expression_or_call_span(&args[0], call_span))?
                .values,
        )
    }

    fn lower_constant_array_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        value: f64,
        context: &str,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let count = self.shape_element_count(args, context, call_span)?;
        let span = array_builtin_args_span(args, call_span);
        let value_count_context = format!("{context} value count");
        let mut values = reg_vec_with_capacity(count, &value_count_context, Some(span))?;
        let value = self.emit_const_at(value, span)?;
        values.resize(count, value);
        Ok(values)
    }

    fn lower_identity_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let dim =
            required_array_builtin_arg(args, rumoca_core::BuiltinFunction::Identity, 0, call_span)?;
        let n = self.eval_compile_time_positive_index(
            dim,
            &self.local_const_bindings,
            "identity dimension",
        )?;
        let span = expression_or_call_span(dim, call_span);
        let count = checked_matrix_extent(n, n, "identity matrix extent", Some(span))?;
        let mut values = reg_vec_with_capacity(count, "identity matrix value count", Some(span))?;
        let zero = self.emit_const_at(0.0, span)?;
        let one = self.emit_const_at(1.0, span)?;
        for row in 0..n {
            for col in 0..n {
                values.push(diagonal_matrix_value(row, col, one, zero));
            }
        }
        Ok(values)
    }

    fn lower_diagonal_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let arg =
            required_array_builtin_arg(args, rumoca_core::BuiltinFunction::Diagonal, 0, call_span)?;
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        let n = values.len();
        let span = expression_or_call_span(arg, call_span);
        let count = checked_matrix_extent(n, n, "diagonal matrix extent", Some(span))?;
        let mut diagonal = reg_vec_with_capacity(count, "diagonal matrix value count", Some(span))?;
        let zero = self.emit_const_at(0.0, span)?;
        for (row, value) in values.iter().copied().enumerate().take(n) {
            for col in 0..n {
                diagonal.push(diagonal_matrix_value(row, col, value, zero));
            }
        }
        Ok(diagonal)
    }

    fn lower_transpose_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let arg = required_array_builtin_arg(
            args,
            rumoca_core::BuiltinFunction::Transpose,
            0,
            call_span,
        )?;
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        let span = expression_or_call_span(arg, call_span);
        match self.infer_expr_dims(arg, scope)?.as_slice() {
            [] | [_] => Err(unsupported_at(
                "transpose() requires an array with at least two dimensions",
                span,
            )),
            [rows, cols]
                if checked_matrix_extent(
                    *rows,
                    *cols,
                    "transpose matrix extent",
                    expression_or_call_span_option(arg, call_span),
                )? == values.len() =>
            {
                transpose_flat_matrix_regs(
                    &values,
                    *rows,
                    *cols,
                    expression_or_call_span_option(arg, call_span),
                )
            }
            dims => Err(unsupported_at(
                format!(
                    "transpose() requires a matrix input, got shape {}",
                    format_usize_dims(dims)
                ),
                span,
            )),
        }
    }

    fn lower_linspace_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        require_array_builtin_arity(args, rumoca_core::BuiltinFunction::Linspace, 3, call_span)?;
        let [start_expr, stop_expr, count_expr] = args else {
            return Err(array_builtin_arity_error(
                args,
                rumoca_core::BuiltinFunction::Linspace,
                3,
                call_span,
            ));
        };
        let count = self.eval_compile_time_positive_index(
            count_expr,
            &self.local_const_bindings,
            "linspace count",
        )?;
        if count < 2 {
            return Err(unsupported_at(
                "linspace() count must be at least 2",
                expression_or_call_span(count_expr, call_span),
            ));
        }
        let span = start_expr
            .span()
            .or_else(|| stop_expr.span())
            .unwrap_or(call_span);
        let mut values = reg_vec_with_capacity(count, "linspace value count", Some(span))?;
        let start = self.lower_expr(start_expr, scope, call_depth)?;
        let stop = self.lower_expr(stop_expr, scope, call_depth)?;
        let delta = self.emit_binary_at(BinaryOp::Sub, stop, start, span)?;
        let denom = self.emit_const_at((count - 1) as f64, span)?;
        let step = self.emit_binary_at(BinaryOp::Div, delta, denom, span)?;
        for idx in 0..count {
            let offset = self.emit_const_at(idx as f64, span)?;
            let scaled = self.emit_binary_at(BinaryOp::Mul, step, offset, span)?;
            values.push(self.emit_binary_at(BinaryOp::Add, start, scaled, span)?);
        }
        Ok(values)
    }

    fn lower_scalar_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let arg =
            required_array_builtin_arg(args, rumoca_core::BuiltinFunction::Scalar, 0, call_span)?;
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        match values.as_slice() {
            [value] => {
                let mut out = reg_vec_with_capacity(1, "scalar value count", arg.span())?;
                out.push(*value);
                Ok(out)
            }
            _ => Err(unsupported_at(
                format!("scalar() requires exactly one value, got {}", values.len()),
                expression_or_call_span(arg, call_span),
            )),
        }
    }

    fn lower_cross_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let [lhs_expr, rhs_expr] = args else {
            return Err(unsupported_at(
                "cross() requires two vector arguments",
                array_builtin_args_span(args, call_span),
            ));
        };
        let lhs = self.lower_array_like_values(lhs_expr, scope, call_depth)?;
        let rhs = self.lower_array_like_values(rhs_expr, scope, call_depth)?;
        let span = lhs_expr
            .span()
            .or_else(|| rhs_expr.span())
            .unwrap_or(call_span);
        if lhs.len() != 3 || rhs.len() != 3 {
            return Err(unsupported_at(
                format!(
                    "cross() requires two Real[3] vectors, got lengths {} and {}",
                    lhs.len(),
                    rhs.len()
                ),
                span,
            ));
        }
        let mut values = reg_vec_with_capacity(3, "cross value count", Some(span))?;
        let x = self.emit_cross_component(lhs[1], rhs[2], lhs[2], rhs[1], span)?;
        let y = self.emit_cross_component(lhs[2], rhs[0], lhs[0], rhs[2], span)?;
        let z = self.emit_cross_component(lhs[0], rhs[1], lhs[1], rhs[0], span)?;
        values.extend_from_slice(&[x, y, z]);
        Ok(values)
    }

    fn lower_skew_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Err(unsupported_at(
                "skew() requires one vector argument",
                array_builtin_args_span(args, call_span),
            ));
        };
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        let span = expression_or_call_span(arg, call_span);
        if values.len() != 3 {
            return Err(unsupported_at(
                format!(
                    "skew() requires a Real[3] vector, got length {}",
                    values.len()
                ),
                span,
            ));
        }
        let mut skew = reg_vec_with_capacity(9, "skew value count", Some(span))?;
        let zero = self.emit_const_at(0.0, span)?;
        let neg_x = self.emit_unary_at(UnaryOp::Neg, values[0], span)?;
        let neg_y = self.emit_unary_at(UnaryOp::Neg, values[1], span)?;
        let neg_z = self.emit_unary_at(UnaryOp::Neg, values[2], span)?;
        skew.extend_from_slice(&[
            zero, neg_z, values[1], values[2], zero, neg_x, neg_y, values[0], zero,
        ]);
        Ok(skew)
    }

    fn lower_outer_product_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let [lhs_expr, rhs_expr] = args else {
            return Err(unsupported_at(
                "outerProduct() requires two vector arguments",
                array_builtin_args_span(args, call_span),
            ));
        };
        let lhs = self.lower_array_like_values(lhs_expr, scope, call_depth)?;
        let rhs = self.lower_array_like_values(rhs_expr, scope, call_depth)?;
        let span = lhs_expr
            .span()
            .or_else(|| rhs_expr.span())
            .unwrap_or(call_span);
        let value_count_span = lhs_expr
            .span()
            .or_else(|| rhs_expr.span())
            .or_else(|| (!call_span.is_dummy()).then_some(call_span));
        let mut values = Vec::new();
        reserve_reg_capacity(
            &mut values,
            checked_builtin_count_product(
                lhs.len(),
                rhs.len(),
                "outerProduct value count",
                value_count_span,
            )?,
            "outerProduct value count",
            value_count_span,
        )?;
        for lhs_value in lhs {
            for rhs_value in rhs.iter().copied() {
                values.push(self.emit_binary_at(BinaryOp::Mul, lhs_value, rhs_value, span)?);
            }
        }
        Ok(values)
    }

    fn required_builtin_call_span(
        &self,
        function: rumoca_core::BuiltinFunction,
        fallback: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Span, LowerError> {
        fallback
            .span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: format!(
                    "{} array lowering requires source span metadata",
                    function.name()
                ),
            })
    }

    fn lower_symmetric_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let arg = required_array_builtin_arg(
            args,
            rumoca_core::BuiltinFunction::Symmetric,
            0,
            call_span,
        )?;
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        let dims = self.infer_expr_dims(arg, scope)?;
        let span = expression_or_call_span(arg, call_span);
        let [rows, cols] = dims.as_slice() else {
            return Err(unsupported_at(
                format!(
                    "symmetric() requires a square matrix, got shape {}",
                    format_usize_dims(&dims)
                ),
                span,
            ));
        };
        if rows != cols
            || checked_matrix_extent(
                *rows,
                *cols,
                "symmetric matrix extent",
                expression_or_call_span_option(arg, call_span),
            )? != values.len()
        {
            return Err(unsupported_at(
                format!(
                    "symmetric() requires a square matrix, got shape {}",
                    format_usize_dims(&dims)
                ),
                span,
            ));
        }
        let n = *rows;
        let mut symmetric = Vec::new();
        reserve_reg_capacity(
            &mut symmetric,
            values.len(),
            "symmetric matrix value count",
            Some(span),
        )?;
        for row in 0..n {
            for col in 0..n {
                symmetric.push(values[symmetric_source_index(row, col, n)]);
            }
        }
        Ok(symmetric)
    }

    fn emit_cross_component(
        &mut self,
        lhs_a: Reg,
        rhs_a: Reg,
        lhs_b: Reg,
        rhs_b: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let first = self.emit_binary_at(BinaryOp::Mul, lhs_a, rhs_a, span)?;
        let second = self.emit_binary_at(BinaryOp::Mul, lhs_b, rhs_b, span)?;
        self.emit_binary_at(BinaryOp::Sub, first, second, span)
    }

    fn shape_element_count(
        &self,
        dims: &[rumoca_core::Expression],
        context: &str,
        call_span: rumoca_core::Span,
    ) -> Result<usize, LowerError> {
        if dims.is_empty() {
            return Err(array_builtin_contract_error(
                format!("{context}() requires at least one dimension argument"),
                (!call_span.is_dummy()).then_some(call_span),
            ));
        }
        let const_scope = &self.local_const_bindings;
        let mut count = 1usize;
        for dim in dims {
            let dim_value = self.eval_compile_time_int(dim, const_scope, context)?;
            let span = expression_or_call_span(dim, call_span);
            let dim_value =
                checked_non_negative_dimension(dim_value, &format!("{context} dimension"), span)?;
            count = checked_builtin_count_product(
                count,
                dim_value,
                &format!("{context} dimensions"),
                Some(span),
            )?;
        }
        Ok(count)
    }

    fn reject_pre_array_like_values(
        &mut self,
        _args: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        Err(unsupported_at(
            "pre() must be lowered to __pre__ parameters before Solve-IR lowering",
            array_builtin_args_span(_args, call_span),
        ))
    }

    fn lower_fill_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        if args.len() < 2 {
            return Err(array_builtin_contract_error(
                format!("fill() requires at least 2 arguments, got {}", args.len()),
                args.first()
                    .and_then(rumoca_core::Expression::span)
                    .or_else(|| (!call_span.is_dummy()).then_some(call_span)),
            ));
        }
        let value_expr = &args[0];
        let count = self.fill_element_count(&args[1..], call_span)?;
        let mut values = reg_vec_with_capacity(count, "fill value count", value_expr.span())?;
        let value = self.lower_expr(value_expr, scope, call_depth)?;
        values.resize(count, value);
        Ok(values)
    }

    fn fill_element_count(
        &self,
        dims: &[rumoca_core::Expression],
        call_span: rumoca_core::Span,
    ) -> Result<usize, LowerError> {
        if dims.is_empty() {
            return Err(array_builtin_contract_error(
                "fill() requires at least one dimension argument",
                (!call_span.is_dummy()).then_some(call_span),
            ));
        }
        let const_scope = &self.local_const_bindings;
        let mut count = 1usize;
        for dim in dims {
            let dim_value = self.eval_compile_time_int(dim, const_scope, "fill dimension")?;
            let span = expression_or_call_span(dim, call_span);
            let dim_value = checked_non_negative_dimension(dim_value, "fill dimension", span)?;
            count = checked_builtin_count_product(count, dim_value, "fill dimensions", Some(span))?;
        }
        Ok(count)
    }

    pub(super) fn lower_synchronous_array_like_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        call_span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let arg = args.first().ok_or_else(|| {
            array_builtin_contract_error(
                format!("{call_name} array lowering requires one argument"),
                (!call_span.is_dummy()).then_some(call_span),
            )
        })?;
        match intrinsic_short_name(call_name) {
            // MLS §16.4 / §16.5.1: previous(v) is element-wise for array
            // arguments and reads the previous clock tick value.
            "previous" => {
                self.lower_array_like_values_in_mode(arg, scope, call_depth, ValueMode::Pre)
            }
            // MLS §16.5.1: value-form clock conversion operators preserve the
            // sampled array value shape; scheduling is represented elsewhere.
            "hold" | "noClock" | "subSample" | "superSample" | "shiftSample" | "backSample" => {
                self.lower_array_like_values(arg, scope, call_depth)
            }
            _ => {
                let mut values =
                    reg_vec_with_capacity(1, "synchronous intrinsic value count", arg.span())?;
                values.push(self.lower_expr(arg, scope, call_depth)?);
                Ok(values)
            }
        }
    }
}

fn transpose_flat_matrix_regs(
    values: &[Reg],
    rows: usize,
    cols: usize,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<Reg>, LowerError> {
    let mut transposed = Vec::new();
    reserve_reg_capacity(
        &mut transposed,
        values.len(),
        "transpose matrix value count",
        span,
    )?;
    for row in 0..cols {
        for col in 0..rows {
            transposed.push(values[col * cols + row]);
        }
    }
    Ok(transposed)
}

fn symmetric_source_index(row: usize, col: usize, n: usize) -> usize {
    row.max(col) * n + row.min(col)
}

fn checked_matrix_extent(
    rows: usize,
    cols: usize,
    context: &str,
    span: Option<rumoca_core::Span>,
) -> Result<usize, LowerError> {
    rows.checked_mul(cols).ok_or_else(|| {
        array_builtin_contract_error(format!("{context} overflows host index range"), span)
    })
}

fn checked_non_negative_dimension(
    value: i64,
    context: &str,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    if value < 0 {
        return Err(unsupported_at(
            format!("{context} must be non-negative"),
            span,
        ));
    }
    usize::try_from(value).map_err(|_| {
        LowerError::contract_violation(format!("{context} {value} exceeds host index range"), span)
    })
}

fn checked_builtin_count_product(
    lhs: usize,
    rhs: usize,
    context: &str,
    span: Option<rumoca_core::Span>,
) -> Result<usize, LowerError> {
    lhs.checked_mul(rhs).ok_or_else(|| {
        array_builtin_contract_error(format!("{context} overflow solve-IR row count"), span)
    })
}

fn reserve_reg_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &str,
    span: Option<rumoca_core::Span>,
) -> Result<(), LowerError> {
    values.try_reserve_exact(capacity).map_err(|_| {
        array_builtin_contract_error(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn reg_vec_with_capacity(
    count: usize,
    context: &str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<Reg>, LowerError> {
    let mut values = Vec::new();
    reserve_reg_capacity(&mut values, count, context, span)?;
    Ok(values)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_like_builtin_lowering_rejects_non_array_dispatch_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_builtins_source_23.mo",
            ),
            2,
            8,
        );
        let arg = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span,
        };
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let err = builder
            .lower_known_builtin_array_like_values(
                rumoca_core::BuiltinFunction::Min,
                &[arg],
                &Scope::new(),
                0,
                span,
            )
            .expect_err("non-array builtin must not reach array-like lowering");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason()
                .contains("min does not have array-like builtin lowering"),
            "{err:?}"
        );
        assert!(!err.reason().contains("Min"), "{err:?}");
    }

    #[test]
    fn array_like_builtin_missing_arg_uses_available_arg_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_builtins_source_24.mo",
            ),
            5,
            9,
        );
        let arg = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(0),
            span,
        };
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let err = builder
            .lower_known_builtin_array_like_values(
                rumoca_core::BuiltinFunction::Smooth,
                &[arg],
                &Scope::new(),
                0,
                span,
            )
            .expect_err("smooth() array lowering must require the value argument");

        assert_eq!(err.source_span(), Some(span));
        assert!(
            err.reason()
                .contains("smooth array lowering requires argument 2"),
            "{err:?}"
        );
        assert!(!err.reason().contains("Smooth"), "{err:?}");
    }

    #[test]
    fn array_like_builtin_missing_arg_uses_call_span_without_args() {
        let call_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("missing_arg_builtin.mo"),
            10,
            18,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let result = builder.lower_known_builtin_array_like_values(
            rumoca_core::BuiltinFunction::Identity,
            &[],
            &Scope::new(),
            0,
            call_span,
        );
        let Err(err) = result else {
            panic!("identity() without a dimension must fail");
        };

        assert_eq!(err.source_span(), Some(call_span));
        assert!(
            err.reason()
                .contains("identity array lowering requires argument 1"),
            "{err:?}"
        );
    }

    #[test]
    fn array_like_builtin_missing_arg_does_not_fabricate_dummy_span() {
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let result = builder.lower_known_builtin_array_like_values(
            rumoca_core::BuiltinFunction::Identity,
            &[],
            &Scope::new(),
            0,
            rumoca_core::Span::DUMMY,
        );
        let Err(err) = result else {
            panic!("identity() without a dimension must fail");
        };

        assert_eq!(err.source_span(), None);
        assert!(
            err.reason()
                .contains("identity array lowering requires argument 1"),
            "{err:?}"
        );
    }

    #[test]
    fn constant_array_builtin_missing_dims_uses_call_span() {
        let call_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("zeros_empty.mo"),
            4,
            11,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let result = builder.lower_known_builtin_array_like_values(
            rumoca_core::BuiltinFunction::Zeros,
            &[],
            &Scope::new(),
            0,
            call_span,
        );
        let Err(err) = result else {
            panic!("zeros() without dimensions must fail");
        };

        assert_eq!(err.source_span(), Some(call_span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: zeros() requires at least one dimension argument"
        );
    }

    #[test]
    fn linspace_arity_error_uses_call_span_without_args() {
        let call_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("linspace_empty.mo"),
            21,
            32,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let result = builder.lower_known_builtin_array_like_values(
            rumoca_core::BuiltinFunction::Linspace,
            &[],
            &Scope::new(),
            0,
            call_span,
        );
        let Err(err) = result else {
            panic!("linspace() without arguments must fail");
        };

        assert_eq!(err.source_span(), Some(call_span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: linspace requires 3 argument(s), got 0"
        );
    }

    #[test]
    fn synchronous_array_intrinsic_missing_arg_uses_call_span() {
        let call_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("previous_empty.mo"),
            6,
            16,
        );
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let result = builder.lower_synchronous_array_like_intrinsic(
            "previous",
            &[],
            &Scope::new(),
            0,
            call_span,
        );
        let Err(err) = result else {
            panic!("previous() without an argument must fail");
        };

        assert_eq!(err.source_span(), Some(call_span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: previous array lowering requires one argument"
        );
    }

    #[test]
    fn transpose_rejects_scalar_input_with_arg_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("transpose_scalar.mo"),
            3,
            10,
        );
        let arg = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span,
        };
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let err = builder
            .lower_known_builtin_array_like_values(
                rumoca_core::BuiltinFunction::Transpose,
                &[arg],
                &Scope::new(),
                0,
                span,
            )
            .expect_err("transpose() scalar input must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "transpose() requires an array with at least two dimensions"
        );
    }

    #[test]
    fn linspace_rejects_short_count_with_count_span() {
        let count_span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("linspace_count.mo"),
            12,
            13,
        );
        let start = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(0.0),
            span: count_span,
        };
        let stop = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Real(1.0),
            span: count_span,
        };
        let count = rumoca_core::Expression::Literal {
            value: rumoca_core::Literal::Integer(1),
            span: count_span,
        };
        let layout = VarLayout::default();
        let functions = IndexMap::new();
        let mut builder = LowerBuilder::new(&layout, &functions);

        let err = builder
            .lower_known_builtin_array_like_values(
                rumoca_core::BuiltinFunction::Linspace,
                &[start, stop, count],
                &Scope::new(),
                0,
                count_span,
            )
            .expect_err("linspace() count below two must fail");

        assert_eq!(err.source_span(), Some(count_span));
        assert_eq!(err.reason(), "linspace() count must be at least 2");
    }

    #[test]
    fn checked_non_negative_dimension_rejects_negative_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_builtins_source_25.mo",
            ),
            1,
            4,
        );

        let err = checked_non_negative_dimension(-1, "fill dimension", span)
            .expect_err("negative dimension must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "fill dimension must be non-negative");
    }

    #[test]
    fn checked_builtin_count_product_rejects_overflow_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_builtins_source_26.mo",
            ),
            9,
            14,
        );

        let err = checked_builtin_count_product(usize::MAX, 2, "fill dimensions", Some(span))
            .expect_err("dimension product overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: fill dimensions overflow solve-IR row count"
        );
    }

    #[test]
    fn checked_builtin_count_product_rejects_overflow_without_span() {
        let err = checked_builtin_count_product(usize::MAX, 2, "fill dimensions", None)
            .expect_err("dimension product overflow must fail");

        assert_eq!(err.source_span(), None);
        assert_eq!(
            err.reason(),
            "invalid IR contract: fill dimensions overflow solve-IR row count"
        );
    }

    #[test]
    fn reserve_reg_capacity_rejects_impossible_capacity_with_span() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_builtins_source_27.mo",
            ),
            3,
            12,
        );
        let mut values = Vec::<Reg>::new();

        let err = reserve_reg_capacity(
            &mut values,
            usize::MAX,
            "outerProduct value count",
            Some(span),
        )
        .expect_err("impossible vector capacity must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: outerProduct value count capacity exceeds host memory limits"
        );
    }

    #[test]
    fn reserve_reg_capacity_rejects_impossible_capacity_without_dummy_span() {
        let mut values = Vec::<Reg>::new();

        let err = reserve_reg_capacity(
            &mut values,
            usize::MAX,
            "outerProduct value count",
            Some(rumoca_core::Span::DUMMY),
        )
        .expect_err("impossible vector capacity must fail");

        assert_eq!(err.source_span(), None);
        assert_eq!(
            err.reason(),
            "invalid IR contract: outerProduct value count capacity exceeds host memory limits"
        );
    }

    #[test]
    fn checked_matrix_extent_rejects_overflow() {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name(
                "phase_solve_lower_array_values_builtins_source_28.mo",
            ),
            4,
            10,
        );

        let err = checked_matrix_extent(usize::MAX, 2, "transpose matrix extent", Some(span))
            .expect_err("matrix extent overflow must fail");

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid IR contract: transpose matrix extent overflows host index range"
        );
    }
}
