use super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_binary(
        &mut self,
        op: rumoca_core::OpBinary,
        lhs: Reg,
        rhs: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        match op {
            rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => {
                self.emit_binary_at(BinaryOp::Add, lhs, rhs, span)
            }
            rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => {
                self.emit_binary_at(BinaryOp::Sub, lhs, rhs, span)
            }
            rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => {
                self.emit_binary_at(BinaryOp::Mul, lhs, rhs, span)
            }
            rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
                self.emit_binary_at(BinaryOp::Div, lhs, rhs, span)
            }
            rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => {
                self.emit_binary_at(BinaryOp::Pow, lhs, rhs, span)
            }
            rumoca_core::OpBinary::And => self.emit_binary_at(BinaryOp::And, lhs, rhs, span),
            rumoca_core::OpBinary::Or => self.emit_binary_at(BinaryOp::Or, lhs, rhs, span),
            rumoca_core::OpBinary::Lt => self.emit_compare_at(CompareOp::Lt, lhs, rhs, span),
            rumoca_core::OpBinary::Le => self.emit_compare_at(CompareOp::Le, lhs, rhs, span),
            rumoca_core::OpBinary::Gt => self.emit_compare_at(CompareOp::Gt, lhs, rhs, span),
            rumoca_core::OpBinary::Ge => self.emit_compare_at(CompareOp::Ge, lhs, rhs, span),
            rumoca_core::OpBinary::Eq => self.emit_compare_at(CompareOp::Eq, lhs, rhs, span),
            rumoca_core::OpBinary::Neq => self.emit_compare_at(CompareOp::Ne, lhs, rhs, span),
            rumoca_core::OpBinary::Assign | rumoca_core::OpBinary::Empty => {
                Err(LowerError::Unsupported {
                    reason: format!("binary operator `{op}` is unsupported"),
                })
            }
        }
    }

    pub(super) fn lower_unary(
        &mut self,
        op: rumoca_core::OpUnary,
        rhs: Reg,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        match op {
            rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => {
                self.emit_unary_at(UnaryOp::Neg, rhs, span)
            }
            rumoca_core::OpUnary::Not => self.emit_unary_at(UnaryOp::Not, rhs, span),
            rumoca_core::OpUnary::Plus
            | rumoca_core::OpUnary::DotPlus
            | rumoca_core::OpUnary::Empty => Ok(rhs),
        }
    }

    fn lower_simple_builtin(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        span: rumoca_core::Span,
    ) -> Option<Result<Reg, LowerError>> {
        let arg = |builder: &mut Self, idx: usize| -> Result<Reg, LowerError> {
            let expr = args.get(idx).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "builtin {} requires argument at index {}, but only {} were provided",
                        function.name(),
                        idx,
                        args.len()
                    ),
                    span,
                )
            })?;
            builder.lower_expr(expr, scope, call_depth)
        };
        let unary = |builder: &mut Self, op: UnaryOp| -> Result<Reg, LowerError> {
            let x = arg(builder, 0)?;
            builder.emit_unary_at(op, x, span)
        };
        let binary = |builder: &mut Self, op: BinaryOp| -> Result<Reg, LowerError> {
            let x = arg(builder, 0)?;
            let y = arg(builder, 1)?;
            builder.emit_binary_at(op, x, y, span)
        };

        let result = match function {
            rumoca_core::BuiltinFunction::Abs => unary(self, UnaryOp::Abs),
            rumoca_core::BuiltinFunction::Sign => unary(self, UnaryOp::Sign),
            rumoca_core::BuiltinFunction::Sqrt => unary(self, UnaryOp::Sqrt),
            rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
                unary(self, UnaryOp::Floor)
            }
            rumoca_core::BuiltinFunction::Ceil => unary(self, UnaryOp::Ceil),
            rumoca_core::BuiltinFunction::Sin => unary(self, UnaryOp::Sin),
            rumoca_core::BuiltinFunction::Cos => unary(self, UnaryOp::Cos),
            rumoca_core::BuiltinFunction::Tan => unary(self, UnaryOp::Tan),
            rumoca_core::BuiltinFunction::Asin => unary(self, UnaryOp::Asin),
            rumoca_core::BuiltinFunction::Acos => unary(self, UnaryOp::Acos),
            rumoca_core::BuiltinFunction::Atan => unary(self, UnaryOp::Atan),
            rumoca_core::BuiltinFunction::Sinh => unary(self, UnaryOp::Sinh),
            rumoca_core::BuiltinFunction::Cosh => unary(self, UnaryOp::Cosh),
            rumoca_core::BuiltinFunction::Tanh => unary(self, UnaryOp::Tanh),
            rumoca_core::BuiltinFunction::Exp => unary(self, UnaryOp::Exp),
            rumoca_core::BuiltinFunction::Log => unary(self, UnaryOp::Log),
            rumoca_core::BuiltinFunction::Log10 => unary(self, UnaryOp::Log10),
            rumoca_core::BuiltinFunction::Atan2 => binary(self, BinaryOp::Atan2),
            rumoca_core::BuiltinFunction::Div => {
                self.lower_div_builtin(args, scope, call_depth, span)
            }
            rumoca_core::BuiltinFunction::Mod => {
                self.lower_remainder_builtin(args, scope, call_depth, span, UnaryOp::Floor, "mod")
            }
            rumoca_core::BuiltinFunction::Rem => {
                self.lower_remainder_builtin(args, scope, call_depth, span, UnaryOp::Trunc, "rem")
            }
            rumoca_core::BuiltinFunction::NoEvent => arg(self, 0),
            rumoca_core::BuiltinFunction::Homotopy => {
                arg(self, usize::from(self.is_initial_mode && args.len() > 1))
            }
            rumoca_core::BuiltinFunction::Smooth => arg(self, 1),
            rumoca_core::BuiltinFunction::Zeros => self.emit_const_at(0.0, span),
            rumoca_core::BuiltinFunction::Ones => self.emit_const_at(1.0, span),
            rumoca_core::BuiltinFunction::Fill
            | rumoca_core::BuiltinFunction::Scalar
            | rumoca_core::BuiltinFunction::Vector
            | rumoca_core::BuiltinFunction::Matrix
            | rumoca_core::BuiltinFunction::Diagonal
            | rumoca_core::BuiltinFunction::Transpose
            | rumoca_core::BuiltinFunction::Linspace
            | rumoca_core::BuiltinFunction::Cat
            | rumoca_core::BuiltinFunction::Cross
            | rumoca_core::BuiltinFunction::Skew
            | rumoca_core::BuiltinFunction::OuterProduct
            | rumoca_core::BuiltinFunction::Symmetric => {
                self.lower_builtin_first_array_like_value(function, args, scope, call_depth, span)
            }
            rumoca_core::BuiltinFunction::Ndims => {
                let [arg] = args else {
                    return Some(Err(LowerError::contract_violation(
                        format!("ndims() requires exactly 1 argument, got {}", args.len()),
                        span,
                    )));
                };
                let dims = match self.infer_expr_dims(arg, scope) {
                    Ok(dims) => dims,
                    Err(err) => return Some(Err(err)),
                };
                self.emit_const_at(dims.len() as f64, span)
            }
            rumoca_core::BuiltinFunction::Identity => self.emit_const_at(1.0, span),
            _ => return None,
        };
        Some(result)
    }

    pub(super) fn lower_div_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        if args.len() != 2 {
            return Err(LowerError::contract_violation(
                format!("div() requires exactly 2 arguments, got {}", args.len()),
                span,
            ));
        }
        let x = self.lower_expr(&args[0], scope, call_depth)?;
        let y = self.lower_expr(&args[1], scope, call_depth)?;
        let q = self.emit_binary_at(BinaryOp::Div, x, y, span)?;
        self.emit_unary_at(UnaryOp::Trunc, q, span)
    }

    fn lower_remainder_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
        span: rumoca_core::Span,
        quotient_rounding: UnaryOp,
        function_name: &str,
    ) -> Result<Reg, LowerError> {
        if args.len() != 2 {
            return Err(LowerError::contract_violation(
                format!(
                    "{function_name}() requires exactly 2 arguments, got {}",
                    args.len()
                ),
                span,
            ));
        }
        let x = self.lower_expr(&args[0], scope, call_depth)?;
        let y = self.lower_expr(&args[1], scope, call_depth)?;
        let q = self.emit_binary_at(BinaryOp::Div, x, y, span)?;
        let q_rounded = self.emit_unary_at(quotient_rounding, q, span)?;
        let product = self.emit_binary_at(BinaryOp::Mul, q_rounded, y, span)?;
        self.emit_binary_at(BinaryOp::Sub, x, product, span)
    }

    pub(super) fn lower_builtin(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(result) = self.lower_simple_builtin(function, args, scope, call_depth, span) {
            return result;
        }

        let arg = |builder: &mut Self, idx: usize| -> Result<Reg, LowerError> {
            let expr = args.get(idx).ok_or_else(|| {
                LowerError::contract_violation(
                    format!(
                        "builtin {} requires argument at index {}, but only {} were provided",
                        function.name(),
                        idx,
                        args.len()
                    ),
                    span,
                )
            })?;
            builder.lower_expr(expr, scope, call_depth)
        };

        match function {
            rumoca_core::BuiltinFunction::Min => self.lower_min_max_builtin(
                args,
                scope,
                call_depth,
                span,
                BinaryOp::Min,
                f64::INFINITY,
            ),
            rumoca_core::BuiltinFunction::Max => self.lower_min_max_builtin(
                args,
                scope,
                call_depth,
                span,
                BinaryOp::Max,
                f64::NEG_INFINITY,
            ),
            // MLS §3.7.2: delay(expr, delayTime) reads expr at a previous
            // time instant. During event iteration it must not feed the
            // current unknown value back into the same discrete solve.
            rumoca_core::BuiltinFunction::Delay => {
                if args.is_empty() {
                    return Err(LowerError::contract_violation(
                        "delay() requires at least 1 argument",
                        span,
                    ));
                }
                self.lower_expr_in_mode(&args[0], scope, call_depth, ValueMode::Pre)
            }
            rumoca_core::BuiltinFunction::SemiLinear => {
                let x = arg(self, 0)?;
                let k1 = arg(self, 1)?;
                let k2 = arg(self, 2)?;
                let zero = self.emit_const_at(0.0, span)?;
                let cond = self.emit_compare_at(CompareOp::Ge, x, zero, span)?;
                let pos = self.emit_binary_at(BinaryOp::Mul, k1, x, span)?;
                let neg = self.emit_binary_at(BinaryOp::Mul, k2, x, span)?;
                self.emit_select_at(cond, pos, neg, span)
            }
            rumoca_core::BuiltinFunction::Der => self.emit_const_at(0.0, span),
            rumoca_core::BuiltinFunction::Pre => Err(unsupported_at(
                "pre() must be lowered to __pre__ parameters before Solve-IR lowering",
                span,
            )),
            rumoca_core::BuiltinFunction::Edge => {
                self.lower_edge_builtin(args, span, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Change => {
                self.lower_change_builtin(args, span, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Initial => self.lower_initial_builtin(span),
            // MLS §8.6: terminal() is false during ordinary simulation
            // evaluation; terminal-event handling can override this phase
            // marker explicitly when that event is implemented.
            rumoca_core::BuiltinFunction::Terminal => self.emit_const_at(0.0, span),
            rumoca_core::BuiltinFunction::Sum => {
                self.lower_sum_builtin(args, span, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Product => {
                self.lower_product_builtin(args, span, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Size => {
                self.lower_size_builtin(args, span, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Sample => {
                self.lower_sample_builtin(args, span, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Reinit => Err(unsupported_at(
                "reinit() must be converted to event update equations before Solve-IR row lowering",
                span,
            )),
            _ => Err(LowerError::contract_violation(
                format!(
                    "builtin {} did not match solve lowering dispatch",
                    function.name()
                ),
                span,
            )),
        }
    }
}
