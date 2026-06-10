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

impl<'a> LowerBuilder<'a> {
    pub(in crate::lower) fn lower_builtin_first_array_like_value(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let values =
            self.lower_known_builtin_array_like_values(function, args, scope, call_depth)?;
        Ok(values
            .first()
            .copied()
            .unwrap_or_else(|| self.emit_const(0.0)))
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
            self.lower_known_builtin_array_like_values(function, args, scope, call_depth)
        } else {
            Ok(vec![self.lower_expr(fallback, scope, call_depth)?])
        }
    }

    fn lower_known_builtin_array_like_values(
        &mut self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        match function {
            rumoca_core::BuiltinFunction::Cat => {
                self.lower_cat_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Der => self.lower_der_array_like_values(args, scope),
            rumoca_core::BuiltinFunction::Pre => self.reject_pre_array_like_values(args),
            rumoca_core::BuiltinFunction::Zeros => {
                self.lower_constant_array_builtin(args, 0.0, "zeros")
            }
            rumoca_core::BuiltinFunction::Ones => {
                self.lower_constant_array_builtin(args, 1.0, "ones")
            }
            rumoca_core::BuiltinFunction::Fill => {
                self.lower_fill_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Identity => self.lower_identity_array_like_values(args),
            rumoca_core::BuiltinFunction::Diagonal => {
                self.lower_diagonal_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Transpose => {
                self.lower_transpose_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Linspace => {
                self.lower_linspace_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Scalar => {
                self.lower_scalar_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Vector | rumoca_core::BuiltinFunction::Matrix => args
                .first()
                .map(|arg| self.lower_array_like_values(arg, scope, call_depth))
                .unwrap_or_else(|| Ok(vec![self.emit_const(0.0)])),
            rumoca_core::BuiltinFunction::Cross => {
                self.lower_cross_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Skew => {
                self.lower_skew_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::OuterProduct => {
                self.lower_outer_product_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::Symmetric => {
                self.lower_symmetric_array_like_values(args, scope, call_depth)
            }
            rumoca_core::BuiltinFunction::NoEvent => args
                .first()
                .map(|arg| self.lower_array_like_values(arg, scope, call_depth))
                .unwrap_or_else(|| Ok(vec![self.emit_const(0.0)])),
            rumoca_core::BuiltinFunction::Smooth => args
                .get(1)
                .map(|arg| self.lower_array_like_values(arg, scope, call_depth))
                .unwrap_or_else(|| Ok(vec![self.emit_const(0.0)])),
            rumoca_core::BuiltinFunction::Sample if is_array_like_sample_value_form(args) => args
                .first()
                .map(|arg| {
                    self.lower_array_like_values_in_mode(arg, scope, call_depth, ValueMode::Pre)
                })
                .unwrap_or_else(|| Ok(vec![self.emit_const(0.0)])),
            function => match unary_array_builtin_op(&function) {
                Some(op) => args
                    .first()
                    .map(|arg| {
                        self.lower_array_like_values(arg, scope, call_depth)
                            .map(|values| {
                                values
                                    .into_iter()
                                    .map(|value| self.emit_unary(op, value))
                                    .collect()
                            })
                    })
                    .unwrap_or_else(|| Ok(vec![self.emit_const(0.0)])),
                None => unreachable!("non-array builtin handled by scalar fallback"),
            },
        }
    }

    fn lower_der_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
    ) -> Result<Vec<Reg>, LowerError> {
        let dims = args
            .first()
            .map(|arg| self.infer_expr_dims(arg, scope))
            .unwrap_or_default();
        let count = shape_size_or_scalar(&dims);
        let zero = self.emit_const(0.0);
        Ok(vec![zero; count])
    }

    fn lower_cat_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        if args.len() <= 1 {
            return Ok(Vec::new());
        }
        let dim = self.eval_compile_time_positive_index(
            &args[0],
            &self.local_const_bindings,
            "cat dimension",
        )?;
        let operands = args
            .iter()
            .skip(1)
            .map(|arg| self.lower_array_operand(arg, scope, call_depth))
            .collect::<Result<Vec<_>, _>>()?;
        // MLS §10.4.2.1: cat(k, A, B, ...) concatenates along dimension k.
        // Do not ignore the leading dimension argument; preserve row-major
        // scalar order for the selected concatenation dimension.
        Ok(cat_array_operands(dim, &operands)?.values)
    }

    fn lower_constant_array_builtin(
        &mut self,
        args: &[rumoca_core::Expression],
        value: f64,
        context: &str,
    ) -> Result<Vec<Reg>, LowerError> {
        let count = self.shape_element_count(args, context)?;
        let value = self.emit_const(value);
        Ok(vec![value; count])
    }

    fn lower_identity_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(dim) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let n = self.eval_compile_time_positive_index(
            dim,
            &self.local_const_bindings,
            "identity dimension",
        )?;
        let zero = self.emit_const(0.0);
        let one = self.emit_const(1.0);
        Ok((0..n)
            .flat_map(|row| (0..n).map(move |col| if row == col { one } else { zero }))
            .collect())
    }

    fn lower_diagonal_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        let n = values.len();
        let zero = self.emit_const(0.0);
        Ok((0..n)
            .flat_map(|row| {
                let values = &values;
                (0..n).map(move |col| if row == col { values[row] } else { zero })
            })
            .collect())
    }

    fn lower_transpose_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        match self.infer_expr_dims(arg, scope).as_slice() {
            [] | [_] => Ok(values),
            [rows, cols] if values.len() == rows * cols => {
                Ok(transpose_flat_matrix_regs(&values, *rows, *cols))
            }
            dims => Err(LowerError::Unsupported {
                reason: format!("transpose requires scalar, vector, or matrix input, got {dims:?}"),
            }),
        }
    }

    fn lower_linspace_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let [start_expr, stop_expr, count_expr] = args else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let count = self.eval_compile_time_positive_index(
            count_expr,
            &self.local_const_bindings,
            "linspace count",
        )?;
        if count == 1 {
            return Ok(vec![self.lower_expr(start_expr, scope, call_depth)?]);
        }
        let start = self.lower_expr(start_expr, scope, call_depth)?;
        let stop = self.lower_expr(stop_expr, scope, call_depth)?;
        let delta = self.emit_binary(BinaryOp::Sub, stop, start);
        let denom = self.emit_const((count - 1) as f64);
        let step = self.emit_binary(BinaryOp::Div, delta, denom);
        let mut values = Vec::with_capacity(count);
        for idx in 0..count {
            let offset = self.emit_const(idx as f64);
            let scaled = self.emit_binary(BinaryOp::Mul, step, offset);
            values.push(self.emit_binary(BinaryOp::Add, start, scaled));
        }
        Ok(values)
    }

    fn lower_scalar_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        match values.as_slice() {
            [value] => Ok(vec![*value]),
            _ => Err(LowerError::Unsupported {
                reason: format!("scalar() requires exactly one value, got {}", values.len()),
            }),
        }
    }

    fn lower_cross_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let [lhs_expr, rhs_expr] = args else {
            return Err(LowerError::Unsupported {
                reason: "cross() requires two vector arguments".to_string(),
            });
        };
        let lhs = self.lower_array_like_values(lhs_expr, scope, call_depth)?;
        let rhs = self.lower_array_like_values(rhs_expr, scope, call_depth)?;
        if lhs.len() != 3 || rhs.len() != 3 {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "cross() requires two Real[3] vectors, got lengths {} and {}",
                    lhs.len(),
                    rhs.len()
                ),
            });
        }
        Ok(vec![
            self.emit_cross_component(lhs[1], rhs[2], lhs[2], rhs[1]),
            self.emit_cross_component(lhs[2], rhs[0], lhs[0], rhs[2]),
            self.emit_cross_component(lhs[0], rhs[1], lhs[1], rhs[0]),
        ])
    }

    fn lower_skew_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Err(LowerError::Unsupported {
                reason: "skew() requires one vector argument".to_string(),
            });
        };
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        if values.len() != 3 {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "skew() requires a Real[3] vector, got length {}",
                    values.len()
                ),
            });
        }
        let zero = self.emit_const(0.0);
        let neg_x = self.emit_unary(UnaryOp::Neg, values[0]);
        let neg_y = self.emit_unary(UnaryOp::Neg, values[1]);
        let neg_z = self.emit_unary(UnaryOp::Neg, values[2]);
        Ok(vec![
            zero, neg_z, values[1], values[2], zero, neg_x, neg_y, values[0], zero,
        ])
    }

    fn lower_outer_product_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let [lhs_expr, rhs_expr] = args else {
            return Err(LowerError::Unsupported {
                reason: "outerProduct() requires two vector arguments".to_string(),
            });
        };
        let lhs = self.lower_array_like_values(lhs_expr, scope, call_depth)?;
        let rhs = self.lower_array_like_values(rhs_expr, scope, call_depth)?;
        let mut values = Vec::with_capacity(lhs.len() * rhs.len());
        for lhs_value in lhs {
            values.extend(
                rhs.iter()
                    .copied()
                    .map(|rhs_value| self.emit_binary(BinaryOp::Mul, lhs_value, rhs_value)),
            );
        }
        Ok(values)
    }

    fn lower_symmetric_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let values = self.lower_array_like_values(arg, scope, call_depth)?;
        let dims = self.infer_expr_dims(arg, scope);
        let [rows, cols] = dims.as_slice() else {
            return Err(LowerError::Unsupported {
                reason: format!("symmetric() requires a square matrix, got shape {dims:?}"),
            });
        };
        if rows != cols || values.len() != rows * cols {
            return Err(LowerError::Unsupported {
                reason: format!("symmetric() requires a square matrix, got shape {dims:?}"),
            });
        }
        let n = *rows;
        let mut symmetric = Vec::with_capacity(values.len());
        for row in 0..n {
            for col in 0..n {
                symmetric.push(values[symmetric_source_index(row, col, n)]);
            }
        }
        Ok(symmetric)
    }

    fn emit_cross_component(&mut self, lhs_a: Reg, rhs_a: Reg, lhs_b: Reg, rhs_b: Reg) -> Reg {
        let first = self.emit_binary(BinaryOp::Mul, lhs_a, rhs_a);
        let second = self.emit_binary(BinaryOp::Mul, lhs_b, rhs_b);
        self.emit_binary(BinaryOp::Sub, first, second)
    }

    fn shape_element_count(
        &self,
        dims: &[rumoca_core::Expression],
        context: &str,
    ) -> Result<usize, LowerError> {
        if dims.is_empty() {
            return Ok(1);
        }
        let const_scope = &self.local_const_bindings;
        let mut count = 1usize;
        for dim in dims {
            let dim_value = self.eval_compile_time_int(dim, const_scope, context)?;
            if dim_value < 0 {
                return Err(LowerError::Unsupported {
                    reason: format!("{context} dimension must be non-negative"),
                });
            }
            count =
                count
                    .checked_mul(dim_value as usize)
                    .ok_or_else(|| LowerError::Unsupported {
                        reason: format!("{context} dimensions overflow solve-IR row count"),
                    })?;
        }
        Ok(count)
    }

    fn reject_pre_array_like_values(
        &mut self,
        _args: &[rumoca_core::Expression],
    ) -> Result<Vec<Reg>, LowerError> {
        Err(LowerError::Unsupported {
            reason: "pre() must be lowered to __pre__ parameters before Solve-IR lowering"
                .to_string(),
        })
    }

    fn lower_fill_array_like_values(
        &mut self,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(value_expr) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
        let count = self.fill_element_count(&args[1..])?;
        let value = self.lower_expr(value_expr, scope, call_depth)?;
        Ok(vec![value; count])
    }

    fn fill_element_count(&self, dims: &[rumoca_core::Expression]) -> Result<usize, LowerError> {
        if dims.is_empty() {
            return Ok(1);
        }
        let const_scope = &self.local_const_bindings;
        let mut count = 1usize;
        for dim in dims {
            let dim_value = self.eval_compile_time_int(dim, const_scope, "fill dimension")?;
            if dim_value < 0 {
                return Err(LowerError::Unsupported {
                    reason: "fill dimension must be non-negative".to_string(),
                });
            }
            count =
                count
                    .checked_mul(dim_value as usize)
                    .ok_or_else(|| LowerError::Unsupported {
                        reason: "fill dimensions overflow solve-IR row count".to_string(),
                    })?;
        }
        Ok(count)
    }

    pub(super) fn lower_synchronous_array_like_intrinsic(
        &mut self,
        call_name: &str,
        args: &[rumoca_core::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(arg) = args.first() else {
            return Ok(vec![self.emit_const(0.0)]);
        };
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
            _ => Ok(vec![self.lower_expr(arg, scope, call_depth)?]),
        }
    }
}

fn transpose_flat_matrix_regs(values: &[Reg], rows: usize, cols: usize) -> Vec<Reg> {
    let mut transposed = Vec::with_capacity(values.len());
    for row in 0..cols {
        transposed.extend((0..rows).map(|col| values[col * cols + row]));
    }
    transposed
}

fn symmetric_source_index(row: usize, col: usize, n: usize) -> usize {
    row.max(col) * n + row.min(col)
}
