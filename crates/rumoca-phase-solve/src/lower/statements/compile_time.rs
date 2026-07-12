//! Compile-time scalar evaluation for solve statement lowering.
//!
//! Evaluates `for` ranges, guards, and structural expressions to concrete
//! values when they are decidable at compile time, including conservative
//! integer interval analysis for symbolic `for` bounds.

use indexmap::IndexMap;
use rumoca_ir_solve::ScalarSlot;

use super::super::helpers::*;
use super::super::{LowerBuilder, LowerError, Scope, size_binding_key, unsupported_at};
use super::{
    positive_size_dimension, start_metadata_refers_to_key, unsupported_with_optional_span,
};

impl<'a> LowerBuilder<'a> {
    pub(super) fn integer_expr_interval(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<Option<(i64, i64)>, LowerError> {
        let expr_span = self.statement_expr_span(expr)?;
        if let Ok(value) = self.eval_compile_time_int(expr, const_scope, "for range bound") {
            return Ok(Some((value, value)));
        }
        let interval = match expr {
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } if subscripts.is_empty() => self.local_integer_bounds.get(name.as_str()).copied(),
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                let Some((min, max)) = self.integer_expr_interval(rhs, const_scope)? else {
                    return Ok(None);
                };
                match op {
                    rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Some((
                        max.checked_neg().ok_or_else(|| {
                            unsupported_at("for range bound overflows Integer", expr_span)
                        })?,
                        min.checked_neg().ok_or_else(|| {
                            unsupported_at("for range bound overflows Integer", expr_span)
                        })?,
                    )),
                    rumoca_core::OpUnary::Plus
                    | rumoca_core::OpUnary::DotPlus
                    | rumoca_core::OpUnary::Empty => Some((min, max)),
                    rumoca_core::OpUnary::Not => None,
                }
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                let Some(lhs) = self.integer_expr_interval(lhs, const_scope)? else {
                    return Ok(None);
                };
                let Some(rhs) = self.integer_expr_interval(rhs, const_scope)? else {
                    return Ok(None);
                };
                integer_binary_interval(op, lhs, rhs, expr_span)?
            }
            _ => None,
        };
        Ok(interval)
    }

    pub(in crate::lower) fn eval_for_index_values(
        &self,
        range: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<Vec<f64>, LowerError> {
        match range {
            rumoca_core::Expression::Range {
                start,
                step,
                end,
                span,
            } => {
                let step_span = step.as_deref().and_then(rumoca_core::Expression::span);
                let start = self.eval_compile_time_int(start, const_scope, "for range start")?;
                let end = self.eval_compile_time_int(end, const_scope, "for range end")?;
                let step = if let Some(step_expr) = step.as_ref() {
                    self.eval_compile_time_int(step_expr, const_scope, "for range step")?
                } else {
                    1
                };
                if step == 0 {
                    return Err(unsupported_at(
                        "for range step cannot be zero",
                        step_span.unwrap_or(*span),
                    ));
                }

                Ok(build_range_values(start, end, step))
            }
            rumoca_core::Expression::Array { elements, .. } => {
                let mut values = crate::lower_vec_with_capacity(
                    elements.len(),
                    "for range array values",
                    self.statement_expr_span(range)?,
                )?;
                for element in elements {
                    let v = self.eval_compile_time_int(
                        element,
                        const_scope,
                        "for range array element",
                    )?;
                    values.push(v as f64);
                }
                Ok(values)
            }
            _ => {
                let value =
                    self.eval_compile_time_int(range, const_scope, "for range expression")?;
                Ok(vec![value as f64])
            }
        }
    }

    pub(in crate::lower) fn eval_compile_time_int(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<i64, LowerError> {
        self.eval_compile_time_int_with_context_span(expr, const_scope, context, None)
    }

    pub(in crate::lower) fn eval_compile_time_int_at(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
        context_span: rumoca_core::Span,
    ) -> Result<i64, LowerError> {
        self.eval_compile_time_int_with_context_span(
            expr,
            const_scope,
            context,
            (!context_span.is_dummy()).then_some(context_span),
        )
    }

    fn eval_compile_time_int_with_context_span(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
        context_span: Option<rumoca_core::Span>,
    ) -> Result<i64, LowerError> {
        let value = self.eval_compile_time_expr(expr, const_scope)?;
        let span = expr.span().or(context_span);
        checked_compile_time_i64(value, context, span)
    }

    pub(in crate::lower) fn eval_compile_time_expr(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        match expr {
            rumoca_core::Expression::Literal { value: lit, .. } => eval_literal(lit),
            rumoca_core::Expression::VarRef {
                name,
                subscripts,
                span,
                ..
            } => self.eval_compile_time_var_ref(name, subscripts, *span, const_scope),
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                self.eval_compile_time_unary(op, rhs, const_scope)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                self.eval_compile_time_binary(op, lhs, rhs, expr, const_scope)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.eval_compile_time_if(branches, else_branch, const_scope),
            rumoca_core::Expression::BuiltinCall {
                function,
                args,
                span,
            } => self.eval_compile_time_builtin(*function, args, *span, const_scope),
            rumoca_core::Expression::FunctionCall {
                name, args, span, ..
            } => self.eval_compile_time_function_call(name, args, *span, const_scope),
            rumoca_core::Expression::ArrayComprehension { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::FieldAccess { .. }
            | rumoca_core::Expression::Index { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Empty { .. } => Err(unsupported_at(
                "unsupported expression in for-loop range",
                self.statement_expr_span(expr)?,
            )),
        }
    }

    pub(in crate::lower) fn eval_compile_time_var_ref(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        span: rumoca_core::Span,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if subscripts.is_empty()
            && let Some(value) = const_scope.get(name.as_str())
        {
            return Ok(*value);
        }
        let key = compile_time_var_key(name, subscripts, const_scope, span)?;
        if let Some(value) = self.structural_bindings.get(key.as_str()) {
            return Ok(*value);
        }
        if let Some(start) = self
            .variable_starts
            .and_then(|starts| starts.get(key.as_str()))
            .filter(|start| !start_metadata_refers_to_key(start, key.as_str()))
            && let Ok(value) = self.eval_compile_time_expr(start, const_scope)
        {
            return Ok(value);
        }
        match self.layout.binding(key.as_str()) {
            Some(ScalarSlot::Constant(value)) => Ok(value),
            Some(_) | None => Err(unsupported_at(
                format!("for-loop range expression requires compile-time constant `{key}`"),
                span,
            )),
        }
    }

    pub(in crate::lower) fn eval_compile_time_unary(
        &self,
        op: &rumoca_core::OpUnary,
        rhs: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let value = self.eval_compile_time_expr(rhs, const_scope)?;
        match op {
            rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => Ok(-value),
            rumoca_core::OpUnary::Plus
            | rumoca_core::OpUnary::DotPlus
            | rumoca_core::OpUnary::Empty => Ok(value),
            rumoca_core::OpUnary::Not => Ok(if value == 0.0 { 1.0 } else { 0.0 }),
        }
    }

    pub(in crate::lower) fn eval_compile_time_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let l = self.eval_compile_time_expr(lhs, const_scope)?;
        let r = self.eval_compile_time_expr(rhs, const_scope)?;
        match op {
            rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Ok(l + r),
            rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Ok(l - r),
            rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => Ok(l * r),
            rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => Ok(l / r),
            rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => Ok(l.powf(r)),
            rumoca_core::OpBinary::Lt => Ok(bool_to_f64(l < r)),
            rumoca_core::OpBinary::Le => Ok(bool_to_f64(l <= r)),
            rumoca_core::OpBinary::Gt => Ok(bool_to_f64(l > r)),
            rumoca_core::OpBinary::Ge => Ok(bool_to_f64(l >= r)),
            rumoca_core::OpBinary::Eq => Ok(bool_to_f64((l - r).abs() < f64::EPSILON)),
            rumoca_core::OpBinary::Neq => Ok(bool_to_f64((l - r).abs() >= f64::EPSILON)),
            rumoca_core::OpBinary::And => Ok(bool_to_f64(l != 0.0 && r != 0.0)),
            rumoca_core::OpBinary::Or => Ok(bool_to_f64(l != 0.0 || r != 0.0)),
            rumoca_core::OpBinary::Assign | rumoca_core::OpBinary::Empty => Err(unsupported_at(
                "unsupported operator in for-loop range expression",
                self.statement_expr_span(expr)?,
            )),
        }
    }

    pub(in crate::lower) fn eval_compile_time_if(
        &self,
        branches: &[(rumoca_core::Expression, rumoca_core::Expression)],
        else_branch: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        for (cond, value) in branches {
            let condition = self.eval_compile_time_expr(cond, const_scope)?;
            if condition != 0.0 {
                return self.eval_compile_time_expr(value, const_scope);
            }
        }
        self.eval_compile_time_expr(else_branch, const_scope)
    }

    pub(in crate::lower) fn eval_compile_time_builtin(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if matches!(function, rumoca_core::BuiltinFunction::Size) {
            return self.eval_compile_time_size(args, span, const_scope);
        }
        let arg0 = eval_builtin_arg(self, args, 0, const_scope)?;
        match function {
            rumoca_core::BuiltinFunction::Abs => Ok(arg0.abs()),
            rumoca_core::BuiltinFunction::Sign => Ok(arg0.signum()),
            rumoca_core::BuiltinFunction::Sqrt => Ok(arg0.sqrt()),
            rumoca_core::BuiltinFunction::Floor | rumoca_core::BuiltinFunction::Integer => {
                Ok(arg0.floor())
            }
            rumoca_core::BuiltinFunction::Ceil => Ok(arg0.ceil()),
            rumoca_core::BuiltinFunction::Min => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                Ok(arg0.min(arg1))
            }
            rumoca_core::BuiltinFunction::Max => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                Ok(arg0.max(arg1))
            }
            rumoca_core::BuiltinFunction::Mod => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                if arg1 == 0.0 {
                    return Err(unsupported_at(
                        "mod() denominator cannot be zero in for-loop range expression",
                        args.get(1)
                            .and_then(rumoca_core::Expression::span)
                            .unwrap_or(span),
                    ));
                }
                Ok(arg0 - (arg0 / arg1).floor() * arg1)
            }
            rumoca_core::BuiltinFunction::Div => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                if arg1 == 0.0 {
                    return Err(unsupported_at(
                        "div() denominator cannot be zero in for-loop range expression",
                        args.get(1)
                            .and_then(rumoca_core::Expression::span)
                            .unwrap_or(span),
                    ));
                }
                Ok((arg0 / arg1).floor())
            }
            rumoca_core::BuiltinFunction::Rem => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                if arg1 == 0.0 {
                    return Err(unsupported_at(
                        "rem() denominator cannot be zero in for-loop range expression",
                        args.get(1)
                            .and_then(rumoca_core::Expression::span)
                            .unwrap_or(span),
                    ));
                }
                Ok(arg0 % arg1)
            }
            _ => Err(unsupported_at(
                format!(
                    "builtin `{}` is unsupported in for-loop range expression",
                    function.name()
                ),
                span,
            )),
        }
    }

    pub(in crate::lower) fn eval_compile_time_size(
        &self,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let dim = if let Some(dim_expr) = args.get(1) {
            positive_size_dimension(
                self.eval_compile_time_int(dim_expr, const_scope, "size dimension")?,
                self.statement_expr_or_context_span(dim_expr, span)?,
            )?
        } else {
            1
        };
        let Some(expr) = args.first() else {
            return Err(unsupported_at("size() requires an array expression", span));
        };
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expr
        else {
            let dims = self.infer_expr_dims(expr, &Scope::new())?;
            return dims
                .get(dim - 1)
                .copied()
                .map(|value| value as f64)
                .ok_or_else(|| {
                    unsupported_at(
                        format!(
                            "size() in for-loop range requires known expression dimension {dim}"
                        ),
                        expr.span().unwrap_or(span),
                    )
                });
        };
        if !subscripts.is_empty() {
            return Ok(1.0);
        }
        let key = size_binding_key(name.as_str(), dim);
        self.structural_bindings
            .get(key.as_str())
            .copied()
            .ok_or_else(|| LowerError::ForRangeUnknownDimension {
                name: name.as_str().to_string(),
            })
    }
}

fn integer_binary_interval(
    op: &rumoca_core::OpBinary,
    lhs: (i64, i64),
    rhs: (i64, i64),
    span: rumoca_core::Span,
) -> Result<Option<(i64, i64)>, LowerError> {
    let overflow = || unsupported_at("for range bound overflows Integer", span);
    let interval = match op {
        rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => Some((
            lhs.0.checked_add(rhs.0).ok_or_else(overflow)?,
            lhs.1.checked_add(rhs.1).ok_or_else(overflow)?,
        )),
        rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => Some((
            lhs.0.checked_sub(rhs.1).ok_or_else(overflow)?,
            lhs.1.checked_sub(rhs.0).ok_or_else(overflow)?,
        )),
        rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => {
            let products = [
                lhs.0.checked_mul(rhs.0).ok_or_else(overflow)?,
                lhs.0.checked_mul(rhs.1).ok_or_else(overflow)?,
                lhs.1.checked_mul(rhs.0).ok_or_else(overflow)?,
                lhs.1.checked_mul(rhs.1).ok_or_else(overflow)?,
            ];
            let min = products.iter().copied().min().ok_or_else(overflow)?;
            let max = products.iter().copied().max().ok_or_else(overflow)?;
            Some((min, max))
        }
        _ => None,
    };
    Ok(interval)
}

pub(super) fn checked_compile_time_i64(
    value: f64,
    context: &str,
    span: Option<rumoca_core::Span>,
) -> Result<i64, LowerError> {
    if !value.is_finite() {
        return Err(unsupported_with_optional_span(
            format!("{context} is not finite"),
            span,
        ));
    }
    let rounded = value.round();
    if (rounded - value).abs() > 1e-9 {
        return Err(unsupported_with_optional_span(
            format!("{context} must evaluate to an integer"),
            span,
        ));
    }
    if rounded < i64::MIN as f64 || rounded >= i64::MAX as f64 {
        return Err(unsupported_with_optional_span(
            format!("{context} overflows i64"),
            span,
        ));
    }
    // Bounds and integrality are checked above; Rust has no TryFrom<f64>.
    Ok(rounded as i64)
}
