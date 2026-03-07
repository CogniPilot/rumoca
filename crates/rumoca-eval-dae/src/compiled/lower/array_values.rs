use super::*;

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_sum_range(
        &mut self,
        expr: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Reg>, LowerError> {
        let dae::Expression::Range { start, step, end } = expr else {
            return Ok(None);
        };

        let start_reg = self.lower_expr(start, scope, call_depth)?;
        let end_reg = self.lower_expr(end, scope, call_depth)?;
        let step_reg = if let Some(step_expr) = step.as_ref() {
            self.lower_expr(step_expr, scope, call_depth)?
        } else {
            let cond = self.emit_compare(CompareOp::Ge, end_reg, start_reg);
            let pos = self.emit_const(1.0);
            let neg = self.emit_const(-1.0);
            self.emit_select(cond, pos, neg)
        };

        let zero = self.emit_const(0.0);
        let step_gt_zero = self.emit_compare(CompareOp::Gt, step_reg, zero);
        let step_lt_zero = self.emit_compare(CompareOp::Lt, step_reg, zero);
        let start_le_end = self.emit_compare(CompareOp::Le, start_reg, end_reg);
        let start_ge_end = self.emit_compare(CompareOp::Ge, start_reg, end_reg);
        let forward_valid = self.emit_binary(BinaryOp::And, step_gt_zero, start_le_end);
        let backward_valid = self.emit_binary(BinaryOp::And, step_lt_zero, start_ge_end);
        let valid = self.emit_binary(BinaryOp::Or, forward_valid, backward_valid);

        let distance = self.emit_binary(BinaryOp::Sub, end_reg, start_reg);
        let ratio = self.emit_binary(BinaryOp::Div, distance, step_reg);
        let ratio_floor = self.emit_unary(UnaryOp::Floor, ratio);
        let one = self.emit_const(1.0);
        let n = self.emit_binary(BinaryOp::Add, ratio_floor, one);
        let two = self.emit_const(2.0);
        let two_start = self.emit_binary(BinaryOp::Mul, two, start_reg);
        let n_minus_one = self.emit_binary(BinaryOp::Sub, n, one);
        let stride = self.emit_binary(BinaryOp::Mul, n_minus_one, step_reg);
        let bracket = self.emit_binary(BinaryOp::Add, two_start, stride);
        let n_half = self.emit_binary(BinaryOp::Div, n, two);
        let sum = self.emit_binary(BinaryOp::Mul, n_half, bracket);

        let fallback = self.emit_const(0.0);
        Ok(Some(self.emit_select(valid, sum, fallback)))
    }

    pub(super) fn lower_array_like_values(
        &mut self,
        expr: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        match expr {
            dae::Expression::VarRef { name, subscripts } if subscripts.is_empty() => {
                let key = name.as_str();
                if let Some(values) = self.lower_indexed_binding_values(key)? {
                    return Ok(values);
                }
                Ok(vec![self.lower_expr(expr, scope, call_depth)?])
            }
            dae::Expression::FieldAccess { base, field } => {
                let key = field_access_binding_key(base, field)?;
                if let Some(values) = self.lower_indexed_binding_values(key.as_str())? {
                    return Ok(values);
                }
                Ok(vec![self.lower_expr(expr, scope, call_depth)?])
            }
            dae::Expression::BuiltinCall {
                function: dae::BuiltinFunction::Cat,
                args,
            } => {
                let mut values = Vec::new();
                for arg in args.iter().skip(1) {
                    values.extend(self.lower_array_like_values(arg, scope, call_depth)?);
                }
                Ok(values)
            }
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
                let mut values = Vec::new();
                for element in elements {
                    values.extend(self.lower_array_like_values(element, scope, call_depth)?);
                }
                Ok(values)
            }
            dae::Expression::Range { start, step, end } => {
                if let Some(values) = lower_static_range_values(start, step.as_deref(), end)? {
                    Ok(values
                        .into_iter()
                        .map(|value| self.emit_const(value))
                        .collect())
                } else {
                    Err(LowerError::Unsupported {
                        reason: "dynamic range array expansion is unsupported in PR2".to_string(),
                    })
                }
            }
            _ => Ok(vec![self.lower_expr(expr, scope, call_depth)?]),
        }
    }

    fn lower_indexed_binding_values(&mut self, key: &str) -> Result<Option<Vec<Reg>>, LowerError> {
        let Some(entries) = self.indexed_bindings.get(key) else {
            return Ok(None);
        };
        let flat = sorted_flat_entries(entries);
        if flat.is_empty() {
            return Ok(None);
        }
        let slots = flat.into_iter().map(|entry| entry.slot).collect::<Vec<_>>();
        let mut values = Vec::with_capacity(slots.len());
        for slot in slots {
            values.push(self.emit_slot_load(slot)?);
        }
        Ok(Some(values))
    }

    pub(super) fn lower_if(
        &mut self,
        branches: &[(dae::Expression, dae::Expression)],
        else_branch: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        let mut result = self.lower_expr(else_branch, scope, call_depth)?;
        for (cond, value) in branches.iter().rev() {
            let cond_reg = self.lower_expr(cond, scope, call_depth)?;
            let value_reg = self.lower_expr(value, scope, call_depth)?;
            result = self.emit_select(cond_reg, value_reg, result);
        }
        Ok(result)
    }
}

fn lower_static_range_values(
    start: &dae::Expression,
    step: Option<&dae::Expression>,
    end: &dae::Expression,
) -> Result<Option<Vec<f64>>, LowerError> {
    let Some(start_v) = lower_static_index_numeric(start)? else {
        return Ok(None);
    };
    let Some(end_v) = lower_static_index_numeric(end)? else {
        return Ok(None);
    };
    let step_v = if let Some(step_expr) = step {
        let Some(value) = lower_static_index_numeric(step_expr)? else {
            return Ok(None);
        };
        value
    } else if end_v >= start_v {
        1.0
    } else {
        -1.0
    };

    if !start_v.is_finite()
        || !end_v.is_finite()
        || !step_v.is_finite()
        || step_v.abs() <= f64::EPSILON
    {
        return Err(LowerError::Unsupported {
            reason: "invalid static range expression in compiled lowering".to_string(),
        });
    }

    let tol = step_v.abs() * 1e-9 + 1e-12;
    let mut values = Vec::new();
    let mut value = start_v;
    for _ in 0..100_000 {
        let past_end =
            (step_v > 0.0 && value > end_v + tol) || (step_v < 0.0 && value < end_v - tol);
        if past_end {
            break;
        }
        values.push(value);
        value += step_v;
    }
    Ok(Some(values))
}
