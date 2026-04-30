use super::*;

struct ArrayComprehensionLowerCtx<'a> {
    indices: &'a [dae::ComprehensionIndex],
    filter: Option<&'a dae::Expression>,
    scope: &'a mut Scope,
    const_scope: &'a mut IndexMap<String, f64>,
    call_depth: usize,
}

impl<'a> LowerBuilder<'a> {
    pub(super) fn lower_structural_index_expr(
        &mut self,
        base: &dae::Expression,
        subscripts: &[dae::Subscript],
        scope: &Scope,
        call_depth: usize,
        projected_field: Option<&str>,
    ) -> Result<Option<Reg>, LowerError> {
        if subscripts.is_empty() {
            return Ok(None);
        }
        match base {
            dae::Expression::Index {
                base: nested_base,
                subscripts: nested_subscripts,
            } => {
                let mut combined = nested_subscripts.to_vec();
                combined.extend_from_slice(subscripts);
                self.lower_structural_index_expr(
                    nested_base,
                    &combined,
                    scope,
                    call_depth,
                    projected_field,
                )
            }
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => self
                .lower_structural_index_elements(
                    elements,
                    subscripts,
                    scope,
                    call_depth,
                    projected_field,
                ),
            _ => Ok(None),
        }
    }

    fn lower_structural_index_elements(
        &mut self,
        elements: &[dae::Expression],
        subscripts: &[dae::Subscript],
        scope: &Scope,
        call_depth: usize,
        projected_field: Option<&str>,
    ) -> Result<Option<Reg>, LowerError> {
        if elements.is_empty() {
            return Ok(Some(self.emit_const(0.0)));
        }

        let selector = self.lower_structural_index_selector(&subscripts[0], scope, call_depth)?;
        let fallback = self.emit_const(0.0);
        let mut merged = fallback;

        for (idx, element) in elements.iter().enumerate().rev() {
            let value = if subscripts.len() == 1 {
                self.lower_structural_index_leaf(element, projected_field, scope, call_depth)?
            } else if let Some(value) = self.lower_structural_index_expr(
                element,
                &subscripts[1..],
                scope,
                call_depth,
                projected_field,
            )? {
                value
            } else {
                continue;
            };
            let index_reg = self.emit_const((idx + 1) as f64);
            let matches = self.emit_compare(CompareOp::Eq, selector, index_reg);
            merged = self.emit_select(matches, value, merged);
        }

        Ok(Some(merged))
    }

    fn lower_structural_index_selector(
        &mut self,
        subscript: &dae::Subscript,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        match subscript {
            dae::Subscript::Index(v) if *v > 0 => Ok(self.emit_const(*v as f64)),
            dae::Subscript::Expr(expr) => {
                let raw = self.lower_expr(expr, scope, call_depth)?;
                Ok(self.emit_round(raw))
            }
            dae::Subscript::Colon => Err(LowerError::Unsupported {
                reason: "slice subscript `:` is unsupported in PR2".to_string(),
            }),
            _ => Err(LowerError::Unsupported {
                reason: "non-positive subscript is unsupported".to_string(),
            }),
        }
    }

    fn lower_structural_index_leaf(
        &mut self,
        element: &dae::Expression,
        projected_field: Option<&str>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Reg, LowerError> {
        if let Some(field) = projected_field {
            return self.lower_field_access(element, field, scope, call_depth);
        }
        self.lower_expr(element, scope, call_depth)
    }

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
                if let Some(values) = scoped_indexed_binding_values(scope, key) {
                    return Ok(values);
                }
                if let Some(reg) = scope.get(key).copied() {
                    return Ok(vec![reg]);
                }
                if let Some(values) = self.lower_indexed_binding_values(key)? {
                    return Ok(values);
                }
                Ok(vec![self.lower_expr(expr, scope, call_depth)?])
            }
            dae::Expression::FieldAccess { base, field } => {
                if let Ok(key) = field_access_binding_key(base, field)
                    && let Some(values) = self.lower_indexed_binding_values(key.as_str())?
                {
                    return Ok(values);
                }
                if let Some(values) =
                    self.lower_structural_field_values(base, field, scope, call_depth)?
                {
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
            dae::Expression::FunctionCall {
                name,
                args,
                is_constructor,
            } if self.is_record_constructor_call(name, *is_constructor) => {
                self.lower_record_constructor_values(name, args, scope, call_depth)
            }
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
                let mut values = Vec::new();
                for element in elements {
                    values.extend(self.lower_array_like_values(element, scope, call_depth)?);
                }
                Ok(values)
            }
            dae::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => self.lower_array_comprehension_values(
                expr,
                indices,
                filter.as_deref(),
                scope,
                call_depth,
            ),
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
            dae::Expression::If {
                branches,
                else_branch,
            } => self.lower_if_array_like_values(branches, else_branch, scope, call_depth),
            dae::Expression::Unary { op, rhs } => {
                let values = self.lower_array_like_values(rhs, scope, call_depth)?;
                values
                    .into_iter()
                    .map(|value| self.lower_unary(op.clone(), value))
                    .collect()
            }
            _ => Ok(vec![self.lower_expr(expr, scope, call_depth)?]),
        }
    }

    pub(super) fn lower_structural_field_values(
        &mut self,
        base: &dae::Expression,
        field: &str,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Option<Vec<Reg>>, LowerError> {
        match base {
            dae::Expression::Array { elements, .. } | dae::Expression::Tuple { elements } => {
                let mut values = Vec::new();
                for element in elements {
                    let projected = dae::Expression::FieldAccess {
                        base: Box::new(element.clone()),
                        field: field.to_string(),
                    };
                    values.extend(self.lower_array_like_values(&projected, scope, call_depth)?);
                }
                Ok(Some(values))
            }
            dae::Expression::ArrayComprehension {
                expr,
                indices,
                filter,
            } => {
                let projected = dae::Expression::FieldAccess {
                    base: Box::new((**expr).clone()),
                    field: field.to_string(),
                };
                self.lower_array_comprehension_values(
                    &projected,
                    indices,
                    filter.as_deref(),
                    scope,
                    call_depth,
                )
                .map(Some)
            }
            _ => Ok(None),
        }
    }

    fn lower_array_comprehension_values(
        &mut self,
        expr: &dae::Expression,
        indices: &[dae::ComprehensionIndex],
        filter: Option<&dae::Expression>,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut scope = scope.clone();
        let mut const_scope = IndexMap::<String, f64>::new();
        let mut values = Vec::new();
        let mut ctx = ArrayComprehensionLowerCtx {
            indices,
            filter,
            scope: &mut scope,
            const_scope: &mut const_scope,
            call_depth,
        };
        self.collect_array_comprehension_values(expr, 0, &mut ctx, &mut values)?;
        Ok(values)
    }

    fn collect_array_comprehension_values(
        &mut self,
        expr: &dae::Expression,
        depth: usize,
        ctx: &mut ArrayComprehensionLowerCtx<'_>,
        out: &mut Vec<Reg>,
    ) -> Result<(), LowerError> {
        if depth >= ctx.indices.len() {
            if let Some(filter_expr) = ctx.filter
                && self.eval_compile_time_expr(filter_expr, ctx.const_scope)? == 0.0
            {
                return Ok(());
            }
            out.extend(self.lower_array_like_values(expr, ctx.scope, ctx.call_depth)?);
            return Ok(());
        }

        let iter = &ctx.indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, ctx.const_scope)?;
        for value in iter_values {
            let iter_reg = self.emit_const(value);
            ctx.scope.insert(iter.name.clone(), iter_reg);
            ctx.const_scope.insert(iter.name.clone(), value);
            self.collect_array_comprehension_values(expr, depth + 1, ctx, out)?;
            ctx.const_scope.shift_remove(&iter.name);
        }
        Ok(())
    }

    fn lower_record_constructor_values(
        &mut self,
        name: &dae::VarName,
        args: &[dae::Expression],
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(function) = self.lookup_function(name).cloned() else {
            let mut values = Vec::new();
            for arg in args {
                values.extend(self.lower_array_like_values(arg, scope, call_depth)?);
            }
            return Ok(values);
        };

        let (named_args, positional_args) =
            function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
        let mut positional_idx = 0usize;
        let mut values = Vec::new();

        for input in &function.inputs {
            let arg_expr = named_args.get(input.name.as_str()).copied().or_else(|| {
                let positional = positional_args.get(positional_idx).copied();
                positional_idx += usize::from(positional.is_some());
                positional
            });

            if let Some(expr) = arg_expr {
                values.extend(self.lower_array_like_values(expr, scope, call_depth)?);
            } else if let Some(default) = input.default.as_ref() {
                values.extend(self.lower_array_like_values(default, scope, call_depth + 1)?);
            } else {
                values.push(self.emit_const(0.0));
            }
        }

        Ok(values)
    }

    fn lower_if_array_like_values(
        &mut self,
        branches: &[(dae::Expression, dae::Expression)],
        else_branch: &dae::Expression,
        scope: &Scope,
        call_depth: usize,
    ) -> Result<Vec<Reg>, LowerError> {
        let mut result = self.lower_array_like_values(else_branch, scope, call_depth)?;
        for (cond, value) in branches.iter().rev() {
            let cond_reg = self.lower_expr(cond, scope, call_depth)?;
            let branch_values = self.lower_array_like_values(value, scope, call_depth)?;
            if branch_values.len() != result.len() {
                return Err(LowerError::Unsupported {
                    // MLS §3.6 / §3.8: if-expression branches must agree on
                    // shape. Keep compiled lowering strict instead of silently
                    // truncating structured results.
                    reason:
                        "if-expression branches with mismatched array-like widths are unsupported in PR2"
                            .to_string(),
                });
            }
            result = branch_values
                .into_iter()
                .zip(result)
                .map(|(if_true, if_false)| self.emit_select(cond_reg, if_true, if_false))
                .collect();
        }
        Ok(result)
    }

    fn lower_indexed_binding_values(&mut self, key: &str) -> Result<Option<Vec<Reg>>, LowerError> {
        let entries = indexed_entries_for_key(self.layout, &self.indexed_bindings, key);
        if entries.is_empty() {
            return Ok(None);
        }
        let flat = sorted_flat_entries(&entries);
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

fn scoped_indexed_binding_values(scope: &Scope, key: &str) -> Option<Vec<Reg>> {
    let prefix = format!("{key}[");
    let mut values = scope
        .iter()
        .filter_map(|(name, reg)| {
            let suffix = name.strip_prefix(&prefix)?;
            let index = suffix.strip_suffix(']')?.parse::<usize>().ok()?;
            Some((index, *reg))
        })
        .collect::<Vec<_>>();
    if values.is_empty() {
        return None;
    }
    values.sort_by_key(|(index, _)| *index);
    Some(values.into_iter().map(|(_, reg)| reg).collect())
}
