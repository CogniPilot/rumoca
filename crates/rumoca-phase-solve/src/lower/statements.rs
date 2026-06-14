use indexmap::{IndexMap, IndexSet};
use rumoca_core::ComponentPath;
use rumoca_ir_solve::{BinaryOp, Reg, ScalarSlot};

use super::function_projection::format_subscript_binding_key;
use super::helpers::*;
use super::{
    BREAK_FLAG_BINDING, LocalIndexedBinding, LowerBuilder, LowerError, RETURN_FLAG_BINDING, Scope,
    size_binding_key, upsert_local_indexed_binding,
};

const MAX_INLINE_WHILE_ITERS: usize = 128;

impl<'a> LowerBuilder<'a> {
    /// Returns `true` when lowering should stop due to `return`.
    pub(super) fn lower_statements(
        &mut self,
        statements: &[rumoca_core::Statement],
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        for statement in statements {
            if self.lower_statement(statement, scope, call_depth)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    pub(super) fn lower_if_statement(
        &mut self,
        cond_blocks: &[rumoca_core::StatementBlock],
        else_block: &Option<Vec<rumoca_core::Statement>>,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if let Some(selected) = self.compile_time_if_selection(cond_blocks, else_block)? {
            return match selected {
                Some(stmts) => self.lower_statements(stmts, scope, call_depth),
                None => Ok(false),
            };
        }

        if cond_blocks.is_empty() {
            if let Some(stmts) = else_block {
                return self.lower_statements(stmts, scope, call_depth);
            }
            return Ok(false);
        }

        let entry_scope = scope.clone();
        // Each branch lowers inside `with_local_lower_frame`, which rolls back
        // the builder-level `local_indexed_bindings` cache afterwards. Snapshot
        // the entry state so array elements a branch reassigns can be merged
        // back as conditional selects below (see `merge_branch_indexed_bindings`).
        let entry_indexed = self.local_indexed_bindings.clone();
        let mut cond_regs = Vec::with_capacity(cond_blocks.len());
        let mut branch_scopes = Vec::with_capacity(cond_blocks.len());
        let mut branch_indexed = Vec::with_capacity(cond_blocks.len());

        for block in cond_blocks {
            let (cond, branch_scope, indexed) = self.with_local_lower_frame(|builder| {
                let cond = builder.lower_expr(&block.cond, &entry_scope, call_depth)?;
                let mut branch_scope = entry_scope.clone();
                let _returned =
                    builder.lower_statements(&block.stmts, &mut branch_scope, call_depth)?;
                let indexed = builder.local_indexed_bindings.clone();
                Ok((cond, branch_scope, indexed))
            })?;
            cond_regs.push(cond);
            branch_scopes.push(branch_scope);
            branch_indexed.push(indexed);
        }

        let (else_scope, else_indexed) = self.with_local_lower_frame(|builder| {
            let mut else_scope = entry_scope.clone();
            if let Some(stmts) = else_block {
                let _returned = builder.lower_statements(stmts, &mut else_scope, call_depth)?;
            }
            let indexed = builder.local_indexed_bindings.clone();
            Ok((else_scope, indexed))
        })?;

        let mut merged_scope = entry_scope.clone();
        let names = collect_scope_names(&merged_scope, &branch_scopes, &else_scope);

        for name in names {
            let Some(mut merged) = else_scope
                .get(&name)
                .copied()
                .or_else(|| entry_scope.get(&name).copied())
                .or_else(|| {
                    (is_control_flag(&name)
                        || branch_scopes.iter().any(|scope| scope.contains_key(&name)))
                    .then(|| self.emit_const(0.0))
                })
            else {
                continue;
            };

            for (cond, branch_scope) in cond_regs.iter().zip(branch_scopes.iter()).rev() {
                merged = merge_branch_select(self, *cond, branch_scope, &name, merged);
            }
            merged_scope.insert(name, merged);
        }

        *scope = merged_scope;

        // The scalar merge above rebuilt scope-level bindings, but the
        // builder-level `local_indexed_bindings` cache (consulted when an
        // inlined function's array output is captured) was rolled back by each
        // branch frame. Reconstruct array elements assigned inside branches so
        // a partially-filled array (e.g. a small-angle guard that writes only
        // some Lie-algebra components per branch) survives intact.
        self.merge_branch_indexed_bindings(
            &entry_indexed,
            &cond_regs,
            &branch_indexed,
            &else_indexed,
        );

        Ok(false)
    }

    /// Re-merge builder-level indexed array-element bindings after if-branches.
    ///
    /// `with_local_lower_frame` rolls back `local_indexed_bindings` for every
    /// branch, so an element assigned only inside branches (e.g. a small-angle
    /// guard that fills part of a Lie-algebra vector) would otherwise vanish
    /// from the cache the function inliner reads when capturing an array output.
    /// For each (base, indices) any branch assigns, rebuild the value as a
    /// `select` over the branch conditions — defaulting to the else-branch
    /// value, then the entry value, then 0 — mirroring the scalar scope merge.
    /// Elements assigned in only some branches therefore become conditional
    /// (never an unconditional leak).
    fn merge_branch_indexed_bindings(
        &mut self,
        entry_indexed: &IndexMap<String, Vec<LocalIndexedBinding>>,
        cond_regs: &[Reg],
        branch_indexed: &[IndexMap<String, Vec<LocalIndexedBinding>>],
        else_indexed: &IndexMap<String, Vec<LocalIndexedBinding>>,
    ) {
        fn lookup(
            store: &IndexMap<String, Vec<LocalIndexedBinding>>,
            base: &str,
            indices: &[usize],
        ) -> Option<Reg> {
            store
                .get(base)
                .and_then(|entries| entries.iter().find(|entry| entry.indices == indices))
                .map(|entry| entry.reg)
        }

        // Every (base, indices) a branch or the else-branch assigns differently
        // from the entry state needs a merged value.
        let mut candidates: IndexSet<(String, Vec<usize>)> = IndexSet::new();
        let assigned_entries = branch_indexed
            .iter()
            .chain(std::iter::once(else_indexed))
            .flat_map(|store| store.iter())
            .flat_map(|(base, entries)| entries.iter().map(move |entry| (base, entry)));
        for (base, entry) in assigned_entries {
            if lookup(entry_indexed, base, &entry.indices) != Some(entry.reg) {
                candidates.insert((base.clone(), entry.indices.clone()));
            }
        }

        for (base, indices) in candidates {
            let mut merged = lookup(else_indexed, &base, &indices)
                .or_else(|| lookup(entry_indexed, &base, &indices))
                .unwrap_or_else(|| self.emit_const(0.0));
            let branch_values = cond_regs
                .iter()
                .zip(branch_indexed.iter())
                .rev()
                .filter_map(|(cond, store)| lookup(store, &base, &indices).map(|reg| (cond, reg)));
            for (cond, reg) in branch_values {
                merged = self.emit_select(*cond, reg, merged);
            }
            upsert_local_indexed_binding(
                self.local_indexed_bindings.entry(base).or_default(),
                &indices,
                merged,
            );
        }
    }

    fn compile_time_if_selection<'b>(
        &self,
        cond_blocks: &'b [rumoca_core::StatementBlock],
        else_block: &'b Option<Vec<rumoca_core::Statement>>,
    ) -> Result<Option<Option<&'b [rumoca_core::Statement]>>, LowerError> {
        for block in cond_blocks {
            let Ok(cond) = self.eval_compile_time_expr(&block.cond, &self.local_const_bindings)
            else {
                return Ok(None);
            };
            if cond != 0.0 {
                return Ok(Some(Some(&block.stmts)));
            }
        }
        Ok(Some(else_block.as_deref()))
    }

    pub(super) fn lower_for_statement(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        equations: &[rumoca_core::Statement],
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let break_key = ComponentPath::from_flat_path(BREAK_FLAG_BINDING);
        let saved_break = scope.get(&break_key).copied();

        let mut const_scope = self.local_const_bindings.clone();
        let returned = scope.with_frame(|scope| {
            self.lower_for_iterations(indices, equations, scope, &mut const_scope, call_depth, 0)
        });
        if let Some(reg) = saved_break {
            scope.insert(break_key.clone(), reg);
        } else {
            scope.shift_remove(&break_key);
        }

        returned
    }

    pub(super) fn lower_while_statement(
        &mut self,
        block: &rumoca_core::StatementBlock,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let break_key = ComponentPath::from_flat_path(BREAK_FLAG_BINDING);
        let saved_break = scope.get(&break_key).copied();
        for _ in 0..MAX_INLINE_WHILE_ITERS {
            let cond = self.lower_expr(&block.cond, scope, call_depth)?;
            let entry_scope = scope.clone();
            let mut body_scope = entry_scope.clone();
            let _returned = self.lower_statements(&block.stmts, &mut body_scope, call_depth)?;
            *scope = merge_while_iteration_scope(self, cond, &entry_scope, &body_scope);
        }

        if let Some(reg) = saved_break {
            scope.insert(break_key.clone(), reg);
        } else {
            scope.shift_remove(&break_key);
        }
        Ok(false)
    }

    pub(super) fn lower_for_iterations(
        &mut self,
        indices: &[rumoca_core::ForIndex],
        equations: &[rumoca_core::Statement],
        scope: &mut Scope,
        const_scope: &mut IndexMap<String, f64>,
        call_depth: usize,
        depth: usize,
    ) -> Result<bool, LowerError> {
        if depth >= indices.len() {
            let saved_bindings =
                std::mem::replace(&mut self.local_const_bindings, const_scope.clone());
            let result = self.lower_statements(equations, scope, call_depth);
            self.local_const_bindings = saved_bindings;
            return result;
        }

        let iter = &indices[depth];
        let iter_values = self.eval_for_index_values(&iter.range, const_scope)?;
        if iter_values.is_empty() {
            return Ok(false);
        }

        for value in iter_values {
            let iter_reg = self.emit_const(value);
            let returned = scope.with_frame(|scope| {
                scope.insert_scoped(ComponentPath::from_flat_path(&iter.ident), iter_reg);
                const_scope.insert(iter.ident.clone(), value);
                let result = self.lower_for_iterations(
                    indices,
                    equations,
                    scope,
                    const_scope,
                    call_depth,
                    depth + 1,
                );
                const_scope.shift_remove(&iter.ident);
                result
            })?;
            if returned {
                return Ok(true);
            }
        }

        Ok(false)
    }

    pub(super) fn eval_for_index_values(
        &self,
        range: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<Vec<f64>, LowerError> {
        match range {
            rumoca_core::Expression::Range {
                start, step, end, ..
            } => {
                let start = self.eval_compile_time_int(start, const_scope, "for range start")?;
                let end = self.eval_compile_time_int(end, const_scope, "for range end")?;
                let step = if let Some(step_expr) = step.as_ref() {
                    self.eval_compile_time_int(step_expr, const_scope, "for range step")?
                } else {
                    1
                };
                if step == 0 {
                    return Err(LowerError::Unsupported {
                        reason: "for range step cannot be zero".to_string(),
                    });
                }

                Ok(build_range_values(start, end, step))
            }
            rumoca_core::Expression::Array { elements, .. } => {
                let mut values = Vec::with_capacity(elements.len());
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

    pub(super) fn eval_compile_time_int(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
        context: &str,
    ) -> Result<i64, LowerError> {
        let value = self.eval_compile_time_expr(expr, const_scope)?;
        if !value.is_finite() {
            return Err(LowerError::Unsupported {
                reason: format!("{context} is not finite"),
            });
        }
        let rounded = value.round();
        if (rounded - value).abs() > 1e-9 {
            return Err(LowerError::Unsupported {
                reason: format!("{context} must evaluate to an integer"),
            });
        }
        if rounded < i64::MIN as f64 || rounded > i64::MAX as f64 {
            return Err(LowerError::Unsupported {
                reason: format!("{context} overflows i64"),
            });
        }
        Ok(rounded as i64)
    }

    pub(super) fn eval_compile_time_expr(
        &self,
        expr: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        match expr {
            rumoca_core::Expression::Literal { value: lit, .. } => Ok(eval_literal(lit)),
            rumoca_core::Expression::VarRef {
                name, subscripts, ..
            } => self.eval_compile_time_var_ref(name, subscripts, const_scope),
            rumoca_core::Expression::Unary { op, rhs, .. } => {
                self.eval_compile_time_unary(op, rhs, const_scope)
            }
            rumoca_core::Expression::Binary { op, lhs, rhs, .. } => {
                self.eval_compile_time_binary(op, lhs, rhs, const_scope)
            }
            rumoca_core::Expression::If {
                branches,
                else_branch,
                ..
            } => self.eval_compile_time_if(branches, else_branch, const_scope),
            rumoca_core::Expression::BuiltinCall { function, args, .. } => {
                self.eval_compile_time_builtin(*function, args, const_scope)
            }
            rumoca_core::Expression::FunctionCall { name, args, .. } => {
                self.eval_compile_time_function_call(name, args, const_scope)
            }
            rumoca_core::Expression::ArrayComprehension { .. }
            | rumoca_core::Expression::Tuple { .. }
            | rumoca_core::Expression::FieldAccess { .. }
            | rumoca_core::Expression::Index { .. }
            | rumoca_core::Expression::Range { .. }
            | rumoca_core::Expression::Array { .. }
            | rumoca_core::Expression::Empty { .. } => Err(LowerError::Unsupported {
                reason: "unsupported expression in for-loop range".to_string(),
            }),
        }
    }

    pub(super) fn eval_compile_time_var_ref(
        &self,
        name: &rumoca_core::Reference,
        subscripts: &[rumoca_core::Subscript],
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if subscripts.is_empty()
            && let Some(value) = const_scope.get(name.as_str())
        {
            return Ok(*value);
        }
        let key = compile_time_var_key(name, subscripts, const_scope)?;
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
            Some(_) | None => Err(LowerError::Unsupported {
                reason: format!("for-loop range expression requires compile-time constant `{key}`"),
            }),
        }
    }

    pub(super) fn eval_compile_time_unary(
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

    pub(super) fn eval_compile_time_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: &rumoca_core::Expression,
        rhs: &rumoca_core::Expression,
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if matches!(op, rumoca_core::OpBinary::And) {
            match self.eval_compile_time_expr(lhs, const_scope) {
                Ok(0.0) => return Ok(0.0),
                Ok(_) => {
                    let rhs = self.eval_compile_time_expr(rhs, const_scope)?;
                    return Ok(bool_to_f64(rhs != 0.0));
                }
                Err(lhs_err) => match self.eval_compile_time_expr(rhs, const_scope) {
                    Ok(0.0) => return Ok(0.0),
                    _ => return Err(lhs_err),
                },
            }
        }
        if matches!(op, rumoca_core::OpBinary::Or) {
            match self.eval_compile_time_expr(lhs, const_scope) {
                Ok(value) if value != 0.0 => return Ok(1.0),
                Ok(_) => {
                    let rhs = self.eval_compile_time_expr(rhs, const_scope)?;
                    return Ok(bool_to_f64(rhs != 0.0));
                }
                Err(lhs_err) => match self.eval_compile_time_expr(rhs, const_scope) {
                    Ok(value) if value != 0.0 => return Ok(1.0),
                    _ => return Err(lhs_err),
                },
            }
        }
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
            rumoca_core::OpBinary::And => unreachable!("handled before operand evaluation"),
            rumoca_core::OpBinary::Or => unreachable!("handled before operand evaluation"),
            rumoca_core::OpBinary::Assign | rumoca_core::OpBinary::Empty => {
                Err(LowerError::Unsupported {
                    reason: "unsupported operator in for-loop range expression".to_string(),
                })
            }
        }
    }

    pub(super) fn eval_compile_time_if(
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

    pub(super) fn eval_compile_time_builtin(
        &self,
        function: rumoca_core::BuiltinFunction,
        args: &[rumoca_core::Expression],
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        if matches!(function, rumoca_core::BuiltinFunction::Size) {
            return self.eval_compile_time_size(args, const_scope);
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
                    return Err(LowerError::Unsupported {
                        reason: "mod() denominator cannot be zero in for-loop range expression"
                            .to_string(),
                    });
                }
                Ok(arg0 - (arg0 / arg1).floor() * arg1)
            }
            rumoca_core::BuiltinFunction::Div => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                if arg1 == 0.0 {
                    return Err(LowerError::Unsupported {
                        reason: "div() denominator cannot be zero in for-loop range expression"
                            .to_string(),
                    });
                }
                Ok((arg0 / arg1).floor())
            }
            rumoca_core::BuiltinFunction::Rem => {
                let arg1 = eval_builtin_arg(self, args, 1, const_scope)?;
                if arg1 == 0.0 {
                    return Err(LowerError::Unsupported {
                        reason: "rem() denominator cannot be zero in for-loop range expression"
                            .to_string(),
                    });
                }
                Ok(arg0 % arg1)
            }
            _ => Err(LowerError::Unsupported {
                reason: format!(
                    "builtin `{}` is unsupported in for-loop range expression",
                    function.name()
                ),
            }),
        }
    }

    pub(super) fn eval_compile_time_size(
        &self,
        args: &[rumoca_core::Expression],
        const_scope: &IndexMap<String, f64>,
    ) -> Result<f64, LowerError> {
        let dim = if let Some(dim_expr) = args.get(1) {
            self.eval_compile_time_int(dim_expr, const_scope, "size dimension")? as usize
        } else {
            1
        };
        let Some(expr) = args.first() else {
            return Err(LowerError::Unsupported {
                reason: "size() requires an array expression".to_string(),
            });
        };
        let rumoca_core::Expression::VarRef {
            name, subscripts, ..
        } = expr
        else {
            let dims = self.infer_expr_dims(expr, &Scope::new());
            if let Some(value) = dims.get(dim.saturating_sub(1)).copied() {
                return Ok(value as f64);
            }
            if dims.is_empty()
                && dim == 1
                && matches!(
                    expr,
                    rumoca_core::Expression::Literal { .. }
                        | rumoca_core::Expression::Empty { .. }
                        | rumoca_core::Expression::Index { .. }
                )
            {
                return Ok(1.0);
            }
            return Err(LowerError::Unsupported {
                reason: format!(
                    "size() in for-loop range requires known expression dimension {dim}"
                ),
            });
        };
        if !subscripts.is_empty() {
            return Ok(1.0);
        }
        let key = size_binding_key(name.as_str(), dim);
        self.structural_bindings
            .get(key.as_str())
            .copied()
            .ok_or_else(|| LowerError::Unsupported {
                reason: format!(
                    "size() in for-loop range requires known dimension `{}`",
                    name.as_str()
                ),
            })
    }

    /// Returns `true` when lowering should stop due to `return`.
    pub(super) fn lower_statement(
        &mut self,
        statement: &rumoca_core::Statement,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        match statement {
            rumoca_core::Statement::Empty { .. } => Ok(false),
            rumoca_core::Statement::Return { .. } => {
                let returned = self.emit_const(1.0);
                scope.insert(ComponentPath::from_flat_path(RETURN_FLAG_BINDING), returned);
                Ok(true)
            }
            rumoca_core::Statement::Assignment { comp, value, .. } => {
                let target = assignment_target(comp, &self.local_const_bindings)?;
                if target.indices.is_none()
                    && self.bind_record_component_assignment(
                        scope,
                        &target.base,
                        value,
                        call_depth,
                    )?
                {
                    return Ok(false);
                }
                let values = self.lower_array_like_values(value, scope, call_depth)?;
                if let Some(indices) = target
                    .indices
                    .as_deref()
                    .filter(|indices| !indices.is_empty())
                {
                    // MLS §11.1.2: algorithm assignments target component references.
                    // A subscripted target updates only the selected array component.
                    let values = self.guard_indexed_assignment_after_return(
                        scope,
                        &target.base,
                        indices,
                        values,
                    );
                    self.bind_indexed_assignment_values(scope, &target.base, indices, &values)?;
                } else {
                    let values = self.guard_assignment_after_return(scope, &target.base, values);
                    self.bind_assignment_values(scope, &target.base, &values);
                    self.bind_record_constructor_assignment_fields(
                        scope,
                        &target.base,
                        value,
                        call_depth,
                    )?;
                }
                Ok(false)
            }
            rumoca_core::Statement::If {
                cond_blocks,
                else_block,
                ..
            } => self.lower_if_statement(cond_blocks, else_block, scope, call_depth),
            rumoca_core::Statement::For {
                indices, equations, ..
            } => self.lower_for_statement(indices, equations, scope, call_depth),
            rumoca_core::Statement::While { block, .. } => {
                self.lower_while_statement(block, scope, call_depth)
            }
            rumoca_core::Statement::FunctionCall {
                comp,
                args,
                outputs,
                span,
            } => self.lower_function_call_statement(comp, args, outputs, *span, scope, call_depth),
            rumoca_core::Statement::Break { .. } => {
                scope.insert(
                    ComponentPath::from_flat_path(BREAK_FLAG_BINDING),
                    self.emit_const(1.0),
                );
                Ok(true)
            }
            _ => Err(LowerError::Unsupported {
                reason: format!(
                    "function statement {:?} is unsupported",
                    statement_tag(statement)
                ),
            }),
        }
    }

    fn bind_record_component_assignment(
        &mut self,
        scope: &mut Scope,
        target: &str,
        value: &rumoca_core::Expression,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = value
            && !*is_constructor
            && self.bind_record_function_call_assignment(scope, target, name, args, call_depth)?
        {
            return Ok(true);
        }

        if self.bind_record_if_assignment(scope, target, value, call_depth)? {
            return Ok(true);
        }

        let Ok(source) = binding_base_key(value) else {
            return Ok(false);
        };
        if source == target {
            return Ok(false);
        }

        let source_prefix = format!("{source}.");
        let scope_components = scope
            .iter()
            .into_iter()
            .filter_map(|(key, reg)| {
                key.as_str()
                    .strip_prefix(source_prefix.as_str())
                    .map(|suffix| (suffix.to_string(), reg))
            })
            .collect::<Vec<_>>();
        let layout_components = self
            .layout
            .bindings()
            .iter()
            .filter_map(|(key, slot)| {
                key.strip_prefix(source_prefix.as_str())
                    .map(|suffix| (key.clone(), suffix.to_string(), *slot))
            })
            .collect::<Vec<_>>();
        let direct_components = self
            .direct_assignments
            .keys()
            .filter_map(|key| {
                key.strip_prefix(source_prefix.as_str())
                    .map(|suffix| (key.clone(), suffix.to_string()))
            })
            .collect::<Vec<_>>();

        if scope_components.is_empty()
            && layout_components.is_empty()
            && direct_components.is_empty()
        {
            return Ok(false);
        }

        self.clear_record_component_bindings(scope, target);
        for (suffix, reg) in scope_components {
            let target_key = format!("{target}.{suffix}");
            scope.insert(ComponentPath::from_flat_path(&target_key), reg);
            self.copy_component_shape(&format!("{source}.{suffix}"), &target_key);
        }
        for (source_key, suffix, slot) in layout_components {
            let target_key = format!("{target}.{suffix}");
            let target_path = ComponentPath::from_flat_path(&target_key);
            if scope.contains_key(&target_path) {
                continue;
            }
            scope.insert(target_path, self.emit_slot_load(slot)?);
            self.copy_component_shape(&source_key, &target_key);
        }
        for (source_key, suffix) in direct_components {
            let target_key = format!("{target}.{suffix}");
            let target_path = ComponentPath::from_flat_path(&target_key);
            if scope.contains_key(&target_path) {
                continue;
            }
            if let Some(values) =
                self.lower_direct_assignment_values_for_key(&source_key, scope, call_depth + 1)?
            {
                self.bind_assignment_values(scope, &target_key, &values);
                self.copy_component_shape(&source_key, &target_key);
            }
        }

        self.copy_indexed_record_component_bindings(&source, target);
        Ok(true)
    }

    fn bind_record_if_assignment(
        &mut self,
        scope: &mut Scope,
        target: &str,
        value: &rumoca_core::Expression,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let Some(fields) = record_if_assignment_fields(value) else {
            return Ok(false);
        };
        self.clear_record_component_bindings(scope, target);
        for field in fields {
            let projected = record_if_field_expression(value, &field)?;
            let values = self.lower_array_like_values(&projected, scope, call_depth)?;
            let values =
                self.guard_assignment_after_return(scope, &format!("{target}.{field}"), values);
            self.bind_assignment_values(scope, &format!("{target}.{field}"), &values);
        }
        Ok(true)
    }

    fn bind_record_function_call_assignment(
        &mut self,
        caller_scope: &mut Scope,
        target: &str,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let Some(materialized) = self.materialize_single_record_function_call_components(
            name,
            args,
            caller_scope,
            call_depth,
        )?
        else {
            return Ok(false);
        };
        if materialized.components.is_empty() {
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: "record function output had no assigned components".to_string(),
            });
        }

        self.clear_record_component_bindings(caller_scope, target);
        for component in materialized.components {
            let target_key = format!("{target}.{}", component.suffix);
            caller_scope.insert(ComponentPath::from_flat_path(&target_key), component.reg);
            if let Some(dims) = component.dims {
                self.local_binding_dims.insert(target_key.clone(), dims);
                self.set_known_empty_local_array(&target_key, component.known_empty);
            }
        }
        for (suffix, bindings) in materialized.indexed_components {
            self.local_indexed_bindings
                .insert(format!("{target}.{suffix}"), bindings);
        }
        Ok(true)
    }

    fn set_known_empty_local_array(&mut self, target_key: &str, known_empty: bool) {
        if known_empty {
            self.known_empty_local_arrays.insert(target_key.to_string());
            return;
        }
        self.known_empty_local_arrays.shift_remove(target_key);
    }

    fn clear_record_component_bindings(&mut self, scope: &mut Scope, target: &str) {
        let target_prefix = format!("{target}.");
        let scope_keys = scope
            .keys()
            .into_iter()
            .filter(|key| key.as_str() == target || key.as_str().starts_with(&target_prefix))
            .collect::<Vec<_>>();
        for key in scope_keys {
            scope.shift_remove(&key);
        }

        let local_indexed_keys = self
            .local_indexed_bindings
            .keys()
            .filter(|key| key.as_str() == target || key.starts_with(target_prefix.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        for key in local_indexed_keys {
            self.local_indexed_bindings.shift_remove(key.as_str());
        }

        let shape_keys = self
            .local_binding_dims
            .keys()
            .filter(|key| key.as_str() == target || key.starts_with(target_prefix.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        for key in shape_keys {
            self.local_binding_dims.shift_remove(key.as_str());
        }

        let empty_keys = self
            .known_empty_local_arrays
            .iter()
            .filter(|key| key.as_str() == target || key.starts_with(target_prefix.as_str()))
            .cloned()
            .collect::<Vec<_>>();
        for key in empty_keys {
            self.known_empty_local_arrays.shift_remove(key.as_str());
        }
    }

    fn copy_indexed_record_component_bindings(&mut self, source: &str, target: &str) {
        let source_prefix = format!("{source}.");
        let copied = self
            .local_indexed_bindings
            .iter()
            .filter_map(|(key, bindings)| {
                key.strip_prefix(source_prefix.as_str())
                    .map(|suffix| (format!("{target}.{suffix}"), bindings.clone()))
            })
            .collect::<Vec<_>>();
        for (target_key, bindings) in copied {
            self.local_indexed_bindings.insert(target_key, bindings);
        }
    }

    fn copy_component_shape(&mut self, source_key: &str, target_key: &str) {
        if let Some(dims) = self.local_binding_dims.get(source_key).cloned() {
            self.local_binding_dims.insert(target_key.to_string(), dims);
            if self.known_empty_local_arrays.contains(source_key) {
                self.known_empty_local_arrays.insert(target_key.to_string());
            } else {
                self.known_empty_local_arrays.shift_remove(target_key);
            }
            return;
        }
        if let Some(shape) = self.layout.shape(source_key) {
            self.local_binding_dims.insert(
                target_key.to_string(),
                shape.iter().map(|dim| *dim as i64).collect(),
            );
            self.known_empty_local_arrays.shift_remove(target_key);
        }
    }

    fn lower_function_call_statement(
        &mut self,
        comp: &rumoca_core::ComponentReference,
        args: &[rumoca_core::Expression],
        outputs: &[rumoca_core::ComponentReference],
        span: rumoca_core::Span,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if outputs.is_empty() {
            return Ok(false);
        }

        let function_name = comp.to_var_name();
        if let Some(reg) =
            self.lower_runtime_string_special_intrinsic(function_name.as_str(), args)?
        {
            for output in outputs {
                self.bind_statement_output_values(scope, output, &[reg])?;
            }
            return Ok(false);
        }
        if self.lower_raw_real_fft_statement(
            function_name.as_str(),
            args,
            outputs,
            scope,
            call_depth,
        )? {
            return Ok(false);
        }
        if self.lower_real_fft_statement(
            function_name.as_str(),
            args,
            outputs,
            scope,
            call_depth,
        )? {
            return Ok(false);
        }

        if outputs.len() == 1 {
            let expr = rumoca_core::Expression::FunctionCall {
                name: function_name.into(),
                args: args.to_vec(),
                is_constructor: false,
                span,
            };
            let values = self.lower_array_like_values(&expr, scope, call_depth + 1)?;
            self.bind_statement_output_values(scope, &outputs[0], &values)?;
            return Ok(false);
        }

        let Some(function) = self.lookup_function_key(function_name.as_str()).cloned() else {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "function statement `{}` with {} outputs cannot be lowered",
                    function_name.as_str(),
                    outputs.len()
                ),
            });
        };
        self.ensure_pure_inline_function(function_name.as_str(), &function)?;
        if function.outputs.len() != outputs.len() {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "function statement `{}` has {} targets for {} outputs",
                    function_name.as_str(),
                    outputs.len(),
                    function.outputs.len()
                ),
            });
        }

        let output_values = self.with_local_lower_frame(|this| {
            let bindings = this.bind_function_inputs_for_name(
                function_name.as_str(),
                &function.inputs,
                args,
                scope,
                call_depth,
            )?;
            let mut function_scope = bindings.scope;
            this.local_const_bindings.extend(bindings.const_bindings);
            this.initialize_function_output_scope(&function, &mut function_scope, call_depth)?;
            let _returned =
                this.lower_statements(&function.body, &mut function_scope, call_depth + 1)?;
            function
                .outputs
                .iter()
                .map(|output| this.scoped_function_output_values(output, &function_scope))
                .collect::<Result<Vec<_>, _>>()
        })?;

        for (target, values) in outputs.iter().zip(output_values.iter()) {
            self.bind_statement_output_values(scope, target, values)?;
        }
        Ok(false)
    }

    fn lower_raw_real_fft_statement(
        &mut self,
        function_name: &str,
        args: &[rumoca_core::Expression],
        outputs: &[rumoca_core::ComponentReference],
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if rumoca_core::top_level_last_segment(function_name) != "rawRealFFT" {
            return Ok(false);
        }
        let [input] = args else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "rawRealFFT requires one vector input".to_string(),
            });
        };
        let [info, amplitudes, phases] = outputs else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "rawRealFFT requires info, amplitudes, and phases outputs".to_string(),
            });
        };

        let samples = self.lower_array_like_values(input, scope, call_depth + 1)?;
        if samples.is_empty() || samples.len() % 2 != 0 {
            let one = self.emit_const(1.0);
            self.bind_statement_output_values(scope, info, &[one])?;
            return Ok(true);
        }

        let info_ok = self.emit_const(0.0);
        self.bind_statement_output_values(scope, info, &[info_ok])?;
        let (amplitude_values, phase_values) = self.lower_raw_real_fft_values(&samples, false);
        self.bind_statement_output_values(scope, amplitudes, &amplitude_values)?;
        self.bind_statement_output_values(scope, phases, &phase_values)?;
        Ok(true)
    }

    fn lower_real_fft_statement(
        &mut self,
        function_name: &str,
        args: &[rumoca_core::Expression],
        outputs: &[rumoca_core::ComponentReference],
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if rumoca_core::top_level_last_segment(function_name) != "realFFT" {
            return Ok(false);
        }
        let Some(samples_expr) = args.first() else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "realFFT requires a vector input".to_string(),
            });
        };
        if outputs.len() != 2 && outputs.len() != 3 {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "realFFT requires info/amplitudes or info/amplitudes/phases outputs"
                    .to_string(),
            });
        }

        let samples = self.lower_array_like_values(samples_expr, scope, call_depth + 1)?;
        if samples.is_empty() || samples.len() % 2 != 0 {
            let one = self.emit_const(1.0);
            self.bind_statement_output_values(scope, &outputs[0], &[one])?;
            return Ok(true);
        }
        let nfi = self
            .statement_output_width(&outputs[1])
            .or_else(|| {
                args.get(1)
                    .and_then(|arg| {
                        self.eval_compile_time_expr(arg, &self.local_const_bindings)
                            .ok()
                    })
                    .map(|value| value.max(1.0) as usize)
            })
            .unwrap_or(samples.len() / 2 + 1)
            .min(samples.len() / 2 + 1);

        let mean = self.lower_mean(&samples);
        let centered = samples
            .iter()
            .map(|sample| self.emit_binary(BinaryOp::Sub, *sample, mean))
            .collect::<Vec<_>>();
        let (mut amplitudes, mut phases) = self.lower_raw_real_fft_values(&centered, true);
        amplitudes.truncate(nfi);
        phases.truncate(nfi);
        if let Some(first) = amplitudes.first_mut() {
            *first = mean;
        }
        if let Some(first) = phases.first_mut() {
            *first = self.emit_const(0.0);
        }
        self.zero_noise_phases(&amplitudes, &mut phases);

        let zero = self.emit_const(0.0);
        self.bind_statement_output_values(scope, &outputs[0], &[zero])?;
        self.bind_statement_output_values(scope, &outputs[1], &amplitudes)?;
        if let Some(phases_output) = outputs.get(2) {
            self.bind_statement_output_values(scope, phases_output, &phases)?;
        }
        Ok(true)
    }

    fn statement_output_width(&self, output: &rumoca_core::ComponentReference) -> Option<usize> {
        let target = assignment_target(output, &self.local_const_bindings).ok()?;
        if target
            .indices
            .as_ref()
            .is_some_and(|indices| !indices.is_empty())
        {
            return Some(1);
        }
        self.local_binding_dims
            .get(&target.base)
            .and_then(|dims| concrete_dims_width(dims))
            .or_else(|| {
                self.layout
                    .shape(&target.base)
                    .map(|shape| shape.iter().product())
            })
    }

    fn bind_statement_output_values(
        &mut self,
        scope: &mut Scope,
        comp: &rumoca_core::ComponentReference,
        values: &[Reg],
    ) -> Result<(), LowerError> {
        let target = assignment_target(comp, &self.local_const_bindings)?;
        if let Some(indices) = target
            .indices
            .as_deref()
            .filter(|indices| !indices.is_empty())
        {
            let values = self.guard_indexed_assignment_after_return(
                scope,
                &target.base,
                indices,
                values.to_vec(),
            );
            self.bind_indexed_assignment_values(scope, &target.base, indices, &values)
        } else {
            let values = self.guard_assignment_after_return(scope, &target.base, values.to_vec());
            self.bind_assignment_values(scope, &target.base, &values);
            Ok(())
        }
    }

    pub(super) fn guard_assignment_after_return(
        &mut self,
        scope: &Scope,
        target: &str,
        values: Vec<Reg>,
    ) -> Vec<Reg> {
        let Some(guard) = self.assignment_control_guard(scope) else {
            return values;
        };
        values
            .into_iter()
            .enumerate()
            .map(|(idx, new_value)| {
                let old_value = old_assignment_value(self, scope, target, idx);
                self.emit_select(guard, old_value, new_value)
            })
            .collect()
    }

    pub(super) fn guard_indexed_assignment_after_return(
        &mut self,
        scope: &Scope,
        target: &str,
        indices: &[usize],
        values: Vec<Reg>,
    ) -> Vec<Reg> {
        let Some(guard) = self.assignment_control_guard(scope) else {
            return values;
        };
        values
            .into_iter()
            .map(|new_value| {
                let old_value = old_indexed_assignment_value(self, scope, target, indices);
                self.emit_select(guard, old_value, new_value)
            })
            .collect()
    }

    fn assignment_control_guard(&mut self, scope: &Scope) -> Option<Reg> {
        let return_key = ComponentPath::from_flat_path(RETURN_FLAG_BINDING);
        let break_key = ComponentPath::from_flat_path(BREAK_FLAG_BINDING);
        match (
            scope.get(&return_key).copied(),
            scope.get(&break_key).copied(),
        ) {
            (Some(returned), Some(broken)) => {
                Some(self.emit_binary(BinaryOp::Or, returned, broken))
            }
            (Some(returned), None) => Some(returned),
            (None, Some(broken)) => Some(broken),
            (None, None) => None,
        }
    }

    pub(super) fn bind_indexed_assignment_values(
        &mut self,
        scope: &mut Scope,
        target: &str,
        indices: &[usize],
        values: &[Reg],
    ) -> Result<(), LowerError> {
        let [value] = values else {
            return Err(LowerError::Unsupported {
                reason: format!(
                    "indexed assignment target `{target}` requires scalar RHS in solve-IR function lowering"
                ),
            });
        };

        let key = format_subscript_binding_key(target, indices);
        scope.insert(ComponentPath::from_flat_path(&key), *value);
        upsert_local_indexed_binding(
            self.local_indexed_bindings
                .entry(target.to_string())
                .or_default(),
            indices,
            *value,
        );
        self.known_empty_local_arrays.shift_remove(target);
        if indices.iter().all(|index| *index == 1) {
            scope.insert(ComponentPath::from_flat_path(target), *value);
        }
        Ok(())
    }

    fn bind_record_constructor_assignment_fields(
        &mut self,
        scope: &mut Scope,
        target: &str,
        value: &rumoca_core::Expression,
        call_depth: usize,
    ) -> Result<(), LowerError> {
        let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = value
        else {
            return Ok(());
        };
        if !self.is_record_constructor_call(name, *is_constructor) {
            return Ok(());
        }
        let Some(constructor) = self.lookup_function(name).cloned() else {
            return Ok(());
        };

        let (named_args, positional_args) =
            super::function_calls::split_named_and_positional_call_args(name.as_str(), args)?;
        let mut positional_idx = 0usize;
        for input in &constructor.inputs {
            let arg_expr = named_args.get(input.name.as_str()).copied().or_else(|| {
                let positional = positional_args.get(positional_idx).copied();
                positional_idx += usize::from(positional.is_some());
                positional
            });
            let values = if let Some(expr) = arg_expr {
                self.lower_array_like_values(expr, scope, call_depth)?
            } else if let Some(default) = input.default.as_ref() {
                self.lower_array_like_values(default, scope, call_depth + 1)?
            } else {
                return Err(LowerError::InvalidFunction {
                    name: name.as_str().to_string(),
                    reason: format!(
                        "record constructor field `{}` has no actual argument or default binding",
                        input.name
                    ),
                });
            };
            let field_target = format!("{target}.{}", input.name);
            self.bind_assignment_values_with_dims(scope, &field_target, &values, &input.dims);
        }
        Ok(())
    }
}

fn start_metadata_refers_to_key(expr: &rumoca_core::Expression, key: &str) -> bool {
    binding_base_key(expr).is_ok_and(|start_key| start_key == key)
}

fn is_control_flag(name: &ComponentPath) -> bool {
    matches!(name.as_str(), RETURN_FLAG_BINDING | BREAK_FLAG_BINDING)
}

fn record_if_assignment_fields(value: &rumoca_core::Expression) -> Option<Vec<String>> {
    let rumoca_core::Expression::If {
        branches,
        else_branch,
        ..
    } = value
    else {
        return None;
    };

    let mut fields = IndexSet::new();
    for (_, branch_expr) in branches {
        collect_record_constructor_fields(branch_expr, &mut fields);
    }
    collect_record_constructor_fields(else_branch, &mut fields);
    (!fields.is_empty()).then(|| fields.into_iter().collect())
}

fn collect_record_constructor_fields(
    expr: &rumoca_core::Expression,
    fields: &mut IndexSet<String>,
) {
    let rumoca_core::Expression::FunctionCall {
        args,
        is_constructor: true,
        ..
    } = expr
    else {
        return;
    };
    for arg in args {
        if let Some((field, _)) = super::function_calls::decode_named_function_arg(arg) {
            fields.insert(field.to_string());
        }
    }
}

fn record_if_field_expression(
    value: &rumoca_core::Expression,
    field: &str,
) -> Result<rumoca_core::Expression, LowerError> {
    let rumoca_core::Expression::If {
        branches,
        else_branch,
        span,
    } = value
    else {
        return Err(LowerError::Unsupported {
            reason: "record field projection requires if expression".to_string(),
        });
    };

    Ok(rumoca_core::Expression::If {
        branches: branches
            .iter()
            .map(|(cond, branch_expr)| {
                (
                    cond.clone(),
                    record_field_projection(branch_expr.clone(), field, *span),
                )
            })
            .collect(),
        else_branch: Box::new(record_field_projection(
            else_branch.as_ref().clone(),
            field,
            *span,
        )),
        span: *span,
    })
}

fn record_field_projection(
    base: rumoca_core::Expression,
    field: &str,
    span: rumoca_core::Span,
) -> rumoca_core::Expression {
    rumoca_core::Expression::FieldAccess {
        base: Box::new(base),
        field: field.to_string(),
        span,
    }
}

fn merge_while_iteration_scope(
    builder: &mut LowerBuilder<'_>,
    cond: Reg,
    entry_scope: &Scope,
    body_scope: &Scope,
) -> Scope {
    let mut merged = entry_scope.clone();
    let names = collect_scope_names(&merged, std::slice::from_ref(body_scope), entry_scope);
    for name in names {
        let old = entry_scope.get(&name).copied().or_else(|| {
            (is_control_flag(&name) || body_scope.contains_key(&name))
                .then(|| builder.emit_const(0.0))
        });
        let new = body_scope
            .get(&name)
            .copied()
            .or(old)
            .unwrap_or_else(|| builder.emit_const(0.0));
        if let Some(old) = old {
            merged.insert(name, builder.emit_select(cond, new, old));
        }
    }
    merged
}

fn old_assignment_value(
    builder: &mut LowerBuilder<'_>,
    scope: &Scope,
    target: &str,
    idx: usize,
) -> Reg {
    let indexed_key = format_subscript_binding_key(target, &[idx + 1]);
    let indexed_path = ComponentPath::from_flat_path(&indexed_key);
    let target_path = ComponentPath::from_flat_path(target);
    scope
        .get(&indexed_path)
        .or_else(|| (idx == 0).then(|| scope.get(&target_path)).flatten())
        .copied()
        .unwrap_or_else(|| builder.emit_const(0.0))
}

fn old_indexed_assignment_value(
    builder: &mut LowerBuilder<'_>,
    scope: &Scope,
    target: &str,
    indices: &[usize],
) -> Reg {
    let indexed_key = format_subscript_binding_key(target, indices);
    let indexed_path = ComponentPath::from_flat_path(&indexed_key);
    let target_path = ComponentPath::from_flat_path(target);
    scope
        .get(&indexed_path)
        .or_else(|| {
            indices
                .iter()
                .all(|index| *index == 1)
                .then(|| scope.get(&target_path))
                .flatten()
        })
        .copied()
        .unwrap_or_else(|| builder.emit_const(0.0))
}

fn concrete_dims_width(dims: &[i64]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        let dim = usize::try_from(*dim).ok()?;
        if dim == 0 {
            return None;
        }
        acc.checked_mul(dim)
    })
}
