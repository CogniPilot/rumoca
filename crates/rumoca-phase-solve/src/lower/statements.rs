//! SPEC_0021 file-size exception: statement lowering coordinates shared scope
//! and control-flow state; split plan: move slice assignment and branch merging
//! into focused sibling modules without duplicating scope ownership.

use indexmap::{IndexMap, IndexSet};
use rumoca_ir_solve::{BinaryOp, Reg};

use super::fft::checked_fft_frequency_count;
use super::function_projection::format_subscript_binding_key;
use super::helpers::*;
use super::{
    BREAK_FLAG_BINDING, LocalIndexedBinding, LowerBuilder, LowerError, RETURN_FLAG_BINDING,
    RecordComponentSources, Scope, generated_scope_key, generated_scope_key_name, unsupported_at,
    upsert_local_indexed_binding,
};

const MAX_INLINE_WHILE_ITERS: usize = 128;
const MAX_SYMBOLIC_FOR_VALUES: usize = 4096;

/// Symbolic iteration candidates: `(index value, runtime membership guard)`.
type SymbolicIterValues = Vec<(f64, Option<Reg>)>;

/// Bounded candidate count of a symbolic for-loop interval domain.
fn checked_symbolic_domain_count(
    domain_start: i64,
    domain_end: i64,
    domain_step: i64,
    span: rumoca_core::Span,
) -> Result<usize, LowerError> {
    let count = if domain_step > 0 && domain_start <= domain_end {
        domain_end
            .checked_sub(domain_start)
            .and_then(|distance| distance.checked_add(1))
    } else if domain_step < 0 && domain_start >= domain_end {
        domain_start
            .checked_sub(domain_end)
            .and_then(|distance| distance.checked_add(1))
    } else {
        Some(0)
    }
    .and_then(|count| usize::try_from(count).ok())
    .ok_or_else(|| unsupported_at("symbolic for-loop domain is too large", span))?;
    if count > MAX_SYMBOLIC_FOR_VALUES {
        return Err(unsupported_at(
            format!(
                "symbolic for-loop domain has {count} values; maximum is {MAX_SYMBOLIC_FOR_VALUES}"
            ),
            span,
        ));
    }
    Ok(count)
}

/// Recursion state for `lower_for_iterations`.
pub(super) struct ForIterationCtx {
    pub(super) call_depth: usize,
    /// Index-variable nesting depth into `indices`.
    pub(super) depth: usize,
    /// Conjunction of enclosing symbolic-iteration guards.
    pub(super) active_guard: Option<Reg>,
}

mod compile_time;
#[cfg(test)]
mod tests;

fn copy_statement_output_regs(
    values: &[Reg],
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let mut copied =
        crate::lower_vec_with_capacity(values.len(), "statement output value count", span)?;
    copied.extend(values.iter().copied());
    Ok(copied)
}

fn copy_statement_call_args(
    args: &[rumoca_core::Expression],
    span: rumoca_core::Span,
) -> Result<Vec<rumoca_core::Expression>, LowerError> {
    let mut copied =
        crate::lower_vec_with_capacity(args.len(), "statement call argument count", span)?;
    copied.extend(args.iter().cloned());
    Ok(copied)
}

fn propagate_loop_body_constants(
    target: &mut IndexMap<String, f64>,
    body: &IndexMap<String, f64>,
    indices: &[rumoca_core::ForIndex],
) {
    let is_iterator = |name: &str| indices.iter().any(|index| index.ident == name);
    target.retain(|name, _| is_iterator(name) || body.contains_key(name));
    for (name, value) in body {
        if !is_iterator(name) {
            target.insert(name.clone(), *value);
        }
    }
}

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

    // SPEC_0021: Exception - statement-branch lowering is a compiler entry point
    // that atomically merges scalar, indexed, and compile-time scopes.
    #[allow(clippy::too_many_lines)]
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
        let branch_span = self.statement_blocks_span(cond_blocks)?;
        let mut cond_regs = crate::lower_vec_with_capacity(
            cond_blocks.len(),
            "if condition registers",
            branch_span,
        )?;
        let mut cond_spans =
            crate::lower_vec_with_capacity(cond_blocks.len(), "if condition spans", branch_span)?;
        let mut branch_scopes =
            crate::lower_vec_with_capacity(cond_blocks.len(), "if branch scopes", branch_span)?;
        let mut branch_indexed = crate::lower_vec_with_capacity(
            cond_blocks.len(),
            "if branch indexed bindings",
            branch_span,
        )?;
        let mut branch_const = crate::lower_vec_with_capacity(
            cond_blocks.len(),
            "if branch constant bindings",
            branch_span,
        )?;

        for block in cond_blocks {
            let cond_span = self.statement_expr_or_context_span(&block.cond, branch_span)?;
            let (cond, branch_scope, indexed, constants) =
                self.with_local_lower_frame(|builder| {
                    let cond = builder.lower_expr(&block.cond, &entry_scope, call_depth)?;
                    let mut branch_scope = entry_scope.clone();
                    let _returned =
                        builder.lower_statements(&block.stmts, &mut branch_scope, call_depth)?;
                    let indexed = builder.local_indexed_bindings.clone();
                    let constants = builder.local_const_bindings.clone();
                    Ok((cond, branch_scope, indexed, constants))
                })?;
            cond_regs.push(cond);
            cond_spans.push(cond_span);
            branch_scopes.push(branch_scope);
            branch_indexed.push(indexed);
            branch_const.push(constants);
        }

        let (else_scope, else_indexed, else_const) = self.with_local_lower_frame(|builder| {
            let mut else_scope = entry_scope.clone();
            if let Some(stmts) = else_block {
                let _returned = builder.lower_statements(stmts, &mut else_scope, call_depth)?;
            }
            let indexed = builder.local_indexed_bindings.clone();
            let constants = builder.local_const_bindings.clone();
            Ok((else_scope, indexed, constants))
        })?;

        let mut merged_scope = entry_scope.clone();
        let names = collect_scope_names(&merged_scope, &branch_scopes, &else_scope, branch_span)?;

        for name in names {
            let mut merged = if let Some(merged) = else_scope
                .get(&name)
                .copied()
                .or_else(|| entry_scope.get(&name).copied())
            {
                merged
            } else if is_control_flag(&name)
                || branch_scopes.iter().any(|scope| scope.contains_key(&name))
            {
                self.emit_const_at(0.0, branch_span)?
            } else {
                continue;
            };

            for ((cond, cond_span), branch_scope) in cond_regs
                .iter()
                .zip(cond_spans.iter())
                .zip(branch_scopes.iter())
                .rev()
            {
                merged = merge_branch_select(self, *cond, *cond_span, branch_scope, &name, merged)?;
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
            &cond_spans,
            &branch_indexed,
            &else_indexed,
            branch_span,
        )?;
        self.local_const_bindings = else_const
            .into_iter()
            .filter(|(name, value)| {
                branch_const
                    .iter()
                    .all(|bindings| bindings.get(name) == Some(value))
            })
            .collect();

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
        cond_spans: &[rumoca_core::Span],
        branch_indexed: &[IndexMap<String, Vec<LocalIndexedBinding>>],
        else_indexed: &IndexMap<String, Vec<LocalIndexedBinding>>,
        merge_span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
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
            let mut merged = if let Some(merged) = lookup(else_indexed, &base, &indices)
                .or_else(|| lookup(entry_indexed, &base, &indices))
            {
                merged
            } else {
                self.emit_const_at(0.0, merge_span)?
            };
            let branch_values = cond_regs
                .iter()
                .zip(cond_spans.iter())
                .zip(branch_indexed.iter())
                .rev()
                .filter_map(|((cond, span), store)| {
                    lookup(store, &base, &indices).map(|reg| (cond, span, reg))
                });
            for (cond, span, reg) in branch_values {
                merged = self.emit_select_at(*cond, reg, merged, *span)?;
            }
            upsert_local_indexed_binding(
                self.local_indexed_bindings.entry(base).or_default(),
                &indices,
                merged,
                merge_span,
            )?;
        }
        Ok(())
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
        let break_key = generated_scope_key(BREAK_FLAG_BINDING);
        let saved_break = scope.get(&break_key).copied();

        let mut const_scope = self.local_const_bindings.clone();
        let returned = scope.with_frame(|scope| {
            self.lower_for_iterations(
                indices,
                equations,
                scope,
                &mut const_scope,
                ForIterationCtx {
                    call_depth,
                    depth: 0,
                    active_guard: None,
                },
            )
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
        let break_key = generated_scope_key(BREAK_FLAG_BINDING);
        let saved_break = scope.get(&break_key).copied();
        for _ in 0..MAX_INLINE_WHILE_ITERS {
            let cond_span = self.statement_expr_span(&block.cond)?;
            let cond = self.lower_expr(&block.cond, scope, call_depth)?;
            let entry_scope = scope.clone();
            let mut body_scope = entry_scope.clone();
            let _returned = self.lower_statements(&block.stmts, &mut body_scope, call_depth)?;
            *scope = merge_while_iteration_scope(self, cond, cond_span, &entry_scope, &body_scope)?;
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
        ctx: ForIterationCtx,
    ) -> Result<bool, LowerError> {
        let ForIterationCtx {
            call_depth,
            depth,
            active_guard,
        } = ctx;
        if depth >= indices.len() {
            let saved_bindings =
                std::mem::replace(&mut self.local_const_bindings, const_scope.clone());
            let result = if let Some(guard) = active_guard {
                self.lower_guarded_for_body(equations, scope, call_depth, guard)
            } else {
                self.lower_statements(equations, scope, call_depth)
            };
            let body_bindings = std::mem::replace(&mut self.local_const_bindings, saved_bindings);
            propagate_loop_body_constants(const_scope, &body_bindings, indices);
            propagate_loop_body_constants(&mut self.local_const_bindings, &body_bindings, indices);
            return result;
        }

        let iter = &indices[depth];
        let iter_values = match self.eval_for_index_values(&iter.range, const_scope) {
            Ok(values) => values
                .into_iter()
                .map(|value| (value, None))
                .collect::<Vec<_>>(),
            Err(compile_time_error) => self
                .lower_symbolic_for_index_values(&iter.range, scope, const_scope, call_depth)?
                .ok_or(compile_time_error)?,
        };
        if iter_values.is_empty() {
            return Ok(false);
        }

        for (value, value_guard) in iter_values {
            let iter_span = self.statement_expr_span(&iter.range)?;
            let iter_reg = self.emit_const_at(value, iter_span)?;
            let iteration_guard = match (active_guard, value_guard) {
                (Some(outer), Some(inner)) => {
                    Some(self.emit_binary_at(BinaryOp::And, outer, inner, iter_span)?)
                }
                (Some(guard), None) | (None, Some(guard)) => Some(guard),
                (None, None) => None,
            };
            let returned = scope.with_frame(|scope| {
                scope.insert_scoped(generated_scope_key(&iter.ident), iter_reg)?;
                const_scope.insert(iter.ident.clone(), value);
                let result = self.lower_for_iterations(
                    indices,
                    equations,
                    scope,
                    const_scope,
                    ForIterationCtx {
                        call_depth,
                        depth: depth + 1,
                        active_guard: iteration_guard,
                    },
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

    fn lower_guarded_for_body(
        &mut self,
        equations: &[rumoca_core::Statement],
        scope: &mut Scope,
        call_depth: usize,
        guard: Reg,
    ) -> Result<bool, LowerError> {
        let span = equations
            .first()
            .map(|statement| self.statement_source_span(statement))
            .transpose()?
            .or(self.source_context_span)
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "guarded for-loop body has no source span".to_string(),
            })?;
        let entry_scope = scope.clone();
        let entry_indexed = self.local_indexed_bindings.clone();
        let (body_scope, body_indexed) = self.with_local_lower_frame(|builder| {
            let mut body_scope = entry_scope.clone();
            let _returned = builder.lower_statements(equations, &mut body_scope, call_depth)?;
            Ok((body_scope, builder.local_indexed_bindings.clone()))
        })?;

        *scope = merge_while_iteration_scope(self, guard, span, &entry_scope, &body_scope)?;
        self.merge_branch_indexed_bindings(
            &entry_indexed,
            &[guard],
            &[span],
            &[body_indexed],
            &entry_indexed,
            span,
        )?;
        // A return/break inside a runtime-guarded iteration is represented by
        // the merged control flag.  It must not stop compile-time unrolling of
        // the remaining possible iterations.
        Ok(false)
    }

    fn lower_symbolic_for_index_values(
        &mut self,
        range: &rumoca_core::Expression,
        scope: &Scope,
        const_scope: &IndexMap<String, f64>,
        call_depth: usize,
    ) -> Result<Option<SymbolicIterValues>, LowerError> {
        let rumoca_core::Expression::Range {
            start,
            step,
            end,
            span,
        } = range
        else {
            return Ok(None);
        };
        let Some((start_min, start_max)) = self.integer_expr_interval(start, const_scope)? else {
            return Ok(None);
        };
        let Some((end_min, end_max)) = self.integer_expr_interval(end, const_scope)? else {
            return Ok(None);
        };
        let step_span = step.as_deref().and_then(rumoca_core::Expression::span);
        let step = if let Some(step_expr) = step {
            match self.eval_compile_time_int(step_expr, const_scope, "for range step") {
                Ok(step) => step,
                Err(_) => return Ok(None),
            }
        } else {
            1
        };
        if step == 0 {
            return Err(unsupported_at(
                "for range step cannot be zero",
                step_span.unwrap_or(*span),
            ));
        }

        let (domain_start, domain_end, domain_step) = if step > 0 {
            (start_min, end_max, 1)
        } else {
            (start_max, end_min, -1)
        };
        let count = checked_symbolic_domain_count(domain_start, domain_end, domain_step, *span)?;

        let start_reg = self.lower_expr(start, scope, call_depth)?;
        let end_reg = self.lower_expr(end, scope, call_depth)?;
        let mut values =
            crate::lower_vec_with_capacity(count, "symbolic for-loop candidate count", *span)?;
        for candidate in build_range_values(domain_start, domain_end, domain_step) {
            let candidate_reg = self.emit_const_at(candidate, *span)?;
            let within_start = if step > 0 {
                self.emit_compare_at(
                    rumoca_ir_solve::CompareOp::Ge,
                    candidate_reg,
                    start_reg,
                    *span,
                )?
            } else {
                self.emit_compare_at(
                    rumoca_ir_solve::CompareOp::Le,
                    candidate_reg,
                    start_reg,
                    *span,
                )?
            };
            let within_end = if step > 0 {
                self.emit_compare_at(
                    rumoca_ir_solve::CompareOp::Le,
                    candidate_reg,
                    end_reg,
                    *span,
                )?
            } else {
                self.emit_compare_at(
                    rumoca_ir_solve::CompareOp::Ge,
                    candidate_reg,
                    end_reg,
                    *span,
                )?
            };
            let mut guard = self.emit_binary_at(BinaryOp::And, within_start, within_end, *span)?;
            if step.unsigned_abs() != 1 {
                let aligned =
                    self.emit_step_alignment_guard(candidate_reg, start_reg, step, *span)?;
                guard = self.emit_binary_at(BinaryOp::And, guard, aligned, *span)?;
            }
            values.push((candidate, Some(guard)));
        }
        Ok(Some(values))
    }

    /// Emit a guard that `candidate` is `start + k*|step|` for integer `k`.
    fn emit_step_alignment_guard(
        &mut self,
        candidate_reg: Reg,
        start_reg: Reg,
        step: i64,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let delta = if step > 0 {
            self.emit_binary_at(BinaryOp::Sub, candidate_reg, start_reg, span)?
        } else {
            self.emit_binary_at(BinaryOp::Sub, start_reg, candidate_reg, span)?
        };
        let modulus = self.emit_const_at(step.unsigned_abs() as f64, span)?;
        let quotient = self.emit_binary_at(BinaryOp::Div, delta, modulus, span)?;
        let quotient_floor = self.emit_unary_at(rumoca_ir_solve::UnaryOp::Floor, quotient, span)?;
        let multiple = self.emit_binary_at(BinaryOp::Mul, quotient_floor, modulus, span)?;
        let remainder = self.emit_binary_at(BinaryOp::Sub, delta, multiple, span)?;
        let zero = self.emit_const_at(0.0, span)?;
        self.emit_compare_at(rumoca_ir_solve::CompareOp::Eq, remainder, zero, span)
    }

    /// Returns `true` when lowering should stop due to `return`.
    // SPEC_0021: Exception - exhaustive Modelica statement dispatch remains in
    // one compiler entry point so return/break control flow is consistent.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    pub(super) fn lower_statement(
        &mut self,
        statement: &rumoca_core::Statement,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        match statement {
            rumoca_core::Statement::Empty { .. } => Ok(false),
            rumoca_core::Statement::Return { span } => {
                let returned = self.emit_const_at(1.0, *span)?;
                scope.insert(generated_scope_key(RETURN_FLAG_BINDING), returned);
                Ok(true)
            }
            rumoca_core::Statement::Assignment { comp, value, span } => {
                if component_reference_has_slice_subscript(comp) {
                    self.lower_slice_assignment(comp, value, scope, call_depth, *span)?;
                    return Ok(false);
                }
                let target = assignment_target(comp, &self.local_const_bindings)?;
                let assigned_const = target
                    .indices
                    .is_none()
                    .then(|| {
                        self.eval_compile_time_expr(value, &self.local_const_bindings)
                            .ok()
                    })
                    .flatten();
                if let Some(indices) = target
                    .indices
                    .as_deref()
                    .filter(|indices| !indices.is_empty())
                {
                    let indexed_target = format_subscript_binding_key(&target.base, indices);
                    if self.bind_record_component_assignment(
                        scope,
                        &indexed_target,
                        value,
                        *span,
                        call_depth,
                    )? {
                        self.local_const_bindings.shift_remove(&target.base);
                        return Ok(false);
                    }
                }
                if target.indices.is_none()
                    && self.bind_record_component_assignment(
                        scope,
                        &target.base,
                        value,
                        *span,
                        call_depth,
                    )?
                {
                    self.local_const_bindings.shift_remove(&target.base);
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
                        comp.span,
                    )?;
                    self.bind_indexed_assignment_values(
                        scope,
                        &target.base,
                        indices,
                        &values,
                        comp.span,
                    )?;
                } else {
                    let values =
                        self.guard_assignment_after_return(scope, &target.base, values, comp.span)?;
                    self.bind_assignment_values_at(scope, &target.base, &values, comp.span)?;
                    self.bind_record_constructor_assignment_fields(
                        scope,
                        &target.base,
                        value,
                        call_depth,
                    )?;
                }
                if let Some(value) = assigned_const {
                    self.local_const_bindings.insert(target.base.clone(), value);
                } else {
                    self.local_const_bindings.shift_remove(&target.base);
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
            rumoca_core::Statement::Break { span } => {
                let broken = self.emit_const_at(1.0, *span)?;
                scope.insert(generated_scope_key(BREAK_FLAG_BINDING), broken);
                Ok(true)
            }
            _ => Err(unsupported_at(
                format!(
                    "function statement {} is unsupported",
                    statement_tag(statement)
                ),
                self.statement_source_span(statement)?,
            )),
        }
    }

    // SPEC_0021: Exception - exhaustive local/layout slice-shape sources share
    // one assignment validation path.
    #[allow(clippy::excessive_nesting)]
    fn lower_slice_assignment(
        &mut self,
        comp: &rumoca_core::ComponentReference,
        value: &rumoca_core::Expression,
        scope: &mut Scope,
        call_depth: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let base = rumoca_core::component_ref_to_base_reference(comp)
            .as_str()
            .to_string();
        let subscripts = comp
            .parts
            .last()
            .map(|part| part.subs.as_slice())
            .ok_or_else(|| {
                LowerError::contract_violation("slice assignment target has no path", span)
            })?;
        let dims = if let Some(dims) = self.local_binding_dims.get(&base) {
            dims.iter()
                .map(|dim| {
                    usize::try_from(*dim).map_err(|_| {
                        LowerError::contract_violation(
                            format!("slice assignment target `{base}` has invalid dimension {dim}"),
                            span,
                        )
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
        } else if let Some(dims) = self.layout.shape(&base) {
            dims.to_vec()
        } else {
            return Err(LowerError::contract_violation(
                format!("slice assignment target `{base}` has no array shape metadata"),
                span,
            ));
        };
        if subscripts.len() > dims.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "slice assignment target `{base}` has {} subscripts for rank {}",
                    subscripts.len(),
                    dims.len()
                ),
                span,
            ));
        }
        let has_runtime_selector = subscripts.iter().any(|subscript| {
            matches!(
                subscript,
                rumoca_core::Subscript::Expr { expr, .. }
                    if !matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
                        && self
                            .eval_compile_time_expr(expr, &self.local_const_bindings)
                            .is_err()
            )
        });
        if has_runtime_selector {
            let values = self.lower_array_like_values(value, scope, call_depth)?;
            return self.lower_dynamic_slice_assignment(
                &base, subscripts, &dims, &values, scope, call_depth, span,
            );
        }
        let mut choices =
            crate::lower_vec_with_capacity(dims.len(), "slice assignment dimension count", span)?;
        for (dimension, dim) in dims.iter().copied().enumerate() {
            let subscript = subscripts
                .get(dimension)
                .cloned()
                .unwrap_or(rumoca_core::Subscript::Colon { span });
            choices.push(self.slice_subscript_indices(&subscript, dim, scope)?);
        }
        let index_tuples = super::array_values::index_choice_tuples(&choices, span)?;
        let values = self.lower_array_like_values(value, scope, call_depth)?;
        if values.len() != index_tuples.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "slice assignment target `{base}` selects {} values, got {} RHS values",
                    index_tuples.len(),
                    values.len()
                ),
                span,
            ));
        }
        for (indices, value) in index_tuples.iter().zip(values) {
            let guarded = self.guard_indexed_assignment_after_return(
                scope,
                &base,
                indices,
                vec![value],
                span,
            )?;
            self.bind_indexed_assignment_values(scope, &base, indices, &guarded, span)?;
        }
        self.local_const_bindings.shift_remove(&base);
        Ok(())
    }

    // SPEC_0021: Exception - dynamic slice lowering carries the full selection
    // context needed to emit guarded scalar assignments.
    #[allow(clippy::too_many_arguments)]
    fn lower_dynamic_slice_assignment(
        &mut self,
        base: &str,
        subscripts: &[rumoca_core::Subscript],
        dims: &[usize],
        values: &[Reg],
        scope: &mut Scope,
        call_depth: usize,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let parts =
            self.lower_array_like_selection_parts(subscripts, dims, span, scope, call_depth)?;
        let slice_choices = parts
            .iter()
            .filter_map(|part| part.slice_indices.clone())
            .collect::<Vec<_>>();
        let output_tuples = super::array_values::index_choice_tuples(&slice_choices, span)?;
        if values.len() != output_tuples.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "dynamic slice assignment target `{base}` selects {} values, got {} RHS values",
                    output_tuples.len(),
                    values.len()
                ),
                span,
            ));
        }
        let mut target_choices = crate::lower_vec_with_capacity(
            dims.len(),
            "dynamic slice assignment target rank",
            span,
        )?;
        for dim in dims {
            let mut choices = crate::lower_vec_with_capacity(
                *dim,
                "dynamic slice assignment dimension size",
                span,
            )?;
            choices.extend(1..=*dim);
            target_choices.push(choices);
        }
        let target_tuples = super::array_values::index_choice_tuples(&target_choices, span)?;
        for indices in target_tuples {
            let Some((_, new_value)) =
                output_tuples
                    .iter()
                    .zip(values.iter().copied())
                    .find(|(output, _)| {
                        super::array_values::slice_indices_match(&parts, &indices, output)
                    })
            else {
                continue;
            };
            let condition = self.emit_non_slice_subscript_match_at(&parts, &indices, span)?;
            let old_value =
                self.old_indexed_assignment_or_dead_local_value(scope, base, &indices, span)?;
            let selected = self.emit_select_at(condition, new_value, old_value, span)?;
            let guarded = self.guard_indexed_assignment_after_return(
                scope,
                base,
                &indices,
                vec![selected],
                span,
            )?;
            self.bind_indexed_assignment_values(scope, base, &indices, &guarded, span)?;
        }
        self.local_const_bindings.shift_remove(base);
        Ok(())
    }

    fn bind_record_component_assignment(
        &mut self,
        scope: &mut Scope,
        target: &str,
        value: &rumoca_core::Expression,
        assignment_span: rumoca_core::Span,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if self.bind_special_record_component_assignment(
            scope,
            target,
            value,
            assignment_span,
            call_depth,
        )? {
            return Ok(true);
        }

        let Ok(source) = binding_base_key(value) else {
            return Ok(false);
        };
        if source == target {
            return Ok(false);
        }
        let span = self.statement_expr_or_context_span(value, assignment_span)?;

        let source_prefix = format!("{source}.");
        let scope_entries = scope.iter_checked("record scope component source count", span)?;
        let mut scope_components = crate::lower_vec_with_capacity(
            scope_entries.len(),
            "record scope component staging count",
            span,
        )?;
        for (key, reg) in scope_entries {
            let Some(key_name) = generated_scope_key_name(&key) else {
                continue;
            };
            let Some(suffix) = key_name.strip_prefix(source_prefix.as_str()) else {
                continue;
            };
            scope_components.push((suffix.to_string(), reg));
        }
        let immutable_components = self.record_component_sources(&source, span)?;

        if scope_components.is_empty()
            && immutable_components.layout.is_empty()
            && immutable_components.direct.is_empty()
        {
            return Ok(false);
        }

        self.clear_record_component_bindings(scope, target, span)?;
        for (suffix, reg) in scope_components {
            let target_key = format!("{target}.{suffix}");
            scope.insert(generated_scope_key(&target_key), reg);
            self.copy_component_shape(&format!("{source}.{suffix}"), &target_key, span)?;
        }
        for (source_key, suffix, slot) in &immutable_components.layout {
            let target_key = format!("{target}.{suffix}");
            let target_scope_key = generated_scope_key(&target_key);
            if scope.contains_key(&target_scope_key) {
                continue;
            }
            scope.insert(target_scope_key, self.emit_slot_load(*slot, span)?);
            self.copy_component_shape(source_key, &target_key, span)?;
        }
        for (source_key, suffix) in &immutable_components.direct {
            let target_key = format!("{target}.{suffix}");
            let target_scope_key = generated_scope_key(&target_key);
            if scope.contains_key(&target_scope_key) {
                continue;
            }
            if let Some(values) =
                self.lower_direct_assignment_values_for_key(source_key, scope, call_depth + 1)?
            {
                self.bind_assignment_values_at(scope, &target_key, &values, span)?;
                self.copy_component_shape(source_key, &target_key, span)?;
            }
        }

        self.copy_indexed_record_component_bindings(&source, target, span)?;
        Ok(true)
    }

    fn record_component_sources(
        &self,
        source: &str,
        span: rumoca_core::Span,
    ) -> Result<std::sync::Arc<RecordComponentSources>, LowerError> {
        if let Some(components) = self
            .record_component_source_cache
            .borrow()
            .get(source)
            .cloned()
        {
            return Ok(components);
        }
        let source_prefix = format!("{source}.");
        let mut components = RecordComponentSources {
            layout: crate::lower_vec_with_capacity(
                self.layout.bindings().len(),
                "record layout component staging count",
                span,
            )?,
            direct: crate::lower_vec_with_capacity(
                self.direct_assignments.len(),
                "record direct component staging count",
                span,
            )?,
        };
        for (key, slot) in self.layout.bindings() {
            if let Some(suffix) = key.strip_prefix(source_prefix.as_str()) {
                components
                    .layout
                    .push((key.clone(), suffix.to_string(), *slot));
            }
        }
        for key in self.direct_assignments.keys() {
            if let Some(suffix) = key.strip_prefix(source_prefix.as_str()) {
                components.direct.push((key.clone(), suffix.to_string()));
            }
        }
        let components = std::sync::Arc::new(components);
        self.record_component_source_cache
            .borrow_mut()
            .insert(source.to_string(), std::sync::Arc::clone(&components));
        Ok(components)
    }

    fn bind_special_record_component_assignment(
        &mut self,
        scope: &mut Scope,
        target: &str,
        value: &rumoca_core::Expression,
        assignment_span: rumoca_core::Span,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if let rumoca_core::Expression::FunctionCall {
            name,
            args,
            is_constructor,
            ..
        } = value
            && !*is_constructor
            && self.bind_record_function_call_assignment(
                scope,
                target,
                name,
                args,
                self.statement_expr_or_context_span(value, assignment_span)?,
                call_depth,
            )?
        {
            return Ok(true);
        }
        self.bind_record_if_assignment(scope, target, value, assignment_span, call_depth)
    }

    fn bind_record_if_assignment(
        &mut self,
        scope: &mut Scope,
        target: &str,
        value: &rumoca_core::Expression,
        assignment_span: rumoca_core::Span,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let Some(fields) = record_if_assignment_fields(value) else {
            return Ok(false);
        };
        let span = self.statement_expr_or_context_span(value, assignment_span)?;
        self.clear_record_component_bindings(scope, target, span)?;
        for field in fields {
            let projected = record_if_field_expression(value, &field, span)?;
            let values = self.lower_array_like_values(&projected, scope, call_depth)?;
            let values = self.guard_assignment_after_return(
                scope,
                &format!("{target}.{field}"),
                values,
                span,
            )?;
            self.bind_assignment_values_at(scope, &format!("{target}.{field}"), &values, span)?;
        }
        Ok(true)
    }

    fn bind_record_function_call_assignment(
        &mut self,
        caller_scope: &mut Scope,
        target: &str,
        name: &rumoca_core::Reference,
        args: &[rumoca_core::Expression],
        span: rumoca_core::Span,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        let Some(materialized) = self.materialize_single_record_function_call_components(
            name,
            args,
            span,
            caller_scope,
            call_depth,
        )?
        else {
            return Ok(false);
        };
        if materialized.components.is_empty() && materialized.empty_components.is_empty() {
            return Err(LowerError::InvalidFunction {
                name: name.as_str().to_string(),
                reason: "record function output had no assigned components".to_string(),
            });
        }

        self.clear_record_component_bindings(caller_scope, target, span)?;
        for component in materialized.components {
            let target_key = format!("{target}.{}", component.suffix);
            caller_scope.insert(generated_scope_key(&target_key), component.reg);
            if let Some(dims) = component.dims {
                self.local_binding_dims.insert(target_key.clone(), dims);
                self.set_known_empty_local_array(&target_key, component.known_empty);
            }
        }
        for (suffix, dims) in materialized.empty_components {
            let target_key = format!("{target}.{suffix}");
            self.local_binding_dims.insert(target_key.clone(), dims);
            self.known_empty_local_arrays.insert(target_key);
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

    fn clear_record_component_bindings(
        &mut self,
        scope: &mut Scope,
        target: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let target_prefix = format!("{target}.");
        let scope_snapshot = scope.keys_checked("record scope cleanup source count", span)?;
        let mut scope_keys = crate::lower_vec_with_capacity(
            scope_snapshot.len(),
            "record scope cleanup key count",
            span,
        )?;
        for key in scope_snapshot {
            if generated_scope_key_name(&key)
                .is_some_and(|name| name == target || name.starts_with(&target_prefix))
            {
                scope_keys.push(key);
            }
        }
        for key in scope_keys {
            scope.shift_remove(&key);
        }

        let mut local_indexed_keys = crate::lower_vec_with_capacity(
            self.local_indexed_bindings.len(),
            "record indexed cleanup key count",
            span,
        )?;
        for key in self.local_indexed_bindings.keys() {
            if key.as_str() == target || key.starts_with(target_prefix.as_str()) {
                local_indexed_keys.push(key.clone());
            }
        }
        for key in local_indexed_keys {
            self.local_indexed_bindings.shift_remove(key.as_str());
        }

        let mut shape_keys = crate::lower_vec_with_capacity(
            self.local_binding_dims.len(),
            "record shape cleanup key count",
            span,
        )?;
        for key in self.local_binding_dims.keys() {
            if key.as_str() == target || key.starts_with(target_prefix.as_str()) {
                shape_keys.push(key.clone());
            }
        }
        for key in shape_keys {
            self.local_binding_dims.shift_remove(key.as_str());
        }

        let mut empty_keys = crate::lower_vec_with_capacity(
            self.known_empty_local_arrays.len(),
            "record empty-array cleanup key count",
            span,
        )?;
        for key in &self.known_empty_local_arrays {
            if key.as_str() == target || key.starts_with(target_prefix.as_str()) {
                empty_keys.push(key.clone());
            }
        }
        for key in empty_keys {
            self.known_empty_local_arrays.shift_remove(key.as_str());
        }
        Ok(())
    }

    fn copy_indexed_record_component_bindings(
        &mut self,
        source: &str,
        target: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let source_prefix = format!("{source}.");
        let mut copied = crate::lower_vec_with_capacity(
            self.local_indexed_bindings.len(),
            "record indexed component copy count",
            span,
        )?;
        for (key, bindings) in &self.local_indexed_bindings {
            let Some(suffix) = key.strip_prefix(source_prefix.as_str()) else {
                continue;
            };
            copied.push((format!("{target}.{suffix}"), bindings.clone()));
        }
        for (target_key, bindings) in copied {
            self.local_indexed_bindings.insert(target_key, bindings);
        }
        Ok(())
    }

    fn copy_component_shape(
        &mut self,
        source_key: &str,
        target_key: &str,
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        if let Some(dims) = self.local_binding_dims.get(source_key).cloned() {
            self.local_binding_dims.insert(target_key.to_string(), dims);
            if self.known_empty_local_arrays.contains(source_key) {
                self.known_empty_local_arrays.insert(target_key.to_string());
            } else {
                self.known_empty_local_arrays.shift_remove(target_key);
            }
            return Ok(());
        }
        if let Some(shape) = self.layout.shape(source_key) {
            let dims = checked_usize_dims_to_i64(shape, "record component shape", span)?;
            self.local_binding_dims.insert(target_key.to_string(), dims);
            self.known_empty_local_arrays.shift_remove(target_key);
        }
        Ok(())
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
            self.lower_runtime_string_special_intrinsic(function_name.as_str(), args, span)?
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
            span,
            scope,
            call_depth,
        )? {
            return Ok(false);
        }
        if self.lower_real_fft_statement(
            function_name.as_str(),
            args,
            outputs,
            span,
            scope,
            call_depth,
        )? {
            return Ok(false);
        }

        if outputs.len() == 1 {
            let expr = rumoca_core::Expression::FunctionCall {
                name: function_name.into(),
                args: copy_statement_call_args(args, span)?,
                is_constructor: false,
                span,
            };
            let values = self.lower_array_like_values(&expr, scope, call_depth + 1)?;
            self.bind_statement_output_values(scope, &outputs[0], &values)?;
            return Ok(false);
        }

        let Some(function) = self.lookup_function_key(function_name.as_str()).cloned() else {
            return Err(unsupported_at(
                format!(
                    "function statement `{}` with {} outputs cannot be lowered",
                    function_name.as_str(),
                    outputs.len()
                ),
                span,
            ));
        };
        self.ensure_pure_inline_function(function_name.as_str(), &function, span)?;
        if function.outputs.len() != outputs.len() {
            return Err(unsupported_at(
                format!(
                    "function statement `{}` has {} targets for {} outputs",
                    function_name.as_str(),
                    outputs.len(),
                    function.outputs.len()
                ),
                span,
            ));
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
            let mut output_values = crate::lower_vec_with_capacity(
                function.outputs.len(),
                "function statement output count",
                span,
            )?;
            for output in &function.outputs {
                output_values.push(this.scoped_function_output_values(output, &function_scope)?);
            }
            Ok(output_values)
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
        call_span: rumoca_core::Span,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if crate::path_utils::leaf_segment(function_name) != "rawRealFFT" {
            return Ok(false);
        }
        let [input] = args else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "rawRealFFT requires one vector input".to_string(),
            }
            .with_fallback_span(call_span));
        };
        let [info, amplitudes, phases] = outputs else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "rawRealFFT requires info, amplitudes, and phases outputs".to_string(),
            }
            .with_fallback_span(call_span));
        };

        let samples = self.lower_array_like_values(input, scope, call_depth + 1)?;
        let span = self.statement_expr_or_context_span(input, call_span)?;
        if samples.is_empty() || samples.len() % 2 != 0 {
            let one = self.emit_const_at(1.0, span)?;
            self.bind_statement_output_values(scope, info, &[one])?;
            return Ok(true);
        }

        let info_ok = self.emit_const_at(0.0, span)?;
        self.bind_statement_output_values(scope, info, &[info_ok])?;
        let (amplitude_values, phase_values) =
            self.lower_raw_real_fft_values(&samples, false, span)?;
        self.bind_statement_output_values(scope, amplitudes, &amplitude_values)?;
        self.bind_statement_output_values(scope, phases, &phase_values)?;
        Ok(true)
    }

    fn lower_real_fft_statement(
        &mut self,
        function_name: &str,
        args: &[rumoca_core::Expression],
        outputs: &[rumoca_core::ComponentReference],
        call_span: rumoca_core::Span,
        scope: &mut Scope,
        call_depth: usize,
    ) -> Result<bool, LowerError> {
        if crate::path_utils::leaf_segment(function_name) != "realFFT" {
            return Ok(false);
        }
        let Some(samples_expr) = args.first() else {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "realFFT requires a vector input".to_string(),
            }
            .with_fallback_span(call_span));
        };
        if outputs.len() != 2 && outputs.len() != 3 {
            return Err(LowerError::InvalidFunction {
                name: function_name.to_string(),
                reason: "realFFT requires info/amplitudes or info/amplitudes/phases outputs"
                    .to_string(),
            }
            .with_fallback_span(call_span));
        }

        let samples = self.lower_array_like_values(samples_expr, scope, call_depth + 1)?;
        let span = self.statement_expr_or_context_span(samples_expr, call_span)?;
        if samples.is_empty() || samples.len() % 2 != 0 {
            let one = self.emit_const_at(1.0, span)?;
            self.bind_statement_output_values(scope, &outputs[0], &[one])?;
            return Ok(true);
        }
        let nfi = if let Some(width) = self.statement_output_width(&outputs[1]) {
            width
        } else if let Some(count) = checked_real_fft_frequency_count_arg(
            self,
            args.get(1),
            &self.local_const_bindings,
            span,
        )? {
            count
        } else {
            samples.len() / 2 + 1
        }
        .min(samples.len() / 2 + 1);

        let mean = self.lower_mean(&samples, span)?;
        let mut centered =
            crate::lower_vec_with_capacity(samples.len(), "centered FFT sample count", span)?;
        for sample in &samples {
            centered.push(self.emit_binary_at(BinaryOp::Sub, *sample, mean, span)?);
        }
        let (mut amplitudes, mut phases) = self.lower_raw_real_fft_values(&centered, true, span)?;
        amplitudes.truncate(nfi);
        phases.truncate(nfi);
        if let Some(first) = amplitudes.first_mut() {
            *first = mean;
        }
        if let Some(first) = phases.first_mut() {
            *first = self.emit_const_at(0.0, span)?;
        }
        self.zero_noise_phases(&amplitudes, &mut phases, span)?;

        let zero = self.emit_const_at(0.0, span)?;
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
                    .and_then(concrete_usize_dims_width)
            })
    }

    fn bind_statement_output_values(
        &mut self,
        scope: &mut Scope,
        comp: &rumoca_core::ComponentReference,
        values: &[Reg],
    ) -> Result<(), LowerError> {
        if component_reference_has_slice_subscript(comp) {
            return self.bind_statement_slice_output_values(scope, comp, values);
        }
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
                copy_statement_output_regs(values, comp.span)?,
                comp.span,
            )?;
            self.bind_indexed_assignment_values(scope, &target.base, indices, &values, comp.span)
        } else {
            let values = self.guard_assignment_after_return(
                scope,
                &target.base,
                copy_statement_output_regs(values, comp.span)?,
                comp.span,
            )?;
            self.bind_assignment_values_at(scope, &target.base, &values, comp.span)?;
            Ok(())
        }
    }

    // SPEC_0021: Exception - exhaustive local/layout slice-shape sources share
    // one multi-output binding validation path.
    #[allow(clippy::excessive_nesting)]
    fn bind_statement_slice_output_values(
        &mut self,
        scope: &mut Scope,
        comp: &rumoca_core::ComponentReference,
        values: &[Reg],
    ) -> Result<(), LowerError> {
        let span = comp.span;
        let base = rumoca_core::component_ref_to_base_reference(comp)
            .as_str()
            .to_string();
        let subscripts = comp
            .parts
            .last()
            .map(|part| part.subs.as_slice())
            .ok_or_else(|| {
                LowerError::contract_violation("slice assignment target has no path", span)
            })?;
        let dims = if let Some(dims) = self.local_binding_dims.get(&base) {
            dims.iter()
                .map(|dim| {
                    usize::try_from(*dim).map_err(|_| {
                        LowerError::contract_violation(
                            format!("slice assignment target `{base}` has invalid dimension {dim}"),
                            span,
                        )
                    })
                })
                .collect::<Result<Vec<_>, _>>()?
        } else if let Some(dims) = self.layout.shape(&base) {
            dims.to_vec()
        } else {
            return Err(LowerError::contract_violation(
                format!("slice assignment target `{base}` has no array shape metadata"),
                span,
            ));
        };
        let has_runtime_selector = subscripts.iter().any(|subscript| {
            matches!(
                subscript,
                rumoca_core::Subscript::Expr { expr, .. }
                    if !matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
                        && self
                            .eval_compile_time_expr(expr, &self.local_const_bindings)
                            .is_err()
            )
        });
        if has_runtime_selector {
            return self
                .lower_dynamic_slice_assignment(&base, subscripts, &dims, values, scope, 0, span);
        }
        let mut choices = crate::lower_vec_with_capacity(
            dims.len(),
            "function output slice assignment dimension count",
            span,
        )?;
        for (dimension, dim) in dims.iter().copied().enumerate() {
            let subscript = subscripts
                .get(dimension)
                .cloned()
                .unwrap_or(rumoca_core::Subscript::Colon { span });
            choices.push(self.slice_subscript_indices(&subscript, dim, scope)?);
        }
        let index_tuples = super::array_values::index_choice_tuples(&choices, span)?;
        if values.len() != index_tuples.len() {
            return Err(LowerError::contract_violation(
                format!(
                    "slice assignment target `{base}` selects {} values, got {} RHS values",
                    index_tuples.len(),
                    values.len()
                ),
                span,
            ));
        }
        for (indices, value) in index_tuples.iter().zip(values.iter().copied()) {
            let guarded = self.guard_indexed_assignment_after_return(
                scope,
                &base,
                indices,
                vec![value],
                span,
            )?;
            self.bind_indexed_assignment_values(scope, &base, indices, &guarded, span)?;
        }
        self.local_const_bindings.shift_remove(&base);
        Ok(())
    }

    pub(super) fn guard_assignment_after_return(
        &mut self,
        scope: &Scope,
        target: &str,
        values: Vec<Reg>,
        span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(guard) = self.assignment_control_guard(scope, span)? else {
            return Ok(values);
        };
        let mut guarded =
            crate::lower_vec_with_capacity(values.len(), "guarded assignment value count", span)?;
        for (idx, new_value) in values.into_iter().enumerate() {
            let old_value = self.old_assignment_or_dead_local_value(scope, target, idx, span)?;
            guarded.push(self.emit_select_at(guard, old_value, new_value, span)?);
        }
        Ok(guarded)
    }

    pub(super) fn guard_indexed_assignment_after_return(
        &mut self,
        scope: &Scope,
        target: &str,
        indices: &[usize],
        values: Vec<Reg>,
        span: rumoca_core::Span,
    ) -> Result<Vec<Reg>, LowerError> {
        let Some(guard) = self.assignment_control_guard(scope, span)? else {
            return Ok(values);
        };
        let mut guarded = crate::lower_vec_with_capacity(
            values.len(),
            "guarded indexed assignment value count",
            span,
        )?;
        for new_value in values {
            let old_value =
                self.old_indexed_assignment_or_dead_local_value(scope, target, indices, span)?;
            guarded.push(self.emit_select_at(guard, old_value, new_value, span)?);
        }
        Ok(guarded)
    }

    fn old_assignment_or_dead_local_value(
        &mut self,
        scope: &Scope,
        target: &str,
        idx: usize,
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        let old_value = self
            .local_binding_dims
            .get(target)
            .and_then(|dims| super::function_projection::projection_indices_for_dims(dims, idx))
            .map_or_else(
                || old_assignment_value(scope, target, idx, span),
                |indices| old_indexed_assignment_value(scope, target, &indices, span),
            );
        match old_value {
            Ok(value) => Ok(value),
            Err(_) if self.guarded_uninitialized_locals.contains(target) => {
                self.emit_const_at(0.0, span)
            }
            Err(err) => Err(err),
        }
    }

    fn old_indexed_assignment_or_dead_local_value(
        &mut self,
        scope: &Scope,
        target: &str,
        indices: &[usize],
        span: rumoca_core::Span,
    ) -> Result<Reg, LowerError> {
        match old_indexed_assignment_value(scope, target, indices, span) {
            Ok(value) => Ok(value),
            Err(_) if self.guarded_uninitialized_locals.contains(target) => {
                self.emit_const_at(0.0, span)
            }
            Err(err) => Err(err),
        }
    }

    fn assignment_control_guard(
        &mut self,
        scope: &Scope,
        span: rumoca_core::Span,
    ) -> Result<Option<Reg>, LowerError> {
        let return_key = generated_scope_key(RETURN_FLAG_BINDING);
        let break_key = generated_scope_key(BREAK_FLAG_BINDING);
        let guard = match (
            scope.get(&return_key).copied(),
            scope.get(&break_key).copied(),
        ) {
            (Some(returned), Some(broken)) => {
                Some(self.emit_binary_at(BinaryOp::Or, returned, broken, span)?)
            }
            (Some(returned), None) => Some(returned),
            (None, Some(broken)) => Some(broken),
            (None, None) => None,
        };
        Ok(guard)
    }

    pub(super) fn bind_indexed_assignment_values(
        &mut self,
        scope: &mut Scope,
        target: &str,
        indices: &[usize],
        values: &[Reg],
        span: rumoca_core::Span,
    ) -> Result<(), LowerError> {
        let [value] = values else {
            return Err(unsupported_at(
                format!(
                    "indexed assignment target `{target}` requires scalar RHS in solve-IR function lowering"
                ),
                span,
            ));
        };

        let key = format_subscript_binding_key(target, indices);
        self.clear_local_const_assignment(target);
        self.clear_local_const_assignment(&key);
        scope.insert(generated_scope_key(&key), *value);
        scope.insert_indexed(&generated_scope_key(target), indices, *value, span)?;
        upsert_local_indexed_binding(
            self.local_indexed_bindings
                .entry(target.to_string())
                .or_default(),
            indices,
            *value,
            span,
        )?;
        self.known_empty_local_arrays.shift_remove(target);
        if indices.iter().all(|index| *index == 1) {
            scope.insert(generated_scope_key(target), *value);
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
            let (values, span) = if let Some(expr) = arg_expr {
                (
                    self.lower_array_like_values(expr, scope, call_depth)?,
                    expr.span().unwrap_or(input.span),
                )
            } else if let Some(default) = input.default.as_ref() {
                (
                    self.lower_array_like_values(default, scope, call_depth + 1)?,
                    default.span().unwrap_or(input.span),
                )
            } else {
                return Err(LowerError::MissingActualArgument {
                    function: name.as_str().to_string(),
                    what: "record constructor field",
                    input: input.name.clone(),
                    span: input.span,
                });
            };
            let field_target = format!("{target}.{}", input.name);
            self.bind_assignment_values_with_dims(
                scope,
                &field_target,
                &values,
                &input.dims,
                span,
            )?;
        }
        Ok(())
    }
}

fn start_metadata_refers_to_key(expr: &rumoca_core::Expression, key: &str) -> bool {
    binding_base_key(expr).is_ok_and(|start_key| start_key == key)
}

fn is_control_flag(name: &rumoca_ir_solve::ComponentReferenceKey) -> bool {
    match name {
        rumoca_ir_solve::ComponentReferenceKey::Generated { name } => {
            matches!(name.as_str(), RETURN_FLAG_BINDING | BREAK_FLAG_BINDING)
        }
        rumoca_ir_solve::ComponentReferenceKey::Source { .. } => false,
    }
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
    owner_span: rumoca_core::Span,
) -> Result<rumoca_core::Expression, LowerError> {
    let rumoca_core::Expression::If {
        branches,
        else_branch,
        span,
    } = value
    else {
        return Err(unsupported_at(
            "record field projection requires if expression",
            owner_span,
        ));
    };

    let span = span_or_owner(*span, owner_span);
    let mut projected_branches =
        crate::lower_vec_with_capacity(branches.len(), "record if branch projection count", span)?;
    for (cond, branch_expr) in branches {
        projected_branches.push((
            cond.clone(),
            record_field_projection(branch_expr.clone(), field, span),
        ));
    }

    Ok(rumoca_core::Expression::If {
        branches: projected_branches,
        else_branch: Box::new(record_field_projection(
            else_branch.as_ref().clone(),
            field,
            span,
        )),
        span,
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
    span: rumoca_core::Span,
    entry_scope: &Scope,
    body_scope: &Scope,
) -> Result<Scope, LowerError> {
    let mut merged = entry_scope.clone();
    let names = collect_scope_names(&merged, std::slice::from_ref(body_scope), entry_scope, span)?;
    for name in names {
        let old = if let Some(old) = entry_scope.get(&name).copied() {
            Some(old)
        } else if is_control_flag(&name) || body_scope.contains_key(&name) {
            Some(builder.emit_const_at(0.0, span)?)
        } else {
            None
        };
        let new = body_scope
            .get(&name)
            .copied()
            .or(old)
            .map(Ok)
            .unwrap_or_else(|| builder.emit_const_at(0.0, span))?;
        if let Some(old) = old {
            merged.insert(name, builder.emit_select_at(cond, new, old, span)?);
        }
    }
    Ok(merged)
}

fn old_assignment_value(
    scope: &Scope,
    target: &str,
    idx: usize,
    span: rumoca_core::Span,
) -> Result<Reg, LowerError> {
    let indexed_key = format_subscript_binding_key(target, &[idx + 1]);
    let indexed_key = generated_scope_key(&indexed_key);
    let target_key = generated_scope_key(target);
    scope
        .get(&indexed_key)
        .or_else(|| (idx == 0).then(|| scope.get(&target_key)).flatten())
        .copied()
        .ok_or_else(|| missing_guarded_assignment_binding(target, span))
}

fn old_indexed_assignment_value(
    scope: &Scope,
    target: &str,
    indices: &[usize],
    span: rumoca_core::Span,
) -> Result<Reg, LowerError> {
    let indexed_key = format_subscript_binding_key(target, indices);
    let indexed_scope_key = generated_scope_key(&indexed_key);
    let target_key = generated_scope_key(target);
    scope
        .get(&indexed_scope_key)
        .or_else(|| {
            indices
                .iter()
                .all(|index| *index == 1)
                .then(|| scope.get(&target_key))
                .flatten()
        })
        .copied()
        .ok_or_else(|| missing_guarded_assignment_binding(indexed_key.as_str(), span))
}

fn missing_guarded_assignment_binding(target: &str, span: rumoca_core::Span) -> LowerError {
    LowerError::contract_violation(
        format!(
            "guarded assignment to `{target}` requires an existing binding to preserve on return/break"
        ),
        span,
    )
}

fn positive_size_dimension(value: i64, span: rumoca_core::Span) -> Result<usize, LowerError> {
    if value <= 0 {
        return Err(unsupported_at("size dimension must be positive", span));
    }
    usize::try_from(value).map_err(|_| {
        LowerError::contract_violation(
            format!("size dimension {value} exceeds host index range"),
            span,
        )
    })
}

fn checked_real_fft_frequency_count_arg(
    builder: &LowerBuilder<'_>,
    arg: Option<&rumoca_core::Expression>,
    const_scope: &IndexMap<String, f64>,
    call_span: rumoca_core::Span,
) -> Result<Option<usize>, LowerError> {
    let Some(arg) = arg else {
        return Ok(None);
    };
    let span = builder.statement_expr_or_context_span(arg, call_span)?;
    let value = builder
        .eval_compile_time_expr(arg, const_scope)
        .map_err(|err| err.with_fallback_span(span))?;
    checked_fft_frequency_count(value, span).map(Some)
}

impl<'a> LowerBuilder<'a> {
    fn statement_blocks_span(
        &self,
        cond_blocks: &[rumoca_core::StatementBlock],
    ) -> Result<rumoca_core::Span, LowerError> {
        cond_blocks
            .iter()
            .find_map(|block| block.cond.span())
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement condition blocks".to_string(),
            })
    }

    fn statement_expr_span(
        &self,
        expr: &rumoca_core::Expression,
    ) -> Result<rumoca_core::Span, LowerError> {
        expr.span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement expression".to_string(),
            })
    }

    fn statement_expr_or_context_span(
        &self,
        expr: &rumoca_core::Expression,
        context_span: rumoca_core::Span,
    ) -> Result<rumoca_core::Span, LowerError> {
        expr.span()
            .or_else(|| (!context_span.is_dummy()).then_some(context_span))
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement expression".to_string(),
            })
    }

    fn statement_source_span(
        &self,
        statement: &rumoca_core::Statement,
    ) -> Result<rumoca_core::Span, LowerError> {
        statement
            .source_span()
            .or_else(|| self.active_source_context_span())
            .ok_or_else(|| LowerError::UnspannedContractViolation {
                reason: "missing source provenance for statement".to_string(),
            })
    }
}

fn checked_usize_dims_to_i64(
    dims: &[usize],
    context: &str,
    span: rumoca_core::Span,
) -> Result<Vec<i64>, LowerError> {
    let mut converted = crate::lower_vec_with_capacity(
        dims.len(),
        "checked usize dimension conversion count",
        span,
    )?;
    for dim in dims {
        converted.push(i64::try_from(*dim).map_err(|_| {
            LowerError::contract_violation(
                format!("{context} dimension {dim} exceeds i64 range"),
                span,
            )
        })?);
    }
    Ok(converted)
}

fn unsupported_with_optional_span(
    reason: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> LowerError {
    let reason = reason.into();
    match span {
        Some(span) => unsupported_at(reason, span),
        None => LowerError::Unsupported { reason },
    }
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

fn concrete_usize_dims_width(dims: &[usize]) -> Option<usize> {
    if dims.is_empty() {
        return None;
    }
    dims.iter().try_fold(1usize, |acc, dim| {
        if *dim == 0 {
            return None;
        }
        acc.checked_mul(*dim)
    })
}

fn component_reference_has_slice_subscript(comp: &rumoca_core::ComponentReference) -> bool {
    comp.parts
        .iter()
        .flat_map(|part| &part.subs)
        .any(|subscript| {
            matches!(subscript, rumoca_core::Subscript::Colon { .. })
                || matches!(
                    subscript,
                    rumoca_core::Subscript::Expr { expr, .. }
                        if matches!(expr.as_ref(), rumoca_core::Expression::Range { .. })
                )
        })
}
