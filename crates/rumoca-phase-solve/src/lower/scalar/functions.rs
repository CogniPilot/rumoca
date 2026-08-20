//! Function calls, record projections, and function loops.
//!
//! A Solve program has no call op: every call is lowered through the callee's
//! own checked result definition, with the caller's arguments bound on an
//! explicit stack so a parameter cannot escape the call that supplies it.

// SPEC_0021 file-size exception - split plan: extract record projection lowering into lower/scalar/functions/records.rs and function-loop lowering into lower/scalar/functions/loops.rs; tracked as RDD2/GALEC cleanup debt (dev/2026-08-11 remediation note).
use super::*;

type RecordCondition<'dae> = (dae::ExprId<'dae>, solve::Reg, dae::ExprId<'dae>);
type FoldInvariantIndexBase<'dae> = (dae::ExprId<'dae>, (solve::Reg, usize));

struct PendingFunctionConditionalRegion<'dae> {
    ops: Vec<solve::LinearOp>,
    captures: Vec<FunctionConditionalCaptureSource<'dae>>,
}

type PendingFunctionConditionalArm<'dae> = (
    PendingFunctionConditionalRegion<'dae>,
    PendingFunctionConditionalRegion<'dae>,
);

#[derive(Clone, Copy)]
struct FunctionConditionalRegisterRange {
    start: solve::Reg,
    count: usize,
}

fn function_conditional_reg_offset(
    start: solve::Reg,
    offset: usize,
    span: Span,
    context: &'static str,
) -> Result<solve::Reg, LowerError> {
    let offset = solve::Reg::try_from(offset)
        .map_err(|_| LowerError::contract(format!("{context} offset exceeds u32"), span))?;
    start
        .checked_add(offset)
        .ok_or_else(|| LowerError::contract(format!("{context} register overflows"), span))
}

#[derive(Clone)]
struct PendingFoldTensorUpdate<'dae> {
    value: dae::ExprId<'dae>,
    subscripts: dae::SubscriptsView<'dae>,
    conditions: Vec<(dae::ExprId<'dae>, bool)>,
}

struct PendingFoldTensorGraph<'dae> {
    updates: Vec<PendingFoldTensorUpdate<'dae>>,
    nodes: Vec<PendingFoldTensorNode<'dae>>,
    result: u32,
}

enum PendingFoldTensorNode<'dae> {
    Update {
        base: u32,
        update: u32,
    },
    Select {
        condition: dae::ExprId<'dae>,
        if_true: u32,
        if_false: u32,
    },
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    fn function_conditional_group(
        &self,
        definition: dae::FunctionDefinitionId<'dae>,
    ) -> Option<(
        Vec<dae::FunctionDefinitionView<'dae>>,
        dae::FunctionConditionalView<'dae>,
        usize,
    )> {
        // SPEC_0021: Exception - exhaustive recursive dispatch over function statements.
        #[allow(clippy::excessive_nesting)]
        fn find<'dae>(
            statements: dae::FunctionStatements<'dae>,
            target: dae::FunctionDefinitionId<'dae>,
        ) -> Option<(
            Vec<dae::FunctionDefinitionView<'dae>>,
            dae::FunctionConditionalView<'dae>,
            usize,
        )> {
            for statement in statements {
                match statement {
                    dae::FunctionStatementView::AssignmentGroup {
                        definitions,
                        conditional: Some(conditional),
                    } => {
                        let definitions = definitions.iter().collect::<Vec<_>>();
                        if let Some(ordinal) = definitions
                            .iter()
                            .position(|candidate| candidate.id() == target)
                        {
                            return Some((definitions, conditional, ordinal));
                        }
                    }
                    dae::FunctionStatementView::For { statements, .. } => {
                        if let Some(group) = find(statements, target) {
                            return Some(group);
                        }
                    }
                    _ => {}
                }
            }
            None
        }

        let function = self.view.function(definition.function())?;
        find(function.statements(), definition)
    }

    /// Lower one SSA definition in the semantic context that owns it.
    ///
    /// A definition may be read from several later conditional arms. Those
    /// reads are projections of the same already-executed function statement;
    /// giving each arm a fresh scalar context duplicates the complete tensor
    /// expression graph. The definition's own conditional RHS still owns any
    /// branch-local evaluation.
    // SPEC_0021: Exception - exhaustive scalar dispatch over function-definition ownership forms.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn function_definition_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.owning_function_context(definition.id().function());
        let key = (context, definition.id(), scalar);
        if let Some(&register) = self.function_definition_scalar_cache.get(&key) {
            return Ok(register);
        }
        if self.function_conditional_captures_definition(definition.id(), context) {
            let count = self.function_conditional_value_width(definition.rhs())?;
            if scalar >= count {
                return Err(LowerError::contract(
                    "function-conditional scalar capture is out of range",
                    span,
                ));
            }
            let range = self.function_conditional_capture_range(
                FunctionConditionalCaptureSource::DefinitionRange {
                    context,
                    definition: definition.id(),
                    count,
                },
                span,
            )?;
            let register = function_conditional_reg_offset(
                range.start,
                scalar,
                span,
                "function-conditional scalar view",
            )?;
            self.function_definition_scalar_cache.insert(key, register);
            return Ok(register);
        }
        if let Some((definitions, conditional, ordinal)) =
            self.function_conditional_group(definition.id())
        {
            let suspended = self.switch_context(context);
            let result = self.pack_function_conditional_group(
                &definitions,
                conditional,
                definition.provenance().span(),
            );
            self.restore_context(suspended);
            let start = result?;
            let preceding = definitions[..ordinal].iter().try_fold(
                0usize,
                |count, value| -> Result<usize, LowerError> {
                    count
                        .checked_add(self.function_conditional_value_width(value.rhs())?)
                        .ok_or_else(|| {
                            LowerError::contract(
                                "function-conditional result offset overflows",
                                definition.provenance().span(),
                            )
                        })
                },
            )?;
            let offset = preceding.checked_add(scalar).ok_or_else(|| {
                LowerError::contract(
                    "function-conditional scalar projection overflows",
                    definition.provenance().span(),
                )
            })?;
            return function_conditional_reg_offset(
                start,
                offset,
                definition.provenance().span(),
                "function-conditional scalar projection",
            );
        }
        let suspended = self.switch_context(context);
        let result = self.expression(definition.rhs(), scalar);
        self.restore_context(suspended);
        let register = result?;
        self.function_definition_scalar_cache.insert(key, register);
        Ok(register)
    }

    // SPEC_0021: Exception - exhaustive aggregate dispatch over function-definition ownership forms.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn pack_function_definition(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.owning_function_context(definition.id().function());
        let key = (context, definition.id());
        if let Some(&register) = self.function_definition_aggregate_cache.get(&key) {
            return Ok(register);
        }
        if self.function_conditional_captures_definition(definition.id(), context) {
            let node = self.node(definition.rhs());
            let ranges = if node.value_type().is_record() {
                let mut ranges = Vec::with_capacity(node.value_type().record_field_count());
                for field in 0..node.value_type().record_field_count() {
                    let count = self.record_field_scalar_count(definition.rhs(), field);
                    ranges.push(self.function_conditional_capture_range(
                        FunctionConditionalCaptureSource::DefinitionRecordFieldRange {
                            context,
                            definition: definition.id(),
                            field,
                            count,
                        },
                        span,
                    )?);
                }
                ranges
            } else {
                let count = self.function_conditional_value_width(definition.rhs())?;
                vec![self.function_conditional_capture_range(
                    FunctionConditionalCaptureSource::DefinitionRange {
                        context,
                        definition: definition.id(),
                        count,
                    },
                    span,
                )?]
            };
            let register = self.pack_function_conditional_capture_ranges(&ranges, span)?;
            self.function_definition_aggregate_cache
                .insert(key, register);
            return Ok(register);
        }
        if let Some((definitions, conditional, ordinal)) =
            self.function_conditional_group(definition.id())
        {
            let suspended = self.switch_context(context);
            let result = self.pack_function_conditional_group(
                &definitions,
                conditional,
                definition.provenance().span(),
            );
            self.restore_context(suspended);
            let start = result?;
            let preceding = definitions[..ordinal].iter().try_fold(
                0usize,
                |count, value| -> Result<usize, LowerError> {
                    count
                        .checked_add(self.function_conditional_value_width(value.rhs())?)
                        .ok_or_else(|| {
                            LowerError::contract(
                                "function-conditional aggregate offset overflows",
                                definition.provenance().span(),
                            )
                        })
                },
            )?;
            return function_conditional_reg_offset(
                start,
                preceding,
                definition.provenance().span(),
                "function-conditional aggregate projection",
            );
        }
        let suspended = self.switch_context(context);
        let result = self.pack_expression(definition.rhs());
        self.restore_context(suspended);
        let register = result?;
        self.function_definition_aggregate_cache
            .insert(key, register);
        Ok(register)
    }

    // SPEC_0021: Exception - exhaustive construction of a checked conditional assignment group.
    #[allow(clippy::too_many_lines)]
    fn pack_function_conditional_group(
        &mut self,
        definitions: &[dae::FunctionDefinitionView<'dae>],
        conditional: dae::FunctionConditionalView<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let first = definitions
            .first()
            .ok_or_else(|| LowerError::contract("function conditional has no targets", span))?;
        let context = self.owning_function_context(first.id().function());
        if let Some(&start) = self
            .function_definition_aggregate_cache
            .get(&(context, first.id()))
        {
            return Ok(start);
        }
        if conditional.branch_count() != conditional.conditions().len() {
            return Err(LowerError::contract(
                "function conditional condition/branch count mismatch",
                span,
            ));
        }

        let target_widths = definitions
            .iter()
            .map(|definition| self.function_conditional_value_width(definition.rhs()))
            .collect::<Result<Vec<_>, _>>()?;
        let owner_key =
            self.function_conditional_owner_key(FunctionConditionalOwnerValue::Definitions(
                definitions
                    .iter()
                    .map(|definition| definition.id())
                    .collect(),
            ));
        let cached = owner_key.as_ref().and_then(|key| {
            self.function_conditional_owners
                .and_then(|owners| owners.borrow().owners.get(key).cloned())
        });
        if let Some(cached) = cached {
            if cached.program.target_widths.as_ref() != target_widths.as_slice() {
                return Err(LowerError::contract(
                    "function-conditional owner target layout changed across exact call frames",
                    span,
                ));
            }
            let capture_sources = cached
                .capture_sources
                .into_iter()
                .map(|source| source.with_context(context))
                .collect();
            return self.emit_function_conditional_program(
                definitions,
                target_widths,
                cached.program,
                capture_sources,
                context,
                span,
            );
        }
        let conditions = conditional.conditions().collect::<Vec<_>>();
        let mut pending_arms = Vec::with_capacity(conditional.branch_count());
        for (ordinal, &condition) in conditions.iter().enumerate() {
            let prior_inactive = conditions[..ordinal]
                .iter()
                .copied()
                .map(|condition| (condition, false))
                .collect::<Vec<_>>();
            let condition_program = self
                .fork_for_function_conditional_region(
                    first.id().function(),
                    context,
                    &prior_inactive,
                )
                .function_conditional_condition_program(condition)?;
            let branch = conditional.branch(ordinal).ok_or_else(|| {
                LowerError::contract("function conditional branch is missing", span)
            })?;
            let mut selected = prior_inactive;
            selected.push((condition, true));
            let result_program = self
                .fork_for_function_conditional_region(first.id().function(), context, &selected)
                .function_conditional_result_program(branch, span)?;
            pending_arms.push((condition_program, result_program));
        }
        let fallback_guards = conditions
            .iter()
            .copied()
            .map(|condition| (condition, false))
            .collect::<Vec<_>>();
        let pending_fallback = self
            .fork_for_function_conditional_region(first.id().function(), context, &fallback_guards)
            .function_conditional_result_program(conditional.fallback(), span)?;
        let owner = match (owner_key.as_ref(), self.function_conditional_owners) {
            (Some(_), Some(owners)) => Some(owners.borrow_mut().issue().ok_or_else(|| {
                LowerError::contract("function-conditional owner id overflow", span)
            })?),
            _ => None,
        };
        let (program, capture_sources) = Self::checked_function_conditional_program(
            owner,
            target_widths.clone(),
            pending_arms,
            pending_fallback,
            span,
        )?;
        if let (Some(key), Some(owners)) = (owner_key, self.function_conditional_owners) {
            let previous = owners.borrow_mut().owners.insert(
                key,
                CachedFunctionConditionalProgram {
                    program: program.clone(),
                    capture_sources: capture_sources.clone(),
                },
            );
            if previous.is_some() {
                return Err(LowerError::contract(
                    "function-conditional owner was issued twice",
                    span,
                ));
            }
        }
        self.emit_function_conditional_program(
            definitions,
            target_widths,
            program,
            capture_sources,
            context,
            span,
        )
    }

    pub(super) fn pack_lazy_conditional_value(
        &mut self,
        expression: dae::ExprId<'dae>,
        operands: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let cache_key = (self.context_id, expression);
        if let Some(&start) = self.packed_expression_cache.get(&cache_key) {
            return Ok(start);
        }
        if operands.len() < 3 || operands.len().is_multiple_of(2) {
            return Err(LowerError::contract(
                "function conditional expression has an invalid operand tuple",
                span,
            ));
        }
        let owner_function = self
            .node(expression)
            .function_scope()
            .ok_or_else(|| LowerError::contract("conditional value has no function owner", span))?;
        let owner_context = self.owning_function_context(owner_function);
        let result_count = self.function_conditional_value_width(expression)?;
        let owner_key = self
            .function_conditional_owner_key(FunctionConditionalOwnerValue::Expression(expression));
        let cached = owner_key.as_ref().and_then(|key| {
            self.function_conditional_owners
                .and_then(|owners| owners.borrow().owners.get(key).cloned())
        });
        if let Some(cached) = cached {
            if cached.program.target_widths.as_ref() != [result_count] {
                return Err(LowerError::contract(
                    "function-conditional expression owner result layout changed",
                    span,
                ));
            }
            let capture_sources = cached
                .capture_sources
                .into_iter()
                .map(|source| source.with_context(owner_context))
                .collect();
            let start = self.emit_lazy_conditional_program(
                cached.program,
                capture_sources,
                result_count,
                span,
            )?;
            self.cache_lazy_conditional_value(expression, start, cache_key, span)?;
            return Ok(start);
        }
        let (pending_arms, pending_fallback) =
            self.lazy_conditional_regions(operands, owner_function, owner_context, span)?;
        let owner = match (owner_key.as_ref(), self.function_conditional_owners) {
            (Some(_), Some(owners)) => Some(owners.borrow_mut().issue().ok_or_else(|| {
                LowerError::contract("function-conditional owner id overflow", span)
            })?),
            _ => None,
        };
        let (program, capture_sources) = Self::checked_function_conditional_program(
            owner,
            vec![result_count],
            pending_arms,
            pending_fallback,
            span,
        )?;
        if let (Some(key), Some(owners)) = (owner_key, self.function_conditional_owners) {
            let previous = owners.borrow_mut().owners.insert(
                key,
                CachedFunctionConditionalProgram {
                    program: program.clone(),
                    capture_sources: capture_sources.clone(),
                },
            );
            if previous.is_some() {
                return Err(LowerError::contract(
                    "function-conditional expression owner was issued twice",
                    span,
                ));
            }
        }
        let start =
            self.emit_lazy_conditional_program(program, capture_sources, result_count, span)?;
        self.cache_lazy_conditional_value(expression, start, cache_key, span)?;
        Ok(start)
    }

    fn lazy_conditional_regions(
        &self,
        operands: dae::ExpressionOperands<'dae>,
        owner_function: dae::FunctionId<'dae>,
        owner_context: u64,
        span: Span,
    ) -> Result<
        (
            Vec<PendingFunctionConditionalArm<'dae>>,
            PendingFunctionConditionalRegion<'dae>,
        ),
        LowerError,
    > {
        let fallback_index = operands.len() - 1;
        let conditions = (0..fallback_index)
            .step_by(2)
            .map(|index| {
                operands
                    .get(index)
                    .ok_or_else(|| LowerError::contract("conditional condition is missing", span))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut arms = Vec::with_capacity(fallback_index / 2);
        for (ordinal, &condition) in conditions.iter().enumerate() {
            let index = ordinal * 2;
            let value = operands
                .get(index + 1)
                .ok_or_else(|| LowerError::contract("conditional branch is missing", span))?;
            let prior_inactive = conditions[..ordinal]
                .iter()
                .copied()
                .map(|condition| (condition, false))
                .collect::<Vec<_>>();
            let condition = self
                .fork_for_function_conditional_region(
                    owner_function,
                    owner_context,
                    &prior_inactive,
                )
                .function_conditional_condition_program(condition)?;
            let mut selected = prior_inactive;
            selected.push((conditions[ordinal], true));
            let result = self
                .fork_for_function_conditional_region(owner_function, owner_context, &selected)
                .function_conditional_result_program([value], span)?;
            arms.push((condition, result));
        }
        let fallback = operands
            .get(fallback_index)
            .ok_or_else(|| LowerError::contract("conditional fallback is missing", span))?;
        let fallback = self
            .fork_for_function_conditional_region(
                owner_function,
                owner_context,
                &conditions
                    .iter()
                    .copied()
                    .map(|condition| (condition, false))
                    .collect::<Vec<_>>(),
            )
            .function_conditional_result_program([fallback], span)?;
        Ok((arms, fallback))
    }

    fn emit_lazy_conditional_program(
        &mut self,
        program: Arc<solve::FunctionConditionalProgram>,
        capture_sources: Vec<FunctionConditionalCaptureSource<'dae>>,
        result_count: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let capture_ranges = capture_sources
            .into_iter()
            .map(|source| self.resolve_function_conditional_capture(source, span))
            .collect::<Result<Vec<_>, _>>()?;
        let capture_start = self.pack_function_conditional_capture_ranges(&capture_ranges, span)?;
        let start = self.next_register;
        for _ in 0..result_count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::FunctionConditional {
            dst_start: start,
            capture_start,
            program,
        });
        Ok(start)
    }

    fn cache_lazy_conditional_value(
        &mut self,
        expression: dae::ExprId<'dae>,
        start: solve::Reg,
        cache_key: (u64, dae::ExprId<'dae>),
        span: Span,
    ) -> Result<(), LowerError> {
        self.packed_expression_cache.insert(cache_key, start);
        if self.node(expression).value_type().is_record() {
            for field in 0..self.node(expression).value_type().record_field_count() {
                let offset =
                    self.function_conditional_record_field_offset(expression, field, span)?;
                let field_start = function_conditional_reg_offset(
                    start,
                    offset,
                    span,
                    "record conditional field cache",
                )?;
                self.record_field_cache
                    .insert((self.context_id, expression, field), field_start);
            }
        }
        Ok(())
    }

    fn checked_function_conditional_program(
        owner: Option<solve::FunctionConditionalOwnerId>,
        target_widths: Vec<usize>,
        pending_arms: Vec<PendingFunctionConditionalArm<'dae>>,
        pending_fallback: PendingFunctionConditionalRegion<'dae>,
        span: Span,
    ) -> Result<
        (
            Arc<solve::FunctionConditionalProgram>,
            Vec<FunctionConditionalCaptureSource<'dae>>,
        ),
        LowerError,
    > {
        let mut capture_sources = Vec::new();
        let arms = pending_arms
            .into_iter()
            .map(|(condition, result)| {
                Ok((
                    Self::merge_function_conditional_region(condition, &mut capture_sources, span)?,
                    Self::merge_function_conditional_region(result, &mut capture_sources, span)?,
                ))
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        let fallback =
            Self::merge_function_conditional_region(pending_fallback, &mut capture_sources, span)?;
        let capture_count = capture_sources.iter().try_fold(0usize, |count, source| {
            count.checked_add(source.width()).ok_or_else(|| {
                LowerError::contract("function-conditional capture ABI overflows", span)
            })
        })?;
        let checked = match owner {
            Some(owner) => solve::FunctionConditionalProgram::checked_owned(
                owner,
                capture_count,
                target_widths,
                arms,
                fallback,
            ),
            None => solve::FunctionConditionalProgram::checked(
                capture_count,
                target_widths,
                arms,
                fallback,
            ),
        }
        .map_err(|error| {
            LowerError::contract(
                format!("function-conditional register proof failed: {error}"),
                span,
            )
        })?;
        Ok((Arc::new(checked), capture_sources))
    }

    fn function_conditional_owner_key(
        &self,
        value: FunctionConditionalOwnerValue<'dae>,
    ) -> Option<FunctionConditionalOwnerKey<'dae>> {
        // Symbolic fold registers are local views of a compact domain, not a
        // stable call-frame identity across independently lowered rows. Their
        // owning fold already retains one program, so do not issue a cross-row
        // owner from register ordinals.
        if !self.symbolic_domain_points.is_empty() || !self.function_fold_values.is_empty() {
            return None;
        }
        Some(FunctionConditionalOwnerKey {
            value,
            function_arguments: self.function_arguments.clone(),
            activation_path: self
                .activation_path
                .iter()
                .map(|guard| (guard.condition, guard.expected))
                .collect(),
            domain_points: self.domain_points.clone(),
            active_clock: self.active_clock,
            sampled_source: self.sampled_source,
            active_parameters: self.active_parameters.clone(),
        })
    }

    fn emit_function_conditional_program(
        &mut self,
        definitions: &[dae::FunctionDefinitionView<'dae>],
        target_widths: Vec<usize>,
        program: Arc<solve::FunctionConditionalProgram>,
        capture_sources: Vec<FunctionConditionalCaptureSource<'dae>>,
        context: u64,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let capture_count = capture_sources.iter().try_fold(0usize, |count, source| {
            count.checked_add(source.width()).ok_or_else(|| {
                LowerError::contract("function-conditional owner capture ABI overflows", span)
            })
        })?;
        if program.capture_count != capture_count {
            return Err(LowerError::contract(
                "function-conditional owner capture layout changed across exact call frames",
                span,
            ));
        }
        let capture_ranges = capture_sources
            .iter()
            .copied()
            .map(|source| self.resolve_function_conditional_capture(source, span))
            .collect::<Result<Vec<_>, _>>()?;
        let capture_start = self.pack_function_conditional_capture_ranges(&capture_ranges, span)?;
        let start = self.next_register;
        for _ in 0..program.result_count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::FunctionConditional {
            dst_start: start,
            capture_start,
            program,
        });
        let mut offset = 0usize;
        for (definition, width) in definitions.iter().zip(target_widths) {
            let target = function_conditional_reg_offset(
                start,
                offset,
                span,
                "function-conditional target projection",
            )?;
            self.function_definition_aggregate_cache
                .insert((context, definition.id()), target);
            for scalar in 0..width {
                self.function_definition_scalar_cache.insert(
                    (context, definition.id(), scalar),
                    function_conditional_reg_offset(
                        target,
                        scalar,
                        span,
                        "function-conditional scalar target",
                    )?,
                );
            }
            offset = offset.checked_add(width).ok_or_else(|| {
                LowerError::contract("function-conditional target offset overflows", span)
            })?;
        }
        Ok(start)
    }

    fn function_conditional_value_width(
        &self,
        expression: dae::ExprId<'dae>,
    ) -> Result<usize, LowerError> {
        let node = self.node(expression);
        if let Some(count) = node.value_type().scalar_count() {
            return Ok(count);
        }
        let mut count = 0usize;
        for field in 0..node.value_type().record_field_count() {
            count = count
                .checked_add(self.record_field_scalar_count(expression, field))
                .ok_or_else(|| {
                    LowerError::contract(
                        "function-conditional record width overflows",
                        node.provenance().span(),
                    )
                })?;
        }
        if count == 0 {
            return Err(LowerError::contract(
                "function-conditional record target has no packed fields",
                node.provenance().span(),
            ));
        }
        Ok(count)
    }

    fn function_conditional_record_field_offset(
        &self,
        expression: dae::ExprId<'dae>,
        field: usize,
        span: Span,
    ) -> Result<usize, LowerError> {
        if field >= self.node(expression).value_type().record_field_count() {
            return Err(LowerError::contract(
                "function-conditional record field is out of range",
                span,
            ));
        }
        (0..field).try_fold(0usize, |offset, ordinal| {
            offset
                .checked_add(self.record_field_scalar_count(expression, ordinal))
                .ok_or_else(|| {
                    LowerError::contract("function-conditional record field offset overflows", span)
                })
        })
    }

    fn function_conditional_condition_program(
        mut self,
        expression: dae::ExprId<'dae>,
    ) -> Result<PendingFunctionConditionalRegion<'dae>, LowerError> {
        let span = self.node(expression).provenance().span();
        let output = self.expression(expression, 0)?;
        self.ops.push(solve::LinearOp::StoreOutput { src: output });
        self.finish_function_conditional_region(span)
    }

    // SPEC_0021: Exception - exhaustive scalar/record result dispatch for conditional regions.
    #[allow(clippy::excessive_nesting)]
    fn function_conditional_result_program(
        mut self,
        expressions: impl IntoIterator<Item = dae::ExprId<'dae>>,
        span: Span,
    ) -> Result<PendingFunctionConditionalRegion<'dae>, LowerError> {
        for expression in expressions {
            let node = self.node(expression);
            if node.value_type().is_record() {
                for field in 0..node.value_type().record_field_count() {
                    let start =
                        self.pack_record_field(expression, field, node.provenance().span())?;
                    let count = self.record_field_scalar_count(expression, field);
                    self.ops.push(solve::LinearOp::StoreOutputRange {
                        start,
                        count,
                        stride: 1,
                    });
                }
            } else {
                let start = self.pack_expression(expression)?;
                let count = self.function_conditional_value_width(expression)?;
                self.ops.push(solve::LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride: 1,
                });
            }
        }
        self.finish_function_conditional_region(span)
    }

    fn finish_function_conditional_region(
        mut self,
        span: Span,
    ) -> Result<PendingFunctionConditionalRegion<'dae>, LowerError> {
        let captures = self
            .deferred_function_conditional_captures
            .take()
            .ok_or_else(|| {
                LowerError::contract(
                    "function-conditional region escaped its capture catalog",
                    span,
                )
            })?;
        Ok(PendingFunctionConditionalRegion {
            ops: self.ops,
            captures: captures.sources,
        })
    }

    fn fork_for_function_conditional_region(
        &self,
        owner_function: dae::FunctionId<'dae>,
        owner_context: u64,
        branch_guards: &[(dae::ExprId<'dae>, bool)],
    ) -> Self {
        let mut compiler = Self::new(self.view, self.layout, None);
        compiler.domain_points = self.domain_points.clone();
        compiler.symbolic_domain_points = self.symbolic_domain_points.clone();
        compiler.function_arguments = self.function_arguments.clone();
        for frame in &mut compiler.function_arguments {
            // The conditional region is its own native control-flow owner, so
            // parent activation registers do not enter its local register
            // file. Parameter suspension must therefore use the region-local
            // empty activation prefix.
            frame.activation_base = 0;
        }
        compiler.activation_path = self
            .activation_path
            .iter()
            .map(|guard| ActivationGuard {
                register: None,
                ..*guard
            })
            .chain(
                branch_guards
                    .iter()
                    .copied()
                    .map(|(condition, expected)| ActivationGuard {
                        condition: ActivationCondition::Expression(condition),
                        register: None,
                        expected,
                    }),
            )
            .collect();
        // The enclosing checked conditional owns these guards at runtime. They
        // remain on the semantic path so separately scheduled call assertions
        // reproduce the same short-circuit path, but folds inside the selected
        // region must not redundantly re-evaluate it.
        compiler.fold_guard_base = compiler.activation_path.len();
        compiler.active_clock = self.active_clock;
        compiler.sampled_source = self.sampled_source;
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.parameter_substitutions = self.parameter_substitutions;
        compiler.active_parameters = self.active_parameters.clone();
        compiler.active_call_assertions = self.active_call_assertions.clone();
        compiler.call_action_compilation = self.call_action_compilation;
        compiler.suppress_function_assertions = self.suppress_function_assertions;
        compiler.context_ids = self.context_ids.clone();
        compiler.context_frames = self.context_frames.clone();
        compiler.context_stack = self.context_stack.clone();
        compiler.context_id = self.context_id;
        compiler.next_context_id = self.next_context_id;
        compiler.deferred_function_conditional_captures =
            Some(DeferredFunctionConditionalCaptures {
                owner_function,
                owner_context,
                sources: Vec::new(),
                locals: Vec::new(),
            });
        compiler.function_conditional_owners = self.function_conditional_owners;
        compiler
    }

    fn function_conditional_captures_definition(
        &self,
        definition: dae::FunctionDefinitionId<'dae>,
        context: u64,
    ) -> bool {
        self.deferred_function_conditional_captures
            .as_ref()
            .is_some_and(|captures| {
                captures.owner_function == definition.function()
                    && captures.owner_context == context
            })
    }

    fn function_conditional_capture_range(
        &mut self,
        source: FunctionConditionalCaptureSource<'dae>,
        span: Span,
    ) -> Result<FunctionConditionalRegisterRange, LowerError> {
        let count = source.width();
        if count == 0 {
            return Err(LowerError::contract(
                "function-conditional capture range is empty",
                span,
            ));
        }
        if let Some(&(_, start)) = self
            .deferred_function_conditional_captures
            .as_ref()
            .and_then(|captures| {
                captures
                    .locals
                    .iter()
                    .find(|(candidate, _)| *candidate == source)
            })
        {
            return Ok(FunctionConditionalRegisterRange { start, count });
        }
        let index_start = self.function_conditional_capture_slot(source, span)?;
        let dst_start = self.next_register;
        for _ in 0..count {
            self.register(span)?;
        }
        self.ops
            .push(solve::LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            });
        self.deferred_function_conditional_captures
            .as_mut()
            .expect("checked conditional capture remains active")
            .locals
            .push((source, dst_start));
        Ok(FunctionConditionalRegisterRange {
            start: dst_start,
            count,
        })
    }

    fn function_conditional_capture_slot(
        &mut self,
        source: FunctionConditionalCaptureSource<'dae>,
        span: Span,
    ) -> Result<usize, LowerError> {
        let captures = self
            .deferred_function_conditional_captures
            .as_mut()
            .ok_or_else(|| {
                LowerError::contract(
                    "function-conditional capture escaped its checked region",
                    span,
                )
            })?;
        let mut base = 0usize;
        for candidate in &captures.sources {
            if *candidate == source {
                return Ok(base);
            }
            base = base.checked_add(candidate.width()).ok_or_else(|| {
                LowerError::contract("function-conditional capture ABI overflows", span)
            })?;
        }
        captures.sources.push(source);
        Ok(base)
    }

    // SPEC_0021: Exception - exhaustive rewrite of conditional capture operation variants.
    #[allow(clippy::excessive_nesting)]
    fn merge_function_conditional_region(
        mut region: PendingFunctionConditionalRegion<'dae>,
        captures: &mut Vec<FunctionConditionalCaptureSource<'dae>>,
        span: Span,
    ) -> Result<Vec<solve::LinearOp>, LowerError> {
        let mut segments = Vec::with_capacity(region.captures.len());
        let mut local_base = 0usize;
        for source in region.captures.iter().copied() {
            let mut owner_base = 0usize;
            let mut found = false;
            for candidate in captures.iter().copied() {
                if candidate == source {
                    found = true;
                    break;
                }
                owner_base = owner_base.checked_add(candidate.width()).ok_or_else(|| {
                    LowerError::contract("function-conditional owner capture ABI overflows", span)
                })?;
            }
            if !found {
                captures.push(source);
            }
            segments.push((local_base, source.width(), owner_base));
            local_base = local_base.checked_add(source.width()).ok_or_else(|| {
                LowerError::contract("function-conditional local capture ABI overflows", span)
            })?;
        }
        for operation in &mut region.ops {
            let (index, count) = match operation {
                solve::LinearOp::LoadFunctionConditionalCapture { index, .. } => (index, 1),
                solve::LinearOp::LoadFunctionConditionalCaptureRange {
                    index_start, count, ..
                } => (index_start, *count),
                _ => continue,
            };
            let local_end = index.checked_add(count).ok_or_else(|| {
                LowerError::contract("function-conditional local capture range overflows", span)
            })?;
            let (segment_local, segment_width, segment_owner) = segments
                .iter()
                .copied()
                .find(|(base, width, _)| {
                    *base <= *index
                        && base
                            .checked_add(*width)
                            .is_some_and(|segment_end| local_end <= segment_end)
                })
                .ok_or_else(|| {
                    LowerError::contract(
                        "function-conditional local capture range is out of bounds",
                        span,
                    )
                })?;
            debug_assert!(count <= segment_width);
            *index = segment_owner
                .checked_add(*index - segment_local)
                .ok_or_else(|| {
                    LowerError::contract("function-conditional owner capture range overflows", span)
                })?;
        }
        Ok(region.ops)
    }

    fn resolve_function_conditional_capture(
        &mut self,
        source: FunctionConditionalCaptureSource<'dae>,
        span: Span,
    ) -> Result<FunctionConditionalRegisterRange, LowerError> {
        match source {
            FunctionConditionalCaptureSource::DefinitionRange {
                context,
                definition,
                count,
            } => {
                let definition = self.view.function_definition(definition).ok_or_else(|| {
                    LowerError::contract(
                        "function-conditional aggregate capture definition is missing",
                        span,
                    )
                })?;
                if self.owning_function_context(definition.id().function()) != context {
                    return Err(LowerError::contract(
                        "function-conditional aggregate capture call frame changed",
                        span,
                    ));
                }
                let expected = self.function_conditional_value_width(definition.rhs())?;
                if count != expected {
                    return Err(LowerError::contract(
                        "function-conditional aggregate capture shape changed",
                        span,
                    ));
                }
                Ok(FunctionConditionalRegisterRange {
                    start: self.pack_function_definition(definition, span)?,
                    count,
                })
            }
            FunctionConditionalCaptureSource::DefinitionRecordFieldRange {
                context,
                definition,
                field,
                count,
            } => {
                let definition = self.view.function_definition(definition).ok_or_else(|| {
                    LowerError::contract(
                        "function-conditional record capture definition is missing",
                        span,
                    )
                })?;
                if self.owning_function_context(definition.id().function()) != context {
                    return Err(LowerError::contract(
                        "function-conditional record capture call frame changed",
                        span,
                    ));
                }
                let expected = self.record_field_scalar_count(definition.rhs(), field);
                if count != expected {
                    return Err(LowerError::contract(
                        "function-conditional record capture shape changed",
                        span,
                    ));
                }
                Ok(FunctionConditionalRegisterRange {
                    start: self.pack_function_definition_record_field(definition, field, span)?,
                    count,
                })
            }
        }
    }

    fn pack_function_conditional_capture_ranges(
        &mut self,
        ranges: &[FunctionConditionalRegisterRange],
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let total = ranges.iter().try_fold(0usize, |count, range| {
            count.checked_add(range.count).ok_or_else(|| {
                LowerError::contract("function-conditional capture range total overflows", span)
            })
        })?;
        if total == 0 {
            return Ok(self.next_register);
        }
        if let [range] = ranges {
            return Ok(range.start);
        }
        let dimensions = vec![u32::try_from(total).map_err(|_| {
            LowerError::contract("function-conditional capture range exceeds u32", span)
        })?]
        .into_boxed_slice();
        let sources = ranges
            .iter()
            .map(|range| {
                Ok(solve::TensorConcatenateSource {
                    start: range.start,
                    dimensions: vec![u32::try_from(range.count).map_err(|_| {
                        LowerError::contract(
                            "function-conditional capture segment exceeds u32",
                            span,
                        )
                    })?]
                    .into_boxed_slice(),
                })
            })
            .collect::<Result<Vec<_>, LowerError>>()?
            .into_boxed_slice();
        let dst_start = self.next_register;
        for _ in 0..total {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::TensorConcatenate {
            dst_start,
            sources,
            dimensions,
            axis: 0,
            lanes: 1,
        });
        Ok(dst_start)
    }

    fn switch_context(&mut self, context: u64) -> (u64, Vec<u64>) {
        let previous = (self.context_id, std::mem::take(&mut self.context_stack));
        let mut ancestors = Vec::new();
        let mut current = context;
        while current != 0 {
            let frame = self
                .context_frames
                .get(&current)
                .expect("non-root scalar context has a frame");
            let parent = match frame {
                ScalarContextFrame::Activation { parent, .. }
                | ScalarContextFrame::Function { parent, .. }
                | ScalarContextFrame::Domain { parent, .. }
                | ScalarContextFrame::Parameter { parent, .. }
                | ScalarContextFrame::Derivative { parent, .. } => *parent,
            };
            ancestors.push(parent);
            current = parent;
        }
        ancestors.reverse();
        self.context_stack = ancestors;
        self.context_id = context;
        previous
    }

    fn restore_context(&mut self, previous: (u64, Vec<u64>)) {
        self.context_id = previous.0;
        self.context_stack = previous.1;
    }

    // SPEC_0021: Exception - exhaustive record-field dispatch over definition ownership forms.
    #[allow(clippy::excessive_nesting)]
    fn pack_function_definition_record_field(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        field: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.owning_function_context(definition.id().function());
        if self.function_conditional_captures_definition(definition.id(), context) {
            let count = self.record_field_scalar_count(definition.rhs(), field);
            return Ok(self
                .function_conditional_capture_range(
                    FunctionConditionalCaptureSource::DefinitionRecordFieldRange {
                        context,
                        definition: definition.id(),
                        field,
                        count,
                    },
                    span,
                )?
                .start);
        }
        if let Some((definitions, conditional, ordinal)) =
            self.function_conditional_group(definition.id())
        {
            let suspended = self.switch_context(context);
            let result = self.pack_function_conditional_group(&definitions, conditional, span);
            self.restore_context(suspended);
            let start = result?;
            let target_offset = definitions[..ordinal].iter().try_fold(
                0usize,
                |offset, value| -> Result<usize, LowerError> {
                    offset
                        .checked_add(self.function_conditional_value_width(value.rhs())?)
                        .ok_or_else(|| {
                            LowerError::contract(
                                "function-conditional record target offset overflows",
                                span,
                            )
                        })
                },
            )?;
            let field_offset =
                self.function_conditional_record_field_offset(definition.rhs(), field, span)?;
            return function_conditional_reg_offset(
                start,
                target_offset.checked_add(field_offset).ok_or_else(|| {
                    LowerError::contract(
                        "function-conditional record projection offset overflows",
                        span,
                    )
                })?,
                span,
                "function-conditional record projection",
            );
        }
        let suspended = self.switch_context(context);
        let result = self.pack_record_field(definition.rhs(), field, span);
        self.restore_context(suspended);
        result
    }

    // SPEC_0021: Exception - exhaustive scalar record-field dispatch over definition ownership forms.
    #[allow(clippy::excessive_nesting)]
    fn function_definition_record_field_value(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.owning_function_context(definition.id().function());
        if self.function_conditional_captures_definition(definition.id(), context) {
            let count = self.record_field_scalar_count(definition.rhs(), field);
            if scalar >= count {
                return Err(LowerError::contract(
                    "function-conditional record scalar capture is out of range",
                    span,
                ));
            }
            let range = self.function_conditional_capture_range(
                FunctionConditionalCaptureSource::DefinitionRecordFieldRange {
                    context,
                    definition: definition.id(),
                    field,
                    count,
                },
                span,
            )?;
            return function_conditional_reg_offset(
                range.start,
                scalar,
                span,
                "function-conditional record scalar view",
            );
        }
        if let Some((definitions, conditional, ordinal)) =
            self.function_conditional_group(definition.id())
        {
            let suspended = self.switch_context(context);
            let result = self.pack_function_conditional_group(&definitions, conditional, span);
            self.restore_context(suspended);
            let start = result?;
            let target_offset = definitions[..ordinal].iter().try_fold(
                0usize,
                |offset, value| -> Result<usize, LowerError> {
                    offset
                        .checked_add(self.function_conditional_value_width(value.rhs())?)
                        .ok_or_else(|| {
                            LowerError::contract(
                                "function-conditional record target offset overflows",
                                span,
                            )
                        })
                },
            )?;
            let field_offset =
                self.function_conditional_record_field_offset(definition.rhs(), field, span)?;
            let offset = target_offset
                .checked_add(field_offset)
                .and_then(|offset| offset.checked_add(scalar))
                .ok_or_else(|| {
                    LowerError::contract(
                        "function-conditional record scalar offset overflows",
                        span,
                    )
                })?;
            return function_conditional_reg_offset(
                start,
                offset,
                span,
                "function-conditional record scalar projection",
            );
        }
        let suspended = self.switch_context(context);
        let result = self.record_field(definition.rhs(), field, scalar, span);
        self.restore_context(suspended);
        result
    }

    pub(super) fn pack_record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let key = (self.context_id, expression, field);
        if let Some(&start) = self.record_field_cache.get(&key) {
            return Ok(start);
        }
        let start = match self.node(expression).operation() {
            dae::ExpressionOperation::Record(fields) => self.pack_expression(
                fields
                    .get(field)
                    .ok_or_else(|| LowerError::contract("record field is out of range", span))?,
            )?,
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => {
                self.pack_record_call_field(expression, function, output, arguments, field, span)?
            }
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.pack_function_definition_record_field(definition, field, span)?
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.pack_function_parameter_record_field(parameter, field, span)?,
            dae::ExpressionOperation::Conditional(operands)
                if self.node(expression).function_scope().is_some() =>
            {
                let start = self.pack_lazy_conditional_value(expression, operands, span)?;
                function_conditional_reg_offset(
                    start,
                    self.function_conditional_record_field_offset(expression, field, span)?,
                    span,
                    "record conditional field view",
                )?
            }
            _ => {
                let count = self.record_field_scalar_count(expression, field);
                let mut values = Vec::with_capacity(count);
                for scalar in 0..count {
                    values.push(self.record_field(expression, field, scalar, span)?);
                }
                self.pack_registers(&values, span)?
            }
        };
        self.record_field_cache.insert(key, start);
        Ok(start)
    }

    fn pack_record_call_field(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        field: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some((start, registered)) =
            self.emit_typed_pure_call(call, function, arguments, span)?
        {
            let range = typed_call_record_field_scalar_range(
                self.view,
                function,
                &registered,
                output as usize,
                field,
                span,
            )?;
            return function_conditional_reg_offset(
                start,
                range.start,
                span,
                "typed pure-call record field",
            );
        }
        let typed_assertions = self.register_root_pure_call(call, function, span)?;
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "aggregate record-function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        let arguments: Vec<_> = arguments.iter().collect();
        let assertion = ActiveCallAssertion {
            call,
            function,
            arguments: arguments.clone(),
        };
        if self.call_action_compilation
            && self.active_call_assertions.contains(&assertion)
            && self.current_function_frame_matches(&assertion)
        {
            return self.pack_record_field(result, field, span);
        }
        let previous_suppression = self.suppress_function_assertions;
        self.suppress_function_assertions |= typed_assertions;
        self.enter_context(ScalarContextFrame::Function {
            parent: self.context_id,
            call,
            function,
            arguments: arguments.clone(),
        });
        self.function_arguments.push(FunctionArgumentsFrame {
            call,
            function,
            arguments,
            activation_base: self.activation_path.len(),
        });
        let owns_assertion = self.active_call_assertions.insert(assertion.clone());
        let lowered = if owns_assertion && !self.suppress_function_assertions {
            self.schedule_function_assertions(
                self.view
                    .function(function)
                    .expect("checked function identity resolves")
                    .statements(),
                span,
            )
        } else {
            Ok(())
        };
        let lowered = lowered.and_then(|()| self.pack_record_field(result, field, span));
        if owns_assertion {
            self.active_call_assertions.remove(&assertion);
        }
        self.function_arguments.pop();
        self.leave_context();
        self.suppress_function_assertions = previous_suppression;
        lowered
    }

    fn pack_function_parameter_record_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.suspend_context();
        let Some(owner_index) = self
            .function_arguments
            .iter()
            .rposition(|frame| frame.function == parameter.function())
        else {
            self.resume_context(context);
            return Err(LowerError::contract(
                "aggregate record parameter escaped its checked call",
                span,
            ));
        };
        let mut suspended_frames = self.function_arguments.split_off(owner_index + 1);
        let frame = self
            .function_arguments
            .pop()
            .expect("owning frame index was just resolved");
        let suspended_activations = self.activation_path.split_off(frame.activation_base);
        let argument = frame.arguments.get(parameter.ordinal() as usize).copied();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::contract("aggregate record parameter has no checked argument", span)
            })
            .and_then(|argument| self.pack_record_field(argument, field, span));
        self.activation_path.extend(suspended_activations);
        self.function_arguments.push(frame);
        self.function_arguments.append(&mut suspended_frames);
        self.resume_context(context);
        lowered
    }

    // SPEC_0021: Exception - exhaustive active/deferred fold-parameter ownership dispatch.
    #[allow(clippy::excessive_nesting)]
    pub(super) fn function_fold_parameter(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(value) = self
            .function_fold_values
            .iter()
            .rev()
            .find_map(|(active, values)| {
                (*active == fold)
                    .then(|| values.get(carried as usize)?.get(scalar).copied())
                    .flatten()
            })
        {
            return Ok(value);
        }
        let source = self
            .deferred_fold_captures
            .as_ref()
            .and_then(|deferred| {
                deferred
                    .fold_values
                    .iter()
                    .rev()
                    .find_map(|(active, values)| {
                        if *active != fold {
                            return None;
                        }
                        values.get(carried as usize)?.get(scalar).copied()
                    })
            })
            .ok_or_else(|| {
                LowerError::contract("function loop parameter escaped its checked fold", span)
            })?;
        self.deferred_fold_capture(source, span)
    }

    pub(super) fn deferred_fold_capture(
        &mut self,
        source: solve::Reg,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(register) = self
            .deferred_fold_captures
            .as_ref()
            .and_then(|deferred| deferred.locals.get(&source).copied())
        {
            return Ok(register);
        }
        let capture_index = self
            .deferred_fold_captures
            .as_ref()
            .map(|deferred| deferred.sources.len())
            .ok_or_else(|| {
                LowerError::contract("fold capture escaped its checked nested fold", span)
            })?;
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::LoadFoldCapture {
            dst,
            index: capture_index,
        });
        let deferred = self
            .deferred_fold_captures
            .as_mut()
            .expect("checked deferred fold capture remains active");
        deferred.sources.push(source);
        deferred.locals.insert(source, dst);
        Ok(dst)
    }

    pub(super) fn record_field(
        &mut self,
        expression: dae::ExprId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let node = self.node(expression);
        match node.operation() {
            dae::ExpressionOperation::Record(fields) => self.expression(
                fields
                    .get(field)
                    .ok_or_else(|| LowerError::contract("record field is out of range", span))?,
                scalar,
            ),
            dae::ExpressionOperation::Call {
                function,
                output,
                arguments,
                ..
            } => self.record_call_field(
                expression,
                function,
                output,
                arguments,
                (field, scalar),
                span,
            ),
            dae::ExpressionOperation::FunctionValue { definition, .. } => {
                self.function_definition_record_field_value(definition, field, scalar, span)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.function_parameter_record_field(parameter, field, scalar, span),
            dae::ExpressionOperation::Conditional(operands) => {
                self.record_conditional_field(operands, field, scalar, span)
            }
            dae::ExpressionOperation::Array(elements) => {
                let first = elements.get(0).expect("checked record array is nonempty");
                let element_count = self.record_field_scalar_count(first, field);
                let element = elements
                    .get(scalar / element_count)
                    .expect("checked record field scalar selects an array element");
                self.record_field(element, field, scalar % element_count, span)
            }
            dae::ExpressionOperation::Comprehension { domain, body } => {
                self.record_comprehension_field(domain, body, field, scalar, span)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                self.indexed_record_field(expression, base, subscripts, field, scalar, span)
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => self.record_array_update_field(
                expression, base, value, subscripts, field, scalar, span,
            ),
            _ => Err(LowerError::contract(
                "record field has no checked aggregate definition",
                span,
            )),
        }
    }

    fn record_call_field(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        projection: (usize, usize),
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let (field, scalar) = projection;
        if let Some((start, registered)) =
            self.emit_typed_pure_call(call, function, arguments, span)?
        {
            let range = typed_call_record_field_scalar_range(
                self.view,
                function,
                &registered,
                output as usize,
                field,
                span,
            )?;
            if scalar >= range.len() {
                return Err(LowerError::contract(
                    "typed pure-call record scalar projection is out of range",
                    span,
                ));
            }
            return function_conditional_reg_offset(
                start,
                range.start + scalar,
                span,
                "typed pure-call record scalar",
            );
        }
        let typed_assertions = self.register_root_pure_call(call, function, span)?;
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        let arguments: Vec<_> = arguments.iter().collect();
        let assertion = ActiveCallAssertion {
            call,
            function,
            arguments: arguments.clone(),
        };
        if self.call_action_compilation
            && self.active_call_assertions.contains(&assertion)
            && self.current_function_frame_matches(&assertion)
        {
            return self.record_field(result, field, scalar, span);
        }
        let previous_suppression = self.suppress_function_assertions;
        self.suppress_function_assertions |= typed_assertions;
        self.enter_context(ScalarContextFrame::Function {
            parent: self.context_id,
            call,
            function,
            arguments: arguments.clone(),
        });
        self.function_arguments.push(FunctionArgumentsFrame {
            call,
            function,
            arguments,
            activation_base: self.activation_path.len(),
        });
        let owns_assertion = self.active_call_assertions.insert(assertion.clone());
        let lowered = if owns_assertion && !self.suppress_function_assertions {
            self.schedule_function_assertions(
                self.view
                    .function(function)
                    .expect("checked function identity resolves")
                    .statements(),
                span,
            )
        } else {
            Ok(())
        };
        let lowered = lowered.and_then(|()| self.record_field(result, field, scalar, span));
        if owns_assertion {
            self.active_call_assertions.remove(&assertion);
        }
        self.function_arguments.pop();
        self.leave_context();
        self.suppress_function_assertions = previous_suppression;
        lowered
    }

    fn record_conditional_field(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let (conditions, fallback) = self.record_conditions(operands)?;
        self.push_inactive_conditions(&conditions);
        let mut selected = self.record_field(fallback, field, scalar, span)?;
        self.pop_activations(conditions.len());
        for (branch, condition) in conditions.iter().copied().enumerate().rev() {
            let value = self.record_conditional_branch(
                &conditions[..branch],
                condition,
                field,
                scalar,
                span,
            )?;
            selected = self.select(condition.1, value, selected, span)?;
        }
        Ok(selected)
    }

    fn record_conditions(
        &mut self,
        operands: dae::ExpressionOperands<'dae>,
    ) -> Result<(Vec<RecordCondition<'dae>>, dae::ExprId<'dae>), LowerError> {
        let fallback_index = operands.len() - 1;
        let mut conditions = Vec::with_capacity(fallback_index / 2);
        let mut fallback = operands
            .get(fallback_index)
            .expect("checked conditional has a fallback");
        for ordinal in (0..fallback_index).step_by(2) {
            let condition = operands
                .get(ordinal)
                .expect("checked conditional condition ordinal");
            let register = self.expression(condition, 0)?;
            if self.integer_register(register) == Some(0) {
                continue;
            }
            let value = operands
                .get(ordinal + 1)
                .expect("checked conditional value ordinal");
            if self.integer_register(register).is_some() {
                fallback = value;
                break;
            }
            conditions.push((condition, register, value));
        }
        Ok((conditions, fallback))
    }

    fn record_conditional_branch(
        &mut self,
        previous: &[RecordCondition<'dae>],
        condition: RecordCondition<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        self.push_inactive_conditions(previous);
        self.push_activation(condition.0, condition.1, true);
        let value = self.record_field(condition.2, field, scalar, span);
        self.pop_activation();
        self.pop_activations(previous.len());
        value
    }

    fn push_inactive_conditions(&mut self, conditions: &[RecordCondition<'dae>]) {
        for (condition, register, _) in conditions {
            self.push_activation(*condition, *register, false);
        }
    }

    fn pop_activations(&mut self, count: usize) {
        for _ in 0..count {
            self.pop_activation();
        }
    }

    fn function_parameter_record_field(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.suspend_context();
        // Same innermost-owner resolution as `function_parameter`: record
        // parameters can be referenced from expressions expanded under
        // deeper call frames.
        let Some(owner_index) = self
            .function_arguments
            .iter()
            .rposition(|frame| frame.function == parameter.function())
        else {
            self.resume_context(context);
            return Err(LowerError::contract(
                "record function parameter escaped its checked call",
                span,
            ));
        };
        let mut suspended_frames = self.function_arguments.split_off(owner_index + 1);
        let frame = self
            .function_arguments
            .pop()
            .expect("owning frame index was just resolved");
        let suspended_activations = self.activation_path.split_off(frame.activation_base);
        let argument = frame.arguments.get(parameter.ordinal() as usize).copied();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::contract("record function parameter has no checked argument", span)
            })
            .and_then(|argument| self.record_field(argument, field, scalar, span));
        self.activation_path.extend(suspended_activations);
        self.function_arguments.push(frame);
        self.function_arguments.append(&mut suspended_frames);
        self.resume_context(context);
        lowered
    }

    fn record_comprehension_field(
        &mut self,
        domain: dae::DomainId<'dae>,
        body: dae::ExprId<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let body_count = self.record_field_scalar_count(body, field);
        let point = scalar / body_count;
        let values = self
            .view
            .domain(domain)
            .expect("checked comprehension domain resolves")
            .structured()
            .index_tuple_at(point)
            .expect("checked comprehension domain remains valid")
            .expect("checked record field scalar selects a domain point");
        self.enter_context(ScalarContextFrame::Domain {
            parent: self.context_id,
            domain,
            values: values.clone(),
        });
        self.domain_points.push((domain, values));
        let result = self.record_field(body, field, scalar % body_count, span);
        self.domain_points.pop();
        self.leave_context();
        result
    }

    fn indexed_record_field(
        &mut self,
        indexed: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let field_width = self.record_layout(indexed, field).field_width();
        let record_scalar = scalar / field_width;
        let field_scalar = scalar % field_width;
        let selected = ScalarSelector::from_points(self.view, &self.domain_points)
            .indexed_base_scalar(
                base,
                subscripts,
                self.node(indexed).value_type().dimensions(),
                record_scalar,
            );
        match selected {
            Ok(selected) => {
                self.record_field(base, field, selected * field_width + field_scalar, span)
            }
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable"
                    || reason == "binder-valued subscript has no active domain" =>
            {
                self.dynamic_indexed_record_field(
                    indexed,
                    base,
                    subscripts,
                    field,
                    record_scalar,
                    field_scalar,
                    span,
                )
            }
            Err(error) => Err(error),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn dynamic_indexed_record_field(
        &mut self,
        indexed: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        record_scalar: usize,
        field_scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let result_dimensions = self.node(indexed).value_type().dimensions();
        let base_dimensions = self.node(base).value_type().dimensions().to_vec();
        if !result_dimensions.is_empty()
            || record_scalar != 0
            || subscripts.len() != base_dimensions.len()
        {
            return Err(LowerError::non_computable(
                "runtime record indexing requires one scalar index per base axis and one record result",
                span,
            ));
        }
        let runtime_indices = self.dynamic_scalar_indices(subscripts, span)?;
        let field_width = self.record_layout(indexed, field).field_width();
        if let Some(selected) = self.constant_index_scalar(&runtime_indices, &base_dimensions) {
            return self.record_field(base, field, selected * field_width + field_scalar, span);
        }
        let count = base_dimensions
            .iter()
            .try_fold(1usize, |count, extent| count.checked_mul(*extent as usize))
            .ok_or_else(|| LowerError::contract("runtime indexed record count overflow", span))?;
        let mut candidates = Vec::with_capacity(count);
        for ordinal in 0..count {
            candidates.push(self.record_field(
                base,
                field,
                ordinal * field_width + field_scalar,
                span,
            )?);
        }
        let base = self.pack_registers(&candidates, span)?;
        let dst = self.register(span)?;
        self.ops.push(solve::LinearOp::LoadIndexedRegister {
            dst,
            base,
            stride: 1,
            dimensions: base_dimensions.into_boxed_slice(),
            indices: runtime_indices
                .into_iter()
                .map(solve::TensorIndex::Runtime)
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        });
        Ok(dst)
    }

    #[allow(clippy::too_many_arguments)]
    fn record_array_update_field(
        &mut self,
        updated: dae::ExprId<'dae>,
        base: dae::ExprId<'dae>,
        value: dae::ExprId<'dae>,
        subscripts: dae::SubscriptsView<'dae>,
        field: usize,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let field_width = self.record_layout(updated, field).field_width();
        let base_record = scalar / field_width;
        let field_scalar = scalar % field_width;
        let selected = ScalarSelector::from_points(self.view, &self.domain_points)
            .array_update_value_scalar(
                base,
                subscripts,
                self.node(value).value_type().dimensions(),
                base_record,
            );
        match selected {
            Ok(Some(value_record)) => self.record_field(
                value,
                field,
                value_record * field_width + field_scalar,
                span,
            ),
            Ok(None) => self.record_field(base, field, scalar, span),
            Err(LowerError::NonComputable { reason, .. })
                if reason == "array subscript is not compile-time computable"
                    || reason == "binder-valued subscript has no active domain" =>
            {
                self.dynamic_record_field_array_update(
                    base,
                    value,
                    subscripts,
                    field,
                    base_record,
                    field_scalar,
                    span,
                )
            }
            Err(error) => Err(error),
        }
    }

    fn record_field_scalar_count(&self, expression: dae::ExprId<'dae>, field: usize) -> usize {
        let layout = self.record_layout(expression, field);
        layout.outer_count() * layout.field_width()
    }

    fn record_layout(&self, expression: dae::ExprId<'dae>, field: usize) -> dae::RecordFieldLayout {
        self.view
            .record_field_layout(self.node(expression).value_type_id(), field)
            .expect("checked record projection has a finite field layout")
    }

    // SPEC_0021: Exception - exhaustive top-level lowering of one checked function fold.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    pub(super) fn function_fold_output(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: u32,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let cache_key = self.function_fold_cache_key(fold);
        if let Some(values) = self.function_fold_output_cache.get(&cache_key) {
            return values
                .get(carried as usize)
                .and_then(|value| value.get(scalar))
                .copied()
                .ok_or_else(|| {
                    LowerError::contract("function fold output scalar is out of range", span)
                });
        }
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let values = fold_view
            .initial_values()
            .rhs_iter()
            .map(|initial| {
                (0..scalar_count(self.view, initial))
                    .map(|element| self.expression(initial, element))
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        domain.structured().scalar_count().map_err(|error| {
            LowerError::contract(
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        let initial_flat = values.iter().flatten().copied().collect::<Vec<_>>();
        let initial_start = self.pack_fold_registers(&initial_flat, span)?;
        let capture_registers = self.inherited_fold_capture_registers();
        let update_expressions = fold_view.update_values().rhs_iter().collect::<Vec<_>>();
        let invariant_tensors = self.fold_invariant_index_bases(fold, &update_expressions)?;
        let mut update = self.fork_for_fold_update(&capture_registers, None, span)?;
        update
            .deferred_fold_captures
            .as_mut()
            .expect("function fold update owns deferred captures")
            .packed_expressions
            .extend(invariant_tensors);
        let mut binder_registers = Vec::with_capacity(domain.structured().binders.len());
        for dimension in 0..domain.structured().binders.len() {
            let dst = update.register(span)?;
            update
                .ops
                .push(solve::LinearOp::LoadFoldIndex { dst, dimension });
            binder_registers.push(dst);
        }
        update
            .symbolic_domain_points
            .push((fold_view.domain(), binder_registers));
        let mut carried_values = Vec::with_capacity(values.len());
        let mut carried_index = 0usize;
        for value in &values {
            let mut carried = Vec::with_capacity(value.len());
            for _ in value {
                let dst = update.register(span)?;
                update.ops.push(solve::LinearOp::LoadFoldCarried {
                    dst,
                    index: carried_index,
                });
                carried.push(dst);
                carried_index += 1;
            }
            carried_values.push(carried);
        }
        update.function_fold_values.push((fold, carried_values));
        let mut update_widths = Vec::new();
        let mut carried_base = 0usize;
        for (carried, expression) in update_expressions.into_iter().enumerate() {
            let width = scalar_count(self.view, expression);
            update_widths.push(width);
            if !update.compact_fold_tensor_update(
                fold,
                carried,
                carried_base,
                expression,
                width,
                span,
            )? && !update.compact_nested_fold_output(fold, carried, expression, width, span)?
            {
                for element in 0..width {
                    let output = update.expression(expression, element)?;
                    update
                        .ops
                        .push(solve::LinearOp::StoreOutput { src: output });
                }
            }
            carried_base = carried_base.checked_add(width).ok_or_else(|| {
                LowerError::contract("function-fold carried offset overflow", span)
            })?;
        }
        let initial_widths = values.iter().map(Vec::len).collect::<Vec<_>>();
        if update_widths != initial_widths {
            return Err(LowerError::contract(
                "checked function fold changed its carried tuple shape",
                span,
            ));
        }
        let capture_registers = update
            .deferred_fold_captures
            .as_ref()
            .map(|deferred| deferred.sources.clone())
            .unwrap_or_default();
        let capture_start = self.pack_fold_registers(&capture_registers, span)?;
        let activation = (self.activation_path.len() > self.fold_guard_base)
            .then(|| self.fold_activation(span))
            .transpose()?;
        let carried_count = initial_flat.len();
        let dst_start = self.next_register;
        let mut folded_flat = Vec::with_capacity(carried_count);
        for _ in 0..carried_count {
            folded_flat.push(self.register(span)?);
        }
        let program = std::sync::Arc::new(
            solve::FunctionFoldProgram::checked(
                domain.structured().clone(),
                carried_count,
                capture_registers.len(),
                update.ops,
            )
            .map_err(|error| {
                LowerError::contract(
                    format!("function-fold update register proof failed: {error}"),
                    span,
                )
            })?,
        );
        self.ops.push(match activation {
            Some(activation) => solve::LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            },
            None => solve::LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            },
        });
        let mut folded = Vec::with_capacity(initial_widths.len());
        let mut offset = 0usize;
        for width in initial_widths {
            folded.push(folded_flat[offset..offset + width].to_vec());
            offset += width;
        }
        let output = values
            .get(carried as usize)
            .and_then(|_| folded.get(carried as usize))
            .and_then(|value| value.get(scalar))
            .copied()
            .ok_or_else(|| {
                LowerError::contract("function fold output scalar is out of range", span)
            })?;
        // Evaluating any projection computes the complete carried tuple. Keep
        // that tuple as the cache value so sibling projections do not expand
        // the same loop again. Caching only `output` turns nested array-valued
        // function loops into combinatorial lowering work because every
        // projection recursively asks for all sibling projections.
        self.function_fold_output_cache.insert(cache_key, folded);
        Ok(output)
    }

    /// Evaluate one assertion statement over an entire function fold without
    /// enumerating its domain in Solve IR.
    ///
    /// The original carried tuple is advanced in the same symbolic update as
    /// an extra Boolean `all_safe` lane. Runtime owns domain traversal through
    /// `FunctionFoldProgram`; the lowering footprint is therefore independent
    /// of the number of points in the checked domain.
    // SPEC_0021: Exception - exhaustive top-level lowering of one checked assertion fold.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    pub(super) fn function_fold_assertion_result(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        condition: dae::ExprId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        // An inactive checked call has the exact identity result `all_safe =
        // true`; its loop-carried algorithm must not execute merely to prove
        // that identity. Materialize the complete outer activation before any
        // fold inputs so expensive guard conditions retain their own strict
        // prefixes as well.
        let outer_activation = (self.active_clock.is_some() || !self.activation_path.is_empty())
            .then(|| self.activation(span))
            .transpose()?;
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let initial_expressions = fold_view.initial_values().rhs_iter().collect::<Vec<_>>();
        let widths = initial_expressions
            .iter()
            .map(|expression| scalar_count(self.view, *expression))
            .collect::<Vec<_>>();
        let values = initial_expressions
            .iter()
            .map(|&initial| {
                (0..scalar_count(self.view, initial))
                    .map(|element| self.expression(initial, element))
                    .collect::<Result<Vec<_>, _>>()
            })
            .collect::<Result<Vec<_>, _>>()?;
        let mut initial_flat = values.iter().flatten().copied().collect::<Vec<_>>();
        initial_flat.push(self.constant(1.0, span)?);
        let initial_start = self.pack_fold_registers(&initial_flat, span)?;

        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        domain.structured().scalar_count().map_err(|error| {
            LowerError::contract(
                format!("checked function fold domain became invalid: {error}"),
                span,
            )
        })?;
        let captures = self.inherited_fold_capture_registers();
        let invariant_tensors = self.fold_invariant_index_bases(fold, &[condition])?;
        let mut update = self.fork_for_fold_update(&captures, None, span)?;
        update
            .deferred_fold_captures
            .as_mut()
            .expect("function fold update owns deferred captures")
            .packed_expressions
            .extend(invariant_tensors);
        let mut binder_registers = Vec::with_capacity(domain.structured().binders.len());
        for dimension in 0..domain.structured().binders.len() {
            let dst = update.register(span)?;
            update
                .ops
                .push(solve::LinearOp::LoadFoldIndex { dst, dimension });
            binder_registers.push(dst);
        }
        update
            .symbolic_domain_points
            .push((fold_view.domain(), binder_registers));

        let mut carried_values = Vec::with_capacity(widths.len());
        let mut carried_index = 0usize;
        for &width in &widths {
            let mut carried = Vec::with_capacity(width);
            for _ in 0..width {
                let dst = update.register(span)?;
                update.ops.push(solve::LinearOp::LoadFoldCarried {
                    dst,
                    index: carried_index,
                });
                carried.push(dst);
                carried_index += 1;
            }
            carried_values.push(carried);
        }
        let previous_safe = update.register(span)?;
        update.ops.push(solve::LinearOp::LoadFoldCarried {
            dst: previous_safe,
            index: carried_index,
        });
        update.function_fold_values.push((fold, carried_values));

        let update_expressions = fold_view.update_values().rhs_iter().collect::<Vec<_>>();
        let mut update_widths = Vec::with_capacity(widths.len());
        let mut output_base = 0usize;
        let mut carried = 0usize;
        while carried < update_expressions.len() {
            let expression = update_expressions[carried];
            let scalar_width = scalar_count(self.view, expression);
            let nested_run = update.nested_fold_output_run(&update_expressions, carried);
            let width = if nested_run > 1 {
                update_expressions[carried..carried + nested_run]
                    .iter()
                    .map(|expression| scalar_count(self.view, *expression))
                    .sum()
            } else {
                scalar_width
            };
            let compact_tensor = update.compact_fold_tensor_update(
                fold,
                carried,
                output_base,
                expression,
                scalar_width,
                span,
            )?;
            let compact_nested = !compact_tensor
                && update.compact_nested_fold_output(fold, carried, expression, width, span)?;
            if !compact_tensor && !compact_nested {
                for element in 0..scalar_width {
                    let output = update.expression(expression, element)?;
                    update
                        .ops
                        .push(solve::LinearOp::StoreOutput { src: output });
                }
            }
            let consumed = if compact_nested { nested_run } else { 1 };
            for expression in &update_expressions[carried..carried + consumed] {
                update_widths.push(scalar_count(self.view, *expression));
            }
            let emitted_width = if compact_nested { width } else { scalar_width };
            output_base = output_base.checked_add(emitted_width).ok_or_else(|| {
                LowerError::contract("function-fold carried offset overflow", span)
            })?;
            carried += consumed;
        }
        if update_widths != widths {
            return Err(LowerError::contract(
                "checked function fold changed its carried tuple shape",
                span,
            ));
        }

        let activation = update.activation(span)?;
        let condition = update.expression(condition, 0)?;
        let inactive = update.unary(dae::UnaryOperator::Not, activation, span)?;
        let safe_at_point = update.binary(dae::BinaryOperator::Or, inactive, condition, span)?;
        let all_safe =
            update.binary(dae::BinaryOperator::And, previous_safe, safe_at_point, span)?;
        update
            .ops
            .push(solve::LinearOp::StoreOutput { src: all_safe });

        let mut capture_sources = captures;
        if let Some(deferred) = update.deferred_fold_captures.as_ref() {
            capture_sources.extend_from_slice(&deferred.sources);
        }
        let capture_start = self.pack_fold_registers(&capture_sources, span)?;
        let carried_count = initial_flat.len();
        let dst_start = self.next_register;
        for _ in 0..carried_count {
            self.register(span)?;
        }
        let program = std::sync::Arc::new(
            solve::FunctionFoldProgram::checked(
                domain.structured().clone(),
                carried_count,
                capture_sources.len(),
                update.ops,
            )
            .map_err(|error| {
                LowerError::contract(
                    format!("function assertion fold register proof failed: {error}"),
                    span,
                )
            })?,
        );
        self.ops.push(match outer_activation {
            Some(activation) => solve::LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            },
            None => solve::LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            },
        });
        dst_start
            .checked_add(u32::try_from(carried_count - 1).map_err(|_| {
                LowerError::contract("function assertion fold output count exceeds u32", span)
            })?)
            .ok_or_else(|| LowerError::contract("function assertion fold output overflows", span))
    }

    // SPEC_0021: Exception - exhaustive nested-fold initial-source dispatch.
    #[allow(clippy::too_many_lines)]
    fn compact_nested_fold_output(
        &mut self,
        parent_fold: dae::FunctionFoldId<'dae>,
        parent_carried: usize,
        expression: dae::ExprId<'dae>,
        width: usize,
        span: Span,
    ) -> Result<bool, LowerError> {
        let expression = self.unwrap_function_value(expression);
        let (expression, condition, nested_when_true) = match self.node(expression).operation() {
            dae::ExpressionOperation::FunctionFoldOutput { .. } => (expression, None, true),
            dae::ExpressionOperation::Conditional(operands) if operands.len() == 3 => {
                let (Some(condition), Some(if_true), Some(if_false)) =
                    (operands.get(0), operands.get(1), operands.get(2))
                else {
                    return Ok(false);
                };
                let if_true = self.unwrap_function_value(if_true);
                let if_false = self.unwrap_function_value(if_false);
                let true_nested = matches!(
                    self.node(if_true).operation(),
                    dae::ExpressionOperation::FunctionFoldOutput { .. }
                );
                let false_nested = matches!(
                    self.node(if_false).operation(),
                    dae::ExpressionOperation::FunctionFoldOutput { .. }
                );
                let is_parent = |branch| {
                    matches!(
                    self.node(branch).operation(),
                    dae::ExpressionOperation::FunctionFoldParameter { fold, carried, .. }
                        if fold == parent_fold && carried as usize == parent_carried
                        )
                };
                if true_nested && is_parent(if_false) {
                    (if_true, Some(self.expression(condition, 0)?), true)
                } else if false_nested && is_parent(if_true) {
                    (if_false, Some(self.expression(condition, 0)?), false)
                } else {
                    return Ok(false);
                }
            }
            _ => return Ok(false),
        };
        let dae::ExpressionOperation::FunctionFoldOutput { fold, carried, .. } =
            self.node(expression).operation()
        else {
            return Ok(false);
        };
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("nested function fold does not resolve", span))?;
        let initial_expressions = fold_view.initial_values().rhs_iter().collect::<Vec<_>>();
        let initial_widths = initial_expressions
            .iter()
            .map(|initial| scalar_count(self.view, *initial))
            .collect::<Vec<_>>();
        let mut initial = Vec::with_capacity(initial_expressions.len());
        for (&expression, &count) in initial_expressions.iter().zip(&initial_widths) {
            let base = self.unwrap_function_value(expression);
            let parent_source = matches!(
                self.node(base).operation(),
                dae::ExpressionOperation::FunctionFoldParameter { fold, .. }
                    if fold == parent_fold
            )
            .then(|| self.fold_carried_tensor_base(base))
            .flatten();
            if let Some(base) = parent_source {
                initial.push(solve::FoldInitialSource::ParentCarried { base, count });
            } else {
                let registers = (0..count)
                    .map(|scalar| self.expression(expression, scalar))
                    .collect::<Result<Vec<_>, _>>()?;
                initial.push(solve::FoldInitialSource::Registers {
                    start: self.pack_fold_registers(&registers, span)?,
                    count,
                });
            }
        }
        let inherited = self.inherited_fold_capture_registers_excluding(Some(parent_fold));
        let (program, captures) = self.build_function_fold_program(
            fold,
            &initial_widths,
            &inherited,
            Some(parent_fold),
            span,
        )?;
        let capture_start = self.pack_fold_registers(&captures, span)?;
        let result_base = initial_widths
            .iter()
            .take(carried as usize)
            .try_fold(0usize, |base, width| base.checked_add(*width))
            .ok_or_else(|| LowerError::contract("nested fold result offset overflow", span))?;
        if result_base
            .checked_add(width)
            .is_none_or(|end| end > program.carried_count)
        {
            return Err(LowerError::contract(
                "nested fold aggregate output range is invalid",
                span,
            ));
        }
        self.ops.push(solve::LinearOp::StoreOutputFunctionFold {
            initial: initial.into_boxed_slice(),
            capture_start,
            program,
            result_base,
            count: width,
            condition,
            nested_when_true,
        });
        Ok(true)
    }

    // SPEC_0021: Exception - exhaustive checked construction of a function-fold program.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    fn build_function_fold_program(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        widths: &[usize],
        captures: &[solve::Reg],
        excluded_parent: Option<dae::FunctionFoldId<'dae>>,
        span: Span,
    ) -> Result<(std::sync::Arc<solve::FunctionFoldProgram>, Vec<solve::Reg>), LowerError> {
        let cache_key = (fold, excluded_parent, widths.to_vec());
        if let Some(cached) = self.function_fold_program_cache.get(&cache_key) {
            return Ok((cached.program.clone(), cached.capture_sources.clone()));
        }
        let fold_view = self
            .view
            .function_fold(fold)
            .ok_or_else(|| LowerError::contract("function fold identity does not resolve", span))?;
        let domain = self
            .view
            .domain(fold_view.domain())
            .expect("checked function fold domain resolves");
        let update_expressions = fold_view.update_values().rhs_iter().collect::<Vec<_>>();
        let invariant_tensors = self.fold_invariant_index_bases(fold, &update_expressions)?;
        let mut update = self.fork_for_fold_update(captures, excluded_parent, span)?;
        update
            .deferred_fold_captures
            .as_mut()
            .expect("function fold update owns deferred captures")
            .packed_expressions
            .extend(invariant_tensors);
        let mut binder_registers = Vec::with_capacity(domain.structured().binders.len());
        for dimension in 0..domain.structured().binders.len() {
            let dst = update.register(span)?;
            update
                .ops
                .push(solve::LinearOp::LoadFoldIndex { dst, dimension });
            binder_registers.push(dst);
        }
        update
            .symbolic_domain_points
            .push((fold_view.domain(), binder_registers));
        let mut carried_values = Vec::with_capacity(widths.len());
        let mut carried_index = 0usize;
        for &width in widths {
            let mut carried = Vec::with_capacity(width);
            for _ in 0..width {
                let dst = update.register(span)?;
                update.ops.push(solve::LinearOp::LoadFoldCarried {
                    dst,
                    index: carried_index,
                });
                carried.push(dst);
                carried_index += 1;
            }
            carried_values.push(carried);
        }
        update.function_fold_values.push((fold, carried_values));
        let mut update_widths = Vec::with_capacity(widths.len());
        let mut output_base = 0usize;
        let mut carried = 0usize;
        while carried < update_expressions.len() {
            let expression = update_expressions[carried];
            let scalar_width = scalar_count(self.view, expression);
            let nested_run = self.nested_fold_output_run(&update_expressions, carried);
            let width = if nested_run > 1 {
                update_expressions[carried..carried + nested_run]
                    .iter()
                    .map(|expression| scalar_count(self.view, *expression))
                    .sum()
            } else {
                scalar_width
            };
            let compact_tensor = update.compact_fold_tensor_update(
                fold,
                carried,
                output_base,
                expression,
                scalar_width,
                span,
            )?;
            let compact_nested = !compact_tensor
                && update.compact_nested_fold_output(fold, carried, expression, width, span)?;
            if !compact_tensor && !compact_nested {
                for element in 0..scalar_width {
                    let output = update.expression(expression, element)?;
                    update
                        .ops
                        .push(solve::LinearOp::StoreOutput { src: output });
                }
            }
            let consumed = if compact_nested { nested_run } else { 1 };
            for expression in &update_expressions[carried..carried + consumed] {
                update_widths.push(scalar_count(self.view, *expression));
            }
            let emitted_width = if compact_nested { width } else { scalar_width };
            output_base = output_base.checked_add(emitted_width).ok_or_else(|| {
                LowerError::contract("function-fold carried offset overflow", span)
            })?;
            carried += consumed;
        }
        if update_widths != widths {
            return Err(LowerError::contract(
                "checked function fold changed its carried tuple shape",
                span,
            ));
        }
        let carried_count = widths.iter().sum();
        let mut capture_sources = captures.to_vec();
        if let Some(deferred) = update.deferred_fold_captures.as_ref() {
            capture_sources.extend_from_slice(&deferred.sources);
        }
        let program = std::sync::Arc::new(
            solve::FunctionFoldProgram::checked(
                domain.structured().clone(),
                carried_count,
                capture_sources.len(),
                update.ops,
            )
            .map_err(|error| {
                LowerError::contract(
                    format!("function-fold update register proof failed: {error}"),
                    span,
                )
            })?,
        );
        self.function_fold_program_cache.insert(
            cache_key,
            CachedFunctionFoldProgram {
                program: program.clone(),
                capture_sources: capture_sources.clone(),
            },
        );
        Ok((program, capture_sources))
    }

    fn nested_fold_output_run(&self, expressions: &[dae::ExprId<'dae>], start: usize) -> usize {
        let first = self.unwrap_function_value(expressions[start]);
        let dae::ExpressionOperation::FunctionFoldOutput {
            fold,
            carried: first_carried,
            ..
        } = self.node(first).operation()
        else {
            return 1;
        };
        let Some(fold_view) = self.view.function_fold(fold) else {
            return 1;
        };
        let initial = fold_view.initial_values().rhs_iter().collect::<Vec<_>>();
        let mut run = 0usize;
        for (offset, expression) in expressions[start..].iter().enumerate() {
            let expression = self.unwrap_function_value(*expression);
            let dae::ExpressionOperation::FunctionFoldOutput {
                fold: candidate,
                carried,
                ..
            } = self.node(expression).operation()
            else {
                break;
            };
            let expected = first_carried as usize + offset;
            if candidate != fold
                || carried as usize != expected
                || initial.get(expected).is_none_or(|initial| {
                    scalar_count(self.view, *initial) != scalar_count(self.view, expression)
                })
            {
                break;
            }
            run += 1;
        }
        run.max(1)
    }

    // SPEC_0021: Exception - exhaustive compact tensor-update expression dispatch.
    #[allow(clippy::excessive_nesting, clippy::too_many_lines)]
    fn compact_fold_tensor_update(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        carried: usize,
        carried_base: usize,
        expression: dae::ExprId<'dae>,
        width: usize,
        span: Span,
    ) -> Result<bool, LowerError> {
        if let Some(graph) = self.fold_tensor_update_graph(fold, carried, expression) {
            return self.emit_fold_tensor_update_graph(
                carried_base,
                expression,
                width,
                graph,
                span,
            );
        }
        if let Some(plan) = self.fold_tensor_update_plan(fold, carried, expression) {
            return self.emit_fold_tensor_update_plan(carried_base, expression, width, plan, span);
        }
        let mut expression = self.unwrap_function_value(expression);
        let mut conditions = Vec::new();
        while let dae::ExpressionOperation::Conditional(operands) =
            self.node(expression).operation()
        {
            if operands.len() != 3 {
                break;
            }
            let Some(condition) = operands.get(0) else {
                break;
            };
            let Some(if_true) = operands.get(1) else {
                break;
            };
            let Some(if_false) = operands.get(2) else {
                break;
            };
            let if_false = self.unwrap_function_value(if_false);
            let dae::ExpressionOperation::FunctionFoldParameter {
                fold: fallback_fold,
                carried: fallback_carried,
                ..
            } = self.node(if_false).operation()
            else {
                break;
            };
            if fallback_fold != fold || fallback_carried as usize != carried {
                break;
            }
            conditions.push(condition);
            expression = self.unwrap_function_value(if_true);
        }
        let dae::ExpressionOperation::ArrayUpdate {
            base,
            value,
            subscripts,
        } = self.node(expression).operation()
        else {
            return Ok(false);
        };
        let base = self.unwrap_function_value(base);
        let dae::ExpressionOperation::FunctionFoldParameter {
            fold: base_fold,
            carried: base_carried,
            ..
        } = self.node(base).operation()
        else {
            return Ok(false);
        };
        if base_fold != fold
            || base_carried as usize != carried
            || self.node(value).value_type().is_record()
        {
            return Ok(false);
        }
        let dimensions = self.node(base).value_type().dimensions().to_vec();
        if dimensions.is_empty() || dimensions.len() != subscripts.len() {
            return Ok(false);
        }
        let mut compact = Vec::with_capacity(dimensions.len());
        for (axis, &extent) in dimensions.iter().enumerate() {
            match subscripts.get(axis) {
                Some(dae::SubscriptView::Whole { .. }) | None => {
                    compact.push(solve::TensorSubscript::Whole);
                }
                Some(dae::SubscriptView::Index { expression, .. }) => {
                    let register = self.expression(expression, 0)?;
                    let index = match self.integer_register(register) {
                        Some(index) => {
                            solve::TensorIndex::Constant(checked_index(index, extent, span)?)
                        }
                        None => solve::TensorIndex::Runtime(register),
                    };
                    compact.push(solve::TensorSubscript::Index(index));
                }
                Some(dae::SubscriptView::Slice { .. }) => return Ok(false),
            }
        }
        let value_count = self
            .node(value)
            .value_type()
            .scalar_count()
            .ok_or_else(|| {
                LowerError::contract("tensor update value has no finite scalar count", span)
            })?;
        let expected_value_count = dimensions
            .iter()
            .zip(compact.iter())
            .try_fold(1usize, |count, (&extent, subscript)| {
                if matches!(subscript, solve::TensorSubscript::Whole) {
                    count.checked_mul(extent as usize)
                } else {
                    Some(count)
                }
            })
            .ok_or_else(|| LowerError::contract("tensor update value extent overflow", span))?;
        if value_count != expected_value_count {
            return Err(LowerError::contract(
                "tensor update value shape does not match its whole axes",
                span,
            ));
        }
        let carried_count = dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
            .ok_or_else(|| LowerError::contract("tensor update carried extent overflow", span))?;
        if carried_count != width {
            return Err(LowerError::contract(
                "tensor update carried shape does not match its fold output",
                span,
            ));
        }
        let value_start = self.pack_expression(value)?;
        let mut condition = None;
        for expression in conditions {
            let next = self.expression(expression, 0)?;
            condition = Some(match condition {
                Some(previous) => self.binary(dae::BinaryOperator::And, previous, next, span)?,
                None => next,
            });
        }
        self.ops.push(solve::LinearOp::StoreOutputFoldTensorUpdate {
            source_base: carried_base,
            source_stride: 1,
            dimensions: dimensions.into_boxed_slice(),
            updates: Box::new([solve::FoldTensorUpdate {
                subscripts: compact.into_boxed_slice(),
                condition,
                value_start,
                value_stride: 1,
            }]),
            nodes: Box::new([solve::FoldTensorNode::Update { base: 0, update: 0 }]),
            result: 1,
            lanes: 1,
        });
        Ok(true)
    }

    fn fold_tensor_update_graph(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: usize,
        expression: dae::ExprId<'dae>,
    ) -> Option<PendingFoldTensorGraph<'dae>> {
        let mut memo = HashMap::new();
        let mut updates = Vec::new();
        let mut nodes = Vec::new();
        let result = self.build_fold_tensor_graph(
            fold,
            carried,
            expression,
            &mut memo,
            &mut updates,
            &mut nodes,
        )?;
        (result != 0).then_some(PendingFoldTensorGraph {
            updates,
            nodes,
            result,
        })
    }

    fn build_fold_tensor_graph(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: usize,
        expression: dae::ExprId<'dae>,
        memo: &mut HashMap<dae::ExprId<'dae>, u32>,
        updates: &mut Vec<PendingFoldTensorUpdate<'dae>>,
        nodes: &mut Vec<PendingFoldTensorNode<'dae>>,
    ) -> Option<u32> {
        let expression = self.unwrap_function_value(expression);
        if let Some(&node) = memo.get(&expression) {
            return Some(node);
        }
        let node = match self.node(expression).operation() {
            dae::ExpressionOperation::FunctionFoldParameter {
                fold: base_fold,
                carried: base_carried,
                ..
            } if base_fold == fold && base_carried as usize == carried => 0,
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                let base =
                    self.build_fold_tensor_graph(fold, carried, base, memo, updates, nodes)?;
                let update = u32::try_from(updates.len()).ok()?;
                updates.push(PendingFoldTensorUpdate {
                    value,
                    subscripts,
                    conditions: Vec::new(),
                });
                let node = u32::try_from(nodes.len() + 1).ok()?;
                nodes.push(PendingFoldTensorNode::Update { base, update });
                node
            }
            dae::ExpressionOperation::Conditional(operands) if operands.len() == 3 => {
                let (Some(condition), Some(if_true), Some(if_false)) =
                    (operands.get(0), operands.get(1), operands.get(2))
                else {
                    return None;
                };
                let if_true =
                    self.build_fold_tensor_graph(fold, carried, if_true, memo, updates, nodes)?;
                let if_false =
                    self.build_fold_tensor_graph(fold, carried, if_false, memo, updates, nodes)?;
                let node = u32::try_from(nodes.len() + 1).ok()?;
                nodes.push(PendingFoldTensorNode::Select {
                    condition,
                    if_true,
                    if_false,
                });
                node
            }
            _ => return None,
        };
        memo.insert(expression, node);
        Some(node)
    }

    fn fold_tensor_update_plan(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: usize,
        expression: dae::ExprId<'dae>,
    ) -> Option<Vec<PendingFoldTensorUpdate<'dae>>> {
        let mut updates = Vec::new();
        if self.collect_fold_tensor_base(fold, carried, expression, &[], &mut updates) {
            return Some(updates);
        }
        updates.clear();
        self.collect_guarded_fold_tensor_base(fold, carried, expression, &[], &mut updates)
            .then_some(updates)
    }

    fn collect_fold_tensor_base(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: usize,
        expression: dae::ExprId<'dae>,
        conditions: &[(dae::ExprId<'dae>, bool)],
        updates: &mut Vec<PendingFoldTensorUpdate<'dae>>,
    ) -> bool {
        let expression = self.unwrap_function_value(expression);
        match self.node(expression).operation() {
            dae::ExpressionOperation::FunctionFoldParameter {
                fold: base_fold,
                carried: base_carried,
                ..
            } => base_fold == fold && base_carried as usize == carried,
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                if !self.collect_fold_tensor_base(fold, carried, base, conditions, updates) {
                    return false;
                }
                updates.push(PendingFoldTensorUpdate {
                    value,
                    subscripts,
                    conditions: conditions.to_vec(),
                });
                true
            }
            dae::ExpressionOperation::Conditional(operands) if operands.len() == 3 => {
                let (Some(condition), Some(if_true), Some(if_false)) =
                    (operands.get(0), operands.get(1), operands.get(2))
                else {
                    return false;
                };
                if !self.collect_fold_tensor_base(fold, carried, if_false, conditions, updates) {
                    return false;
                }
                let mut branch_conditions = conditions.to_vec();
                branch_conditions.push((condition, true));
                self.collect_fold_tensor_delta(if_true, if_false, &branch_conditions, updates)
            }
            _ => false,
        }
    }

    fn collect_fold_tensor_delta(
        &self,
        expression: dae::ExprId<'dae>,
        baseline: dae::ExprId<'dae>,
        conditions: &[(dae::ExprId<'dae>, bool)],
        updates: &mut Vec<PendingFoldTensorUpdate<'dae>>,
    ) -> bool {
        let expression = self.unwrap_function_value(expression);
        let baseline = self.unwrap_function_value(baseline);
        if expression == baseline {
            return true;
        }
        match self.node(expression).operation() {
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                if !self.collect_fold_tensor_delta(base, baseline, conditions, updates) {
                    return false;
                }
                updates.push(PendingFoldTensorUpdate {
                    value,
                    subscripts,
                    conditions: conditions.to_vec(),
                });
                true
            }
            dae::ExpressionOperation::Conditional(operands) if operands.len() == 3 => {
                let (Some(condition), Some(if_true), Some(if_false)) =
                    (operands.get(0), operands.get(1), operands.get(2))
                else {
                    return false;
                };
                if !self.collect_fold_tensor_delta(if_false, baseline, conditions, updates) {
                    return false;
                }
                let mut branch_conditions = conditions.to_vec();
                branch_conditions.push((condition, true));
                self.collect_fold_tensor_delta(if_true, if_false, &branch_conditions, updates)
            }
            _ => false,
        }
    }

    fn collect_guarded_fold_tensor_base(
        &self,
        fold: dae::FunctionFoldId<'dae>,
        carried: usize,
        expression: dae::ExprId<'dae>,
        conditions: &[(dae::ExprId<'dae>, bool)],
        updates: &mut Vec<PendingFoldTensorUpdate<'dae>>,
    ) -> bool {
        if updates.len() > 4096 {
            return false;
        }
        let expression = self.unwrap_function_value(expression);
        match self.node(expression).operation() {
            dae::ExpressionOperation::FunctionFoldParameter {
                fold: base_fold,
                carried: base_carried,
                ..
            } => base_fold == fold && base_carried as usize == carried,
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                if !self.collect_guarded_fold_tensor_base(fold, carried, base, conditions, updates)
                {
                    return false;
                }
                updates.push(PendingFoldTensorUpdate {
                    value,
                    subscripts,
                    conditions: conditions.to_vec(),
                });
                true
            }
            dae::ExpressionOperation::Conditional(operands) if operands.len() == 3 => {
                let (Some(condition), Some(if_true), Some(if_false)) =
                    (operands.get(0), operands.get(1), operands.get(2))
                else {
                    return false;
                };
                let mut false_conditions = conditions.to_vec();
                false_conditions.push((condition, false));
                if !self.collect_guarded_fold_tensor_base(
                    fold,
                    carried,
                    if_false,
                    &false_conditions,
                    updates,
                ) {
                    return false;
                }
                let mut true_conditions = conditions.to_vec();
                true_conditions.push((condition, true));
                self.collect_guarded_fold_tensor_base(
                    fold,
                    carried,
                    if_true,
                    &true_conditions,
                    updates,
                )
            }
            _ => false,
        }
    }

    fn emit_fold_tensor_update_graph(
        &mut self,
        carried_base: usize,
        expression: dae::ExprId<'dae>,
        width: usize,
        graph: PendingFoldTensorGraph<'dae>,
        span: Span,
    ) -> Result<bool, LowerError> {
        let mut nodes = Vec::with_capacity(graph.nodes.len());
        for node in graph.nodes {
            nodes.push(match node {
                PendingFoldTensorNode::Update { base, update } => {
                    solve::FoldTensorNode::Update { base, update }
                }
                PendingFoldTensorNode::Select {
                    condition,
                    if_true,
                    if_false,
                } => solve::FoldTensorNode::Select {
                    condition: self.expression(condition, 0)?,
                    if_true,
                    if_false,
                },
            });
        }
        if !self.emit_fold_tensor_update_plan(
            carried_base,
            expression,
            width,
            graph.updates,
            span,
        )? {
            return Ok(false);
        }
        let Some(solve::LinearOp::StoreOutputFoldTensorUpdate {
            nodes: emitted_nodes,
            result,
            ..
        }) = self.ops.last_mut()
        else {
            return Err(LowerError::contract(
                "tensor graph lowering lost its aggregate output owner",
                span,
            ));
        };
        *emitted_nodes = nodes.into_boxed_slice();
        *result = graph.result;
        Ok(true)
    }

    // SPEC_0021: Exception - exhaustive tensor-update subscript emission dispatch.
    #[allow(clippy::excessive_nesting)]
    fn emit_fold_tensor_update_plan(
        &mut self,
        carried_base: usize,
        expression: dae::ExprId<'dae>,
        width: usize,
        plan: Vec<PendingFoldTensorUpdate<'dae>>,
        span: Span,
    ) -> Result<bool, LowerError> {
        if plan.is_empty() {
            return Ok(false);
        }
        let dimensions = self.node(expression).value_type().dimensions().to_vec();
        let carried_count = dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
            .ok_or_else(|| LowerError::contract("tensor update carried extent overflow", span))?;
        if dimensions.is_empty() || carried_count != width {
            return Ok(false);
        }
        let mut updates = Vec::with_capacity(plan.len());
        for pending in plan {
            if self.node(pending.value).value_type().is_record()
                || pending.subscripts.len() != dimensions.len()
            {
                return Ok(false);
            }
            let mut subscripts = Vec::with_capacity(dimensions.len());
            for (axis, &extent) in dimensions.iter().enumerate() {
                match pending.subscripts.get(axis) {
                    Some(dae::SubscriptView::Whole { .. }) | None => {
                        subscripts.push(solve::TensorSubscript::Whole);
                    }
                    Some(dae::SubscriptView::Index { expression, .. }) => {
                        let register = self.expression(expression, 0)?;
                        let index = match self.integer_register(register) {
                            Some(index) => {
                                solve::TensorIndex::Constant(checked_index(index, extent, span)?)
                            }
                            None => solve::TensorIndex::Runtime(register),
                        };
                        subscripts.push(solve::TensorSubscript::Index(index));
                    }
                    Some(dae::SubscriptView::Slice { .. }) => return Ok(false),
                }
            }
            let value_count = self
                .node(pending.value)
                .value_type()
                .scalar_count()
                .ok_or_else(|| {
                    LowerError::contract("tensor update value has no finite scalar count", span)
                })?;
            let expected = dimensions
                .iter()
                .zip(subscripts.iter())
                .try_fold(1usize, |count, (&extent, subscript)| {
                    if matches!(subscript, solve::TensorSubscript::Whole) {
                        count.checked_mul(extent as usize)
                    } else {
                        Some(count)
                    }
                })
                .ok_or_else(|| LowerError::contract("tensor update value extent overflow", span))?;
            if value_count != expected {
                return Err(LowerError::contract(
                    "tensor update value shape does not match its whole axes",
                    span,
                ));
            }
            let mut condition = None;
            for (expression, expected) in pending.conditions {
                let mut next = self.expression(expression, 0)?;
                if !expected {
                    next = self.unary(dae::UnaryOperator::Not, next, span)?;
                }
                condition = Some(match condition {
                    Some(previous) => {
                        self.binary(dae::BinaryOperator::And, previous, next, span)?
                    }
                    None => next,
                });
            }
            updates.push(solve::FoldTensorUpdate {
                subscripts: subscripts.into_boxed_slice(),
                condition,
                value_start: self.pack_expression(pending.value)?,
                value_stride: 1,
            });
        }
        let nodes = (0..updates.len())
            .map(|index| solve::FoldTensorNode::Update {
                base: index as u32,
                update: index as u32,
            })
            .collect::<Vec<_>>();
        self.ops.push(solve::LinearOp::StoreOutputFoldTensorUpdate {
            source_base: carried_base,
            source_stride: 1,
            dimensions: dimensions.into_boxed_slice(),
            updates: updates.into_boxed_slice(),
            result: nodes.len() as u32,
            nodes: nodes.into_boxed_slice(),
            lanes: 1,
        });
        Ok(true)
    }

    fn unwrap_function_value(&self, mut expression: dae::ExprId<'dae>) -> dae::ExprId<'dae> {
        while let dae::ExpressionOperation::FunctionValue { definition, .. } =
            self.node(expression).operation()
        {
            expression = definition.rhs();
        }
        expression
    }

    fn pack_fold_registers(
        &mut self,
        values: &[solve::Reg],
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some(&start) = self.fold_register_pack_cache.get(values) {
            return Ok(start);
        }
        if let Some(&start) = values.first()
            && values
                .iter()
                .copied()
                .enumerate()
                .all(|(offset, register)| {
                    u32::try_from(offset)
                        .ok()
                        .and_then(|offset| start.checked_add(offset))
                        == Some(register)
                })
        {
            self.fold_register_pack_cache.insert(values.to_vec(), start);
            return Ok(start);
        }
        let start = self.next_register;
        for &src in values {
            let dst = self.register(span)?;
            self.ops.push(solve::LinearOp::Move { dst, src });
        }
        self.fold_register_pack_cache.insert(values.to_vec(), start);
        Ok(start)
    }

    fn inherited_fold_capture_registers(&self) -> Vec<solve::Reg> {
        Vec::new()
    }

    fn inherited_fold_capture_registers_excluding(
        &self,
        _excluded: Option<dae::FunctionFoldId<'dae>>,
    ) -> Vec<solve::Reg> {
        Vec::new()
    }

    /// Pack aggregate/index bases and nested folds that are invariant across
    /// one compact fold.
    ///
    /// A dynamic `a[i]` inside a reduction must not rebuild `a` for every
    /// value of `i`. The parent program owns `a`; the fold update receives its
    /// contiguous lanes through `LoadFoldCapture` and performs only the indexed
    /// load. This is construction-time loop-invariant capture, so the tensor is
    /// never scalar-expanded and later rediscovered.
    // SPEC_0021: Exception - exhaustive expression-tree collection of fold-invariant owners.
    #[allow(clippy::excessive_nesting)]
    fn fold_invariant_index_bases(
        &mut self,
        fold: dae::FunctionFoldId<'dae>,
        roots: &[dae::ExprId<'dae>],
    ) -> Result<Vec<FoldInvariantIndexBase<'dae>>, LowerError> {
        let mut expressions = Vec::new();
        for &root in roots {
            dae::for_each_expression(self.view, root, |expression_id, expression| {
                if let dae::ExpressionOperation::Index { base, .. } = expression.operation()
                    && !expressions.contains(&base)
                    && self.expression_depends_on_fold(expression_id, fold)
                    && !self.expression_depends_on_fold(base, fold)
                {
                    expressions.push(base);
                }
            });
        }
        for &root in roots {
            dae::for_each_expression(self.view, root, |expression_id, expression| {
                if matches!(
                    expression.operation(),
                    dae::ExpressionOperation::FunctionFoldOutput { .. }
                ) && !expressions.contains(&expression_id)
                    && !self.expression_depends_on_fold(expression_id, fold)
                {
                    expressions.push(expression_id);
                }
            });
        }
        expressions
            .into_iter()
            .map(|base| {
                let count = scalar_count(self.view, base);
                self.pack_expression(base)
                    .map(|start| (base, (start, count)))
            })
            .collect()
    }

    // SPEC_0021: Exception - exhaustive recursive dispatch over expression operation variants.
    #[allow(clippy::excessive_nesting)]
    fn expression_depends_on_fold(
        &self,
        root: dae::ExprId<'dae>,
        fold: dae::FunctionFoldId<'dae>,
    ) -> bool {
        let domain = self
            .view
            .function_fold(fold)
            .expect("checked function fold resolves")
            .domain();
        let mut pending = vec![root];
        let mut visited = HashSet::new();
        while let Some(root) = pending.pop() {
            if !visited.insert(root) {
                continue;
            }
            let mut dependent = false;
            dae::for_each_expression(self.view, root, |_, expression| {
                match expression.operation() {
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder)) => {
                        let available_in_parent = self
                            .domain_points
                            .iter()
                            .any(|(candidate, _)| *candidate == binder.domain())
                            || self
                                .symbolic_domain_points
                                .iter()
                                .any(|(candidate, _)| *candidate == binder.domain())
                            || self
                                .deferred_fold_captures
                                .as_ref()
                                .is_some_and(|captures| {
                                    captures
                                        .symbolic_domain_points
                                        .iter()
                                        .any(|(candidate, _)| *candidate == binder.domain())
                                });
                        if binder.domain() == domain || !available_in_parent {
                            dependent = true;
                        }
                    }
                    dae::ExpressionOperation::Coordinate(
                        dae::CoordinateView::FunctionParameter(parameter),
                    ) => {
                        if let Some(argument) = self
                            .function_arguments
                            .iter()
                            .rfind(|frame| frame.function == parameter.function())
                            .and_then(|frame| frame.arguments.get(parameter.ordinal() as usize))
                            .copied()
                        {
                            pending.push(argument);
                        } else {
                            dependent = true;
                        }
                    }
                    dae::ExpressionOperation::FunctionFoldParameter {
                        fold: candidate, ..
                    } => {
                        let available_in_parent = self
                            .function_fold_values
                            .iter()
                            .any(|(active, _)| *active == candidate)
                            || self
                                .deferred_fold_captures
                                .as_ref()
                                .is_some_and(|captures| {
                                    captures
                                        .fold_values
                                        .iter()
                                        .any(|(active, _)| *active == candidate)
                                });
                        if candidate == fold || !available_in_parent {
                            dependent = true;
                        }
                    }
                    dae::ExpressionOperation::FunctionFoldOutput {
                        fold: candidate, ..
                    } if candidate == fold => {
                        dependent = true;
                    }
                    _ => {}
                }
            });
            if dependent {
                return true;
            }
        }
        false
    }

    fn fork_for_fold_update(
        &mut self,
        captures: &[solve::Reg],
        _excluded: Option<dae::FunctionFoldId<'dae>>,
        span: Span,
    ) -> Result<Self, LowerError> {
        let inherited_symbolic = self
            .deferred_fold_captures
            .as_ref()
            .map(|deferred| deferred.symbolic_domain_points.clone())
            .unwrap_or_default();
        let mut symbolic_domain_points = self.symbolic_domain_points.clone();
        for (domain, sources) in inherited_symbolic {
            let registers = sources
                .into_iter()
                .map(|source| self.deferred_fold_capture(source, span))
                .collect::<Result<Vec<_>, _>>()?;
            symbolic_domain_points.push((domain, registers));
        }
        let mut compiler = Self::new(self.view, self.layout, None);
        compiler.domain_points = self.domain_points.clone();
        if !captures.is_empty() {
            return Err(LowerError::contract(
                "eager function-fold captures violate the compact fold ABI",
                span,
            ));
        }
        compiler.function_arguments = self.function_arguments.clone();
        compiler.deferred_fold_captures = Some(DeferredFoldCaptures {
            fold_values: self.function_fold_values.clone(),
            symbolic_domain_points,
            packed_expressions: HashMap::new(),
            packed_capture_ranges: HashMap::new(),
            sources: Vec::new(),
            locals: HashMap::new(),
        });
        compiler.symbolic_domain_points = Vec::new();
        compiler.function_fold_values = Vec::new();
        compiler.activation_path = self
            .activation_path
            .iter()
            .copied()
            .map(|guard| {
                let register = guard
                    .register
                    .map(|source| compiler.deferred_fold_capture(source, span))
                    .transpose()?;
                Ok(ActivationGuard { register, ..guard })
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        compiler.fold_guard_base = compiler.activation_path.len();
        compiler.active_clock = self.active_clock;
        compiler.sampled_source = self.sampled_source;
        compiler.derivative_definitions = self.derivative_definitions;
        compiler.parameter_substitutions = self.parameter_substitutions;
        compiler.active_parameters = self.active_parameters.clone();
        compiler.active_call_assertions = self.active_call_assertions.clone();
        compiler.call_action_compilation = self.call_action_compilation;
        compiler.suppress_function_assertions = self.suppress_function_assertions;
        compiler.context_ids = self.context_ids.clone();
        compiler.context_frames = self.context_frames.clone();
        compiler.context_stack = self.context_stack.clone();
        compiler.context_id = self.context_id;
        compiler.next_context_id = self.next_context_id;
        compiler.function_conditional_owners = self.function_conditional_owners;
        Ok(compiler)
    }

    pub(super) fn function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.suspend_context();
        // A lazily expanded expression can reference a parameter of any
        // function on the activation stack, not only the innermost call:
        // argument and result bindings from outer calls are lowered while
        // deeper frames are active. Resolve against the innermost frame of
        // the owning function and suspend every frame above it, so the
        // argument lowers in the owning call's own caller context.
        let Some(owner_index) = self
            .function_arguments
            .iter()
            .rposition(|frame| frame.function == parameter.function())
        else {
            let stack: Vec<String> = self
                .function_arguments
                .iter()
                .map(|entry| self.function_name_for_diagnostic(entry.function))
                .collect();
            self.resume_context(context);
            return Err(LowerError::non_computable(
                format!(
                    "function parameter escaped its checked call owner: \
                     parameter {} of `{}` with call stack [{}]",
                    parameter.ordinal(),
                    self.function_name_for_diagnostic(parameter.function()),
                    stack.join(", "),
                ),
                span,
            ));
        };
        let mut suspended_frames = self.function_arguments.split_off(owner_index + 1);
        let frame = self
            .function_arguments
            .pop()
            .expect("owning frame index was just resolved");
        let suspended_activations = self.activation_path.split_off(frame.activation_base);
        let argument = frame.arguments.get(parameter.ordinal() as usize).copied();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::non_computable(
                    format!(
                        "function parameter escaped its checked call owner: \
                         parameter {} of `{}` has no checked argument",
                        parameter.ordinal(),
                        self.function_name_for_diagnostic(parameter.function()),
                    ),
                    span,
                )
            })
            .and_then(|argument| self.expression(argument, scalar));
        self.activation_path.extend(suspended_activations);
        self.function_arguments.push(frame);
        self.function_arguments.append(&mut suspended_frames);
        self.resume_context(context);
        lowered
    }

    pub(super) fn pack_function_parameter(
        &mut self,
        parameter: dae::FunctionParameterId<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.suspend_context();
        let Some(owner_index) = self
            .function_arguments
            .iter()
            .rposition(|frame| frame.function == parameter.function())
        else {
            self.resume_context(context);
            return Err(LowerError::non_computable(
                format!(
                    "aggregate function parameter {} escaped `{}`",
                    parameter.ordinal(),
                    self.function_name_for_diagnostic(parameter.function()),
                ),
                span,
            ));
        };
        let mut suspended_frames = self.function_arguments.split_off(owner_index + 1);
        let frame = self
            .function_arguments
            .pop()
            .expect("owning frame index was just resolved");
        let suspended_activations = self.activation_path.split_off(frame.activation_base);
        let argument = frame.arguments.get(parameter.ordinal() as usize).copied();
        let lowered = argument
            .ok_or_else(|| {
                LowerError::non_computable(
                    format!(
                        "aggregate function parameter {} of `{}` has no checked argument",
                        parameter.ordinal(),
                        self.function_name_for_diagnostic(parameter.function()),
                    ),
                    span,
                )
            })
            .and_then(|argument| self.pack_expression(argument));
        self.activation_path.extend(suspended_activations);
        self.function_arguments.push(frame);
        self.function_arguments.append(&mut suspended_frames);
        self.resume_context(context);
        lowered
    }

    pub(super) fn function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some((start, registered)) =
            self.emit_typed_pure_call(call, function, arguments, span)?
        {
            let range = typed_call_result_scalar_range(&registered, output as usize, span)?;
            if scalar >= range.len() {
                return Err(LowerError::contract(
                    "typed pure-call scalar result projection is out of range",
                    span,
                ));
            }
            return function_conditional_reg_offset(
                start,
                range.start + scalar,
                span,
                "typed pure-call result",
            );
        }
        let typed_assertions = self.register_root_pure_call(call, function, span)?;
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        let arguments: Vec<_> = arguments.iter().collect();
        let assertion = ActiveCallAssertion {
            call,
            function,
            arguments: arguments.clone(),
        };
        if self.call_action_compilation
            && self.active_call_assertions.contains(&assertion)
            && self.current_function_frame_matches(&assertion)
        {
            return self.expression(result, scalar);
        }
        let previous_suppression = self.suppress_function_assertions;
        self.suppress_function_assertions |= typed_assertions;
        self.enter_context(ScalarContextFrame::Function {
            parent: self.context_id,
            call,
            function,
            arguments: arguments.clone(),
        });
        self.function_arguments.push(FunctionArgumentsFrame {
            call,
            function,
            arguments,
            activation_base: self.activation_path.len(),
        });
        let owns_assertion = self.active_call_assertions.insert(assertion.clone());
        let lowered = if owns_assertion && !self.suppress_function_assertions {
            self.schedule_function_assertions(
                self.view
                    .function(function)
                    .expect("checked function identity resolves")
                    .statements(),
                span,
            )
        } else {
            Ok(())
        };
        let lowered = lowered.and_then(|()| self.expression(result, scalar));
        if owns_assertion {
            self.active_call_assertions.remove(&assertion);
        }
        self.function_arguments.pop();
        self.leave_context();
        self.suppress_function_assertions = previous_suppression;
        lowered
    }

    pub(super) fn pack_function_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        output: u32,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if let Some((start, registered)) =
            self.emit_typed_pure_call(call, function, arguments, span)?
        {
            let range = typed_call_result_scalar_range(&registered, output as usize, span)?;
            return function_conditional_reg_offset(
                start,
                range.start,
                span,
                "typed pure-call aggregate result",
            );
        }
        let typed_assertions = self.register_root_pure_call(call, function, span)?;
        if self.function_arguments.len() >= 256 {
            return Err(LowerError::non_computable(
                "aggregate function lowering exceeded the checked recursion limit",
                span,
            ));
        }
        let result = self.function_result(function, output, span)?;
        let arguments: Vec<_> = arguments.iter().collect();
        let assertion = ActiveCallAssertion {
            call,
            function,
            arguments: arguments.clone(),
        };
        if self.call_action_compilation
            && self.active_call_assertions.contains(&assertion)
            && self.current_function_frame_matches(&assertion)
        {
            return self.pack_expression(result);
        }
        let previous_suppression = self.suppress_function_assertions;
        self.suppress_function_assertions |= typed_assertions;
        self.enter_context(ScalarContextFrame::Function {
            parent: self.context_id,
            call,
            function,
            arguments: arguments.clone(),
        });
        self.function_arguments.push(FunctionArgumentsFrame {
            call,
            function,
            arguments,
            activation_base: self.activation_path.len(),
        });
        let owns_assertion = self.active_call_assertions.insert(assertion.clone());
        let lowered = if owns_assertion && !self.suppress_function_assertions {
            self.schedule_function_assertions(
                self.view
                    .function(function)
                    .expect("checked function identity resolves")
                    .statements(),
                span,
            )
        } else {
            Ok(())
        };
        let lowered = lowered.and_then(|()| self.pack_expression(result));
        if owns_assertion {
            self.active_call_assertions.remove(&assertion);
        }
        self.function_arguments.pop();
        self.leave_context();
        self.suppress_function_assertions = previous_suppression;
        lowered
    }

    fn function_name_for_diagnostic(&self, function: dae::FunctionId<'dae>) -> String {
        self.view
            .function(function)
            .map(|definition| definition.name().to_string())
            .unwrap_or_else(|| "<unresolved>".to_string())
    }

    fn register_root_pure_call(
        &self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        span: Span,
    ) -> Result<bool, LowerError> {
        if !self.function_arguments.is_empty() {
            return Ok(false);
        }
        let registered = self
            .layout
            .pure_calls
            .borrow_mut()
            .register_root(self.view, call)
            .map_err(|error| LowerError::contract(error.to_string(), span))?;
        let scheduled = self.active_clock.is_none() && !registered.assertions.is_empty();
        if scheduled {
            self.schedule_typed_pure_call_assertions(call, function, &registered, span)?;
        }
        Ok(scheduled)
    }

    pub(super) fn emit_typed_pure_call(
        &mut self,
        call: dae::ExprId<'dae>,
        function: dae::FunctionId<'dae>,
        arguments: dae::ExpressionOperands<'dae>,
        span: Span,
    ) -> Result<
        Option<(
            solve::Reg,
            crate::lower::typed_functions::RegisteredCall<'dae>,
        )>,
        LowerError,
    > {
        // A root call is represented by its issued compact owner in every
        // execution domain. Calls encountered while constructing an owner's
        // body remain on the nested-call path so construction establishes the
        // closed owner graph exactly once.
        if !self.function_arguments.is_empty() {
            return Ok(None);
        }
        let owner = match self.node(call).operation() {
            dae::ExpressionOperation::Call { owner, .. } => owner,
            _ => {
                return Err(LowerError::contract(
                    "typed pure-call projection has no issued call owner",
                    span,
                ));
            }
        };
        let key = (self.context_id, owner);
        if let Some((start, registered)) = self.typed_pure_call_cache.get(&key) {
            return Ok(Some((*start, registered.clone())));
        }
        let registered = self
            .layout
            .pure_calls
            .borrow_mut()
            .register_root(self.view, call)
            .map_err(|error| LowerError::contract(error.to_string(), span))?;
        if self.active_clock.is_none()
            && !self.call_action_compilation
            && registered.site.directional().is_none()
        {
            return Ok(None);
        }
        let function_id = function;
        let function = self.view.function(function_id).ok_or_else(|| {
            LowerError::contract("typed pure-call function does not resolve", span)
        })?;
        let arguments = arguments.iter().collect::<Vec<_>>();
        if arguments.len() != function.parameter_types().len() {
            return Err(LowerError::contract(
                "typed pure-call argument count does not match its function",
                span,
            ));
        }
        let mut input_starts = Vec::new();
        for (argument, value_type) in arguments
            .iter()
            .copied()
            .zip(function.parameter_types().iter())
        {
            self.pack_typed_call_input_leaves(argument, value_type, span, &mut input_starts)?;
        }
        if input_starts.len() != registered.site.inputs().len()
            || input_starts
                .iter()
                .zip(registered.site.inputs())
                .any(|(start, value_type)| {
                    (*start as usize)
                        .checked_add(value_type.scalar_count() as usize)
                        .is_none()
                })
        {
            return Err(LowerError::contract(
                "typed pure-call compact input ABI does not match its owner",
                span,
            ));
        }
        let output_count = registered
            .site
            .output_scalar_count()
            .ok_or_else(|| LowerError::contract("typed pure-call output width overflows", span))?;
        let start = self.next_register;
        for _ in 0..output_count {
            self.register(span)?;
        }
        self.ops.push(solve::LinearOp::PureCall {
            dst_start: start,
            input_starts: input_starts.into_boxed_slice(),
            site: registered.site.clone(),
        });
        self.typed_pure_call_cache
            .insert(key, (start, registered.clone()));
        if !self.call_action_compilation && !registered.assertions.is_empty() {
            self.schedule_typed_pure_call_assertions(call, function_id, &registered, span)?;
        }
        Ok(Some((start, registered)))
    }

    fn pack_typed_call_input_leaves(
        &mut self,
        argument: dae::ExprId<'dae>,
        value_type: dae::ValueTypeId<'dae>,
        span: Span,
        starts: &mut Vec<solve::Reg>,
    ) -> Result<(), LowerError> {
        let value = self.view.value_type(value_type).ok_or_else(|| {
            LowerError::contract("typed pure-call argument type does not resolve", span)
        })?;
        // A zero-size array holds no scalars, so the owner's interface holds no
        // leaf for it (see `lower_value_type_leaves`). Packing a start here
        // would make the call site one leaf wider than the owner it calls.
        if value.dimensions().contains(&0) {
            return Ok(());
        }
        if !value.is_record() {
            starts.push(self.pack_expression(argument)?);
            return Ok(());
        }
        if !value.dimensions().is_empty() {
            return Err(LowerError::contract(
                "typed pure-call record-array inputs are not construction-complete",
                span,
            ));
        }
        for field in 0..value.record_field_count() {
            let (_, field_type) = self.view.record_field(value_type, field).ok_or_else(|| {
                LowerError::contract("typed pure-call record field does not resolve", span)
            })?;
            let field_value = self.view.value_type(field_type).ok_or_else(|| {
                LowerError::contract("typed pure-call record field type does not resolve", span)
            })?;
            if field_value.is_record() {
                return Err(LowerError::contract(
                    "nested record typed pure-call inputs are not construction-complete",
                    span,
                ));
            }
            if field_value.dimensions().contains(&0) {
                continue;
            }
            starts.push(self.pack_record_field(argument, field, span)?);
        }
        Ok(())
    }

    /// Resolve the checked result definition one call lowers through.
    ///
    /// Solve executes only programs it owns. An MLS §12.9 external body is
    /// foreign code with no Solve op, so lowering fails with the call's exact
    /// provenance rather than emitting a substitute value.
    fn function_result(
        &self,
        function: dae::FunctionId<'dae>,
        output: u32,
        span: Span,
    ) -> Result<dae::ExprId<'dae>, LowerError> {
        let definition = self
            .view
            .function(function)
            .ok_or_else(|| LowerError::contract("function identity does not resolve", span))?;
        if let Some(external) = definition.external() {
            return Err(LowerError::non_computable(
                format!(
                    "external {} function `{}` calls `{}`, which the Solve runtime cannot execute",
                    external.language().as_str(),
                    definition.name(),
                    external.symbol()
                ),
                span,
            ));
        }
        definition
            .result_values()
            .rhs(output as usize)
            .ok_or_else(|| LowerError::contract("function result ordinal is out of range", span))
    }
}

fn typed_call_result_scalar_range(
    registered: &crate::lower::typed_functions::RegisteredCall<'_>,
    output: usize,
    span: Span,
) -> Result<std::ops::Range<usize>, LowerError> {
    let leaves = registered.result_ranges.get(output).ok_or_else(|| {
        LowerError::contract("typed pure-call result ordinal is out of range", span)
    })?;
    let scalar_offset = |leaf: usize| {
        registered.site.outputs()[..leaf]
            .iter()
            .try_fold(0usize, |count, output| {
                count.checked_add(output.value_type().scalar_count() as usize)
            })
    };
    let start = scalar_offset(leaves.start)
        .ok_or_else(|| LowerError::contract("typed pure-call result offset overflows", span))?;
    let end = scalar_offset(leaves.end)
        .ok_or_else(|| LowerError::contract("typed pure-call result offset overflows", span))?;
    Ok(start..end)
}

fn typed_call_record_field_scalar_range<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionId<'dae>,
    registered: &crate::lower::typed_functions::RegisteredCall<'dae>,
    output: usize,
    field: usize,
    span: Span,
) -> Result<std::ops::Range<usize>, LowerError> {
    let function = view.function(function).ok_or_else(|| {
        LowerError::contract("typed pure-call record function does not resolve", span)
    })?;
    let result_type = function.result_types().get(output).ok_or_else(|| {
        LowerError::contract("typed pure-call record result is out of range", span)
    })?;
    let record = view.value_type(result_type).ok_or_else(|| {
        LowerError::contract("typed pure-call record result type does not resolve", span)
    })?;
    if !record.is_record() || !record.dimensions().is_empty() {
        return Err(LowerError::contract(
            "typed pure-call field projection does not name one record",
            span,
        ));
    }
    let result_leaves = registered.result_ranges.get(output).ok_or_else(|| {
        LowerError::contract("typed pure-call record result is out of range", span)
    })?;
    for (leaf, ordinal) in (result_leaves.start..).zip(0..record.record_field_count()) {
        let (_, field_type) = view.record_field(result_type, ordinal).ok_or_else(|| {
            LowerError::contract("typed pure-call record field does not resolve", span)
        })?;
        let field_type = view.value_type(field_type).ok_or_else(|| {
            LowerError::contract("typed pure-call record field type does not resolve", span)
        })?;
        if field_type.is_record() {
            return Err(LowerError::contract(
                "nested record typed pure-call results are not construction-complete",
                span,
            ));
        }
        if ordinal == field {
            let start = typed_call_scalar_offset(registered, leaf).ok_or_else(|| {
                LowerError::contract("typed pure-call record field offset overflows", span)
            })?;
            let end = typed_call_scalar_offset(registered, leaf + 1).ok_or_else(|| {
                LowerError::contract("typed pure-call record field offset overflows", span)
            })?;
            return Ok(start..end);
        }
    }
    Err(LowerError::contract(
        "typed pure-call record field is out of range",
        span,
    ))
}

fn typed_call_scalar_offset(
    registered: &crate::lower::typed_functions::RegisteredCall<'_>,
    leaf: usize,
) -> Option<usize> {
    registered.site.outputs()[..leaf]
        .iter()
        .try_fold(0usize, |count, output| {
            count.checked_add(output.value_type().scalar_count() as usize)
        })
}
