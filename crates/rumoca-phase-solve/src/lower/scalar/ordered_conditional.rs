//! Ordered function conditionals as one multi-target scalar conditional program.
//!
//! A source-ordered function conditional issues one output definition per
//! value every one of its branches completes. The scalar path lowers all of
//! them through one checked program whose targets are those outputs: each
//! branch region stores, for every output, the definition that branch ends
//! with. A definition issued inside the selected branch is computed in that
//! region, so an unselected branch never runs; a definition issued before the
//! conditional (the incoming value a missing `else` keeps) is a capture the
//! owner resolves ahead of the op.

use super::conditional_emission::{ConditionalOutcome, OrderedOutput, PendingConditionalEmission};
use super::functions::{
    PendingFunctionConditionalArm, PendingFunctionConditionalRegion,
    function_conditional_reg_offset,
};
use super::*;

const REQUIRES_TYPED_INVOCATION: &str =
    "ordered function conditional requires its checked typed invocation";

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    /// The register range of one output of an ordered function conditional.
    pub(super) fn pack_function_conditional_output(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        let context = self.owning_function_context(definition.id().function());
        if let Some(&register) = self
            .function_definition_aggregate_cache
            .get(&(context, definition.id()))
        {
            return Ok(register);
        }
        let (pending, offset) = self.prepare_ordered_conditional(definition, span)?;
        let block = self.emit_conditional(pending)?;
        function_conditional_reg_offset(block, offset, span, "ordered function conditional output")
    }

    /// One scalar of one output of an ordered function conditional.
    pub(super) fn function_conditional_output_scalar(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        scalar: usize,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        if scalar >= self.function_conditional_value_width(definition.rhs())? {
            return Err(LowerError::contract(
                "ordered function conditional scalar is out of range",
                span,
            ));
        }
        let start = self.pack_function_conditional_output(definition, span)?;
        function_conditional_reg_offset(start, scalar, span, "ordered function conditional scalar")
    }

    /// The checked program of the ordered conditional that issued `definition`
    /// and its capture sources, with the offset of `definition` in its result.
    pub(super) fn prepare_ordered_conditional(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        span: Span,
    ) -> Result<(PendingConditionalEmission<'dae>, usize), LowerError> {
        let dae::ExpressionOperation::FunctionConditionalOutput { conditional, .. } =
            self.node(definition.rhs()).operation()
        else {
            return Err(LowerError::contract(
                "ordered function conditional output has another form",
                span,
            ));
        };
        let function = definition.id().function();
        let context = self.owning_function_context(function);
        let conditional = self
            .view
            .function_conditional(conditional)
            .ok_or_else(|| LowerError::contract("function conditional does not resolve", span))?;
        let outputs = conditional.outputs().collect::<Vec<_>>();
        let (members, widths) = self.ordered_outputs(&outputs, span)?;
        let first = outputs
            .first()
            .ok_or_else(|| {
                LowerError::contract("ordered function conditional has no output", span)
            })?
            .rhs();
        let (program, sources) =
            self.issued_conditional_program(first, widths.clone(), context, span, |compiler| {
                compiler.ordered_conditional_regions(conditional, &outputs, &widths, context, span)
            })?;
        let offset = members
            .iter()
            .find(|member| member.definition == definition.id())
            .map(|member| member.offset)
            .ok_or_else(|| {
                LowerError::contract("ordered function conditional lost its output", span)
            })?;
        let pending = PendingConditionalEmission::checked(
            program,
            sources,
            span,
            ConditionalOutcome::Ordered {
                context,
                outputs: members,
            },
        )?;
        Ok((pending, offset))
    }

    /// Each output with its offset in the stacked result, and the widths.
    fn ordered_outputs(
        &self,
        outputs: &[dae::FunctionDefinitionView<'dae>],
        span: Span,
    ) -> Result<(Vec<OrderedOutput<'dae>>, Vec<usize>), LowerError> {
        let mut members = Vec::with_capacity(outputs.len());
        let mut widths = Vec::with_capacity(outputs.len());
        let mut offset = 0usize;
        for output in outputs {
            if self.node(output.rhs()).value_type().is_record() {
                return Err(LowerError::non_computable(REQUIRES_TYPED_INVOCATION, span));
            }
            let width = self.function_conditional_value_width(output.rhs())?;
            members.push(OrderedOutput {
                definition: output.id(),
                expression: output.rhs(),
                offset,
            });
            widths.push(width);
            offset = offset.checked_add(width).ok_or_else(|| {
                LowerError::contract("ordered function conditional width overflows", span)
            })?;
        }
        Ok((members, widths))
    }

    fn ordered_conditional_regions(
        &mut self,
        conditional: dae::FunctionConditionalBodyView<'dae>,
        outputs: &[dae::FunctionDefinitionView<'dae>],
        widths: &[usize],
        context: u64,
        span: Span,
    ) -> Result<
        (
            Vec<PendingFunctionConditionalArm<'dae>>,
            PendingFunctionConditionalRegion<'dae>,
        ),
        LowerError,
    > {
        let function = conditional.id().function();
        let conditions = conditional.conditions().collect::<Vec<_>>();
        let mut arms = Vec::with_capacity(conditions.len());
        for (ordinal, branch) in conditional.branches().enumerate() {
            let prior_inactive = conditions[..ordinal]
                .iter()
                .map(|condition| (*condition, false))
                .collect::<Vec<_>>();
            let condition = self
                .fork_for_function_conditional_region(function, context, &prior_inactive)
                .function_conditional_condition_program(conditions[ordinal])?;
            let mut selected = prior_inactive;
            selected.push((conditions[ordinal], true));
            let result = self
                .fork_for_function_conditional_region(function, context, &selected)
                .with_region_scope(branch.body_scope())
                .ordered_branch_result(branch, outputs, widths, span)?;
            arms.push((condition, result));
        }
        let all_inactive = conditions
            .iter()
            .map(|condition| (*condition, false))
            .collect::<Vec<_>>();
        let fallback_branch = conditional.fallback();
        let fallback = self
            .fork_for_function_conditional_region(function, context, &all_inactive)
            .with_region_scope(fallback_branch.body_scope())
            .ordered_branch_result(fallback_branch, outputs, widths, span)?;
        Ok((arms, fallback))
    }

    /// Store, for every output, the definition `branch` ends with.
    fn ordered_branch_result(
        mut self,
        branch: dae::FunctionBranchView<'dae>,
        outputs: &[dae::FunctionDefinitionView<'dae>],
        widths: &[usize],
        span: Span,
    ) -> Result<PendingFunctionConditionalRegion<'dae>, LowerError> {
        for (output, &count) in outputs.iter().zip(widths) {
            let Some(dae::FunctionAssignedState::Assigned(ended)) =
                branch.outgoing(output.target())
            else {
                return Err(LowerError::non_computable(REQUIRES_TYPED_INVOCATION, span));
            };
            let ended = self.view.function_definition(ended).ok_or_else(|| {
                LowerError::contract("ordered branch definition does not resolve", span)
            })?;
            let start = self.pack_function_definition(ended, span)?;
            self.emit(solve::LinearOp::StoreOutputRange {
                start,
                count,
                stride: 1,
            })?;
        }
        self.finish_function_conditional_region(span)
    }
}
