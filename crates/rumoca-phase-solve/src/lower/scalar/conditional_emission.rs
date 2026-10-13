//! Emission of function-conditional programs and the definitions they capture.
//!
//! A function conditional reads earlier definitions of its function through
//! captures, and every capture must be packed before the conditional op that
//! reads it. When those definitions are conditionals themselves (a straight
//! chain `x1 := if .. x0 ..; x2 := if .. x1 ..;` in SSA form) resolving a
//! capture by a nested call would nest one emission per link, so the native
//! stack rather than the model would bound the chain.
//!
//! Emission is therefore a driver over an explicit stack of pending
//! emissions. A pending emission is a checked program with its ordered capture
//! sources; the driver resolves the sources of the top frame left to right and,
//! when a source is a definition that is still unpacked and is itself a
//! conditional, builds that definition's pending emission in the definition's
//! context and pushes it, so the stack depth is data on the heap and the call
//! depth is constant. A frame whose sources are all resolved emits its op and
//! the caches that the pack path of its owner would have written, so the
//! parent's resolution of the same source is a cache hit.

use super::functions::{FunctionConditionalRegisterRange, function_conditional_reg_offset};
use super::*;

/// The value-cache entries a finished conditional emission records.
pub(super) enum ConditionalOutcome<'dae> {
    /// One conditional expression; `definition` identifies its right-hand-side
    /// owner when reached through one.
    Value {
        expression: dae::ExprId<'dae>,
        cache_key: (u64, dae::ExprId<'dae>),
        definition: Option<(u64, dae::FunctionDefinitionId<'dae>)>,
    },
    /// The outputs of one ordered function conditional, in the order their
    /// values are stored in the program's result range.
    Ordered {
        context: u64,
        outputs: Vec<OrderedOutput<'dae>>,
    },
}

/// One output definition of an ordered function conditional and where its
/// value sits in the program's result range.
pub(super) struct OrderedOutput<'dae> {
    pub(super) definition: dae::FunctionDefinitionId<'dae>,
    pub(super) expression: dae::ExprId<'dae>,
    pub(super) offset: usize,
}

/// A checked conditional program whose captures are still to be resolved.
pub(super) struct PendingConditionalEmission<'dae> {
    program: Arc<solve::FunctionConditionalProgram>,
    sources: Vec<FunctionConditionalCaptureSource<'dae>>,
    span: Span,
    outcome: ConditionalOutcome<'dae>,
}

impl<'dae> PendingConditionalEmission<'dae> {
    pub(super) fn checked(
        program: Arc<solve::FunctionConditionalProgram>,
        sources: Vec<FunctionConditionalCaptureSource<'dae>>,
        span: Span,
        outcome: ConditionalOutcome<'dae>,
    ) -> Result<Self, LowerError> {
        let capture_count = sources.iter().try_fold(0usize, |count, source| {
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
        Ok(Self {
            program,
            sources,
            span,
            outcome,
        })
    }
}

/// A conditional that is either already packed or waits for its captures.
pub(super) enum PreparedConditional<'dae> {
    Packed(solve::Reg),
    Pending(PendingConditionalEmission<'dae>),
}

/// The context a pending emission's definition was built in, to be restored
/// once it is emitted.
type SuspendedContext = (u64, Vec<u64>);

/// What one driver step did.
enum Advance {
    /// A capture was resolved or a child frame pushed.
    Pending,
    /// The top frame emitted its op and register range.
    Emitted(solve::Reg),
}

struct EmissionFrame<'dae> {
    pending: PendingConditionalEmission<'dae>,
    ranges: Vec<FunctionConditionalRegisterRange>,
    suspended: Option<SuspendedContext>,
}

impl<'layout, 'dae> ScalarCompiler<'layout, 'dae> {
    /// Emit `root`, resolving its captures (and theirs) without call recursion.
    pub(super) fn emit_conditional(
        &mut self,
        root: PendingConditionalEmission<'dae>,
    ) -> Result<solve::Reg, LowerError> {
        let span = root.span;
        let mut frames = vec![EmissionFrame {
            pending: root,
            ranges: Vec::new(),
            suspended: None,
        }];
        let result = self.drive_conditional_frames(&mut frames, span);
        // An error leaves the definition contexts of the unfinished frames
        // switched; unwind them innermost first.
        while let Some(frame) = frames.pop() {
            if let Some(suspended) = frame.suspended {
                self.restore_context(suspended);
            }
        }
        result
    }

    fn drive_conditional_frames(
        &mut self,
        frames: &mut Vec<EmissionFrame<'dae>>,
        span: Span,
    ) -> Result<solve::Reg, LowerError> {
        loop {
            match self.advance_conditional_frame(frames, span)? {
                Advance::Pending => {}
                Advance::Emitted(start) if frames.is_empty() => return Ok(start),
                Advance::Emitted(_) => {}
            }
        }
    }

    /// One step of the driver: resolve the next capture of the top frame, push
    /// the frame of an unpacked definition it reads, or emit the top frame
    /// when all of its captures are resolved.
    fn advance_conditional_frame(
        &mut self,
        frames: &mut Vec<EmissionFrame<'dae>>,
        span: Span,
    ) -> Result<Advance, LowerError> {
        let Some(top) = frames.last() else {
            return Err(LowerError::contract(
                "function-conditional emission has no pending frame",
                span,
            ));
        };
        let Some(&source) = top.pending.sources.get(top.ranges.len()) else {
            return self.finish_top_frame(frames, span);
        };
        let source_span = top.pending.span;
        if let Some(child) = self.definition_emission_frame(source, source_span)? {
            frames.push(child);
            return Ok(Advance::Pending);
        }
        let range = self.resolve_function_conditional_capture(source, source_span)?;
        if let Some(top) = frames.last_mut() {
            top.ranges.push(range);
        }
        Ok(Advance::Pending)
    }

    fn finish_top_frame(
        &mut self,
        frames: &mut Vec<EmissionFrame<'dae>>,
        span: Span,
    ) -> Result<Advance, LowerError> {
        let Some(finished) = frames.pop() else {
            return Err(LowerError::contract(
                "function-conditional emission has no pending frame",
                span,
            ));
        };
        let EmissionFrame {
            pending,
            ranges,
            suspended,
        } = finished;
        let emitted = self.finish_conditional_emission(pending, &ranges);
        if let Some(suspended) = suspended {
            self.restore_context(suspended);
        }
        emitted.map(Advance::Emitted)
    }

    /// The pending emission of the definition a capture reads, built in that
    /// definition's context, when the definition is unpacked and is itself a
    /// conditional. Any other source resolves directly: it is cached, a
    /// capture slot, or not a conditional chain link.
    fn definition_emission_frame(
        &mut self,
        source: FunctionConditionalCaptureSource<'dae>,
        span: Span,
    ) -> Result<Option<EmissionFrame<'dae>>, LowerError> {
        let FunctionConditionalCaptureSource::DefinitionRange {
            context,
            definition,
            ..
        } = source
        else {
            return Ok(None);
        };
        let Some(definition) = self.view.function_definition(definition) else {
            return Ok(None);
        };
        if self.owning_function_context(definition.id().function()) != context
            || self.function_conditional_captures_definition(definition.id(), context)
            || self
                .function_definition_aggregate_cache
                .contains_key(&(context, definition.id()))
        {
            return Ok(None);
        }
        let suspended = self.switch_context(context);
        match self.pending_definition_emission(definition, context, span) {
            Ok(Some(pending)) => Ok(Some(EmissionFrame {
                pending,
                ranges: Vec::new(),
                suspended: Some(suspended),
            })),
            other => {
                self.restore_context(suspended);
                other.map(|_| None)
            }
        }
    }

    /// Mirror of the pack path of one unpacked definition (a group member, or a
    /// conditional right-hand side) up to, not including, its capture
    /// resolution. `None` when the definition has another form or is already
    /// packed.
    fn pending_definition_emission(
        &mut self,
        definition: dae::FunctionDefinitionView<'dae>,
        context: u64,
        span: Span,
    ) -> Result<Option<PendingConditionalEmission<'dae>>, LowerError> {
        let expression = definition.rhs();
        let node = self.node(expression);
        if matches!(
            node.operation(),
            dae::ExpressionOperation::FunctionConditionalOutput { .. }
        ) {
            return self
                .prepare_ordered_conditional(definition, span)
                .map(|(pending, _)| Some(pending));
        }
        let dae::ExpressionOperation::Conditional(operands) = node.operation() else {
            return Ok(None);
        };
        if node.value_type().is_record()
            || node.function_scope().is_none()
            || self
                .packed_expression_cache
                .contains_key(&(self.context_id, expression))
            || self
                .deferred_fold_captures
                .as_ref()
                .is_some_and(|captures| captures.packed_expressions.contains_key(&expression))
        {
            return Ok(None);
        }
        Ok(
            match self.prepare_lazy_conditional(
                expression,
                operands,
                Some((context, definition.id())),
                span,
            )? {
                PreparedConditional::Packed(_) => None,
                PreparedConditional::Pending(pending) => Some(pending),
            },
        )
    }

    /// Push the conditional op over the resolved capture `ranges` and record
    /// what the pack path of its owner records.
    fn finish_conditional_emission(
        &mut self,
        pending: PendingConditionalEmission<'dae>,
        ranges: &[FunctionConditionalRegisterRange],
    ) -> Result<solve::Reg, LowerError> {
        let PendingConditionalEmission {
            program,
            span,
            outcome,
            ..
        } = pending;
        let capture_start = self.pack_register_ranges(ranges, span)?;
        let start = self.register_range(program.result_count, span)?;
        self.emit(solve::LinearOp::FunctionConditional {
            dst_start: start,
            capture_start,
            program,
        })?;
        match outcome {
            ConditionalOutcome::Value {
                expression,
                cache_key,
                definition,
            } => {
                self.cache_lazy_conditional_value(expression, start, cache_key, span)?;
                if let Some(key) = definition {
                    self.function_definition_aggregate_cache.insert(key, start);
                }
            }
            ConditionalOutcome::Ordered { context, outputs } => {
                for output in outputs {
                    let register = function_conditional_reg_offset(
                        start,
                        output.offset,
                        span,
                        "ordered function conditional output",
                    )?;
                    self.packed_expression_cache
                        .insert((context, output.expression), register);
                    self.function_definition_aggregate_cache
                        .insert((context, output.definition), register);
                }
            }
        }
        Ok(start)
    }
}
