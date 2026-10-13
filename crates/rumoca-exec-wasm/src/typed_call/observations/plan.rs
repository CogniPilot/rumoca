//! Capacity follows execution, while source descriptors follow compact structure.
use super::super::layout::FramePlan;
use super::*;

#[derive(Default, Clone)]
pub(super) struct SourceContext {
    regions: Vec<(usize, usize)>,
    loops: Vec<rumoca_core::StructuredIndexDomain>,
}

impl SourceContext {
    fn region(&self, operation: usize, arm: usize) -> Self {
        let mut context = self.clone();
        context.regions.push((operation, arm));
        context
    }

    fn frame(&self, owner: solve::SolvePureCallOwnerId, operation: usize) -> ObservationFrame {
        ObservationFrame {
            owner,
            operation,
            regions: self.regions.clone(),
            loops: self.loops.clone(),
            arguments: Vec::new(),
        }
    }
}

impl ObservationPlan {
    pub(super) fn collect(
        &mut self,
        owner: &solve::SolvePureCallOwner,
        program: &solve::TypedProgram,
        callees: &[Option<FramePlan>],
        context: &SourceContext,
    ) -> Result<u32, TypedCallCompileError> {
        program
            .operations()
            .iter()
            .enumerate()
            .try_fold(0, |capacity, (index, operation)| {
                add(
                    capacity,
                    self.operation(owner, index, operation, callees, context)?,
                )
            })
    }

    fn operation(
        &mut self,
        owner: &solve::SolvePureCallOwner,
        index: usize,
        spanned: &solve::SolveSpannedOperation,
        callees: &[Option<FramePlan>],
        context: &SourceContext,
    ) -> Result<u32, TypedCallCompileError> {
        match spanned.operation() {
            solve::SolveOperation::CheckAssertion {
                predicate_output,
                message_outputs,
                ..
            } => {
                self.direct(
                    owner,
                    context.frame(owner.id(), index),
                    *predicate_output,
                    message_outputs,
                    spanned.provenance(),
                )?;
                Ok(1)
            }
            solve::SolveOperation::Call {
                owner: child,
                assertion_forwarding,
                ..
            } => {
                let child = callees
                    .get(child.index() as usize)
                    .and_then(Option::as_ref)
                    .ok_or(TypedCallCompileError::SiteMismatch)?;
                self.call(
                    owner,
                    context.frame(owner.id(), index),
                    child,
                    assertion_forwarding,
                )
            }
            solve::SolveOperation::AssignedConditional { program } => self.arms(
                owner,
                index,
                [program.if_true(), program.if_false()],
                callees,
                context,
            ),
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => self.arms(owner, index, [if_true, if_false], callees, context),
            solve::SolveOperation::Fold {
                domain,
                transition,
                continuation,
                ..
            } => {
                let iterations = count(
                    domain
                        .scalar_count()
                        .map_err(|_| TypedCallCompileError::SizeLimit)?,
                )?;
                let mut nested = context.clone();
                nested.loops.push(domain.clone());
                let transition =
                    self.collect(owner, transition.body(), callees, &nested.region(index, 0))?;
                let continuation = continuation
                    .as_deref()
                    .map(|body| self.collect(owner, body.body(), callees, &nested.region(index, 1)))
                    .transpose()?
                    .unwrap_or(0);
                // The emitter checks the finite bound before continuation;
                // neither region executes in a zero-iteration domain.
                mul(iterations, add(transition, continuation)?)
            }
            // CheckedAssertionFlow excludes observed Map/message regions.
            _ => Ok(0),
        }
    }

    fn direct(
        &mut self,
        owner: &solve::SolvePureCallOwner,
        frame: ObservationFrame,
        predicate: usize,
        messages: &[usize],
        provenance: rumoca_core::Span,
    ) -> Result<(), TypedCallCompileError> {
        let captures = messages
            .iter()
            .map(|output| {
                Ok((
                    *output,
                    owner
                        .outputs()
                        .get(*output)
                        .ok_or(TypedCallCompileError::SiteMismatch)?
                        .value_type()
                        .clone(),
                ))
            })
            .collect::<Result<Vec<_>, TypedCallCompileError>>()?;
        let payload_bytes = captures
            .iter()
            .try_fold(mul(count(frame.loops.len())?, 8)?, |n, (_, ty)| {
                add(n, super::super::layout::bytes(ty)?)
            })?;
        self.sources.push(ObservationSource {
            predicate,
            #[cfg(any(test, all(target_arch = "wasm32", target_os = "unknown")))]
            origin_predicate: predicate,
            projected_captures: messages.to_vec(),
            provenance,
            frames: vec![frame],
            captures,
            payload_bytes,
        });
        count(self.sources.len())?;
        Ok(())
    }

    fn call(
        &mut self,
        owner: &solve::SolvePureCallOwner,
        frame: ObservationFrame,
        child: &FramePlan,
        forwarding: &[solve::SolveAssertionForwarding],
    ) -> Result<u32, TypedCallCompileError> {
        let Some(observed) = &child.observations else {
            return Ok(0);
        };
        let prefix_bytes = add(mul(count(frame.loops.len())?, 8)?, child.input_bytes)?;
        for (tag, source) in observed.sources.iter().enumerate() {
            let binding = forwarding
                .iter()
                .find(|binding| binding.child_predicate() == source.predicate)
                .ok_or(TypedCallCompileError::SiteMismatch)?;
            let mut source = source.clone();
            // Root input types are retained on every owner frame plan.
            source.frames[0].arguments = child.input_types.clone();
            source.frames.insert(0, frame.clone());
            source.predicate = binding.parent_predicate();
            source.projected_captures = owner
                .outputs()
                .iter()
                .enumerate()
                .filter_map(|(output, value)| {
                    (value.message_predicate_output(output) == Some(source.predicate))
                        .then_some(output)
                })
                .collect();
            if source.projected_captures.len() != source.captures.len() {
                return Err(TypedCallCompileError::SiteMismatch);
            }
            source.payload_bytes = add(prefix_bytes, source.payload_bytes)?;
            self.forwarding.push(ObservationForwarding {
                regions: frame.regions.clone(),
                operation: frame.operation,
                child_tag: tag,
                parent_tag: self.sources.len(),
                prefix_bytes,
            });
            self.sources.push(source);
            count(self.sources.len())?;
        }
        Ok(observed.capacity)
    }

    fn arms(
        &mut self,
        owner: &solve::SolvePureCallOwner,
        operation: usize,
        arms: [&solve::SolveProgramRegion; 2],
        callees: &[Option<FramePlan>],
        context: &SourceContext,
    ) -> Result<u32, TypedCallCompileError> {
        let left = self.collect(
            owner,
            arms[0].body(),
            callees,
            &context.region(operation, 0),
        )?;
        let right = self.collect(
            owner,
            arms[1].body(),
            callees,
            &context.region(operation, 1),
        )?;
        Ok(left.max(right))
    }
}
