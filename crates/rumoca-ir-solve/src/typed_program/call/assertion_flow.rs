//! Restricted assertion-flow metadata derived from the canonical owner body.

use super::*;
use crate::{SolveRegisterId, SolveValueKind};

/// Exact supported assertion-check and forwarded-call publication of this owner.
///
/// Cloneable descriptive metadata only: this detached object grants no authority.
/// Observation projection borrows the exact issuing owner, not this object.
/// This proves neither model/action membership nor native/C25 admission.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedAssertionFlow {
    sources: Box<[AssertionSource]>,
}

/// A region coordinate, descriptive rather than model projection authority.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AssertionRegionKind {
    ConditionalThen,
    ConditionalElse,
    FoldContinuation,
    FoldTransition,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssertionRegionStep {
    operation: usize,
    kind: AssertionRegionKind,
}

impl AssertionRegionStep {
    #[must_use]
    pub fn operation(&self) -> usize {
        self.operation
    }
    #[must_use]
    pub fn kind(&self) -> AssertionRegionKind {
        self.kind
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssertionSource {
    regions: Box<[AssertionRegionStep]>,
    operation: usize,
}

impl AssertionSource {
    #[must_use]
    pub fn regions(&self) -> &[AssertionRegionStep] {
        &self.regions
    }
    #[must_use]
    pub fn operation(&self) -> usize {
        self.operation
    }
}

impl CheckedAssertionFlow {
    /// Canonical structural coordinates; execution order remains control-owned.
    #[must_use]
    pub fn sources(&self) -> &[AssertionSource] {
        &self.sources
    }
}

struct Publication {
    predicate: usize,
    condition: SolveRegisterId,
    messages: Vec<(usize, SolveRegisterId)>,
    depth: usize,
}

pub(super) fn derive(
    body: &TypedProgram,
    input_count: usize,
    outputs: &[SolvePureCallOutput],
    calls: SolvePureCallTableView<'_>,
) -> Option<CheckedAssertionFlow> {
    let mut publications = Vec::new();
    let mut sources = Vec::new();
    collect(body, outputs, calls, &[], &mut publications, &mut sources)?;
    let predicates = outputs
        .iter()
        .filter(|output| output.assertion_level().is_some())
        .count();
    if publications.len() != predicates
        || !publication_stores_match(body, input_count, outputs, &publications)
    {
        return None;
    }
    Some(CheckedAssertionFlow {
        sources: sources.into_boxed_slice(),
    })
}

fn collect(
    body: &TypedProgram,
    outputs: &[SolvePureCallOutput],
    calls: SolvePureCallTableView<'_>,
    regions: &[AssertionRegionStep],
    publications: &mut Vec<Publication>,
    sources: &mut Vec<AssertionSource>,
) -> Option<()> {
    let mut predicates_seen = std::collections::BTreeSet::new();
    predicates_seen.extend(publications.iter().map(|publication| publication.predicate));
    for (index, operation) in body.operations().iter().enumerate() {
        match operation.operation() {
            SolveOperation::CheckAssertion {
                predicate_output,
                condition,
                message_outputs,
                destinations,
                message,
                ..
            } => {
                if outputs.get(*predicate_output)?.assertion_level().is_none()
                    || !predicates_seen.insert(*predicate_output)
                    || !message_is_unobserved(message, calls)
                {
                    return None;
                }
                publications.push(Publication {
                    predicate: *predicate_output,
                    condition: *condition,
                    messages: message_outputs
                        .iter()
                        .copied()
                        .zip(destinations.iter().copied())
                        .collect(),
                    depth: regions.len(),
                });
                sources.push(AssertionSource {
                    regions: regions.into(),
                    operation: index,
                });
            }
            SolveOperation::Call {
                owner,
                destinations,
                assertion_forwarding,
                ..
            } => {
                let mut forwarded = forwarded_publications(
                    *owner,
                    destinations,
                    assertion_forwarding,
                    outputs,
                    calls,
                )?;
                for publication in &mut forwarded {
                    publication.depth = regions.len();
                }
                if !forwarded.is_empty() {
                    sources.push(AssertionSource {
                        regions: regions.into(),
                        operation: index,
                    });
                }
                if forwarded
                    .iter()
                    .any(|publication| !predicates_seen.insert(publication.predicate))
                {
                    return None;
                }
                publications.extend(forwarded);
            }
            operation @ (SolveOperation::Conditional { .. } | SolveOperation::Fold { .. }) => {
                for (kind, region) in child_regions(operation).into_iter().flatten() {
                    collect_region(
                        AssertionRegionStep {
                            operation: index,
                            kind,
                        },
                        region,
                        outputs,
                        calls,
                        regions,
                        publications,
                        sources,
                    )?;
                }
                predicates_seen
                    .extend(publications.iter().map(|publication| publication.predicate));
            }
            SolveOperation::Map { body, .. } => {
                unobserved_region(body, calls)?;
            }
            operation if !unobserved_operation(operation) => return None,
            _ => {}
        }
    }
    Some(())
}

fn child_regions(
    operation: &SolveOperation,
) -> [Option<(AssertionRegionKind, &crate::SolveProgramRegion)>; 2] {
    match operation {
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => [
            Some((AssertionRegionKind::ConditionalThen, if_true)),
            Some((AssertionRegionKind::ConditionalElse, if_false)),
        ],
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => [
            continuation
                .as_deref()
                .map(|region| (AssertionRegionKind::FoldContinuation, region)),
            Some((AssertionRegionKind::FoldTransition, transition)),
        ],
        _ => [None, None],
    }
}

fn collect_region(
    step: AssertionRegionStep,
    region: &crate::SolveProgramRegion,
    outputs: &[SolvePureCallOutput],
    calls: SolvePureCallTableView<'_>,
    regions: &[AssertionRegionStep],
    publications: &mut Vec<Publication>,
    sources: &mut Vec<AssertionSource>,
) -> Option<()> {
    let mut path = regions.to_vec();
    path.push(step);
    let first = publications.len();
    collect(region.body(), outputs, calls, &path, publications, sources)?;
    // Region-local message storage cannot escape as an ordinary region result.
    let local = publications[first..]
        .iter()
        .filter(|publication| publication.depth == path.len())
        .flat_map(|publication| {
            publication
                .messages
                .iter()
                .map(|(_, register)| register.index())
        })
        .collect::<std::collections::BTreeSet<_>>();
    if region.body().operations().iter().any(|operation| {
        let mut escape = false;
        operation
            .operation()
            .visit_input_registers(|input| escape |= local.contains(&input.index()));
        escape
    }) {
        return None;
    }
    Some(())
}

fn forwarded_publications(
    owner: SolvePureCallOwnerId,
    destinations: &[SolveRegisterId],
    forwarding: &[crate::SolveAssertionForwarding],
    outputs: &[SolvePureCallOutput],
    calls: SolvePureCallTableView<'_>,
) -> Option<Vec<Publication>> {
    let child = calls.get(owner.index() as usize)?;
    child.assertion_flow?;
    let count = child
        .outputs
        .iter()
        .filter(|output| output.assertion_level().is_some())
        .count();
    if count != forwarding.len() {
        return None;
    }
    Some(
        forwarding
            .iter()
            .map(|forwarding| {
                let child_messages =
                    assertion_message_outputs(child.outputs, forwarding.child_predicate());
                let parent_messages =
                    assertion_message_outputs(outputs, forwarding.parent_predicate());
                Publication {
                    predicate: forwarding.parent_predicate(),
                    condition: destinations[forwarding.child_predicate()],
                    messages: child_messages
                        .into_iter()
                        .zip(parent_messages)
                        .map(|(child_output, parent_offset)| {
                            (parent_offset, destinations[child_output])
                        })
                        .collect(),
                    depth: 0,
                }
            })
            .collect(),
    )
}

fn message_is_unobserved(
    message: &crate::SolveAssertionMessage,
    calls: SolvePureCallTableView<'_>,
) -> bool {
    match message {
        crate::SolveAssertionMessage::NoCaptures => true,
        crate::SolveAssertionMessage::Captures { program } => {
            unobserved_region(program, calls).is_some()
        }
    }
}

fn unobserved_region(
    region: &crate::SolveProgramRegion,
    calls: SolvePureCallTableView<'_>,
) -> Option<()> {
    let mut publications = Vec::new();
    let mut sources = Vec::new();
    collect(
        region.body(),
        &[],
        calls,
        &[],
        &mut publications,
        &mut sources,
    )?;
    (publications.is_empty() && sources.is_empty()).then_some(())
}

fn unobserved_operation(operation: &SolveOperation) -> bool {
    match operation {
        SolveOperation::CheckAssertion { .. }
        | SolveOperation::Conditional { .. }
        | SolveOperation::Map { .. }
        | SolveOperation::Fold { .. } => false,
        SolveOperation::Call { .. } => false,
        SolveOperation::Constant { .. }
        | SolveOperation::Load { .. }
        | SolveOperation::Store { .. }
        | SolveOperation::Unary { .. }
        | SolveOperation::Binary { .. }
        | SolveOperation::Compare { .. }
        | SolveOperation::Convert { .. }
        | SolveOperation::Select { .. }
        | SolveOperation::Scale { .. }
        | SolveOperation::BroadcastBinary { .. }
        | SolveOperation::Transpose { .. }
        | SolveOperation::MatrixMultiply { .. }
        | SolveOperation::Cross { .. }
        | SolveOperation::Reduce { .. }
        | SolveOperation::Identity { .. }
        | SolveOperation::Diagonal { .. }
        | SolveOperation::Concatenate { .. }
        | SolveOperation::Fill { .. }
        | SolveOperation::ConstructAggregate { .. }
        | SolveOperation::ProjectElement { .. }
        | SolveOperation::ProjectElementDynamic { .. }
        | SolveOperation::ProjectSlice { .. }
        | SolveOperation::ProjectView { .. }
        | SolveOperation::SelectElement { .. }
        | SolveOperation::UpdateElement { .. }
        | SolveOperation::UpdateSlice { .. }
        | SolveOperation::UpdateView { .. }
        | SolveOperation::LinearSolve { .. }
        | SolveOperation::Native { .. } => true,
    }
}

fn publication_stores_match(
    body: &TypedProgram,
    input_count: usize,
    outputs: &[SolvePureCallOutput],
    publications: &[Publication],
) -> bool {
    let constants = body
        .operations()
        .iter()
        .filter_map(|operation| match operation.operation() {
            SolveOperation::Constant { destination, value } => Some((destination.index(), value)),
            _ => None,
        })
        .collect::<std::collections::BTreeMap<_, _>>();
    let inactive = inactive_publications(outputs, publications);
    let true_registers = body
        .operations()
        .iter()
        .filter_map(|operation| match operation.operation() {
            SolveOperation::Constant { destination, value }
                if value.kind() == SolveValueKind::Boolean(true) =>
            {
                Some(destination.index())
            }
            _ => None,
        })
        .collect::<std::collections::BTreeSet<_>>();
    let predicates = publications
        .iter()
        .filter(|publication| publication.depth == 0)
        .map(|publication| (publication.predicate, publication.condition))
        .collect::<std::collections::BTreeMap<_, _>>();
    let messages = publications
        .iter()
        .filter(|publication| publication.depth == 0)
        .flat_map(|publication| publication.messages.iter().copied())
        .collect::<std::collections::BTreeMap<_, _>>();
    let destinations = messages
        .iter()
        .map(|(output, register)| (register.index(), *output))
        .collect::<std::collections::BTreeMap<_, _>>();
    body.operations().iter().all(|operation| {
        if let SolveOperation::Store { slot, source } = operation.operation()
            && let Some(output) = slot.index().checked_sub(input_count)
            && let Some(expected) = inactive.get(&output)
            && constants.get(&source.index()).copied() != Some(expected) {
            return false;
        }
        if let SolveOperation::Store { slot, source } = operation.operation()
            && let Some(output) = slot.index().checked_sub(input_count)
            && (predicates.get(&output).is_some_and(|condition| source != condition && !true_registers.contains(&source.index()))
                || messages.get(&output).is_some_and(|destination| source != destination)) {
            return false;
        }
        let mut valid_reads = true;
        operation.operation().visit_input_registers(|input| {
            if let Some(output) = destinations.get(&input.index()) {
                valid_reads &= matches!(operation.operation(), SolveOperation::Store { slot, source }
                    if slot.index() == input_count + output && *source == input);
            }
        });
        valid_reads
    })
}

fn inactive_publications(
    outputs: &[SolvePureCallOutput],
    publications: &[Publication],
) -> std::collections::BTreeMap<usize, crate::SolveValue> {
    publications
        .iter()
        .filter(|publication| publication.depth > 0)
        .flat_map(|publication| {
            std::iter::once((publication.predicate, crate::SolveValue::boolean(true))).chain(
                assertion_message_outputs(outputs, publication.predicate)
                    .into_iter()
                    .map(|output| {
                        (
                            output,
                            crate::SolveValue::inactive_assertion_message(
                                outputs[output].value_type().element_type(),
                            ),
                        )
                    }),
            )
        })
        .collect()
}
