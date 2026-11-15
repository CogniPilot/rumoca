//! Non-wire execution obligations over the dependency owner's checked flow.
//!
//! The obligations of an owner are a dependency summary of one scalar: the
//! input coordinates whose values can decide whether execution meets a guard,
//! an assertion or a fault. Relations keep their coordinates, so a guard on
//! one lane of a vector input reads that lane alone. A branch or a fold that
//! the dependency flow derives charges its own obligations through its
//! captures; any other region conservatively covers its captures and control
//! when it can fail or observe an assertion. Presence is retained even for a
//! constant guard: its enclosing branch choice still decides whether execution
//! encounters that guard.

use super::dependency::{self, SolveCallDependency};
use super::{SolveOperation, SolveProgramConstructionError, SolvePureCallTableView, TypedProgram};
use crate::{FaultClass, SolveAssignedStateOperation, SolveValueType};

/// One input leaf the obligations read, with the ascending flat elements of
/// that leaf they read, `None` for every element.
pub(in crate::typed_program) type ExecutionReadElements = (usize, Option<Box<[usize]>>);

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(in crate::typed_program) struct ExecutionReads {
    /// Canonical summary of one scalar over the owner's input leaves.
    reads: Box<[SolveCallDependency]>,
    /// The input leaves `reads` names, ascending.
    inputs: Box<[usize]>,
    /// The elements `reads` names per leaf, listed once at construction for
    /// every structural walk of a call site.
    elements: Box<[ExecutionReadElements]>,
    present: bool,
}

impl ExecutionReads {
    pub(in crate::typed_program) fn recursive() -> Self {
        // Even a value-only recursive group can exhaust its issued depth bound.
        Self {
            present: true,
            ..Self::default()
        }
    }

    /// `input` gives the type of an input leaf; a leaf without one is read
    /// whole.
    fn new<'t>(
        reads: Box<[SolveCallDependency]>,
        present: bool,
        input: impl Fn(usize) -> Option<&'t SolveValueType>,
    ) -> Self {
        let elements = listed_elements(&reads, input);
        Self {
            inputs: elements.iter().map(|(leaf, _)| *leaf).collect(),
            reads,
            elements,
            present,
        }
    }

    /// Whole typed input leaves any obligation reads part of.
    pub(in crate::typed_program) fn inputs(&self) -> &[usize] {
        &self.inputs
    }

    /// The obligation summary, by input coordinate.
    pub(in crate::typed_program) fn reads(&self) -> &[SolveCallDependency] {
        &self.reads
    }

    /// Each input leaf the obligations read, with the elements they read.
    pub(in crate::typed_program) fn elements(&self) -> &[ExecutionReadElements] {
        &self.elements
    }

    pub(super) fn retain_recursive_obligation(&mut self) {
        self.present = true;
    }

    /// The same obligations over whole input leaves: a recursive group's
    /// fixed point iterates over that finite lattice.
    pub(super) fn widen(self) -> Self {
        let reads = dependency::widen(vec![self.reads].into_boxed_slice())
            .into_vec()
            .pop()
            .unwrap_or_default();
        Self::new(reads, self.present, |_| None)
    }

    pub(in crate::typed_program) fn has_obligations(&self) -> bool {
        self.present
    }
}

/// Each leaf `reads` names, with the ascending elements its relations list;
/// a relation whose elements cannot be listed covers its leaf.
fn listed_elements<'t>(
    reads: &[SolveCallDependency],
    input: impl Fn(usize) -> Option<&'t SolveValueType>,
) -> Box<[ExecutionReadElements]> {
    let mut leaves = std::collections::BTreeMap::<usize, Option<Vec<usize>>>::new();
    let read_leaves = reads
        .iter()
        .flat_map(|read| read.inputs().map(move |leaf| (read, leaf)));
    for (read, leaf) in read_leaves {
        let elements =
            input(leaf).and_then(|input| read.input_elements(&obligation_type(), 0, input));
        let entry = leaves.entry(leaf).or_insert_with(|| Some(Vec::new()));
        match (entry.take(), elements) {
            (Some(mut held), Some(elements)) => {
                held.extend(elements);
                *entry = Some(held);
            }
            _ => *entry = None,
        }
    }
    leaves
        .into_iter()
        .map(|(leaf, elements)| {
            let elements = elements.map(|mut elements| {
                elements.sort_unstable();
                elements.dedup();
                elements.into_boxed_slice()
            });
            (leaf, elements)
        })
        .collect()
}

/// The value type an obligation summary is stated for: one Boolean, whether
/// execution meets the obligation.
pub(super) fn obligation_type() -> SolveValueType {
    SolveValueType::scalar(crate::SolveScalarType::Boolean)
}

/// Construction-only union of obligation relations. Sealing is the sole
/// conversion.
#[derive(Default)]
pub(super) struct ExecutionReadBuilder {
    reads: Vec<SolveCallDependency>,
    present: bool,
}

impl ExecutionReadBuilder {
    /// Seal the obligations of `body`, whose first `input_count` slots are
    /// its inputs.
    pub(super) fn finish(self, body: &TypedProgram, input_count: usize) -> ExecutionReads {
        ExecutionReads::new(dependency::union(self.reads), self.present, |leaf| {
            body.slots()
                .get(leaf)
                .filter(|_| leaf < input_count)
                .map(|slot| slot.value_type())
        })
    }

    pub(super) fn observe(
        &mut self,
        body: &TypedProgram,
        index: usize,
        registers: &[Vec<SolveCallDependency>],
        assigned: &[Vec<SolveCallDependency>],
        available: SolvePureCallTableView<'_>,
    ) -> Result<(), SolveProgramConstructionError> {
        let spanned = &body.operations()[index];
        let provenance = spanned.provenance();
        let operation = spanned.operation();
        match operation {
            SolveOperation::Call {
                owner, arguments, ..
            } => {
                let callee = available.get(owner.index() as usize).ok_or(
                    SolveProgramConstructionError::InvalidCallInterface { provenance },
                )?;
                self.present |= callee.execution_reads.present;
                self.reads.extend(dependency::substitute_obligations(
                    callee.execution_reads.reads(),
                    callee.inputs,
                    |leaf| arguments.get(leaf).map(|argument| argument.index()),
                    registers,
                    provenance,
                )?);
                return Ok(());
            }
            // The dependency flow derives these regions and charges their
            // obligations through their captures (see `region`).
            SolveOperation::Conditional { .. } => return Ok(()),
            SolveOperation::Fold { state_captures, .. } if state_captures.is_empty() => {
                return Ok(());
            }
            _ => {}
        }
        if !has_operation_obligation(body, index, available) {
            return Ok(());
        }
        self.present = true;
        let mut error = None;
        operation.visit_input_registers(|register| {
            let dimensions = body.register_types()[register.index()].dimensions();
            self.extend(&registers[register.index()], dimensions, provenance, &mut error);
        });
        match operation {
            SolveOperation::AssignedConditional { program } => {
                for state in program.state_captures() {
                    let dimensions = body.assigned_state_types()[state.index()].dimensions();
                    self.extend(&assigned[state.index()], dimensions, provenance, &mut error);
                }
            }
            SolveOperation::Fold { state_captures, .. } => {
                for state in state_captures {
                    let dimensions = body.assigned_state_types()[state.index()].dimensions();
                    self.extend(&assigned[state.index()], dimensions, provenance, &mut error);
                }
            }
            SolveOperation::AssignedState {
                operation: SolveAssignedStateOperation::ProjectChecked { source, .. },
            } => {
                let dimensions = body.assigned_state_types()[source.index()].dimensions();
                self.extend(&assigned[source.index()], dimensions, provenance, &mut error);
            }
            _ => {}
        }
        error.map_or(Ok(()), Err)
    }

    /// A derived region's obligations: `present` when any of its executions
    /// can meet one, and what they read over the enclosing body's inputs.
    pub(super) fn region(&mut self, present: bool, reads: Vec<SolveCallDependency>) {
        self.present |= present;
        self.reads.extend(reads);
    }

    /// Every element of a value whose dependencies are `sources`.
    fn extend(
        &mut self,
        sources: &[SolveCallDependency],
        dimensions: &[u32],
        provenance: rumoca_core::Span,
        error: &mut Option<SolveProgramConstructionError>,
    ) {
        match dependency::whole_value_obligation(sources, dimensions, provenance) {
            Ok(reads) => self.reads.extend(reads),
            Err(failure) => {
                error.get_or_insert(failure);
            }
        }
    }
}

fn has_obligations(body: &TypedProgram, available: SolvePureCallTableView<'_>) -> bool {
    body.operations()
        .iter()
        .enumerate()
        .any(|(index, _)| has_operation_obligation(body, index, available))
}

fn has_operation_obligation(
    body: &TypedProgram,
    index: usize,
    available: SolvePureCallTableView<'_>,
) -> bool {
    match body.operations()[index].operation() {
        SolveOperation::Call { owner, .. } => available
            .get(owner.index() as usize)
            .is_none_or(|callee| callee.execution_reads.present),
        SolveOperation::CheckAssertion { .. } => true,
        SolveOperation::AssignedConditional { program } => {
            has_obligations(program.if_true().body(), available)
                || has_obligations(program.if_false().body(), available)
        }
        SolveOperation::Conditional {
            if_true, if_false, ..
        } => {
            has_obligations(if_true.body(), available)
                || has_obligations(if_false.body(), available)
        }
        SolveOperation::Map { body, .. } => has_obligations(body.body(), available),
        SolveOperation::Fold {
            transition,
            continuation,
            ..
        } => {
            has_obligations(transition.body(), available)
                || continuation
                    .as_ref()
                    .is_some_and(|region| has_obligations(region.body(), available))
        }
        // Local stores and assigned-state bookkeeping are not observable owner
        // effects. A checked projection can fail; its state is read above.
        SolveOperation::Store { .. } => false,
        SolveOperation::AssignedState { operation } => matches!(
            operation,
            SolveAssignedStateOperation::ProjectChecked { .. }
        ),
        _ => !matches!(body.fault_class(index), FaultClass::CannotFault),
    }
}
