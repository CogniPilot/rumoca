use super::*;

pub(super) fn construct<'program>(
    source: &'program TypedProgram,
    proposed_slots: &[SolveSlotId],
) -> Result<SolveParameterStageCandidate<'program>, SolveParameterStageError> {
    let proposed = validate_slots(source, proposed_slots)?;
    let mut analysis = Analysis::new(source, &proposed);
    for (index, operation) in source.operations.iter().enumerate() {
        analysis.observe(index, operation.operation());
    }
    Ok(analysis.finish())
}

fn validate_slots(
    source: &TypedProgram,
    proposed: &[SolveSlotId],
) -> Result<BTreeSet<usize>, SolveParameterStageError> {
    let mut slots = BTreeSet::new();
    for id in proposed {
        let slot = source
            .slots
            .get(id.index())
            .ok_or(SolveParameterStageError::UnknownSlot(*id))?;
        if slot.access != SolveSlotAccess::ReadOnly {
            return Err(SolveParameterStageError::MutableSlot(*id));
        }
        if !slot.storage.initialized_externally() {
            return Err(SolveParameterStageError::InternalSlot(*id));
        }
        if !slots.insert(id.index()) {
            return Err(SolveParameterStageError::DuplicateSlot(*id));
        }
    }
    Ok(slots)
}

struct Analysis<'program, 'slots> {
    source: &'program TypedProgram,
    proposed: &'slots BTreeSet<usize>,
    dependencies: Vec<Option<BTreeSet<usize>>>,
    definitions: Vec<Option<usize>>,
    staged: Vec<bool>,
}

impl<'program, 'slots> Analysis<'program, 'slots> {
    fn new(source: &'program TypedProgram, proposed: &'slots BTreeSet<usize>) -> Self {
        Self {
            source,
            proposed,
            dependencies: vec![None; source.register_types.len()],
            definitions: vec![None; source.register_types.len()],
            staged: vec![false; source.operations.len()],
        }
    }

    fn observe(&mut self, index: usize, operation: &SolveOperation) {
        let dependencies = self.dependencies_of(index, operation);
        operation.visit_output_registers(|register| {
            self.definitions[register.index()] = Some(index);
            self.dependencies[register.index()] = dependencies.clone();
        });
        if dependencies.as_ref().is_some_and(|slots| !slots.is_empty())
            && !matches!(operation, SolveOperation::Load { .. })
        {
            self.staged[index] = true;
        }
    }

    fn dependencies_of(&self, index: usize, operation: &SolveOperation) -> Option<BTreeSet<usize>> {
        if !self.source.is_prunable(index) || !supported(operation) {
            return None;
        }
        if let SolveOperation::Load { slot, .. } = operation {
            return self
                .proposed
                .contains(&slot.index())
                .then(|| BTreeSet::from([slot.index()]));
        }
        let mut dependencies = Some(BTreeSet::new());
        operation.visit_input_registers(|register| {
            dependencies = dependencies.take().and_then(|mut slots| {
                slots.extend(self.dependencies[register.index()].as_ref()?);
                Some(slots)
            });
        });
        dependencies
    }

    fn finish(mut self) -> SolveParameterStageCandidate<'program> {
        // Reverse source order closes the selected roots over their exact
        // register definitions. No operation is reconstructed or renumbered.
        for index in (0..self.staged.len()).rev() {
            if !self.staged[index] {
                continue;
            }
            self.stage_inputs(index);
        }
        let exported = self.exports();
        let values = self.exported_values(&exported);
        let mut stage = Vec::new();
        let mut call = Vec::new();
        let mut slots = BTreeSet::new();
        for (index, operation) in self.source.operations.iter().enumerate() {
            if !self.staged[index] {
                call.push(SolveParameterCallStep::Keep { operation: index });
                continue;
            }
            stage.push(index);
            if let SolveOperation::Load { slot, .. } = operation.operation() {
                slots.insert(slot.index());
            }
            append_loads(operation.operation(), index, &exported, &mut call);
        }
        SolveParameterStageCandidate {
            source: self.source,
            required_slots: slots
                .into_iter()
                .map(|index| self.source.slots[index].id)
                .collect(),
            stage: stage.into_boxed_slice(),
            values: values.into_boxed_slice(),
            call: call.into_boxed_slice(),
        }
    }

    fn stage_inputs(&mut self, index: usize) {
        self.source.operations[index]
            .operation()
            .visit_input_registers(|register| {
                if let Some(definition) = self.definitions[register.index()] {
                    self.staged[definition] = true;
                }
            });
    }

    fn exports(&self) -> Vec<usize> {
        let mut exports = BTreeSet::new();
        for (index, operation) in self.source.operations.iter().enumerate() {
            if self.staged[index] {
                continue;
            }
            self.export_inputs(operation.operation(), &mut exports);
        }
        exports.into_iter().collect()
    }

    fn export_inputs(&self, operation: &SolveOperation, exports: &mut BTreeSet<usize>) {
        operation.visit_input_registers(|register| {
            if self.definitions[register.index()].is_some_and(|index| self.staged[index]) {
                exports.insert(register.index());
            }
        });
    }

    fn exported_values(&self, exported: &[usize]) -> Vec<SolveParameterStageValue> {
        let mut values = Vec::with_capacity(exported.len());
        for (index, operation) in self.source.operations.iter().enumerate() {
            if !self.staged[index] {
                continue;
            }
            append_exports(operation.operation(), index, exported, &mut values);
        }
        values.sort_by_key(|value| value.register.index());
        values
    }
}

fn append_loads(
    operation: &SolveOperation,
    index: usize,
    exported: &[usize],
    call: &mut Vec<SolveParameterCallStep>,
) {
    operation.visit_output_registers(|register| {
        if let Ok(stage_slot) = exported.binary_search(&register.index()) {
            call.push(SolveParameterCallStep::StageLoad {
                operation: index,
                stage_slot,
            });
        }
    });
}

fn append_exports(
    operation: &SolveOperation,
    index: usize,
    exported: &[usize],
    values: &mut Vec<SolveParameterStageValue>,
) {
    operation.visit_output_registers(|register| {
        if exported.binary_search(&register.index()).is_ok() {
            values.push(SolveParameterStageValue {
                register,
                operation: index,
            });
        }
    });
}

/// A deliberate conservative boundary, read together with the canonical
/// fault certificate. Unhandled structured owners remain whole at call time.
fn supported(operation: &SolveOperation) -> bool {
    matches!(
        operation,
        SolveOperation::Constant { .. }
            | SolveOperation::Load { .. }
            | SolveOperation::Unary { .. }
            | SolveOperation::Binary { .. }
            | SolveOperation::Compare { .. }
            | SolveOperation::Select { .. }
            | SolveOperation::Fill { .. }
            | SolveOperation::Identity { .. }
            | SolveOperation::Transpose { .. }
            | SolveOperation::Diagonal { .. }
            | SolveOperation::ConstructAggregate { .. }
            | SolveOperation::Concatenate { .. }
            | SolveOperation::ProjectElement { .. }
            | SolveOperation::ProjectSlice { .. }
    )
}
