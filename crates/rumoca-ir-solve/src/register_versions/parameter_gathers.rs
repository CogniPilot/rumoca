//! Elide tensor materialization only when every reader is a checked gather.
//!
//! P is read-only throughout one emitted program; carried state is published
//! only after its programs complete. Register-version receipts prove that a
//! gather still reads the original load, not a later overwritten register.
use super::RegisterVersionProgram;
use crate::{LinearOp, TensorIndex, TensorInputKind};
use std::collections::{BTreeMap, BTreeSet};

/// Source-point proof that a checked gather can read its parameter input
/// directly and that no other operand needs the skipped register load.
#[derive(Default)]
pub struct BorrowedParameterGathers {
    gathers: BTreeMap<usize, (usize, usize)>,
    skipped: BTreeSet<usize>,
}

impl BorrowedParameterGathers {
    /// Issue the borrowed inputs for this exact linear program.
    pub fn derive(program: &[LinearOp]) -> Self {
        if !program
            .iter()
            .any(|op| matches!(op, LinearOp::LoadIndexedRegister { .. }))
        {
            return Self::default();
        }
        let Some(versions) = RegisterVersionProgram::derive(program) else {
            return Self::default();
        };
        let mut gathers = BTreeMap::new();
        let mut skipped = BTreeSet::new();
        for (index, op) in program.iter().enumerate() {
            if let Some((producer, input)) = input_view(program, &versions, index, op) {
                gathers.insert(index, (producer, input));
                skipped.insert(producer);
            }
        }
        // One other reader requires the original materialized value. In
        // particular an index operand is not the gather's array operand.
        let operands = versions
            .operations()
            .iter()
            .enumerate()
            .flat_map(|(index, op)| op.operands().iter().map(move |piece| (index, piece)));
        for (index, piece) in operands {
            let gather_input = matches!(program[index], LinearOp::LoadIndexedRegister { base, stride, .. }
                if piece.selection() == (base as usize, stride))
                && gathers
                    .get(&index)
                    .is_some_and(|(producer, _)| *producer == piece.producer());
            if !gather_input {
                skipped.remove(&piece.producer());
            }
        }
        gathers.retain(|_, (producer, _)| skipped.contains(producer));
        Self { gathers, skipped }
    }

    /// Whether the issued parameter view replaces this materialized load.
    pub fn skips(&self, operation: usize) -> bool {
        self.skipped.contains(&operation)
    }

    /// First parameter slot of the issued source for a checked gather.
    pub fn parameter_start(&self, operation: usize) -> Option<usize> {
        self.gathers.get(&operation).map(|(_, first)| *first)
    }
}

fn input_view(
    program: &[LinearOp],
    versions: &RegisterVersionProgram,
    index: usize,
    operation: &LinearOp,
) -> Option<(usize, usize)> {
    let LinearOp::LoadIndexedRegister {
        base,
        stride: 1,
        dimensions,
        indices,
        ..
    } = operation
    else {
        return None;
    };
    let start = *base as usize;
    let count = dimensions
        .iter()
        .try_fold(1usize, |n, d| n.checked_mul(*d as usize))?;
    let range = start..start.checked_add(count)?;
    // The dynamic index is a separate operand and still needs its register.
    // A one-element range can give both operands the same selection geometry.
    if indices.iter().any(|index| {
        matches!(index, TensorIndex::Runtime(register) if range.contains(&(*register as usize)))
    }) {
        return None;
    }
    let piece = versions.operations()[index]
        .operands()
        .iter()
        .find(|piece| piece.selection() == (start, 1) && piece.registers() == range)?;
    let LinearOp::TensorLoad {
        dst_start,
        input: TensorInputKind::P,
        input_start,
        seed_start: None,
        lanes: 1,
        ..
    } = program[piece.producer()]
    else {
        return None;
    };
    Some((
        piece.producer(),
        input_start.checked_add(start.checked_sub(dst_start as usize)?)?,
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn source() -> Vec<LinearOp> {
        vec![
            LinearOp::LoadP { dst: 0, index: 0 },
            LinearOp::TensorLoad {
                dst_start: 1,
                input: TensorInputKind::P,
                input_start: 1,
                count: 4096,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::LoadIndexedRegister {
                dst: 4097,
                base: 1,
                stride: 1,
                dimensions: vec![4096].into(),
                indices: vec![TensorIndex::Runtime(0)].into(),
            },
            LinearOp::StoreOutput { src: 4097 },
        ]
    }

    #[test]
    fn parameter_gather_borrows_the_checked_input_and_omits_the_whole_load() {
        let view = BorrowedParameterGathers::derive(&source());
        assert!(view.skips(1));
        assert_eq!(view.parameter_start(2), Some(1));
    }

    #[test]
    fn another_reader_or_changed_source_keeps_materialization() {
        let mut program = source();
        program.push(LinearOp::StoreOutput { src: 1 });
        let view = BorrowedParameterGathers::derive(&program);
        assert!(!view.skips(1));
        assert_eq!(view.parameter_start(2), None);
        program = source();
        program.insert(2, LinearOp::Const { dst: 1, value: 73. });
        let view = BorrowedParameterGathers::derive(&program);
        assert!(!view.skips(1));
    }

    #[test]
    fn the_parameter_load_remains_when_a_gather_uses_it_as_its_index() {
        let program = [
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::P,
                input_start: 0,
                count: 1,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::LoadIndexedRegister {
                dst: 1,
                base: 0,
                stride: 1,
                dimensions: vec![1].into(),
                indices: vec![TensorIndex::Runtime(0)].into(),
            },
            LinearOp::StoreOutput { src: 1 },
        ];
        let issued = BorrowedParameterGathers::derive(&program);
        assert!(!issued.skips(0));
        assert_eq!(issued.parameter_start(1), None);
    }
}
