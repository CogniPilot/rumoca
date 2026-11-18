//! Elide tensor materialization only when every reader is a checked gather.
//!
//! P is read-only throughout one emitted program; carried state is published
//! only after its programs complete. Register-version receipts prove that a
//! gather still reads the original load, not a later overwritten register.
use rumoca_ir_solve::{LinearOp, RegisterVersionProgram, TensorInputKind};
use std::collections::{BTreeMap, BTreeSet};

#[derive(Default)]
pub(in crate::emit) struct InputViews {
    gathers: BTreeMap<usize, (usize, usize)>,
    skipped: BTreeSet<usize>,
}

impl InputViews {
    pub(in crate::emit) fn derive(program: &[LinearOp]) -> Result<Self, String> {
        let Some(versions) = RegisterVersionProgram::derive(program) else {
            return Ok(Self::default());
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
        for (index, op) in versions.operations().iter().enumerate() {
            for piece in op.operands() {
                if !matches!(gathers.get(&index), Some((producer, _))
                    if *producer == piece.producer()
                    && matches!(program[index], LinearOp::LoadIndexedRegister { base, stride, .. }
                        if piece.selection() == (base as usize, stride)))
                {
                    skipped.remove(&piece.producer());
                }
            }
        }
        gathers.retain(|_, (producer, _)| skipped.contains(producer));
        Ok(Self { gathers, skipped })
    }

    pub(in crate::emit) fn skips(&self, operation: usize) -> bool {
        self.skipped.contains(&operation)
    }

    pub(in crate::emit) fn parameter_start(&self, operation: usize) -> Option<usize> {
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
    use rumoca_ir_solve::TensorIndex;

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
        let view = InputViews::derive(&source()).unwrap();
        assert!(view.skips(1));
        assert_eq!(view.parameter_start(2), Some(1));
    }

    #[test]
    fn another_reader_or_changed_source_keeps_materialization() {
        let mut program = source();
        program.push(LinearOp::StoreOutput { src: 1 });
        let view = InputViews::derive(&program).unwrap();
        assert!(!view.skips(1));
        assert_eq!(view.parameter_start(2), None);
        program = source();
        program.insert(2, LinearOp::Const { dst: 1, value: 73. });
        let view = InputViews::derive(&program).unwrap();
        assert!(!view.skips(1));
    }
}
