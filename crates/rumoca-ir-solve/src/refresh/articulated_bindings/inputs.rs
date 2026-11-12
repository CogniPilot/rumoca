//! A bounded read-only channel relation, never a new executable expression IR.
use super::super::assignment_shape::producers::ProgramPrefix;
use crate::{LinearOp, Reg, TensorConcatenateSource, TensorInputKind, UnaryOp};

#[derive(Clone, Copy, Debug)]
pub(crate) enum SourceCell {
    Literal(f64),
    Storage {
        input: TensorInputKind,
        index: usize,
        negated: bool,
    },
}

impl SourceCell {
    fn negated(self) -> Self {
        match self {
            Self::Literal(value) => Self::Literal(-value),
            Self::Storage {
                input,
                index,
                negated,
            } => Self::Storage {
                input,
                index,
                negated: !negated,
            },
        }
    }
}

pub(super) fn cell(prefix: ProgramPrefix<'_>, register: Reg, depth: usize) -> Option<SourceCell> {
    if depth >= 32 {
        return None;
    }
    let position = prefix.producer_position(register)?;
    let source = prefix.before(position)?;
    match prefix.operation(position)? {
        LinearOp::Const { value, .. } if value.is_finite() => Some(SourceCell::Literal(*value)),
        LinearOp::LoadY { index, .. } => Some(storage(TensorInputKind::Y, *index)),
        LinearOp::LoadP { index, .. } => Some(storage(TensorInputKind::P, *index)),
        LinearOp::TensorLoad {
            dst_start,
            input,
            input_start,
            seed_start: None,
            lanes: 1,
            ..
        } => Some(storage(
            *input,
            input_start.checked_add(register.checked_sub(*dst_start)? as usize)?,
        )),
        LinearOp::Move { src, .. } => cell(source, *src, depth + 1),
        LinearOp::Unary {
            op: UnaryOp::Neg,
            arg,
            ..
        } => cell(source, *arg, depth + 1).map(SourceCell::negated),
        LinearOp::TensorConcatenate {
            dst_start,
            sources,
            dimensions,
            axis: 0,
            lanes: 1,
        } if dimensions.len() == 1 => cell(
            source,
            concatenated(sources, register.checked_sub(*dst_start)? as usize)?,
            depth + 1,
        ),
        _ => None,
    }
}

fn storage(input: TensorInputKind, index: usize) -> SourceCell {
    SourceCell::Storage {
        input,
        index,
        negated: false,
    }
}

fn concatenated(sources: &[TensorConcatenateSource], mut offset: usize) -> Option<Reg> {
    for source in sources {
        let count = source
            .dimensions
            .iter()
            .try_fold(1usize, |n, &d| n.checked_mul(d as usize))?;
        if offset < count {
            return source.start.checked_add(u32::try_from(offset).ok()?);
        }
        offset = offset.checked_sub(count)?;
    }
    None
}
