//! Hash-consed symbolic values of scalar programs: the term table the
//! shared-value construction keys its values by and its checker evaluates
//! both sides over.

use std::collections::{BTreeMap, HashMap};

use super::super::*;
use super::registers::{
    RegisterTable, Role, read_registers, records_operand_offsets, renamable, visit_registers,
};

/// The id of one hash-consed symbolic value.
pub(super) type Term = usize;

/// An operation's interned shape and its operands as (field or register
/// offset, term): the structure a [`ValueKey`] stands for.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct OperationKey {
    shape: usize,
    operands: Vec<(usize, Term)>,
}

/// The hash-consed id of one operation's [`OperationKey`] and the first term of
/// its outputs. Each output of an operation is a term over this id, so the
/// operands of a wide operation are hashed once per operation, not once per
/// output, and its outputs are one block of consecutive terms that no table
/// holds an entry for.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(super) struct ValueKey {
    id: usize,
    base: Term,
    /// The number of outputs the key was issued with.
    width: usize,
}

/// The hash-consed terms and interned operation shapes of one derivation or
/// check.
#[derive(Default)]
pub(super) struct Terms {
    slots: HashMap<usize, Term>,
    operations: HashMap<OperationKey, ValueKey>,
    shapes: BTreeMap<String, usize>,
    /// The first term no value or slot owns yet.
    next: Term,
}

impl Terms {
    /// The term of solver slot `index` as the sequence found it.
    pub(super) fn slot(&mut self, index: usize) -> Term {
        let next = self.next;
        let term = *self.slots.entry(index).or_insert(next);
        self.next += usize::from(term == next);
        term
    }

    /// The key of the value of shape `shape` over `operands`, with `width`
    /// outputs.
    pub(super) fn key(
        &mut self,
        shape: &str,
        operands: Vec<(usize, Term)>,
        width: usize,
    ) -> ValueKey {
        let next = self.shapes.len();
        let shape = match self.shapes.get(shape) {
            Some(&id) => id,
            None => *self.shapes.entry(shape.to_string()).or_insert(next),
        };
        let next = ValueKey {
            id: self.operations.len(),
            base: self.next,
            width,
        };
        let key = *self
            .operations
            .entry(OperationKey { shape, operands })
            .or_insert(next);
        if key == next {
            self.next += width;
        }
        key
    }

    /// The term of output `output` of `key`; `None` when `output` is not
    /// below the width the key was issued with, since that term would belong
    /// to the next value.
    pub(super) fn value(key: &ValueKey, output: usize) -> Option<Term> {
        (output < key.width).then_some(key.base + output)
    }
}

/// The register-independent form of one value operation, derived once: its
/// shape with register fields normalized, and each operand as (field or
/// register offset, register). An operation whose sources are single
/// registers keys each operand in field order; any other keys every register
/// it reads by its offset from the lowest one, so a uniform shift of its
/// registers keeps the key.
#[derive(Clone, Debug)]
pub(super) struct ValueShape {
    pub(super) shape: String,
    pub(super) operands: Vec<(usize, Reg)>,
    /// Every register the operation reads, ascending.
    pub(super) reads: Vec<Reg>,
    /// Whether a source is a register range, which a segment gathers into
    /// the operation's own layout.
    pub(super) ranged: bool,
}

/// The shape of `op`; `None` when `op` cannot be validated at the top level.
pub(super) fn value_shape(op: &LinearOp) -> Option<ValueShape> {
    let reads = read_registers(op)?;
    if !renamable(op) {
        let lowest = reads.first().copied().unwrap_or(0);
        let operands = reads.iter().map(|&r| ((r - lowest) as usize, r)).collect();
        return Some(ValueShape {
            shape: format!("{op:?}"),
            operands,
            reads,
            ranged: true,
        });
    }
    let ranged = records_operand_offsets(op);
    let mut scalar = Vec::new();
    let mut probe = op.clone();
    visit_registers(&mut probe, &mut |role, register| {
        if role == Role::Scalar {
            scalar.push(*register);
        }
    });
    let lowest = reads.first().copied().unwrap_or(0);
    let mut normalized = op.clone();
    visit_registers(&mut normalized, &mut |role, register| {
        *register = match role {
            Role::Destination => 0,
            Role::Scalar if !ranged => 0,
            _ => *register - lowest,
        };
    });
    let operands = if ranged {
        reads.iter().map(|&r| ((r - lowest) as usize, r)).collect()
    } else {
        scalar.into_iter().enumerate().collect()
    };
    Some(ValueShape {
        shape: format!("{normalized:?}"),
        operands,
        reads,
        ranged,
    })
}

/// Symbolic evaluation of an ordered program sequence: the current term of
/// every solver slot, committed after each program.
#[derive(Default)]
pub(super) struct SymbolicSlots {
    slots: HashMap<usize, Term>,
}

impl SymbolicSlots {
    pub(super) fn slot(&self, terms: &mut Terms, index: usize) -> Term {
        match self.slots.get(&index) {
            Some(&term) => term,
            None => terms.slot(index),
        }
    }

    pub(super) fn commit(&mut self, targets: &[usize], outputs: &[Term]) {
        for (&target, &term) in targets.iter().zip(outputs) {
            self.slots.insert(target, term);
        }
    }

    pub(super) fn final_slots(&self) -> &HashMap<usize, Term> {
        &self.slots
    }

    /// The output terms of one program, committed to its targets; `None` when
    /// an operation cannot be evaluated or the output count is not the
    /// target count.
    pub(super) fn run(
        &mut self,
        terms: &mut Terms,
        ops: &[LinearOp],
        targets: &[usize],
    ) -> Option<Vec<Term>> {
        let mut registers = RegisterTable::<Term>::default();
        let mut outputs = Vec::with_capacity(targets.len());
        for op in ops {
            self.step(terms, op, &mut registers, &mut outputs)?;
        }
        if outputs.len() != targets.len() {
            return None;
        }
        self.commit(targets, &outputs);
        Some(outputs)
    }

    fn step(
        &self,
        terms: &mut Terms,
        op: &LinearOp,
        registers: &mut RegisterTable<Term>,
        outputs: &mut Vec<Term>,
    ) -> Option<()> {
        match *op {
            LinearOp::LoadY { dst, index } => {
                registers.insert(dst, self.slot(terms, index));
            }
            LinearOp::TensorLoad {
                dst_start,
                input: TensorInputKind::Y,
                input_start,
                count,
                seed_start: None,
                lanes: 1,
            } => {
                for offset in 0..count {
                    registers.insert(
                        dst_start + offset as Reg,
                        self.slot(terms, input_start + offset),
                    );
                }
            }
            LinearOp::Move { dst, src } => {
                let term = registers.get(src)?;
                registers.insert(dst, term);
            }
            LinearOp::StoreOutput { src } => outputs.push(registers.get(src)?),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    let register = start + (ordinal * stride) as Reg;
                    outputs.push(registers.get(register)?);
                }
            }
            _ => return value_step(terms, op, registers, outputs),
        }
        Some(())
    }
}

/// Any other operation: its outputs are the value terms of its key; fold
/// output stores append that many opaque terms.
fn value_step(
    terms: &mut Terms,
    op: &LinearOp,
    registers: &mut RegisterTable<Term>,
    outputs: &mut Vec<Term>,
) -> Option<()> {
    let shape = value_shape(op)?;
    let operands = shape
        .operands
        .iter()
        .map(|&(field, register)| Some((field, registers.get(register)?)))
        .collect::<Option<Vec<_>>>()?;
    let stored = match op {
        LinearOp::StoreOutputFunctionFold { count, .. } => *count,
        LinearOp::StoreOutputFoldTensorUpdate {
            dimensions, lanes, ..
        } => dimensions.iter().fold(*lanes, |count, &extent| {
            count.saturating_mul(extent as usize)
        }),
        _ => 0,
    };
    let written = op.dst_register().map_or(0, |_| op.dst_register_count());
    let key = terms.key(&shape.shape, operands, stored.max(written));
    for ordinal in 0..stored {
        outputs.push(Terms::value(&key, ordinal)?);
    }
    if let Some(start) = op.dst_register() {
        for offset in 0..written {
            registers.insert(start + offset as Reg, Terms::value(&key, offset)?);
        }
    }
    Some(())
}
