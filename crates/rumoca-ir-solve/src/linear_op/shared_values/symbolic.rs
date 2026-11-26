//! Hash-consed symbolic values of scalar programs: the term table the
//! shared-value construction keys its values by and its checker evaluates
//! both sides over.

use std::collections::HashMap;

use super::super::*;
use super::registers::RegisterTable;
use super::shape::{ShapeKey, value_shape};

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
    shapes: HashMap<ShapeKey, usize>,
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
        shape: &ShapeKey,
        operands: Vec<(usize, Term)>,
        width: usize,
    ) -> ValueKey {
        let next = self.shapes.len();
        let shape = match self.shapes.get(shape) {
            Some(&id) => id,
            None => *self.shapes.entry(shape.clone()).or_insert(next),
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

/// Symbolic evaluation of an ordered program sequence: the current term of
/// every solver slot, committed after each program.
#[derive(Default)]
pub(super) struct SymbolicSlots {
    recursive_conditionals: bool,
    slots: HashMap<usize, Term>,
}

impl SymbolicSlots {
    pub(super) fn value_proof() -> Self {
        Self {
            recursive_conditionals: true,
            ..Self::default()
        }
    }
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
    /// an operation cannot be represented by a term or the output count is not the
    /// target count.
    pub(super) fn run(
        &mut self,
        terms: &mut Terms,
        ops: &[LinearOp],
        targets: &[usize],
    ) -> Option<Vec<Term>> {
        self.run_transformed(terms, ops, targets, |_, op| {
            Some(std::borrow::Cow::Borrowed(op))
        })
    }

    pub(super) fn run_transformed<'a>(
        &mut self,
        terms: &mut Terms,
        ops: &'a [LinearOp],
        targets: &[usize],
        mut transform: impl FnMut(usize, &'a LinearOp) -> Option<std::borrow::Cow<'a, LinearOp>>,
    ) -> Option<Vec<Term>> {
        let outputs = self.derive_region_terms(terms, ops, &[], &mut transform)?;
        if outputs.len() != targets.len() {
            return None;
        }
        self.commit(targets, &outputs);
        Some(outputs)
    }

    // Derive structural identities; arithmetic remains an opaque operation key.
    fn derive_region_terms<'a>(
        &self,
        terms: &mut Terms,
        ops: &'a [LinearOp],
        captures: &[Term],
        transform: &mut impl FnMut(usize, &'a LinearOp) -> Option<std::borrow::Cow<'a, LinearOp>>,
    ) -> Option<Vec<Term>> {
        let mut registers = RegisterTable::<Term>::default();
        let mut outputs = Vec::new();
        for (index, op) in ops.iter().enumerate() {
            let projected = transform(index, op)?;
            self.step(terms, &projected, captures, &mut registers, &mut outputs)?;
        }
        Some(outputs)
    }

    fn derive_conditional_terms(
        &self,
        terms: &mut Terms,
        program: &FunctionConditionalProgram,
        capture_start: Reg,
        registers: &RegisterTable<Term>,
    ) -> Option<Vec<Term>> {
        let captures = (0..program.capture_count)
            .map(|offset| registers.get(capture_start.checked_add(Reg::try_from(offset).ok()?)?))
            .collect::<Option<Vec<_>>>()?;
        let derive_terms = |terms: &mut Terms, ops: &[LinearOp]| {
            self.derive_region_terms(terms, ops, &captures, &mut |_, op| {
                Some(std::borrow::Cow::Borrowed(op))
            })
        };
        let mut result = derive_terms(terms, &program.fallback)?;
        if result.len() != program.result_count {
            return None;
        }
        for arm in program.arms.iter().rev() {
            let condition = derive_terms(terms, &arm.condition)?;
            let selected = derive_terms(terms, &arm.result)?;
            if condition.len() != 1 || selected.len() != result.len() {
                return None;
            }
            for (found, selected) in result.iter_mut().zip(selected) {
                let key = terms.key(
                    &ShapeKey::Conditional,
                    vec![(0, condition[0]), (1, selected), (2, *found)],
                    1,
                );
                *found = Terms::value(&key, 0)?;
            }
        }
        Some(result)
    }

    fn step(
        &self,
        terms: &mut Terms,
        op: &LinearOp,
        captures: &[Term],
        registers: &mut RegisterTable<Term>,
        outputs: &mut Vec<Term>,
    ) -> Option<()> {
        match *op {
            LinearOp::LoadFunctionConditionalCapture { dst, index }
                if self.recursive_conditionals =>
            {
                registers.insert(dst, *captures.get(index)?);
            }
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                ref program,
            } if self.recursive_conditionals && program.owner.is_none() => {
                let result =
                    self.derive_conditional_terms(terms, program, capture_start, registers)?;
                for (offset, value) in result.into_iter().enumerate() {
                    registers.insert(dst_start.checked_add(Reg::try_from(offset).ok()?)?, value);
                }
            }
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
