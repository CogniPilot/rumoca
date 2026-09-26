//! Hash-consed symbolic values of scalar programs: the term table the
//! shared-value construction keys its values by and its checker evaluates
//! both sides over.

use std::collections::{BTreeMap, HashMap};

use super::super::*;
use super::registers::{Role, read_registers, renamable, visit_registers};

/// The id of one hash-consed symbolic value.
pub(super) type Term = usize;

/// One symbolic value.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum TermKey {
    /// A solver slot as the sequence found it.
    Slot(usize),
    /// Output `output` of the operation value `key`.
    Value { key: ValueKey, output: usize },
}

/// The value key of one operation: its interned shape and its operands as
/// (field or register offset, term).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(super) struct ValueKey {
    shape: usize,
    operands: Vec<(usize, Term)>,
}

/// The hash-consed terms and interned operation shapes of one derivation or
/// check.
#[derive(Default)]
pub(super) struct Terms {
    ids: HashMap<TermKey, Term>,
    shapes: BTreeMap<String, usize>,
}

impl Terms {
    fn intern(&mut self, key: TermKey) -> Term {
        let next = self.ids.len();
        *self.ids.entry(key).or_insert(next)
    }

    /// The key of the value of shape `shape` over `operands`.
    pub(super) fn key(&mut self, shape: &str, operands: Vec<(usize, Term)>) -> ValueKey {
        let next = self.shapes.len();
        let shape = match self.shapes.get(shape) {
            Some(&id) => id,
            None => *self.shapes.entry(shape.to_string()).or_insert(next),
        };
        ValueKey { shape, operands }
    }

    pub(super) fn value(&mut self, key: &ValueKey, output: usize) -> Term {
        self.intern(TermKey::Value {
            key: key.clone(),
            output,
        })
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
    let (mut scalar, mut ranged) = (Vec::new(), false);
    let mut probe = op.clone();
    visit_registers(&mut probe, &mut |role, register| match role {
        Role::Scalar => scalar.push(*register),
        Role::RangeStart => ranged = true,
        Role::Destination => {}
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
            None => terms.intern(TermKey::Slot(index)),
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
        let mut registers: HashMap<Reg, Term> = HashMap::new();
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
        registers: &mut HashMap<Reg, Term>,
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
                let term = *registers.get(&src)?;
                registers.insert(dst, term);
            }
            LinearOp::StoreOutput { src } => outputs.push(*registers.get(&src)?),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    let register = start + (ordinal * stride) as Reg;
                    outputs.push(*registers.get(&register)?);
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
    registers: &mut HashMap<Reg, Term>,
    outputs: &mut Vec<Term>,
) -> Option<()> {
    let shape = value_shape(op)?;
    let operands = shape
        .operands
        .iter()
        .map(|&(field, register)| Some((field, *registers.get(&register)?)))
        .collect::<Option<Vec<_>>>()?;
    let key = terms.key(&shape.shape, operands);
    let stored = match op {
        LinearOp::StoreOutputFunctionFold { count, .. } => *count,
        LinearOp::StoreOutputFoldTensorUpdate {
            dimensions, lanes, ..
        } => dimensions.iter().fold(*lanes, |count, &extent| {
            count.saturating_mul(extent as usize)
        }),
        _ => 0,
    };
    for ordinal in 0..stored {
        outputs.push(terms.value(&key, ordinal));
    }
    if let Some(start) = op.dst_register() {
        for offset in 0..op.dst_register_count() {
            registers.insert(start + offset as Reg, terms.value(&key, offset));
        }
    }
    Some(())
}
